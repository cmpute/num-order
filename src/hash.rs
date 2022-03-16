// REF: https://github.com/python/cpython/blob/3.10/Lib/fractions.py#L637
//      https://github.com/python/cpython/blob/3.10/Python/pyhash.c#L92
//      https://docs.rs/num-rational/0.4.0/src/num_rational/lib.rs.html#394-408

use crate::NumHash;

use num_modular::{ModularCoreOps, ModularOps};
use num_traits::Float;
use core::hash::{Hash, Hasher};

const P64: u64 = 0xFFFFFFFFFFFFFFC5; // largest prime under 2^64
const M61: i64 = 0x1FFFFFFFFFFFFFFF; // largest mersenne prime under 2^63
const HASH_INF: i64 = i64::MAX;

// Case1: directly hash the i64 and u64 number
impl NumHash for i64 {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        (self % M61).hash(state)
    }
}
impl NumHash for u64 {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        ((self % M61 as u64) as i64).hash(state)
    }
}

// Case2: convert other integers to 64 bit integer
macro_rules! impl_hash_for_small_int {
    ($($signed:ty)*) => ($(
        impl NumHash for $signed {
            #[inline]
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                (&(*self as i64)).hash(state) // these integers are ensured to be smaller than M61
            }
        }
    )*);
}
impl_hash_for_small_int! { i8 i16 i32 u8 u16 u32 }

impl NumHash for usize {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        (&(*self as u64)).num_hash(state)
    }
}

impl NumHash for isize {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        (&(*self as i64)).num_hash(state)
    }
}

impl NumHash for u128 {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        ((self % M61 as u128) as i64).hash(state)
    }
}
impl NumHash for i128 {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        ((self % M61 as i128) as i64).hash(state)
    }
}

#[cfg(feature = "num-bigint")]
mod _num_bigint {
    use super::*;
    use num_bigint::{BigInt, BigUint};
    use num_traits::ToPrimitive;

    impl NumHash for BigUint {
        fn num_hash<H: Hasher>(&self, state: &mut H) {
            (self % BigUint::from(M61 as u64)).to_i64().unwrap().hash(state)
        }
    }
    impl NumHash for BigInt {
        fn num_hash<H: Hasher>(&self, state: &mut H) {
            (self % BigInt::from(M61)).to_i64().unwrap().hash(state)
        }
    }
}

// Case3: for rational(a, b), the hash is `hash(a * b^-1 mod M61)` (b > 0)
macro_rules! impl_hash_for_float {
    ($($float:ty)*) => ($(
        impl NumHash for $float {
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                if self.is_nan() {
                    0i64.num_hash(state) // assign 0 to nan
                } else if self.is_infinite() {
                    if self.is_sign_positive() {
                        HASH_INF.num_hash(state)
                    } else {
                        (-HASH_INF).num_hash(state)
                    }
                } else {
                    let (mantissa, exp, sign) = self.integer_decode();
                    // m * 2^e mod M61 = m * 2^(e mod 61) mod M61
                    let exp = if exp > 0 { (exp as u64) % 61 } else { ModularCoreOps::<u64>::negm(&(-exp as u64), &61) };
                    let v = mantissa.mulm(1u64 << ((exp as u64) % 61), &(M61 as u64));
                    (v as i64 * sign as i64).hash(state);
                }
            }
        }
    )*);
}
impl_hash_for_float! { f32 f64 }

#[cfg(feature = "num-rational")]
mod _num_rational {
    use super::*;
    use core::ops::Neg;
    use num_rational::Ratio;

    macro_rules! impl_hash_for_ratio {
        ($($int:ty)*) => ($(
            impl NumHash for Ratio<$int> {
                fn num_hash<H: Hasher>(&self, state: &mut H) {
                    let ub = *self.denom() as u64;
                    let binv = if ub % M61 as u64 != 0 {
                        ModularOps::<&u64>::invm(&ub, &(M61 as u64)).unwrap()
                    } else {
                        HASH_INF as u64 // no modular inverse, use INF as the result
                    };

                    let ua = if self.numer() < &0 { (*self.numer() as u64).wrapping_neg() } else { *self.numer() as u64 };
                    let ab = (ua % M61 as u64).mulm(binv, &(M61 as u64));
                    if self.numer() >= &0 {
                        (ab as i64).hash(state)
                    } else {
                        (ab as i64).neg().hash(state)
                    }
                }
            }
        )*);
    }

    impl_hash_for_ratio!(i8 i16 i32 i64 isize);

    impl NumHash for Ratio<i128> {
        fn num_hash<H: Hasher>(&self, state: &mut H) {
            let ub = (self.denom() % M61 as i128) as u64;
            let binv = if ub > 0 {
                ModularOps::<&u64>::invm(&ub, &(M61 as u64)).unwrap()
            } else {
                HASH_INF as u64 // no modular inverse, use INF as the result
            };
            let ua = if self.numer() < &0 { (*self.numer() as u128).wrapping_neg() } else { *self.numer() as u128 };
            let ua = (ua % M61 as u128) as u64;
            let ab = ua.mulm(&binv, &(M61 as u64));
            if self.numer() >= &0 {
                (ab as i64).hash(state)
            } else {
                (ab as i64).neg().hash(state)
            }
        }
    }
}

// Case4: for a + b*sqrt(r), the hash is `hash(a + P64^2*b^2*r)`, (a, b are rational numbers)
// The generalized version is that, hash of (a + b*r^k) will be `hash(a + P64^k*b^k*r)`
#[cfg(feature = "num-complex")]
mod _num_complex {

}
