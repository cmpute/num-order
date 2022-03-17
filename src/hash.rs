//! We use the Mersenne prime 2^127-1 (i128::MAX) as the main modulo, which maximize the space of available hashing slots.
//! (The largest Mersenne prime under 2^64 is only 2^61-1, so we use u128 for hashing which is also future proof).
//! 
//! Note that modulo arithmetic with Mersenne prime can be implemented more efficiently, but currently we just use
//! the naive implementation from the `num-modular` crate.

// TODO: Migrate to MontegomeryMersenne for modular operations.
// REF: https://github.com/python/cpython/blob/3.10/Lib/fractions.py#L637
//      https://github.com/python/cpython/blob/3.10/Python/pyhash.c#L92
//      https://docs.rs/num-rational/0.4.0/src/num_rational/lib.rs.html#394-408

use crate::NumHash;

use num_modular::{ModularCoreOps, ModularOps};
use num_traits::Float;
use core::hash::{Hash, Hasher};

const M127: i128 = i128::MAX; // 127th Mersenne prime
const M127U: u128 = M127 as u128;
const M127D: u128 = M127U + M127U;
const P127: i128 = M127 - 24; // largest prime under M127
const HASH_INF: i128 = i128::MAX;
const HASH_NEGINF: i128 = i128::MIN;

// Case1: directly hash the i128 and u128 number (mod M127)
impl NumHash for i128 {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        const MINP1: i128 = i128::MIN + 1;
        match *self {
            i128::MAX | MINP1 => 0i128.hash(state),
            i128::MIN => (-1i128).hash(state),
            u => u.hash(state),
        }
    }
}
impl NumHash for u128 {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            u128::MAX => 1i128.hash(state),
            M127D => 0i128.hash(state),
            u if u >= M127U => ((u - M127U) as i128).hash(state),
            u => (u as i128).hash(state)
        }
    }
}

// Case2: convert other integers to 64 bit integer
macro_rules! impl_hash_for_small_int {
    ($($signed:ty)*) => ($(
        impl NumHash for $signed {
            #[inline]
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                (&(*self as i128)).hash(state) // these integers are always smaller than M127
            }
        }
    )*);
}
impl_hash_for_small_int! { i8 i16 i32 i64 u8 u16 u32 u64}

impl NumHash for usize {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        #[cfg(target_pointer_width = "32")]
        return (&(*self as u32)).num_hash(state);
        #[cfg(target_pointer_width = "64")]
        return (&(*self as u64)).num_hash(state);
    }
}

impl NumHash for isize {
    #[inline]
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        #[cfg(target_pointer_width = "32")]
        return (&(*self as i32)).num_hash(state);
        #[cfg(target_pointer_width = "64")]
        return (&(*self as i64)).num_hash(state);
    }
}

#[cfg(feature = "num-bigint")]
mod _num_bigint {
    use super::*;
    use num_bigint::{BigInt, BigUint};
    use num_traits::ToPrimitive;

    impl NumHash for BigUint {
        fn num_hash<H: Hasher>(&self, state: &mut H) {
            (self % BigUint::from(M127U)).to_i128().unwrap().hash(state)
        }
    }
    impl NumHash for BigInt {
        fn num_hash<H: Hasher>(&self, state: &mut H) {
            (self % BigInt::from(M127)).to_i128().unwrap().hash(state)
        }
    }
}

// Case3: for rational(a, b), the hash is `hash(a * b^-1 mod M127)` (b > 0)
macro_rules! impl_hash_for_float {
    ($($float:ty)*) => ($(
        impl NumHash for $float {
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                if self.is_nan() {
                    0i128.hash(state) // assign 0 to nan
                } else if self.is_infinite() {
                    if self.is_sign_positive() {
                        HASH_INF.hash(state)
                    } else {
                        HASH_NEGINF.hash(state)
                    }
                } else {
                    let (mantissa, exp, sign) = self.integer_decode();
                    // m * 2^e mod M127 = m * 2^(e mod 127) mod M127
                    let exp = if exp > 0 { (exp as u16) % 127 } else { ModularCoreOps::<u16>::negm(&(-exp as u16), &127) };
                    let v = (mantissa as u128).mulm(1u128 << exp, &M127U);
                    (v as i128 * sign as i128).num_hash(state);
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
                    let ub = *self.denom() as u128;
                    let binv = if ub % M127U != 0 {
                        ModularOps::<&u128>::invm(&ub, &M127U).unwrap()
                    } else {
                        HASH_NEGINF as u128 // no modular inverse, use NEGINF as the result
                    };

                    let ua = if self.numer() < &0 { (*self.numer() as u128).wrapping_neg() } else { *self.numer() as u128 };
                    let ab = ua.mulm(binv, &M127U);
                    if self.numer() >= &0 {
                        (ab as i128).hash(state)
                    } else {
                        (ab as i128).neg().hash(state)
                    }
                }
            }
        )*);
    }

    impl_hash_for_ratio!(i8 i16 i32 i64 i128 isize);
}

// Case4: for a + b*sqrt(r), the hash is `hash(a + P64^2*b^2*r)`, (a, b are rational numbers)
// The generalized version is that, hash of (a + b*r^k) will be `hash(a + P64^k*b^k*r)`
// TODO: how to deal with the sign of b?
#[cfg(feature = "num-complex")]
mod _num_complex {

}
