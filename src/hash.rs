// REF: https://github.com/python/cpython/blob/3.10/Lib/fractions.py#L637
//      https://github.com/python/cpython/blob/3.10/Python/pyhash.c#L92
//      https://docs.rs/num-rational/0.4.0/src/num_rational/lib.rs.html#394-408

use crate::NumHash;

use num_modular::ModularCoreOps;
use num_traits::Float;
use core::hash::{Hash, Hasher};

const M61: i64 = 0x1FFFFFFFFFFFFFFF; // largest mersenne prime under 2^64
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
macro_rules! impl_hash_for_small_sint {
    ($($signed:ty)*) => ($(
        impl NumHash for $signed {
            #[inline]
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                (&(*self as i64)).num_hash(state)
            }
        }
    )*);
}
impl_hash_for_small_sint! { i8 i16 i32 isize }

macro_rules! impl_hash_for_small_uint {
    ($($unsigned:ty)*) => ($(
        impl NumHash for $unsigned {
            #[inline]
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                (&(*self as u64)).num_hash(state)
            }
        }
    )*);
}
impl_hash_for_small_uint! { u8 u16 u32 usize }

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

// Case3: for rational(a, b), the hash is `hash(a * b^-1 mod M61)`
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

// Case4: for a + b*sqrt(r), the hash is `hash(a)` and `hash(b^2*r)`
#[cfg(feature = "num-complex")]
mod _num_complex {

}
