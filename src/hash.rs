//! We use the Mersenne prime 2^127-1 (i128::MAX) as the main modulo, which maximize the space of available hashing slots.
//! (The largest Mersenne prime under 2^64 is only 2^61-1, so we use u128 for hashing which is also future proof).
//! 
//! Note that modulo arithmetic with Mersenne prime can be implemented more efficiently, but currently we just use
//! the naive implementation from the `num-modular` crate.

use crate::NumHash;

use num_modular::{ModularAbs, FixedMersenneInt, ModularInteger};
use core::hash::{Hash, Hasher};

// we use 2^127 - 1 (a Mersenne prime) as modulus
type MInt = FixedMersenneInt::<127, 1>;
const M127: i128 = i128::MAX;
const M127U: u128 = M127 as u128;
const M127D: u128 = M127U + M127U;
#[cfg(feature = "num-complex")]
const PROOT: u128 = i32::MAX as u128; // a Mersenne prime
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

// Case3: for rational(a, b) including floating numbers, the hash is `hash(a * b^-1 mod M127)` (b > 0)
trait FloatHash {
    // Calculate mantissa * exponent^-1 mod M127
    fn fhash(&self) -> i128;
}

impl FloatHash for f32 {
    fn fhash(&self) -> i128 {
        let bits = self.to_bits();
        let sign_bit = bits >> 31;
        let mantissa_bits = bits & 0x7fffff;
        let mut exponent: i16 = ((bits >> 23) & 0xff) as i16;

        if exponent == 0xff {
            // deal with special floats
            if mantissa_bits != 0 { // nan
                0i128
            } else if sign_bit > 0 {
                HASH_NEGINF // -inf
            } else {
                HASH_INF // inf
            }
        } else {
            // then deal with normal floats
            let mantissa = if exponent == 0 {
                mantissa_bits << 1
            } else {
                mantissa_bits | 0x800000
            };
            exponent -= 0x7f + 23;
            
            // calculate hash
            let mantissa = MInt::new(mantissa as u128, &M127U);
            // m * 2^e mod M127 = m * 2^(e mod 127) mod M127
            let pow = mantissa.convert(1 << exponent.absm(&127));
            let v = mantissa * pow;
            v.residue() as i128 * if sign_bit == 0 { 1 } else { -1 }
        }
    }
}

impl NumHash for f32 {
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        self.fhash().num_hash(state)
    }
}

impl FloatHash for f64 {
    fn fhash(&self) -> i128 {
        let bits = self.to_bits();
        let sign_bit = bits >> 63;
        let mantissa_bits = bits & 0xfffffffffffff;
        let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;

        if exponent == 0x7ff {
            // deal with special floats
            if mantissa_bits != 0 { // nan
                0i128
            } else if sign_bit > 0 {
                HASH_NEGINF // -inf
            } else {
                HASH_INF // inf
            }
        } else {
            // deal with normal floats
            let mantissa = if exponent == 0 {
                mantissa_bits << 1
            } else {
                mantissa_bits | 0x10000000000000
            };
            // Exponent bias + mantissa shift
            exponent -= 0x3ff + 52;

            // calculate hash
            let mantissa = MInt::new(mantissa as u128, &M127U);
            // m * 2^e mod M127 = m * 2^(e mod 127) mod M127
            let pow = mantissa.convert(1 << exponent.absm(&127));
            let v = mantissa * pow;
            v.residue() as i128 * if sign_bit == 0 { 1 } else { -1 }
        }
    }
}

impl NumHash for f64 {
    fn num_hash<H: Hasher>(&self, state: &mut H) {
        self.fhash().num_hash(state)
    }
}

#[cfg(feature = "num-rational")]
mod _num_rational {
    use super::*;
    use core::ops::Neg;
    use num_rational::Ratio;

    macro_rules! impl_hash_for_ratio {
        ($($int:ty)*) => ($(
            impl NumHash for Ratio<$int> {
                fn num_hash<H: Hasher>(&self, state: &mut H) {
                    let ub = *self.denom() as u128; // denom is always positive in Ratio
                    let binv = if ub != M127U {
                        MInt::new(ub, &M127U).inv().unwrap()
                    } else {
                        // no modular inverse, use NEGINF as the result
                        MInt::new(HASH_NEGINF as u128, &M127U)
                    };

                    let ua = if self.numer() < &0 { (*self.numer() as u128).wrapping_neg() } else { *self.numer() as u128 };
                    let ua = binv.convert(ua);
                    let ab = (ua * binv).residue() as i128;
                    if self.numer() >= &0 {
                        ab.num_hash(state)
                    } else {
                        ab.neg().num_hash(state)
                    }
                }
            }
        )*);
    }

    impl_hash_for_ratio!(i8 i16 i32 i64 i128 isize);

    #[cfg(feature = "num-bigint")]
    mod _num_bigint {
        use super::*;
        use num_bigint::{BigInt, BigUint};
        use num_traits::{ToPrimitive, Zero, Signed};

        impl NumHash for Ratio<BigInt> {
            fn num_hash<H: Hasher>(&self, state: &mut H) {
                let ub = (self.denom().magnitude() % BigUint::from(M127U)).to_u128().unwrap();
                let binv = if !ub.is_zero() {
                    MInt::new(ub, &M127U).inv().unwrap()
                } else {
                    // no modular inverse, use NEGINF as the result
                    MInt::new(HASH_NEGINF as u128, &M127U)
                };

                let ua = (self.numer().magnitude() % BigUint::from(M127U)).to_u128().unwrap();
                let ua = binv.convert(ua);
                let ab = (ua * binv).residue() as i128;
                if self.numer().is_negative() {
                    ab.neg().num_hash(state)
                } else {
                    ab.num_hash(state)
                }
            }
        }
    }
}

// Case4: for a + b*sqrt(r) where a, b are rational numbers, the hash is
// - `hash(a + PROOT^2*b^2*r)` if b > 0
// - `hash(a - PROOT^2*b^2*r)` if b < 0
// The generalized version is that, hash of (a + b*r^(1/k)) will be `hash(a + PROOT^k*b^k*r)`
// Some Caveats:
// 1. if r = 1, the hash is not consistent with normal integer, but r = 1 is forbidden in QuadraticSurd
// 2. a - b*sqrt(r) and a + b*sqrt(-r) has the same hash, which is usually not a problem
#[cfg(feature = "num-complex")]
mod _num_complex {
    use super::*;
    use num_complex::Complex;

    macro_rules! impl_complex_hash_for_float {
        ($($float:ty)*) => ($(
            impl NumHash for Complex<$float> {
                fn num_hash<H: Hasher>(&self, state: &mut H) {
                    let a = self.re.fhash();
                    let b = self.im.fhash();

                    let bterm = if b >= 0 {
                        let pb = MInt::new(b as u128, &M127U) * PROOT;
                        -((pb * pb).residue() as i128)
                    } else {
                        let pb = MInt::new((-b) as u128, &M127U) * PROOT;
                        (pb * pb).residue() as i128
                    };
                    (a + bterm).num_hash(state)
                }
            }
        )*);
    }
    impl_complex_hash_for_float!(f32 f64);
}
