use crate::NumOrd;
use core::convert::TryFrom;
use core::cmp::Ordering;

// Case0: swap operand, this introduces overhead so only used for non-primitive types
#[allow(unused_macros)]
macro_rules! impl_ord_by_swap {
    ($($t1:ty | $t2:ty;)*) => ($(
        impl NumOrd<$t2> for $t1 {
            #[inline]
            fn num_partial_cmp(&self, other: &$t2) -> Option<Ordering> {
                other.num_partial_cmp(self).map(Ordering::reverse)
            }
        }
    )*);
}

// Case1: forward to builtin operator for same types
macro_rules! impl_ord_equal_types {
    ($($t:ty)*) => ($(
        impl NumOrd<$t> for $t {
            #[inline]
            fn num_partial_cmp(&self, other: &$t) -> Option<Ordering> {
                self.partial_cmp(&other)
            }
        }
    )*);
}

mod _primitive {
    use super::*;

    impl_ord_equal_types! {
        u8 u16 u32 u64 u128 usize
        i8 i16 i32 i64 i128 isize
        f32 f64
    }

    // Case2: forward to same types by safe casting
    macro_rules! impl_ord_by_casting {
        ($($small:ty => $big:ty;)*) => ($(
            impl NumOrd<$small> for $big {
                #[inline]
                fn num_partial_cmp(&self, other: &$small) -> Option<Ordering> {
                    self.partial_cmp(&<$big>::from(*other))
                }
            }
            impl NumOrd<$big> for $small {
                #[inline]
                fn num_partial_cmp(&self, other: &$big) -> Option<Ordering> {
                    <$big>::from(*self).partial_cmp(other)
                }
            }
        )*);
    }

    impl_ord_by_casting! {
        // uN, uM for N < M
        u8  => u128; u8  => u64; u8  => u32; u8 => u16;
        u16 => u128; u16 => u64; u16 => u32;
        u32 => u128; u32 => u64;
        u64 => u128;

        // iN, iM for N > M
        i8  => i128; i8  => i64; i8  => i32; i8 => i16;
        i16 => i128; i16 => i64; i16 => i32;
        i32 => i128; i32 => i64;
        i64 => i128;

        // iN, uM for N > M
        u8  => i128; u8  => i64; u8  => i32; u8 => i16;
        u16 => i128; u16 => i64; u16 => i32;
        u32 => i128; u32 => i64;
        u64 => i128;

        // fN, fM for N > M
        f32 => f64;

        // f32, uM for 24 >= M, since f32 can exactly represent all integers (-2^24,2^24)
        // f64, uM for 53 >= M, since f64 can exactly represent all integers (-2^53,2^53)
        u8 => f32; u16 => f32;
        u8 => f64; u16 => f64; u32 => f64;

        // f32, iM for 24 >= M
        // f64, iM for 53 >= M
        // since iM's range [-2^(M-1),2^(M-1)) includes -2^(M-1), bounds do not change
        i8 => f32; i16 => f32;
        i8 => f64; i16 => f64; i32 => f64;
    }

    // Case3: trivial logic for comparing signed and unsigned integers
    macro_rules! impl_ord_between_diff_sign {
        ($($signed:ty => $unsigned:ty;)*) => ($(
            impl NumOrd<$signed> for $unsigned {
                #[inline]
                fn num_partial_cmp(&self, other: &$signed) -> Option<Ordering> {
                    if other < &0 {
                        Some(Ordering::Greater)
                    } else {
                        self.partial_cmp(&<$unsigned>::try_from(*other).unwrap())
                    }
                }
            }
            impl NumOrd<$unsigned> for $signed {
                #[inline]
                fn num_partial_cmp(&self, other: &$unsigned) -> Option<Ordering> {
                    if self < &0 {
                        Some(Ordering::Less)
                    } else {
                        <$unsigned>::try_from(*self).unwrap().partial_cmp(other)
                    }
                }
            }
        )*);
    }

    impl_ord_between_diff_sign! {
        i8   => u128; i8  => u64; i8  => u32 ; i8  => u16; i8 => u8;
        i16  => u128; i16 => u64; i16 => u32 ; i16 => u16;
        i32  => u128; i32 => u64; i32 => u32 ;
        i64  => u128; i64 => u64;
        i128 => u128; isize => usize;
    }

    // Case4: special handling for comparing float and integer types
    // Note: if `a` is an integer, `a cmp b` equals to `(a, trunc(b)) cmp (trunc(b), b)` (lexicographically)
    #[cfg(feature = "libm")]
    trait Trunc {
        fn trunc(self) -> Self;
    }
    #[cfg(feature = "libm")]
    impl Trunc for f32 {
        fn trunc(self) -> Self {
            libm::truncf(self)
        }
    }
    #[cfg(feature = "libm")]
    impl Trunc for f64 {
        fn trunc(self) -> Self {
            libm::trunc(self)
        }
    }

    macro_rules! impl_ord_between_int_float {
        ($($float:ty | $int:ty;)*) => ($(
            impl NumOrd<$float> for $int {
                #[inline]
                fn num_partial_cmp(&self, other: &$float) -> Option<Ordering> {
                    if other.is_nan() {
                        None
                    } else if other < &(<$int>::MIN as $float) { // integer min is on binary boundary
                        Some(Ordering::Greater)
                    } else if other >= &(<$int>::MAX as $float) { // integer max is not on binary boundary
                        Some(Ordering::Less)
                    } else {
                        let trunc = other.trunc();
                        (self, &trunc).partial_cmp(&(&(trunc as $int), other))
                    }
                }
            }
            impl NumOrd<$int> for $float {
                #[inline]
                fn num_partial_cmp(&self, other: &$int) -> Option<Ordering> {
                    if self.is_nan() {
                        None
                    } else if self < &(<$int>::MIN as $float) { // integer min is on binary boundary
                        Some(Ordering::Less)
                    } else if self >= &(<$int>::MAX as $float) { // integer max is not on binary boundary
                        Some(Ordering::Greater)
                    } else {
                        let trunc = self.trunc();
                        (&(trunc as $int), self).partial_cmp(&(other, &trunc))
                    }
                }
            }
        )*);
    }

    impl_ord_between_int_float! {
        f32|u128; f32|i128; f32|u64; f32|i64; f32|u32; f32|i32;
        f64|u128; f64|i128; f64|u64; f64|i64;
    }

    // Case5: forward size integers to corresponding concrete types
    macro_rules! impl_ord_with_size_types {
        ($($t:ty)*) => ($(
            impl NumOrd<$t> for usize {
                #[inline]
                fn num_partial_cmp(&self, other: &$t) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    { (*self as u32).num_partial_cmp(other) }
                    #[cfg(target_pointer_width = "64")]
                    { (*self as u64).num_partial_cmp(other) }
                }
            }
            impl NumOrd<usize> for $t {
                #[inline]
                fn num_partial_cmp(&self, other: &usize) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    { self.num_partial_cmp(&(*other as u32)) }
                    #[cfg(target_pointer_width = "64")]
                    { self.num_partial_cmp(&(*other as u64)) }
                }
            }
            impl NumOrd<$t> for isize {
                #[inline]
                fn num_partial_cmp(&self, other: &$t) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    { (*self as i32).num_partial_cmp(other) }
                    #[cfg(target_pointer_width = "64")]
                    { (*self as i64).num_partial_cmp(other) }
                }
            }
            impl NumOrd<isize> for $t {
                #[inline]
                fn num_partial_cmp(&self, other: &isize) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    { self.num_partial_cmp(&(*other as i32)) }
                    #[cfg(target_pointer_width = "64")]
                    { self.num_partial_cmp(&(*other as i64)) }
                }
            }
        )*);
    }

    impl_ord_with_size_types!(u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64);
}

// Case6: separate handling for special types
#[cfg(feature = "num-bigint")]
mod _num_bigint {
    use super::*;
    use num_bigint::{BigInt, BigUint};
    use num_traits::{FromPrimitive, Signed};

    impl_ord_equal_types!(BigInt BigUint);
    impl_ord_by_casting! {
        u8 => BigUint; u16 => BigUint; u32 => BigUint; u64 => BigUint; u128 => BigUint;
        i8 => BigInt; i16 => BigInt; i32 => BigInt; i64 => BigInt; i128 => BigInt;
        u8 => BigInt; u16 => BigInt; u32 => BigInt; u64 => BigInt; u128 => BigInt;
    }
    impl_ord_between_diff_sign! {
        i8 => BigUint; i16 => BigUint; i32 => BigUint; i64 => BigUint; i128 => BigUint;
    }
    impl_ord_with_size_types!(BigInt BigUint);

    // specialized implementations
    impl NumOrd<f32> for BigUint {
        #[inline]
        fn num_partial_cmp(&self, other: &f32) -> Option<Ordering> {
            if other.is_nan() {
                None
            } else if other < &0. {
                Some(Ordering::Greater)
            } else if other.is_infinite() && other.is_sign_positive() {
                Some(Ordering::Less)
            } else {
                let trunc = other.trunc();
                (self, &trunc).partial_cmp(&(&BigUint::from_f32(trunc).unwrap(), other))
            }
        }
    }
    impl NumOrd<f64> for BigUint {
        #[inline]
        fn num_partial_cmp(&self, other: &f64) -> Option<Ordering> {
            if other.is_nan() {
                None
            } else if other < &0. {
                Some(Ordering::Greater)
            } else if other.is_infinite() && other.is_sign_positive() {
                Some(Ordering::Less)
            } else {
                let trunc = other.trunc();
                (self, &trunc).partial_cmp(&(&BigUint::from_f64(trunc).unwrap(), other))
            }
        }
    }
    impl NumOrd<f32> for BigInt {
        #[inline]
        fn num_partial_cmp(&self, other: &f32) -> Option<Ordering> {
            if other.is_nan() {
                None
            } else if other.is_infinite() {
                if other.is_sign_positive() {
                    Some(Ordering::Less)
                } else {
                    Some(Ordering::Greater)
                }
            } else {
                let trunc = other.trunc();
                (self, &trunc).partial_cmp(&(&BigInt::from_f32(trunc).unwrap(), other))
            }
        }
    }
    impl NumOrd<f64> for BigInt {
        #[inline]
        fn num_partial_cmp(&self, other: &f64) -> Option<Ordering> {
            if other.is_nan() {
                None
            } else if other.is_infinite() {
                if other.is_sign_positive() {
                    Some(Ordering::Less)
                } else {
                    Some(Ordering::Greater)
                }
            } else {
                let trunc = other.trunc();
                (self, &trunc).partial_cmp(&(&BigInt::from_f64(trunc).unwrap(), other))
            }
        }
    }
    impl NumOrd<BigInt> for BigUint {
        #[inline]
        fn num_partial_cmp(&self, other: &BigInt) -> Option<Ordering> {
            if other.is_negative() {
                Some(Ordering::Greater)
            } else {
                self.partial_cmp(other.magnitude())
            }
        }
    }
    impl_ord_by_swap!{ f32|BigInt; f32|BigUint; f64|BigInt; f64|BigUint; BigInt|BigUint; }
}

// FIXME: Implementations for templated numeric types are directly specialized, because there is no
// negative impl or specialization support yet in rust. We could have a generalized way to implement
// the comparsion if the specialization is supported.

#[cfg(feature = "num-rational")]
mod _num_rational {
    use super::*;
    use num_rational::Ratio;
    use num_traits::{float::FloatCore, Signed};

    impl_ord_equal_types!(
        Ratio<i8> Ratio<i16> Ratio<i32> Ratio<i64> Ratio<i128> Ratio<isize>
        Ratio<u8> Ratio<u16> Ratio<u32> Ratio<u64> Ratio<u128> Ratio<usize>
    );

    macro_rules! impl_ratio_ord_with_int {
        ($($t:ty)*) => ($(
            impl NumOrd<Ratio<$t>> for $t {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$t>) -> Option<Ordering> {
                    (self * other.denom()).partial_cmp(other.numer())
                }
            }
            impl NumOrd<$t> for Ratio<$t> {
                #[inline]
                fn num_partial_cmp(&self, other: &$t) -> Option<Ordering> {
                    self.numer().partial_cmp(&(*other * self.denom()))
                }
            }
        )*);
    }
    impl_ratio_ord_with_int!(i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize);

    macro_rules! impl_ratio_ord_by_casting {
        ($($small:ty => $big:ty;)*) => ($(
            // between ratios
            impl NumOrd<Ratio<$small>> for Ratio<$big> {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$small>) -> Option<Ordering> {
                    let rhs_num = *other.numer() as $big;
                    let rhs_den = *other.denom() as $big;
                    let ord = (self.numer() * rhs_den).num_partial_cmp(&(rhs_num * self.denom()));
                    if self.denom() * rhs_den > 0 {
                        ord
                    } else {
                        ord.map(Ordering::reverse)
                    }
                }
            }
            impl NumOrd<Ratio<$big>> for Ratio<$small> {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$big>) -> Option<Ordering> {
                    let lhs_num = *self.numer() as $big;
                    let lhs_den = *self.denom() as $big;
                    let ord = (lhs_num * other.denom()).num_partial_cmp(&(other.numer() * lhs_den));
                    if lhs_den * other.denom() > 0 {
                        ord
                    } else {
                        ord.map(Ordering::reverse)
                    }
                }
            }

            // between ratio and ints
            impl NumOrd<$small> for Ratio<$big> {
                #[inline]
                fn num_partial_cmp(&self, other: &$small) -> Option<Ordering> {
                    self.numer().partial_cmp(&(*other as $big * self.denom()))
                }
            }
            impl NumOrd<Ratio<$big>> for $small {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$big>) -> Option<Ordering> {
                    (*self as $big * other.denom()).partial_cmp(other.numer())
                }
            }
            impl NumOrd<$big> for Ratio<$small> {
                #[inline]
                fn num_partial_cmp(&self, other: &$big) -> Option<Ordering> {
                    (*self.numer() as $big).partial_cmp(&(other * *self.denom() as $big))
                }
            }
            impl NumOrd<Ratio<$small>> for $big {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$small>) -> Option<Ordering> {
                    (self * *other.denom() as $big).partial_cmp(&(*other.numer() as $big))
                }
            }
        )*);
    }
    impl_ratio_ord_by_casting! {
        // uN, uM for N < M
        u8  => u128; u8  => u64; u8  => u32; u8 => u16;
        u16 => u128; u16 => u64; u16 => u32;
        u32 => u128; u32 => u64;
        u64 => u128;
    
        // iN, iM for N > M
        i8  => i128; i8  => i64; i8  => i32; i8 => i16;
        i16 => i128; i16 => i64; i16 => i32;
        i32 => i128; i32 => i64;
        i64 => i128;
    
        // iN, uM for N > M
        u8  => i128; u8  => i64; u8  => i32; u8 => i16;
        u16 => i128; u16 => i64; u16 => i32;
        u32 => i128; u32 => i64;
        u64 => i128;
    }

    // cast unsigned integers for comparison
    macro_rules! impl_ratio_ord_between_diff_sign {
        ($($int:ty => $uint:ty;)*) => ($(
            // between ratios
            impl NumOrd<Ratio<$uint>> for Ratio<$int> {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$uint>) -> Option<Ordering> {
                    if self.is_negative() {
                        Some(Ordering::Less)
                    } else {
                        let r = Ratio::<$uint>::new(*self.numer() as $uint, *self.denom() as $uint);
                        r.partial_cmp(other)
                    }
                }
            }
            impl NumOrd<Ratio<$int>> for Ratio<$uint> {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$int>) -> Option<Ordering> {
                    if other.is_negative() {
                        Some(Ordering::Greater)
                    } else {
                        let r = Ratio::<$uint>::new(*other.numer() as $uint, *other.denom() as $uint);
                        self.partial_cmp(&r)
                    }
                }
            }

            // between ratio and integers
            impl NumOrd<$uint> for Ratio<$int> {
                #[inline]
                fn num_partial_cmp(&self, other: &$uint) -> Option<Ordering> {
                    if self.is_negative() {
                        Some(Ordering::Less)
                    } else {
                        (*self.numer() as $uint).partial_cmp(&(*self.denom() as $uint * other))
                    }
                }
            }
            impl NumOrd<Ratio<$int>> for $uint {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$int>) -> Option<Ordering> {
                    if other.is_negative() {
                        Some(Ordering::Greater)
                    } else {
                        (*other.denom() as $uint * self).partial_cmp(&(*other.numer() as $uint))
                    }
                }
            }
            impl NumOrd<$int> for Ratio<$uint> {
                #[inline]
                fn num_partial_cmp(&self, other: &$int) -> Option<Ordering> {
                    if other.is_negative() {
                        Some(Ordering::Greater)
                    } else {
                        self.numer().partial_cmp(&(*other as $uint * self.denom()))
                    }
                }
            }
            impl NumOrd<Ratio<$uint>> for $int {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$uint>) -> Option<Ordering> {
                    if self.is_negative() {
                        Some(Ordering::Less)
                    } else {
                        (*self as $uint * other.denom()).partial_cmp(other.numer())
                    }
                }
            }
        )*);
    }
    impl_ratio_ord_between_diff_sign! {
        i8  => u128; i8  => u64; i8  => u32; i8  => u16; i8 => u8;
        i16 => u128; i16 => u64; i16 => u32; i16 => u16;
        i32 => u128; i32 => u64; i32 => u32;
        i64 => u128; i64 => u64;
        i128 => u128; isize => usize;
    }

    // special handling for f64 against u64 and i64
    // FIXME: comparing Ratio<u128> or Ratio<i128> against floats is not supported yet since we need double width multiplication.
    // We can implement it efficiently with carrying_mul implemented (rust#85532)
    impl NumOrd<f64> for Ratio<u64> {
        fn num_partial_cmp(&self, other: &f64) -> Option<Ordering> {
            if other.is_nan() {
                return None
            }
            
            // other = sign * man * 2^exp
            let (man, exp, sign) = other.integer_decode();
            if sign < 0 {
                return Some(Ordering::Greater)
            }

            // self = a / b
            let a = *self.numer();
            let b = *self.denom();

            let result = if exp >= 0 {
                // f / r = (man * 2^exp * b) / a if exp >= 0
                if let Some(num) = man.checked_mul(b).and_then(|v| v.checked_shl(exp as u32)) {
                    num.partial_cmp(&a).unwrap()
                } else {
                    Ordering::Greater
                }
            } else {
                // f / r = (man * b) / (a * 2^(-exp)) if exp < 0
                let num = (man as u128).checked_mul(b as u128).unwrap();
                if let Some(den) = (a as u128).checked_shl((-exp) as u32) {
                    num.partial_cmp(&den).unwrap()
                } else {
                    Ordering::Less
                }
            };
            Some(result)
        }
    }
    impl NumOrd<f64> for Ratio<i64> {
        fn num_partial_cmp(&self, other: &f64) -> Option<Ordering> {
            if other.is_nan() {
                return None
            }
            
            // other = sign * man * 2^exp
            let (man, exp, sign) = other.integer_decode();
            let reverse = match (self.is_positive(), sign > 0) {
                (true, false) => return Some(Ordering::Greater),
                (false, true) => return Some(Ordering::Less),
                (true, true) => false,
                (false, false) => true,
            };

            // self = a / b
            let a = self.numer().abs() as u64;
            let b = self.denom().abs() as u64;

            let result = if exp >= 0 {
                // f / r = (man * 2^exp * b) / a if exp >= 0
                if let Some(num) = man.checked_mul(b).and_then(|v| v.checked_shl(exp as u32)) {
                    num.partial_cmp(&a).unwrap()
                } else {
                    Ordering::Greater
                }
            } else {
                // f / r = (man * b) / (a * 2^(-exp)) if exp < 0
                let num = (man as u128).checked_mul(b as u128).unwrap();
                if let Some(den) = (a as u128).checked_shl((-exp) as u32) {
                    num.partial_cmp(&den).unwrap()
                } else {
                    Ordering::Less
                }
            };

            if reverse {
                Some(result.reverse())
            } else {
                Some(result)
            }
        }
    }
    impl_ord_by_swap!(f64|Ratio<i64>; f64|Ratio<u64>;);

    // cast to f64 and i64 for comparison
    macro_rules! impl_ratio_ord_with_floats_by_casting {
        ($($float:ty => $bfloat:ty | $int:ty => $bint:ty;)*) => ($(
            impl NumOrd<$float> for Ratio<$int> {
                #[inline]
                fn num_partial_cmp(&self, other: &$float) -> Option<Ordering> {
                    let bratio = Ratio::<$bint>::new(*self.numer() as $bint, *self.denom() as $bint);
                    bratio.num_partial_cmp(&(*other as $bfloat))
                }
            }
            impl NumOrd<Ratio<$int>> for $float {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$int>) -> Option<Ordering> {
                    let bratio = Ratio::<$bint>::new(*other.numer() as $bint, *other.denom() as $bint);
                    (*self as $bfloat).num_partial_cmp(&bratio)
                }
            }
        )*);
    }
    impl_ratio_ord_with_floats_by_casting! {
        f32 => f64|i8 => i64; f32 => f64|i16 => i64; f32 => f64|i32 => i64; f32 => f64|i64 => i64;
        f64 => f64|i8 => i64; f64 => f64|i16 => i64; f64 => f64|i32 => i64;
        f32 => f64|u8 => u64; f32 => f64|u16 => u64; f32 => f64|u32 => u64; f32 => f64|u64 => u64;
        f64 => f64|u8 => u64; f64 => f64|u16 => u64; f64 => f64|u32 => u64;
    }

    // deal with size types
    macro_rules! impl_ratio_with_size_types_ord {
        ($($t:ty)*) => ($(
            impl NumOrd<$t> for Ratio<isize> {
                #[inline]
                fn num_partial_cmp(&self, other: &$t) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    let r = Ratio::<i32>::new(*self.numer() as i32, *self.denom() as i32);
                    #[cfg(target_pointer_width = "64")]
                    let r = Ratio::<i64>::new(*self.numer() as i64, *self.denom() as i64);

                    r.num_partial_cmp(other)
                }
            }
            impl NumOrd<$t> for Ratio<usize> {
                #[inline]
                fn num_partial_cmp(&self, other: &$t) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    let r = Ratio::<u32>::new(*self.numer() as u32, *self.denom() as u32);
                    #[cfg(target_pointer_width = "64")]
                    let r = Ratio::<u64>::new(*self.numer() as u64, *self.denom() as u64);

                    r.num_partial_cmp(other)
                }
            }
            impl_ord_by_swap!($t|Ratio<isize>; $t|Ratio<usize>;);
        )*);
    }
    impl_ratio_with_size_types_ord!(i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64);
    
    macro_rules! impl_ratio_ord_with_size_types {
        ($($t:ty)*) => ($(
            impl NumOrd<Ratio<$t>> for isize {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$t>) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    return (*self as i32).num_partial_cmp(other);
                    #[cfg(target_pointer_width = "64")]
                    return (*self as i64).num_partial_cmp(other);
                }
            }
            impl NumOrd<Ratio<$t>> for usize {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$t>) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    return (*self as u32).num_partial_cmp(other);
                    #[cfg(target_pointer_width = "64")]
                    return (*self as u64).num_partial_cmp(other);
                }
            }
            impl NumOrd<Ratio<$t>> for Ratio<isize> {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$t>) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    let r = Ratio::<i32>::new(*self.numer() as i32, *self.denom() as i32);
                    #[cfg(target_pointer_width = "64")]
                    let r = Ratio::<i64>::new(*self.numer() as i64, *self.denom() as i64);

                    r.num_partial_cmp(other)
                }
            }
            impl NumOrd<Ratio<$t>> for Ratio<usize> {
                #[inline]
                fn num_partial_cmp(&self, other: &Ratio<$t>) -> Option<Ordering> {
                    #[cfg(target_pointer_width = "32")]
                    let r = Ratio::<u32>::new(*self.numer() as u32, *self.denom() as u32);
                    #[cfg(target_pointer_width = "64")]
                    let r = Ratio::<u64>::new(*self.numer() as u64, *self.denom() as u64);

                    r.num_partial_cmp(other)
                }
            }
            impl_ord_by_swap!(Ratio<$t>|isize; Ratio<$t>|usize; Ratio<$t>|Ratio<isize>; Ratio<$t>|Ratio<usize>;);
        )*);
    }
    impl_ratio_ord_with_size_types!(i8 i16 i32 i64 u8 u16 u32 u64);
}

#[cfg(feature = "num-complex")]
mod _num_complex {

}
