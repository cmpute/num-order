use crate::NumOrd;
use core::cmp::Ordering;

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

impl_ord_equal_types! {
    u8 u16 u32 u64 u128 usize
    i8 i16 i32 i64 i128 isize
    f32 f64
}

// Case2: forward to same types by safe casting
macro_rules! impl_ord_with_casting {
    ($($small:ty => $big:ty;)*) => ($(
        impl NumOrd<$small> for $big {
            #[inline]
            fn num_partial_cmp(&self, other: &$small) -> Option<Ordering> {
                self.partial_cmp(&(*other as $big))
            }
        }
        impl NumOrd<$big> for $small {
            #[inline]
            fn num_partial_cmp(&self, other: &$big) -> Option<Ordering> {
                (*self as $big).partial_cmp(other)
            }
        }
    )*);
}

impl_ord_with_casting! {
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
                    self.partial_cmp(&(*other as $unsigned))
                }
            }
        }
        impl NumOrd<$unsigned> for $signed {
            #[inline]
            fn num_partial_cmp(&self, other: &$unsigned) -> Option<Ordering> {
                if self < &0 {
                    Some(Ordering::Less)
                } else {
                    (*self as $unsigned).partial_cmp(other)
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

#[cfg(target_pointer_width = "64")]
impl_ord_with_size_types!(u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64);
