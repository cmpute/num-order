use super::*;
use std::hash::{Hash, Hasher};
use std::fmt;
use std::vec::Vec;
use std::cmp::Ordering::{self, *};

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
enum N {
    u8(u8), u16(u16), u32(u32), u64(u64), u128(u128), usize(usize),
    i8(i8), i16(i16), i32(i32), i64(i64), i128(i128), isize(isize),
    f32(f32), f64(f64),
}

macro_rules! repeat_arms {
    ($e:expr; $v:ident => $arm:expr) => {
        match $e {
            N::u8($v) => $arm, N::u16($v) => $arm, N::u32($v) => $arm,
            N::u64($v) => $arm, N::u128($v) => $arm, N::usize($v) => $arm,
            N::i8($v) => $arm, N::i16($v) => $arm, N::i32($v) => $arm,
            N::i64($v) => $arm, N::i128($v) => $arm, N::isize($v) => $arm,
            N::f32($v) => $arm, N::f64($v) => $arm,
        }
    };
}

impl Hash for N {
    fn hash<H: Hasher>(&self, state: &mut H) {
        repeat_arms! {
            self; v => v.num_hash(state)
        }
    }
}

// create list of `N` objects with given value (arg1) and types (arg2)
macro_rules! n {
    ($e:expr; $($t:ident),*) => (&[$(N::$t($e as $t)),*]);
}

const B32: f64 = (1u64<<32) as f64;
const F32_SUBNORMAL_MIN: f32 = 1.4e-45;
const F64_SUBNORMAL_MIN: f64 = 4.9e-324;

// list of selected numbers ordered ascendingly
const NUMBERS: &'static [&'static [N]] = &[
    // f64 min boundary and infinity
    n!(f64::NEG_INFINITY; f32, f64),
    n!(f64::MIN; f64),

    // f32 min boundary
    n!((-B32 * B32 - 0x1000 as f64) * B32 * B32; f64), // -2^128 - 2^76
    n!(-B32 * B32 * B32 * B32; f64), // -2^128
    n!((-B32 * B32 + 0x800 as f64) * B32 * B32; f64), // -2^128 + 2^75
    n!(f32::MIN; f32), // -2^128 + 2^104

    // i128/f32 min boundary
    n!(-(0x8000_0100_0000_0000_0000_0000_0000_0000u128 as f32); f32),
    n!(-(0x8000_0000_0000_0800_0000_0000_0000_0000u128 as f64); f64),
    n!(-0x8000_0000_0000_0000_0000_0000_0000_0000i128; i128, f32),
    n!(-0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi128; i128),
    n!(-0x7fff_ffff_ffff_fc00_0000_0000_0000_0000i128; i128, f64),
    n!(-0x7fff_ff80_0000_0000_0000_0000_0000_0000i128; i128, f32),

    // i64 min boundary
    n!(-0x8000_0100_0000_0000i128; i128, f32),
    n!(-0x8000_0000_0000_0800i128; i128, f64),
    n!(-0x8000_0000_0000_0001i128; i128),
    n!(-0x8000_0000_0000_0000i64; i64, f32),
    n!(-0x7fff_ffff_ffff_ffffi64; i64),
    n!(-0x7fff_ffff_ffff_fc00i64; i64, f64),
    n!(-0x7fff_ff80_0000_0000i64; i64, f32),

    // f64 min exact int boundary
    n!(-0x20_0000_4000_0000i64; i64, f32),
    n!(-0x20_0000_0000_0002i64; i64, f64),
    n!(-0x20_0000_0000_0001i64; i64),
    n!(-0x20_0000_0000_0000i64; i64, f32),
    n!(-0x1f_ffff_ffff_ffffi64; i64, f64),
    n!(-0x1f_ffff_e000_0000i64; i64, f32),

    // f64 min exact half int boundary
    n!(-0x10_0000_2000_0000i64; i64, f32),
    n!(-0x10_0000_0000_0002i64; i64, f64),
    n!(-0x10_0000_0000_0001i64; i64, f64),
    n!(-0x10_0000_0000_0000i64; i64, f32),
    n!(-0xf_ffff_ffff_ffffi64 as f64 - 0.5; f64),
    n!(-0xf_ffff_ffff_ffffi64; i64, f64),
    n!(-0xf_ffff_f000_0000i64; i64, f32),

    // i32 min boundary
    n!(-0x8000_0100i64; i64, f32),
    n!(-0x8000_0001i64; i64, f64),
    n!(-0x8000_0000i64 as f64 - 0.5; f64),
    n!(-0x8000_0000i64; i64, f32),
    n!(-0x7fff_ffff as f64 - 0.5; f64),
    n!(-0x7fff_ffff; i32, f64),
    n!(-0x7fff_ff80; i32, f32),

    // f32 min exact int boundary
    n!(-0x100_0002; i32, f32),
    n!(-0x100_0001 as f64 - 0.5; f64),
    n!(-0x100_0001; i32, f64),
    n!(-0x100_0000 as f64 - 0.5; f64),
    n!(-0x100_0000; i32, f32),
    n!(-0xff_ffff as f64 - 0.5; f64),
    n!(-0xff_ffff; i32, f32),
    n!(-0xff_fffe as f64 - 0.5; f64),
    n!(-0xff_fffe; i32, f32),

    // f32 min exact half int boundary
    n!(-0x80_0002; i32, f32),
    n!(-0x80_0001 as f64 - 0.5; f64),
    n!(-0x80_0001; i32, f32),
    n!(-0x80_0000 as f64 - 0.5; f64),
    n!(-0x80_0000; i32, f32),
    n!(-0x7f_ffff as f64 - 0.5; f32),
    n!(-0x7f_ffff; i32, f32),
    n!(-0x7f_fffe as f64 - 0.5; f32),
    n!(-0x7f_fffe; i32, f32),

    // i16 min boundary
    n!(-0x8002; i32, f32),
    n!(-0x8001 as f32 - 0.5; f32),
    n!(-0x8001; i32, f32),
    n!(-0x8000 as f32 - 0.5; f32),
    n!(-0x8000; i16, f32),
    n!(-0x7fff as f32 - 0.5; f32),
    n!(-0x7fff; i16, f32),

    // i8 min boundary
    n!(-0x82; i16, f32),
    n!(-0x81 as f32 - 0.5; f32),
    n!(-0x81; i16, f32),
    n!(-0x80 as f32 - 0.5; f32),
    n!(-0x80; i8, f32),
    n!(-0x7f as f32 - 0.5; f32),
    n!(-0x7f; i8, f32),

    // around zero
    n!(-2; i8, f32),
    n!(-1.5; f32),
    n!(-1.0 - f32::EPSILON * 2.0; f32),
    n!(-1.0 - f32::EPSILON; f32),
    n!(-1.0 - f64::EPSILON * 2.0; f64),
    n!(-1.0 - f64::EPSILON; f64),
    n!(-1; i8, f32),
    n!(-1.0 + f64::EPSILON / 2.0; f64),
    n!(-1.0 + f64::EPSILON; f64),
    n!(-1.0 + f32::EPSILON / 2.0; f32),
    n!(-1.0 + f32::EPSILON; f32),
    n!(-0.5; f32),
    n!(-0.1; f32),
    n!(-f32::MIN_POSITIVE; f32),
    n!(-F32_SUBNORMAL_MIN; f32),
    n!(-f64::MIN_POSITIVE; f64),
    n!(-F64_SUBNORMAL_MIN; f64),
    &[N::u8(0), N::i8(0), N::f32(0.0), N::f32(-0.0)], // negative zeros should be handled!
    n!(F64_SUBNORMAL_MIN; f64),
    n!(f64::MIN_POSITIVE; f64),
    n!(F32_SUBNORMAL_MIN; f32),
    n!(f32::MIN_POSITIVE; f32),
    n!(0.1; f32),
    n!(0.5; f32),
    n!(1.0 - f32::EPSILON; f32),
    n!(1.0 - f32::EPSILON / 2.0; f32),
    n!(1.0 - f64::EPSILON; f64),
    n!(1.0 - f64::EPSILON / 2.0; f64),
    n!(1; u8, i8, f32),
    n!(1.0 + f64::EPSILON; f64),
    n!(1.0 + f64::EPSILON * 2.0; f64),
    n!(1.0 + f32::EPSILON; f32),
    n!(1.0 + f32::EPSILON * 2.0; f32),
    n!(1.5; f32),
    n!(2; u8, i8, f32),

    // i8 max boundary
    n!(0x7e; u8, i8, f32),
    n!(0x7f as f32 - 0.5; f32),
    n!(0x7f; u8, i8, f32),
    n!(0x7f as f32 + 0.5; f32),
    n!(0x80; u8, i16, f32),
    n!(0x80 as f32 + 0.5; f32),
    n!(0x81; u8, i16, f32),

    // u8 max boundary
    n!(0xfe; u8, i16, f32),
    n!(0xff as f32 - 0.5; f32),
    n!(0xff; u8, i16, f32),
    n!(0xff as f32 + 0.5; f32),
    n!(0x100; u16, i16, f32),
    n!(0x100 as f32 + 0.5; f32),
    n!(0x101; u16, i16, f32),

    // i16 max boundary
    n!(0x7ffe; u16, i16, f32),
    n!(0x7fff as f32 - 0.5; f32),
    n!(0x7fff; u16, i16, f32),
    n!(0x7fff as f32 + 0.5; f32),
    n!(0x8000; u16, i32, f32),
    n!(0x8000 as f32 + 0.5; f32),
    n!(0x8001; u16, i32, f32),

    // u16 max boundary
    n!(0xfffe; u16, i32, f32),
    n!(0xffff as f32 - 0.5; f32),
    n!(0xffff; u16, i32, f32),
    n!(0xffff as f32 + 0.5; f32),
    n!(0x1_0000; u32, i32, f32),
    n!(0x1_0000 as f32 + 0.5; f32),
    n!(0x1_0001; u32, i32, f32),

    // f32 max exact half int boundary
    n!(0x7f_fffe; u32, i32, f32),
    n!(0x7f_ffff as f64 - 0.5; f32),
    n!(0x7f_ffff; u32, i32, f32),
    n!(0x7f_ffff as f64 + 0.5; f32),
    n!(0x80_0000; u32, i32, f32),
    n!(0x80_0000 as f64 + 0.5; f64),
    n!(0x80_0001; u32, i32, f32),
    n!(0x80_0001 as f64 + 0.5; f64),
    n!(0x80_0002; u32, i32, f32),

    // f32 max exact int boundary
    n!(0xff_fffe; u32, i32, f32),
    n!(0xff_ffff as f64 - 0.5; f64),
    n!(0xff_ffff; u32, i32, f32),
    n!(0xff_ffff as f64 + 0.5; f64),
    n!(0x100_0000; u32, i32, f32),
    n!(0x100_0000 as f64 + 0.5; f64),
    n!(0x100_0001; u32, i32, f64),
    n!(0x100_0001 as f64 + 0.5; f64),
    n!(0x100_0002; u32, i32, f32),

    // i32 max boundary
    n!(0x7fff_ff80; u32, i32, f32),
    n!(0x7fff_ffff; u32, i32, f64),
    n!(0x7fff_ffff as f64 + 0.5; f64),
    n!(0x8000_0000u64; u32, i64, f32),
    n!(0x8000_0000u64 as f64 + 0.5; f64),
    n!(0x8000_0001u64; u32, i64, f64),
    n!(0x8000_0100u64; u32, i64, f32),

    // u32 max boundary
    n!(0xffff_ff00u64; u32, i64, f32),
    n!(0xffff_ffffu64; u32, i64, f64),
    n!(0xffff_ffffu64 as f64 + 0.5; f64),
    n!(0x1_0000_0000u64; u64, i64, f32),
    n!(0x1_0000_0000u64 as f64 + 0.5; f64),
    n!(0x1_0000_0001u64; u64, i64, f64),
    n!(0x1_0000_0200u64; u64, i64, f32),

    // f64 max exact half int boundary
    n!(0xf_ffff_f000_0000u64; u64, i64, f32),
    n!(0xf_ffff_ffff_ffffu64; u64, i64, f64),
    n!(0xf_ffff_ffff_ffffu64 as f64 + 0.5; f64),
    n!(0x10_0000_0000_0000u64; u64, i64, f32),
    n!(0x10_0000_0000_0001u64; u64, i64, f64),
    n!(0x10_0000_0000_0002u64; u64, i64, f64),
    n!(0x10_0000_2000_0000u64; u64, i64, f32),

    // f64 max exact int boundary
    n!(0x1f_ffff_e000_0000u64; u64, i64, f32),
    n!(0x1f_ffff_ffff_ffffu64; u64, i64, f64),
    n!(0x20_0000_0000_0000u64; u64, i64, f32),
    n!(0x20_0000_0000_0001u64; u64, i64),
    n!(0x20_0000_0000_0002u64; u64, i64, f64),
    n!(0x20_0000_4000_0000u64; u64, i64, f32),

    // i64 max boundary
    n!(0x7fff_ff80_0000_0000u64; u64, i64, f32),
    n!(0x7fff_ffff_ffff_fc00u64; u64, i64, f64),
    n!(0x7fff_ffff_ffff_ffffu64; u64, i64),
    n!(0x8000_0000_0000_0000u64; u64, i128, f32),
    n!(0x8000_0000_0000_0001u64; u64, i128),
    n!(0x8000_0000_0000_0800u64; u64, i128, f64),
    n!(0x8000_0100_0000_0000u64; u64, i128, f32),

    // u64 max boundary
    n!(0xffff_ff00_0000_0000u64; u64, i128, f32),
    n!(0xffff_ffff_ffff_f800u64; u64, i128, f64),
    n!(0xffff_ffff_ffff_ffffu64; u64, i128),
    n!(0x1_0000_0000_0000_0000u128; u128, i128, f32),
    n!(0x1_0000_0000_0000_0001u128; u128, i128),
    n!(0x1_0000_0000_0000_1000u128; u128, i128, f64),
    n!(0x1_0000_0200_0000_0000u128; u128, i128, f32),

    // i128 max boundary
    n!(0x7fff_ff80_0000_0000_0000_0000_0000_0000u128; u128, i128, f32),
    n!(0x7fff_ffff_ffff_fc00_0000_0000_0000_0000u128; u128, i128, f64),
    n!(0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128; u128, i128),
    n!(0x8000_0000_0000_0000_0000_0000_0000_0000u128; u128, f32),
    n!(0x8000_0000_0000_0000_0000_0000_0000_0001u128; u128),
    n!(0x8000_0000_0000_0800_0000_0000_0000_0000u128; u128, f64),
    n!(0x8000_0100_0000_0000_0000_0000_0000_0000u128; u128, f32),

    // u128/f32 max boundary
    n!(0xffff_ff00_0000_0000_0000_0000_0000_0000u128; u128, f32),
    n!(0xffff_ffff_ffff_f800_0000_0000_0000_0000u128; u128, f64), // 2^128 - 2^75
    n!(0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128; u128),
    n!(B32 * B32 * B32 * B32; f64), // 2^128
    n!((B32 * B32 + 0x1000 as f64) * B32 * B32; f64), // 2^128 + 2^76

    // f64 max boundary and infinity
    n!(f64::MAX; f64),
    n!(f64::INFINITY; f32, f64),
];

fn expand_equiv_class(cls: &[N]) -> Vec<N> {
    let mut ret = Vec::new();

    for e in cls {
        // size extension
        match e {
            N::u8(v) => ret.extend_from_slice(&[N::u8(*v), N::u16(*v as u16), N::u32(*v as u32),
                                                N::u64(*v as u64)]),
            N::u16(v) => ret.extend_from_slice(&[N::u16(*v), N::u32(*v as u32), N::u64(*v as u64)]),
            N::u32(v) => ret.extend_from_slice(&[N::u32(*v), N::u64(*v as u64)]),
            N::u64(v) => ret.push(N::u64(*v)),
            N::usize(v) => ret.push(N::usize(*v)),
            N::i8(v) => ret.extend_from_slice(&[N::i8(*v), N::i16(*v as i16), N::i32(*v as i32),
                                                N::i64(*v as i64)]),
            N::i16(v) => ret.extend_from_slice(&[N::i16(*v), N::i32(*v as i32), N::i64(*v as i64)]),
            N::i32(v) => ret.extend_from_slice(&[N::i32(*v), N::i64(*v as i64)]),
            N::i64(v) => ret.push(N::i64(*v)),
            N::isize(v) => ret.push(N::isize(*v)),
            N::f32(v) => ret.extend_from_slice(&[N::f32(*v), N::f64(*v as f64)]),
            N::f64(v) => ret.push(N::f64(*v)),
            _ => {}
        }

        // size extension for i128
        match e {
            N::u8(v) => ret.push(N::u128(*v as u128)),
            N::u16(v) => ret.push(N::u128(*v as u128)),
            N::u32(v) => ret.push(N::u128(*v as u128)),
            N::u64(v) => ret.push(N::u128(*v as u128)),
            N::u128(v) => ret.push(N::u128(*v)),
            N::i8(v) => ret.push(N::i128(*v as i128)),
            N::i16(v) => ret.push(N::i128(*v as i128)),
            N::i32(v) => ret.push(N::i128(*v as i128)),
            N::i64(v) => ret.push(N::i128(*v as i128)),
            N::i128(v) => ret.push(N::i128(*v)),
            _ => {}
        }

        // insert equivalent usize/isize
        #[cfg(target_pointer_width = "32")]
        match e {
            N::u8(v) => ret.push(N::usize(*v as usize)),
            N::u16(v) => ret.push(N::usize(*v as usize)),
            N::u32(v) => ret.push(N::usize(*v as usize)),
            N::i8(v) => ret.push(N::isize(*v as isize)),
            N::i16(v) => ret.push(N::isize(*v as isize)),
            N::i32(v) => ret.push(N::isize(*v as isize)),
            _ => {}
        }
        #[cfg(target_pointer_width = "64")]
        match e {
            N::u8(v) => ret.push(N::usize(*v as usize)),
            N::u16(v) => ret.push(N::usize(*v as usize)),
            N::u32(v) => ret.push(N::usize(*v as usize)),
            N::u64(v) => ret.push(N::usize(*v as usize)),
            N::i8(v) => ret.push(N::isize(*v as isize)),
            N::i16(v) => ret.push(N::isize(*v as isize)),
            N::i32(v) => ret.push(N::isize(*v as isize)),
            N::i64(v) => ret.push(N::isize(*v as isize)),
            _ => {}
        }
    }

    ret
}

fn assert_cmp<T: Into<Option<Ordering>>>(lhs: &N, rhs: &N, expected: T) {
    #[derive(PartialEq)]
    struct Result {
        ord: Option<Ordering>,
        eq: bool, ne: bool, lt: bool, gt: bool, le: bool, ge: bool,
    }

    impl fmt::Debug for Result {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            if let Some(ord) = self.ord {
                write!(f, "<{:?} (", ord)?;
            } else {
                write!(f, "<_ (")?;
            }
            let neg = |b: bool| if b { "" } else { "!" };
            write!(f, "{}eq {}ne {}lt {}gt {}le {}ge)>",
                   neg(self.eq), neg(self.ne),
                   neg(self.lt), neg(self.gt),
                   neg(self.le), neg(self.gt))
        }
    }

    let expected: Option<Ordering> = expected.into();
    let expected = match expected {
        Some(Less) => {
            Result { ord: expected, eq: false, ne: true,
                     lt: true, gt: false, le: true, ge: false }
        },
        Some(Equal) => {
            Result { ord: expected, eq: true, ne: false,
                     lt: false, gt: false, le: true, ge: true }
        },
        Some(Greater) => {
            Result { ord: expected, eq: false, ne: true,
                     lt: false, gt: true, le: false, ge: true }
        },
        None => {
            Result { ord: expected, eq: false, ne: true,
                     lt: false, gt: false, le: false, ge: false }
        },
    };

    let actual = repeat_arms! {
        lhs; x => {
            repeat_arms! {
                rhs; y => 
                     Result { ord: x.num_partial_cmp(y), eq: x.num_eq(y), ne: x.num_ne(y),
                              lt: x.num_lt(y), gt: x.num_gt(y), le: x.num_le(y), ge: x.num_ge(y) }
            }
        }
    };

    assert_eq!(expected, actual, "failed to compare {:?} against {:?}",
        lhs, rhs);
}

#[test]
fn test_ordering() {
    let numbers: Vec<_> = NUMBERS.iter().map(|cls| expand_equiv_class(cls)).collect();

    // comparison between numbers
    for icls in 0..numbers.len() {
        for jcls in 0..numbers.len() {
            let expected = icls.cmp(&jcls);
            for i in &numbers[icls] {
                for j in &numbers[jcls] {
                    assert_cmp(i, j, expected);
                }
            }
        }
    }
}

#[test]
fn test_nan() {
    let numbers: Vec<_> = NUMBERS.iter().map(|cls| expand_equiv_class(cls)).collect();

    // comparison between numbers and NaNs
    for cls in &numbers {
        for i in cls {
            assert_cmp(i, &N::f32(f32::NAN), None);
            assert_cmp(i, &N::f64(f64::NAN), None);
            assert_cmp(&N::f32(f32::NAN), i, None);
            assert_cmp(&N::f64(f64::NAN), i, None);
        }
    }

    // comparison between NaNs themselves
    assert_cmp(&N::f32(f32::NAN), &N::f32(f32::NAN), None);
    assert_cmp(&N::f32(f32::NAN), &N::f64(f64::NAN), None);
    assert_cmp(&N::f64(f64::NAN), &N::f32(f32::NAN), None);
    assert_cmp(&N::f64(f64::NAN), &N::f64(f64::NAN), None);
}


#[test]
fn test_hash() {
    use std::collections::hash_map::DefaultHasher;
    
    fn hash(num: &N) -> u64 {
        let mut hasher = DefaultHasher::new();
        num.hash(&mut hasher);
        hasher.finish()
    }

    for &equiv in NUMBERS {
        let hashes: Vec<u64> = equiv.iter().map(|n| hash(n)).collect();
        for i in 1..equiv.len() {
            assert_eq!(hashes[0], hashes[i], "Hash mismatch between {:?} and {:?}", equiv[0], equiv[i]);
        }
    }
}
