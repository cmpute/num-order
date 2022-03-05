//!
//! `num-order` implements numerically consistent [Eq][std::cmp::Eq], [Ord][std::cmp::Ord] and
//! [Hash][std::hash::Hash] for various `num` types
//! 
//! # Example:
//! ```rust
//! use std::cmp::Ordering;
//! use num_order::NumOrd;
//! 
//! assert!(NumOrd::num_eq(&3u64, &3.0f32));
//! assert!(NumOrd::num_lt(&-4.7f64, &-4i8));
//! assert!(!NumOrd::num_ge(&-3i8, &1u16));
//! 
//! // 40_000_000 can be exactly represented in f32, 40_000_001 cannot
//! // 40_000_001 becames 40_000_000.0 in f32
//! assert_eq!(NumOrd::num_cmp(&40_000_000f32, &40_000_000u32), Ordering::Equal);
//! assert_ne!(NumOrd::num_cmp(&40_000_001f32, &40_000_001u32), Ordering::Equal);
//! assert_eq!(NumOrd::num_partial_cmp(&f32::NAN, &40_000_002u32), None);
//! ```
//! 
//! This crate serves applications where [float-ord](https://crates.io/crates/float-ord),
//! [num-cmp](https://crates.io/crates/num-cmp), [numcmp](https://crates.io/crates/numcmp) can be used,
//! but supports more numeric types and hashing.
//! 

#![no_std]
#[cfg(any(feature = "std", test))]
extern crate std;
#[cfg(all(not(feature = "std"), feature = "libm"))]
extern crate libm;

use core::cmp::Ordering;
use core::hash::Hasher;

pub trait NumOrd<Other> {
    fn num_partial_cmp(&self, other: &Other) -> Option<Ordering>;

    #[inline]
    fn num_eq(&self, other: &Other) -> bool {
        matches!(self.num_partial_cmp(other), Some(Ordering::Equal))
    }
    #[inline]
    fn num_ne(&self, other: &Other) -> bool {
        !self.num_eq(other)
    }
    #[inline]
    fn num_lt(&self, other: &Other) -> bool {
        matches!(self.num_partial_cmp(other), Some(Ordering::Less))
    }
    #[inline]
    fn num_le(&self, other: &Other) -> bool {
        matches!(self.num_partial_cmp(other), Some(Ordering::Equal) | Some(Ordering::Less))
    }
    #[inline]
    fn num_gt(&self, other: &Other) -> bool {
        matches!(self.num_partial_cmp(other), Some(Ordering::Greater))
    }
    #[inline]
    fn num_ge(&self, other: &Other) -> bool {
        matches!(self.num_partial_cmp(other), Some(Ordering::Equal) | Some(Ordering::Greater))
    }
    #[inline]
    fn num_cmp(&self, other: &Other) -> Ordering {
        self.num_partial_cmp(other).unwrap()
    }
}

pub trait NumHash {
    fn num_hash<H: Hasher>(&self, state: &mut H);
}

mod ord;
mod hash;
#[cfg(test)] mod tests;
