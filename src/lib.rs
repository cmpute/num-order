//!
//! Compare with [float-ord](https://crates.io/crates/float-ord), [num-cmp](https://crates.io/crates/num-cmp), [numcmp](https://crates.io/crates/numcmp)
//! 


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
