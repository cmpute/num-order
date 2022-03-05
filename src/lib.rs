use core::cmp::Ordering;

pub trait NumOrd<Other> {
    fn num_cmp(self, other: &Other) -> Ordering;
}

pub trait NumHash {
    fn num_hash<H: Hasher>(&self, state: &mut H);
}
