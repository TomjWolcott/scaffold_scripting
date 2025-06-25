use std::any::Any;
use std::fmt::{Debug, Formatter};
use dyn_clone::DynClone;

// DynPartialEq stuff is from https://quinedot.github.io/rust-learning/dyn-trait-eq.html#:~:text=The%20general%20idea%20is%20that,by%20upcasting%20to%20dyn%20DynCompare%20.

pub trait AsDynPartialEq: Any {
    fn as_any(&self) -> &dyn Any;

    fn as_dyn_partial_eq(&self) -> &dyn DynPartialEq;
}

impl<T: Any + PartialEq> AsDynPartialEq for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_dyn_partial_eq(&self) -> &dyn DynPartialEq {
        self
    }
}

pub trait DynPartialEq: AsDynPartialEq {
    fn dyn_eq(&self, other: &dyn DynPartialEq) -> bool;
}

impl<T: Any + PartialEq> DynPartialEq for T {
    fn dyn_eq(&self, other: &dyn DynPartialEq) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<T>() {
            self == other
        } else {
            false
        }
    }
}

impl PartialEq<dyn DynPartialEq> for dyn DynPartialEq {
    fn eq(&self, other: &dyn DynPartialEq) -> bool {
        self.dyn_eq(other)
    }
}

impl PartialEq<dyn AnyValue> for dyn AnyValue {
    fn eq(&self, other: &dyn AnyValue) -> bool {
        self.as_dyn_partial_eq() == other.as_dyn_partial_eq()
    }
}

impl PartialEq<&Self> for Box<dyn AnyValue> {
    fn eq(&self, other: &&Self) -> bool {
        <Self as PartialEq>::eq(self, *other)
    }
}

pub trait AnyValue: Any + DynClone + DynPartialEq {}

impl Debug for dyn AnyValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<AnyValue>")
    }
}

impl<T: Any + DynClone + DynPartialEq> AnyValue for T { }

dyn_clone::clone_trait_object!(AnyValue);