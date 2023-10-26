#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An abstraction over `PlutusInterval`, allowing valid values only
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Interval<T> {
    Finite(T, T),
    StartAt(T),
    EndAt(T),
    Always,
    Empty,
}

/// An interval of `T`s.
///
/// The interval may be either closed or open at either end, meaning
/// that the endpoints may or may not be included in the interval.
///
/// The interval can also be unbounded on either side.
///
/// The 'Eq' instance gives equality of the intervals, not structural equality.
/// There is no 'Ord' instance, but 'contains' gives a partial order.
///
/// Note that some of the functions on `Interval` rely on `Enum` in order to
/// handle non-inclusive endpoints. For this reason, it may not be safe to
/// use `Interval`s with non-inclusive endpoints on types whose `Enum`
/// instances have partial methods.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PlutusInterval<T> {
    from: Extended<T>,
    to: Extended<T>,
}

/// A set extended with a positive and negative infinity.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Extended<T> {
    NegInf,
    Finite(T),
    PosInf,
}
