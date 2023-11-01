use crate::feature_traits::FeatureTraits;
#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(not(feature = "lbf"))]
pub trait FeatureTraits {}
#[cfg(not(feature = "lbf"))]
impl<T> FeatureTraits for T {}

/// An abstraction over `PlutusInterval`, allowing valid values only
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Interval<T> {
    Finite(T, T),
    StartAt(T),
    EndAt(T),
    Always,
    Never,
}

/// Loosely following the CTL implementation of `intervalToPlutusInterval`
/// However, as we don't have Semiring classes, the interval upper bounds are always closed
impl<T> From<Interval<T>> for PlutusInterval<T>
where
    T: FeatureTraits,
{
    fn from(interval: Interval<T>) -> Self {
        match interval {
            Interval::Finite(start, end) => PlutusInterval {
                from: LowerBound {
                    bound: Extended::Finite(start),
                    closed: true,
                },
                to: UpperBound {
                    bound: Extended::Finite(end),
                    closed: true,
                },
            },
            Interval::StartAt(end) => PlutusInterval {
                from: LowerBound {
                    bound: Extended::NegInf,
                    closed: true,
                },
                to: UpperBound {
                    bound: Extended::Finite(end),
                    closed: true,
                },
            },
            Interval::EndAt(start) => PlutusInterval {
                from: LowerBound {
                    bound: Extended::Finite(start),
                    closed: true,
                },
                to: UpperBound {
                    bound: Extended::PosInf,
                    closed: true,
                },
            },
            Interval::Always => PlutusInterval {
                from: LowerBound {
                    bound: Extended::NegInf,
                    closed: true,
                },
                to: UpperBound {
                    bound: Extended::PosInf,
                    closed: true,
                },
            },
            Interval::Never => PlutusInterval {
                from: LowerBound {
                    bound: Extended::PosInf,
                    closed: true,
                },
                to: UpperBound {
                    bound: Extended::NegInf,
                    closed: true,
                },
            },
        }
    }
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
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PlutusInterval<T>
where
    T: FeatureTraits,
{
    pub from: LowerBound<T>,
    pub to: UpperBound<T>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct UpperBound<T>
where
    T: FeatureTraits,
{
    pub bound: Extended<T>,
    pub closed: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LowerBound<T>
where
    T: FeatureTraits,
{
    pub bound: Extended<T>,
    pub closed: bool,
}

/// A set extended with a positive and negative infinity.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Extended<T>
where
    T: FeatureTraits,
{
    NegInf,
    Finite(T),
    PosInf,
}
