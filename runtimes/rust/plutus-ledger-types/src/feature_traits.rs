#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;

/// FeatureTraits is an intermediate trait which inherits different traits depending on the
/// compilation feature flag. This makes it cleaner to implement structs with generics, with
/// optional trait bounds.
#[cfg(feature = "lbf")]
pub trait FeatureTraits: Json {}
#[cfg(feature = "lbf")]
impl<T> FeatureTraits for T where T: Json {}
