use num_bigint::BigInt;

/// Curried eq function
pub fn eq<'a, T>(x: &'a T) -> Box<dyn FnOnce(&'a T) -> bool + 'a>
where
    T: PartialEq + 'a,
{
    Box::new(move |y| x == y)
}

/// Curried boolean and function
pub fn and<'a>(x: bool) -> Box<dyn FnOnce(bool) -> bool + 'a> {
    Box::new(move |y| x && y)
}

/// Map a BigInt to T
pub fn case_int<'a, T: 'a>(
    i: &BigInt,
    cases: Vec<(BigInt, T)>,
    other_case: Box<dyn 'a + FnOnce(&BigInt) -> T>,
) -> T {
    cases
        .into_iter()
        .find(|(i2, _)| i == i2)
        .map(|(_, res)| res)
        .unwrap_or_else(|| other_case(&i))
}
