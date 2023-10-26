#[cfg(test)]
mod json_derive_macro_tests {

    #[test]
    fn tests() {
        let t = trybuild::TestCases::new();

        t.pass("tests/newtype.rs");
        t.pass("tests/struct.rs");
    }
}
