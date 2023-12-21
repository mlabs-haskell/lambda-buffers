#[cfg(test)]
mod goldens;

mod json_tests {
    use crate::goldens;
    use lbr_prelude::json::Json;
    use std::collections::HashMap;
    use std::fs;

    fn find_goldens(
        golden_dir: &str,
        title: &str,
        ext: &str,
        goldens_length: usize,
    ) -> HashMap<usize, (String, String)> {
        (0..goldens_length).fold(HashMap::new(), move |mut found, ix| {
            let golden_file_path = format!("{}/{}.{}{}", golden_dir, title, ix, ext);
            let file = fs::read_to_string(golden_file_path.clone());
            match file {
                Err(_) => found,
                Ok(golden_text) => {
                    found.insert(ix, (golden_file_path, golden_text));
                    found
                }
            }
        })
    }
    fn assert_goldens<T>(
        golden_dir: &str,
        title: &str,
        ext: &str,
        assert_golden: impl Fn(&T, usize, &str, &str),
        golden_data: Vec<T>,
    ) {
        let goldens = find_goldens(golden_dir, title, ext, golden_data.len());
        if goldens.is_empty() {
            panic!(
                "Expected to find some goldens {}{}. Did you forget to (re)generate goldens? {}",
                title, ext, golden_dir
            );
        }
        golden_data
            .iter()
            .take(goldens.len())
            .enumerate()
            .for_each(|(ix, golden)| {
                let (fp, text) = goldens.get(&ix).expect(&format!(
                    "Golden value index not in goldens {} {}",
                    title, ix
                ));
                assert_golden(golden, ix, fp, text);
            });
    }
    fn from_to_golden_test<T: Json + std::fmt::Debug + Eq>(title: &str, goldens: Vec<T>) {
        let golden_dir = "data/lbt-prelude-golden-data";
        assert_goldens(
            golden_dir,
            title,
            ".json",
            |golden, _index, _fp, text| {
                let res = T::from_json_string(text).unwrap();
                assert_eq!(golden, &res);
                assert_eq!(text, res.to_json_string());
                assert_eq!(text, res.to_json_string());
            },
            goldens,
        )
    }

    #[test]
    fn foo_a_from_to_golden_tests() {
        from_to_golden_test("Foo.A", goldens::a_goldens())
    }

    #[test]
    fn foo_b_from_to_golden_tests() {
        from_to_golden_test("Foo.B", goldens::b_goldens())
    }

    #[test]
    fn foo_c_from_to_golden_tests() {
        from_to_golden_test("Foo.C", goldens::c_goldens())
    }

    #[test]
    fn foo_d_from_to_golden_tests() {
        from_to_golden_test("Foo.D", goldens::d_goldens())
    }

    #[test]
    fn foo_f_int_from_to_golden_tests() {
        from_to_golden_test("Foo.FInt", goldens::f_int_goldens())
    }

    #[test]
    fn foo_g_int_from_to_golden_tests() {
        from_to_golden_test("Foo.GInt", goldens::g_int_goldens())
    }

    #[test]
    fn days_from_to_golden_test() {
        from_to_golden_test("Days.Day", goldens::day_goldens())
    }

    #[test]
    fn workdays_from_to_golden_test() {
        from_to_golden_test("Days.WorkDay", goldens::workday_goldens())
    }

    #[test]
    fn freedays_from_to_golden_test() {
        from_to_golden_test("Days.FreeDay", goldens::freeday_goldens())
    }

    #[test]
    fn bool_from_to_golden_test() {
        from_to_golden_test("Prelude.Bool", goldens::bool_goldens())
    }

    #[test]
    fn char_from_to_golden_test() {
        from_to_golden_test("Prelude.Char", goldens::char_goldens())
    }

    #[test]
    fn integer_from_to_golden_test() {
        from_to_golden_test("Prelude.Integer", goldens::integer_goldens())
    }

    #[test]
    fn text_from_to_golden_test() {
        from_to_golden_test("Prelude.Text", goldens::text_goldens())
    }

    #[test]
    fn bytes_from_to_golden_test() {
        from_to_golden_test("Prelude.Bytes", goldens::bytes_goldens())
    }

    #[test]
    fn maybe_from_to_golden_test() {
        from_to_golden_test("Prelude.Maybe", goldens::maybe_goldens())
    }

    #[test]
    fn either_from_to_golden_test() {
        from_to_golden_test("Prelude.Either", goldens::either_goldens())
    }

    #[test]
    fn list_from_to_golden_test() {
        from_to_golden_test("Prelude.List", goldens::list_goldens())
    }

    #[test]
    fn set_from_to_golden_test() {
        from_to_golden_test("Prelude.Set", goldens::set_goldens())
    }

    #[test]
    fn map_from_to_golden_test() {
        from_to_golden_test("Prelude.Map", goldens::map_goldens())
    }
}
