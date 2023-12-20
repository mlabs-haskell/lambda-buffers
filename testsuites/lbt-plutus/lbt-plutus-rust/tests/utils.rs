use std::collections::HashMap;
use std::fs;

pub fn find_goldens(
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
pub fn assert_goldens<T>(
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
    golden_data.iter().enumerate().for_each(|(ix, golden)| {
        let (fp, text) = goldens.get(&ix).expect(&format!(
            "Golden value index not in goldens {} {}",
            title, ix
        ));
        assert_golden(golden, ix, fp, text);
    });
}
