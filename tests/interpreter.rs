use libtest_mimic::{Arguments, Failed, Trial};
use similar::{ChangeTag, TextDiff};
use std::{cell::RefCell, path::Path, rc::Rc};

fn main() {
    let args = Arguments::from_args();
    let mut trials = Vec::new();

    for entry in std::fs::read_dir("tests/scripts").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let name = entry.file_name().to_str().unwrap().to_owned();
        trials.push(Trial::test(name, move || run_one(&path)));
    }

    libtest_mimic::run(&args, trials).exit();
}

fn run_one(dir: &Path) -> Result<(), Failed> {
    let filename = dir.file_name().unwrap().to_str().unwrap();
    let source = std::fs::read_to_string(dir.join(format!("{filename}.pinky")))?;
    let expected_output = std::fs::read(dir.join(format!("{filename}.exp")))?;

    let out = Rc::new(RefCell::new(vec![]));
    rpinky::run(&source, None, out.clone());

    if out.borrow().clone() != expected_output {
        let expected = String::from_utf8(expected_output)?;
        let actual = String::from_utf8(out.borrow().clone())?;
        let diff = TextDiff::from_lines(&expected, &actual);
        let mut buf = String::new();
        for change in diff.iter_all_changes() {
            let sign = match change.tag() {
                ChangeTag::Delete => "-",
                ChangeTag::Insert => "+",
                ChangeTag::Equal => " ",
            };
            buf += &format!("{sign}{change}");
        }
        return Err(buf.into());
    }
    Ok(())
}
