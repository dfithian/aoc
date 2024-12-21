use std::env::args_os;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

/// Parse an input file including empty lines.
pub fn parse_input_file_raw<A, F>(init: A, mut f: F) -> A
where
    F: FnMut(A, String) -> A,
{
    let mut args = args_os();
    let _ = args.next();
    let input_path = args
        .next()
        .map(|s| PathBuf::from(s.as_os_str()))
        .expect("input path not given as argument");
    let input_reader = BufReader::new(File::open(input_path).expect("input path is invalid"));
    let mut acc = init;

    for line in input_reader.lines() {
        let line = line.unwrap();
        acc = f(acc, line);
    }
    acc
}

/// Parse an input file ignoring empty lines.
pub fn parse_input_file<A, F>(init: A, mut f: F) -> A
where
    F: FnMut(A, String) -> A,
{
    parse_input_file_raw(
        init,
        |acc, line| {
            if !line.is_empty() {
                f(acc, line)
            } else {
                acc
            }
        },
    )
}

/// Parse an input file assuming one empty line in the middle of the file.
pub fn parse_input_file_split<A, F, G>(init: A, mut f: F, mut g: G) -> A
where
    F: FnMut(A, String) -> A,
    G: FnMut(A, String) -> A,
{
    let mut has_split = false;
    parse_input_file_raw(init, |acc, line| {
        if !line.is_empty() {
            if !has_split {
                f(acc, line)
            } else {
                g(acc, line)
            }
        } else {
            has_split = true;
            acc
        }
    })
}
