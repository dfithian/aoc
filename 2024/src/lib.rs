use std::env::args_os;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub fn parse_input_file<A, F>(init: A, f: F) -> A
where
    F: Fn(A, String) -> A,
{
    let mut args = args_os();
    let _ = args.next();
    let input_path = args
        .next()
        .map(|s| PathBuf::from(s.as_os_str()))
        .expect("input path not given as argument");
    let input_reader = BufReader::new(File::open(input_path).expect("input_path is invalid"));
    let mut acc = init;
    for line in input_reader.lines() {
        let line = line.unwrap();
        acc = f(acc, line);
    }
    acc
}
