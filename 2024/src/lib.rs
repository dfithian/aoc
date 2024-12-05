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
        if !line.is_empty() {
            acc = f(acc, line);
        }
    }
    acc
}

pub fn parse_input_file_split<A, F, G>(init: A, f: F, g: G) -> A
where
    F: Fn(A, String) -> A,
    G: Fn(A, String) -> A,
{
    let mut args = args_os();
    let _ = args.next();
    let input_path = args
        .next()
        .map(|s| PathBuf::from(s.as_os_str()))
        .expect("input path not given as argument");
    let input_reader = BufReader::new(File::open(input_path).expect("input_path is invalid"));
    let mut acc = init;

    let mut has_split = false;
    for line in input_reader.lines() {
        let line = line.unwrap();
        if !line.is_empty() {
            if !has_split {
                acc = f(acc, line);
            } else {
                acc = g(acc, line);
            }
        } else {
            has_split = true;
        }
    }
    acc
}
