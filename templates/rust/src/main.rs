use std::io;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let lines = stdin.lock().lines().map(|x| x.unwrap()).collect();
    lines
}

fn part1(_lines: Vec<String>) -> String {
    return "part1".into();
}

fn part2(_lines: Vec<String>) -> String {
    return "part2".into();
}

fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
