use anyhow::Result;
use clap::Parser;

mod aoc2023;
mod util;

use aoc2023::*;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  /// Day to run solution for
  #[arg(short, long, default_value_t = 1)]
  day: u8,

  /// Use test input
  #[arg(short, long, default_value_t = false)]
  test: bool,
}

fn main() -> Result<()> {
  let args = Args::parse();
  
  match args.day {
    1 => {
      if args.test {
        return AOC::run_test::<Day01>();
      }
      AOC::run::<Day01>()
    },
    // 2 => AOC::run_test::<Day02>(),
    _ => panic!(""),
  }
}
