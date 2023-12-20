use std::iter::Peekable;

use anyhow::{Context, Result};

use crate::util::read_input;

use super::Day;

#[derive(Default)]
pub struct Day01;

impl Day for Day01 {
  fn run(&self) -> Result<()> {
    self.run_part1()?;
    self.run_part2()?;
    Ok(())
  }

  fn run_test(&self) -> Result<()> {
    self.run_part1_test()?;
    self.run_part2_test()?;
    Ok(())
  }
}

/// --- Day 1: Trebuchet?! ---
///
/// Something is wrong with global snow production, and you've been selected
/// to take a look. The Elves have even given you a map; on it, they've used
/// stars to mark the top fifty locations that are likely to be having problems.
///
/// You've been doing this long enough to know that to restore snow operations,
/// you need to check all fifty stars by December 25th.
///
/// Collect stars by solving puzzles. Two puzzles will be made available on each
/// day in the Advent calendar; the second puzzle is unlocked when you complete
/// the first. Each puzzle grants one star. Good luck!
///
/// You try to ask why they can't just use a weather machine ("not powerful
/// enough") and where they're even sending you ("the sky") and why your map
/// looks mostly blank ("you sure ask a lot of questions") and hang on did you
/// just say the sky ("of course, where do you think snow comes from") when you
/// realize that the Elves are already loading you into a trebuchet ("please
/// hold still, we need to strap you in").
///
/// As they're making the final adjustments, they discover that their calibration
/// document (your puzzle input) has been amended by a very young Elf who was
/// apparently just excited to show off her art skills. Consequently, the Elves
/// are having trouble reading the values on the document.
///
/// The newly-improved calibration document consists of lines of text; each
/// line originally contained a specific calibration value that the Elves now
/// need to recover. On each line, the calibration value can be found by
/// combining the first digit and the last digit (in that order) to form a
/// single two-digit number.
impl Day01 {
  fn solution_part1(&self, input: String) -> i32 {
    let mut nums = vec![];

    for line in input.lines() {
      if line.is_empty() {
        continue;
      }

      let first = line.chars().skip_while(|c| !c.is_digit(10)).next().unwrap();

      let last = line
        .chars()
        .rev()
        .skip_while(|c| !c.is_digit(10))
        .next()
        // this shouldn't be necessary, but just in case, reuse first digit
        .unwrap_or(first.clone());

      let num = format!("{first}{last}");
      let num = num.parse::<i32>().unwrap();
      nums.push(num);
    }

    nums.iter().sum()
  }

  fn run_part1(&self) -> Result<()> {
    let input = read_input("day01-1.txt").context("Read part 1 input")?;
    let output = self.solution_part1(input.to_string());
    println!("Day 1 - part 1: {output}");

    Ok(())
  }

  fn run_part1_test(&self) -> Result<()> {
    let input = r#"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"#;
    let output = self.solution_part1(input.to_string());
    println!("Day 1 - part 1 (test): {output}");

    Ok(())
  }
}

/// --- Part Two ---
///
/// Your calculation isn't quite right. It looks like some of the digits are
/// actually spelled out with letters: one, two, three, four, five, six, seven,
/// eight, and nine also count as valid "digits".
///
/// Equipped with this new information, you now need to find the real first and
/// last digit on each line. For example:
impl Day01 {
  // This tokenizer can read a stream forwards and backwards and extract
  // out the number
  fn tokenize_digit<C>(&self, chars: &mut Peekable<C>) -> Option<i32>
  where
    C: Iterator<Item = char>,
  {
    let Some(c) = chars.next() else {
      return None;
    };
    match c {
      '1'..='9' => c.to_digit(10).map(|x| x as i32),
      'e' => {
        let Some(c) = chars.next_if(|x| ['i', 'n', 'e', 'v'].contains(x)) else {
          return None;
        };
        match c {
          'i' => {
            let Some(_) = chars.next_if_eq(&'g') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'h') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'t') else {
              return None;
            };
            Some(8)
          }
          'n' => {
            let Some(c) = chars.next_if(|x| ['i', 'o'].contains(x)) else {
              return None;
            };
            match c {
              'o' => Some(1),
              'i' => {
                let Some(_) = chars.next_if_eq(&'n') else {
                  return None;
                };
                Some(9)
              }
              _ => unreachable!("Some how got {c} while parsing 'eno' or 'enin'"),
            }
          }
          'e' => {
            let Some(_) = chars.next_if_eq(&'r') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'h') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'t') else {
              return None;
            };
            Some(3)
          }
          'v' => {
            let Some(_) = chars.next_if_eq(&'i') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'f') else {
              return None;
            };
            Some(5)
          }
          _ => unreachable!(
            "Some how got {c} while parsing 'eight', 'evif', 'eno', 'enin', or 'eerht'"
          ),
        }
      }
      'f' => {
        let Some(c) = chars.next_if(|x| ['i', 'o'].contains(x)) else {
          return None;
        };
        match c {
          'i' => {
            let Some(_) = chars.next_if_eq(&'v') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'e') else {
              return None;
            };
            Some(5)
          }
          'o' => {
            let Some(_) = chars.next_if_eq(&'u') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'r') else {
              return None;
            };
            Some(4)
          }
          _ => unreachable!("Some how got {c} while parsing 'four' or 'five'"),
        }
      }
      'n' => {
        let Some(c) = chars.next_if(|x| ['e', 'i'].contains(x)) else {
          return None;
        };
        match c {
          'i' => {
            let Some(_) = chars.next_if_eq(&'n') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'e') else {
              return None;
            };
            Some(9)
          }
          'e' => {
            let Some(_) = chars.next_if_eq(&'v') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'e') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'s') else {
              return None;
            };
            Some(7)
          }
          _ => unreachable!("Some how got {c} while parsing 'nine' or 'neves'"),
        }
      }
      'o' => {
        let Some(c) = chars.next_if(|x| ['n', 'w'].contains(x)) else {
          return None;
        };
        match c {
          'n' => {
            let Some(_) = chars.next_if_eq(&'e') else {
              return None;
            };
            Some(1)
          }
          'w' => {
            let Some(_) = chars.next_if_eq(&'t') else {
              return None;
            };
            Some(2)
          }
          _ => unreachable!("Some how got {c} while parsing 'one' or 'owt'"),
        }
      }
      'r' => {
        let Some(_) = chars.next_if_eq(&'u') else {
          return None;
        };
        let Some(_) = chars.next_if_eq(&'o') else {
          return None;
        };
        let Some(_) = chars.next_if_eq(&'f') else {
          return None;
        };
        Some(4)
      }
      's' => {
        let Some(c) = chars.next_if(|x| ['e', 'i'].contains(x)) else {
          return None;
        };
        match c {
          'i' => {
            let Some(_) = chars.next_if_eq(&'x') else {
              return None;
            };
            Some(6)
          }
          'e' => {
            let Some(_) = chars.next_if_eq(&'v') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'e') else {
              return None;
            };
            let Some(_) = chars.next_if_eq(&'n') else {
              return None;
            };
            Some(7)
          }
          _ => unreachable!("Some how got {c} while parsing 'six' or 'seven'"),
        }
      }
      't' => {
        let Some(c) = chars.next_if(|x| ['h', 'w'].contains(x)) else {
          return None;
        };
        match c {
          'w' => {
            let Some(_) = chars.next_if_eq(&'o') else {
              return None;
            };
            Some(2)
          }
          'h' => {
            let Some(c) = chars.next_if(|x| ['r', 'g'].contains(x)) else {
              return None;
            };
            match c {
              'r' => {
                let Some(_) = chars.next_if_eq(&'e') else {
                  return None;
                };
                let Some(_) = chars.next_if_eq(&'e') else {
                  return None;
                };
                Some(3)
              }
              'g' => {
                let Some(_) = chars.next_if_eq(&'i') else {
                  return None;
                };
                let Some(_) = chars.next_if_eq(&'e') else {
                  return None;
                };
                Some(8)
              }
              _ => unreachable!("Some how got {c} while parsing 'three' or 'thgie'"),
            }
          }
          _ => unreachable!("Some how got {c} while parsing 'two', 'three', or 'thgie'"),
        }
      }
      'x' => {
        let Some(_) = chars.next_if_eq(&'i') else {
          return None;
        };
        let Some(_) = chars.next_if_eq(&'s') else {
          return None;
        };
        Some(6)
      }
      _ => None,
    }
  }

  fn solution_part2(&self, input: String) -> i32 {
    let mut nums = vec![];
    let lines = input.lines();

    for line in lines {
      if line.is_empty() {
        continue;
      }

      let mut chars = line.chars().peekable();
      let mut first: Option<i32> = None;
      while first == None {
        if chars.peek().is_none() {
          panic!("Didn't find a number");
        }
        first = self.tokenize_digit(&mut chars);
      }

      let mut chars = line.chars().rev().peekable();
      let mut last: Option<i32> = None;
      while last == None {
        if chars.peek().is_none() {
          panic!("Didn't find a number");
        }
        last = self.tokenize_digit(&mut chars);
      }

      let num = (first.unwrap() * 10) + last.unwrap();
      nums.push(num);
    }

    nums.iter().sum()
  }

  fn run_part2(&self) -> Result<()> {
    let input = read_input("day01-2.txt").context("Read part 1 input")?;
    let output = self.solution_part2(input.to_string());
    println!("Day 1 - part 2: {output}");

    Ok(())
  }

  fn run_part2_test(&self) -> Result<()> {
    let input = r#"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"#;
    let output = self.solution_part2(input.to_string());
    println!("Day 1 - part 2 (test): {output}");

    Ok(())
  }
}
