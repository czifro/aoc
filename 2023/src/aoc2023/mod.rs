use anyhow::Result;

mod day01;

pub use day01::Day01;

pub trait Day: Default {
  fn run(&self) -> Result<()>;
  fn run_test(&self) -> Result<()>;
}

#[derive(Default)]
pub struct AOC;

impl AOC {
  pub fn run<D>() -> Result<()>
  where
    D: Day,
  {
    let day = D::default();
    day.run()
  }

  pub fn run_test<D>() -> Result<()>
  where
    D: Day,
  {
    let day = D::default();
    day.run_test()
  }
}
