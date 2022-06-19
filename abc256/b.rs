use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
  input! {
    n: usize,
    a: [usize; n],
  }
  let ans = a.iter().rev().cumsum().collect_vec().into_iter().rev().take_while(|v: &usize| v.ge(&4)).count();
  println!("{:?}", ans);
}