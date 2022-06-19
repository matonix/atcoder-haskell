use std::vec;

use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
  input! {
    n: usize,
    lrs: [(usize, usize); n],
  }
  let mut v = vec![0isize; 200001];
  for (l, r) in lrs {
    v[l] += 1;
    v[r] -= 1;
  }
  let mut ans = vec![];
  let mut inRange = false;
  for (i, x) in v.into_iter().cumsum::<isize>().enumerate() {
    if x > 0 {
      if !inRange {
        ans.push(i);
      }
      inRange = true;
    } else {
      if inRange {
        ans.push(i);
      }
      inRange = false;
    }
  }
  for (l, r) in ans.iter().tuples() {
    println!("{:?} {:?}", l, r);
  }
}