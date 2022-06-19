use std::cmp::min;

use proconio::input;

fn main() {
  input! {
    h1: isize,
    h2: isize,
    h3: isize,
    w1: isize,
    w2: isize,
    w3: isize,
  }
  let mut ans = 0; 
  for a11 in 1..=min(h1, w1)-2 {
    for a12 in 1..=min(h2, w1-a11)-1 {
      for a21 in 1..=min(h1-a11, w2)-1 {
        for a22 in 1..=min(h2-a12, w2-a21)-1 {
          let a13 = w1 - a11 - a12;
          let a23 = w2 - a21 - a22;
          let a31 = h1 - a11 - a21;
          let a32 = h2 - a12 - a22;
          let a33h = h3 - a13 - a23;
          let a33w = w3 - a31 - a32;
          if a33h == a33w && a33h >= 1 && a33h <= h3 && a33w <= w3 {
            ans += 1;
          }
        }  
      }
    }
  }
  println!("{:?}", ans);
}