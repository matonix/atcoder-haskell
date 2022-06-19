use itertools::Itertools;
use proconio::input;
#[allow(unused_imports)]
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, VecDeque};

// 多分これ https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_2_B

fn main() {
  input! {
    n: usize,
    xs: [usize; n],
    cs: [usize; n],
  }
  let mut ess = vec![];
  for i in 0..n {
    for j in 0..n {
      ess.push((i, j, 0));
    }
  }
  let es = xs.iter().zip(cs).enumerate().map(|(i, (&j, c))| (i, j-1, c)).collect_vec();
  // let graph = Graph::<(), usize, Directed>::from_edges(&es);
  let mut g = vec![vec![]; n];
  for (i, j, c) in ess {
    g[i].push(Edge(j, c as u64));
  }
  for (i, j, c) in es {
      g[i][j] = Edge(j, c as u64);
      // g[i].push(Edge(j, c as u64));
  }
  let mut ans = u64::MAX;
  for r in 1..=n {
    ans = ans.min(chu_liu_edmonds(&g, r));
  }
  println!("{:?}", ans);
}

// from: https://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=3601720#1

pub struct SCC<'a> {
  g: &'a [Vec<usize>],
  r_g: Vec<Vec<usize>>,
  post_order: VecDeque<usize>,
  used: Vec<bool>,
  pub order: Vec<usize>,
}

impl <'a> SCC<'a> {
  pub fn new(g: &'a [Vec<usize>]) -> Self {
      let n = g.len();
      let mut r_g = vec![vec![]; n];
      for u in 0..n {
          let conn = &g[u];
          for &v in conn {
              r_g[v].push(u);
          }
      }
      Self {
          g,
          r_g,
          post_order: VecDeque::new(),
          used: vec![false; n],
          order: vec![n; n],
      }
  }
  fn dfs(&mut self, u: usize) {
      self.used[u] = true;
      for i in 0 .. self.g[u].len() {
          let v = self.g[u][i];
          if !self.used[v] {
              self.dfs(v);
          }
      }
      self.post_order.push_front(u);
  }
  fn rdfs(&mut self, u: usize, k: usize) {
      self.used[u] = true;
      self.order[u] = k;
      for i in 0 .. self.r_g[u].len() {
          let v = self.r_g[u][i];
          if !self.used[v] {
              self.rdfs(v, k);
          }
      }
  }
  pub fn build(&mut self) {
      for v in 0 .. self.g.len() {
          if !self.used[v] {
              self.dfs(v);
          }
      }
      self.used = vec![false; self.g.len()];
      let mut k = 0;
      for i in 0 .. self.post_order.len() {
          let v = self.post_order[i];
          if !self.used[v] {
              self.rdfs(v, k);
              k += 1;
          }
      }
  }
}

#[derive(Debug, Clone, Copy)]
struct Edge(usize, u64);
fn min_edge(edges: &[Edge]) -> &Edge {
  let mut r = &edges[0];
  for e in edges {
      if e.1 < r.1 {
          r = e;
      }
  }
  r
}
fn chu_liu_edmonds(in_g: &[Vec<Edge>], root: usize) -> u64 {
  let mut min_in_g: Vec<Edge> = vec![];
  let mut min_out_g: Vec<Vec<usize>> = vec![vec![]; in_g.len()];
  for to in 0..in_g.len() {
      if to == root {
          min_in_g.push(Edge(1<<40, 0)); // null value
          continue;
      }
      let e = min_edge(&in_g[to]);
      min_in_g.push(e.clone());
      min_out_g[e.0].push(to);
  }

  let mut scc = SCC::new(&min_out_g);
  scc.build();

  let mut max_cmp = 0;
  for &cmp in &scc.order {
      if cmp > max_cmp {
          max_cmp = cmp;
      }
  }

  let no_loop = max_cmp == scc.order.len()-1;
  if no_loop {
      let mut res = 0;
      for e in &min_in_g {
          res += e.1;
      }
      return res;
  }

  let mut groups = vec![vec![]; max_cmp+1];
  for v in 0..scc.order.len() {
      let cmp = scc.order[v];
      groups[cmp].push(v);
  }

  let mut contracted_cost = 0;
  let mut new_in_g = vec![vec![]; max_cmp+1];
  for group in groups {
      if group.len() > 1 { // loop
          let cmp_to = scc.order[group[0]];
          for &v in &group {
              let cur_e = min_in_g[v].clone();

              contracted_cost += cur_e.1;

              for e in &in_g[v] {
                  let in_group = group.contains(&e.0);
                  if !in_group {
                      let cmp_from = scc.order[e.0];
                      let diff_cost = e.1 - cur_e.1;
                      new_in_g[cmp_to].push(Edge(cmp_from, diff_cost));
                  }
              }
          }
      } else {
          assert!(group.len() == 1);
          let v = group[0];
          for e in &in_g[v] {
              let cmp_to = scc.order[v];
              let cmp_from = scc.order[e.0];
              new_in_g[cmp_to].push(Edge(cmp_from, e.1));
          }
      }
  }

  let new_root = scc.order[root];

  contracted_cost + chu_liu_edmonds(&new_in_g, new_root)
}