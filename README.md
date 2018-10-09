# atcoder

Here is my atcoder log (using mainly Haskell).

## memo

- Haskell environment
  - I use stack `lts-12.11 (ghc-8.4.3)` (latest stable) for vscode w/ hie
  - I also use `ghc-7.10.3 -O2` for testing w/ contest's environment
    - `stack install ghc --resolver ghc-7.10.3`
    - `stack ghc --resolver ghc-7.10.3 -- `
  - To make contest dir, run `stack new CONTEST_NAME https://raw.githubusercontent.com/matonix/atcoder/master/stack-templates/atcoder.hsfile`