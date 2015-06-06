Bananasnake!
===

Make the classic game [Snake](http://en.wikipedia.org/wiki/Snake_(video_game)) using Functional Reactive Programming in
Haskell via [reactive-banana](https://hackage.haskell.org/package/reactive-banana
).

Setup
---

1. Clone this git repo
2. Recommended if you have no haskell setup:
  1. [Install nix](https://nixos.org/nix/)
  2. Run `nix-shell`. Wait a possibly long time.
  3. Run `cabal repl` to open the repl (the `:main` to start) or `cabal run` to run it directly.
3. Or `cabal sandbox init`, `cabal install --only-dependencies`, `cabal run`.
