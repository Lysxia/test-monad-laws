# checkers-mtl

A library for testing implementations of `mtl` classes.

checkers-mtl defines laws for monadic effects (`mtl`, freer) as testable
properties.

Supported classes:

- `mtl`: `MonadExcept`, `MonadReader`, `MonadState`, `MonadWriter`
- `transformers`: `MonadTrans`
- `transformers-base`: `MonadBase`
- `monad-control`: `MonadTransControl`, `MonadBaseControl`

This project also tests the effectiveness of these laws, by including some
incorrect implementations, called *mutants*, and some invalid laws.

Related links and references
----------------------------

- [validity](https://github.com/NorfairKing/validity)
- [checkers](https://hackage.haskell.org/package/checkers)
- [quickcheck-classes](http://hackage.haskell.org/package/quickcheck-classes)
- [ClassLaws](https://hackage.haskell.org/package/ClassLaws)
- [test-invariant](https://hackage.haskell.org/package/test-invariant-0.4.5.0/docs/Test-Invariant.html)

- Just `do` it: Simple Monadic Equational Reasoning. Jeremy Gibbons, Ralf Hinze.
- Proof abstraction for imperative languages. William L. Harrison.

Hackage search terms:
[laws](https://hackage.haskell.org/packages/search?terms=laws),
[properties](https://hackage.haskell.org/packages/search?terms=properties).
