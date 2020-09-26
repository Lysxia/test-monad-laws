# Testable monad and mtl laws

A library for testing implementations of *mtl* classes.

*test-monad-laws* defines laws for monadic effects as QuickCheck
properties.

Supported classes:

- `mtl`: `MonadExcept`, `MonadReader`, `MonadState`, `MonadWriter`
- `transformers`: `MonadTrans`
- `transformers-base`: `MonadBase`
- `monad-control`: `MonadTransControl`, `MonadBaseControl`

This project also tests the effectiveness of these laws, by including some
incorrect implementations, called *mutants*, and some invalid laws.

Organization
------------

For every *mtl* class, for example `MonadReader`:

- The most important module is `Test.Monad.Reader`, defining laws for the class.
  Note that these laws are not official. But if your instance does not satisfy them,
  now you know.
- Mutants and bad laws are in `Test.Monad.Reader.Mutants`.
- For convenience, all the good laws are gathered in a single list in
  `Test.Monad.Reader.Checkers`.
  It can easily be consumed by the library *tasty-quickcheck*.

Related links and references
----------------------------

- [validity](https://github.com/NorfairKing/validity)
- [checkers](https://hackage.haskell.org/package/checkers)
- [quickcheck-classes](http://hackage.haskell.org/package/quickcheck-classes)
- [hedgehog-classes](https://hackage.haskell.org/package/hedgehog-classes)
- [ClassLaws](https://hackage.haskell.org/package/ClassLaws)
- [test-invariant](https://hackage.haskell.org/package/test-invariant-0.4.5.0/docs/Test-Invariant.html)

Papers with some relevant laws:

- Just `do` it: Simple Monadic Equational Reasoning. Jeremy Gibbons, Ralf Hinze.
- Proof abstraction for imperative languages. William L. Harrison.

Hackage search terms:
[laws](https://hackage.haskell.org/packages/search?terms=laws),
[properties](https://hackage.haskell.org/packages/search?terms=properties).
