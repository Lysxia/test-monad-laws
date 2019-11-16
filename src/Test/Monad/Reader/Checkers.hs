{-# LANGUAGE
    AllowAmbiguousTypes,
    FlexibleContexts,
    ScopedTypeVariables,
    TypeApplications #-}

module Test.Monad.Reader.Checkers where

import Control.Monad.Reader
import Control.Monad.State (StateT)
import Test.QuickCheck (Gen, Property)
import Test.QuickCheck.HigherOrder (CoArbitrary, Constructible, TestEq, ok, ko)

import Test.Monad.Instances ()
import Test.Monad.Morph
import Test.Monad.Reader
import Test.Monad.Reader.Mutants

checkReader
  :: forall m a b r
  .  ( MonadReader r m
     , CoArbitrary Gen b, CoArbitrary Gen r
     , Constructible a, Constructible r, Constructible (m a), Constructible (m b)
     , TestEq (m a), TestEq (m r))
  => [(String, Property)]
checkReader =
  [ ok "ask-ask"         (ask_ask @m)
  , ok "local-ask"       (local_ask @m)
  , ok "local-local"     (local_local @m @a)
  , ok "bindHom-local"   (\f -> bindHom @m @_ @b @a (local f))
  , ok "returnHom-local" (\f -> returnHom @m @_ @a (local f))
  ]
{-# NOINLINE checkReader #-}

checkReader_ :: [(String, Property)]
checkReader_
  =  checkReader @(Reader Int) @Int @Int
  ++ checkReader @(StateT Int (Reader Int)) @Int @Int

type Mutant1 = MutantReader LocalId Int
type Mutant2 = MutantReaderT LocalRunsTwice Int []

checkReader' :: [(String, Property)]
checkReader' =
  [ ok "mut-1-ask-ask"         (ask_ask @Mutant1)
  , ko "mut-1-local-ask"       (local_ask @Mutant1)
  , ok "mut-1-local-local"     (local_local @Mutant1 @Int)
  , ok "mut-1-bindHom-local"   (\f -> bindHom @Mutant1 @_ @Int @Int (local f))
  , ok "mut-1-returnHom-local" (\f -> returnHom @Mutant1 @_ @Int (local f))

  , ok "mut-2-ask-ask"         (ask_ask @Mutant2)
  , ok "mut-2-local-ask"       (local_ask @Mutant2)
  , ko "mut-2-local-local"     (local_local @Mutant2 @Int)
  , ko "mut-2-bindHom-local"   (\f -> bindHom @Mutant2 @_ @Int @Int (local f))
  , ok "mut-2-returnHom-local" (\f -> returnHom @Mutant2 @_ @Int (local f))
  ]
{-# NOINLINE checkReader' #-}
