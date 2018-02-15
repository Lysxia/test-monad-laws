{-# LANGUAGE TypeApplications #-}

module Test.Monad.Reader.Checkers where

import Control.Monad.Reader
import Test.QuickCheck (Property)
import Test.QuickCheck.HigherOrder (ok, ko)

import Test.Monad.Instances ()
import Test.Monad.Morph
import Test.Monad.Reader
import Test.Monad.Reader.Mutants

checkReader :: [(String, Property)]
checkReader =
  [ ok "ask-ask"         (ask_ask @(Reader Int))
  , ok "local-ask"       (local_ask @(Reader Int))
  , ok "local-local"     (local_local @(Reader Int) @Int)
  , ok "bindHom-local"   (\f -> bindHom @(Reader Int) @_ @Int @Int (local f))
  , ok "returnHom-local" (\f -> returnHom @(Reader Int) @_ @Int (local f))
  ]

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
