{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Except.Checkers where

import Control.Monad.Except
import Control.Monad.State
import Test.QuickCheck (Property)
import Test.QuickCheck.HigherOrder (CoArbitrary, Constructible, TestEq, ok, ko)

import Test.Monad.Instances ()
import Test.Monad.Except
import Test.Monad.Except.Mutants

checkExcept
  :: forall m a b e
  .  ( MonadError e m
     , CoArbitrary b, CoArbitrary e
     , TestEq (m a)
     , Constructible a, Constructible e, Constructible (m a), Constructible (m b))
  => [(String, Property)]
checkExcept =
  [ ok "throwZero"    (throwZero @m @a @b)
  , ok "throw-catch"  (throw_catch @m @a)
  , ok "catch-throw"  (catch_throw @m @a)
  , ok "catch-catch"  (catch_catch @m @a)  -- this takes one minute in test/prism-error?!
  , ok "catch-return" (catch_return @m @a)
  ]
{-# NOINLINE checkExcept #-}

checkExcept_ :: [(String, Property)]
checkExcept_ =
     checkExcept @(Either Int) @Int @Int
  ++ checkExcept @(StateT Int (Either Int)) @Int @Int
  ++ checkExcept @(ExceptT Int (State Int)) @Int @Int
  ++ checkExcept @(StateT Int (ExceptT Int (State Int))) @Int @Int

checkExcept' :: [(String, Property)]
checkExcept' =
  [ ok "catch-bind-e"      (catch_bind @(Except Int) @Int @Int)
  , ko "catch-bind-se"     (catch_bind @(StateT Int (Either Int)) @Int @Int)
  , ok "catch-bind-es"     (catch_bind @(ExceptT Int (State Int)) @Int @Int)
  , ko "catch-bind-ses"    (catch_bind @(StateT Int (ExceptT Int (State Int))) @Int @Int)

  , ko "bad-throwZero"     (bad_throwZero @(Except Int) @Int @Int)
  , ko "bad-throw-catch"   (bad_throw_catch @(Except Int) @Int)
  , ko "bad-catch-catch-1" (bad_catch_catch_1 @(Except Int) @Int)
  , ko "bad-catch-catch-2" (bad_catch_catch_2 @(Except Int) @Int)
  , ko "bad-catch-bind"    (bad_catch_bind @(Except Int) @Int @Int)

  , ko "mut-1-throw-catch"       (throw_catch @(MutantExcept1 Int) @Int)
  , ok "mut-1-bad-throw-catch"   (bad_throw_catch @(MutantExcept1 Int) @Int)
  , ok "mut-1-bad-catch-catch-1" (bad_catch_catch_1 @(MutantExcept1 Int) @Int)
  , ok "mut-1-bad-catch-catch-2" (bad_catch_catch_2 @(MutantExcept1 Int) @Int)
  , ok "mut-1-bad-catch-bind"    (bad_catch_bind @(MutantExcept1 Int) @Int @Int)

  , ko "mut-2-throw-catch" (throw_catch @(MutantExcept2 Int) @Int)
  , ko "mut-2-catch-catch" (catch_catch @(MutantExcept2 Int) @Int)
  , ko "mut-2-catch-bind"  (catch_bind @(MutantExcept2 Int) @Int @Int)
  ]
{-# NOINLINE checkExcept' #-}
