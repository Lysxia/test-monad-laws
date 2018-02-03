{-# LANGUAGE TypeApplications #-}

module Test.Monad.Except.Checkers where

import Control.Monad.Except

import Test.Checkers
import Test.Checkers.Instances ()
import Test.Monad.Except
import Test.Monad.Except.Mutants

checkExcept :: IO ()
checkExcept = do
  ok "throwZero"    (throwZero @(Except Int) @Int @Int)
  ok "throw-catch"  (throw_catch @(Except Int) @Int)
  ok "catch-throw"  (catch_throw @(Except Int) @Int)
  ok "catch-catch"  (catch_catch @(Except Int) @Int)
  ok "catch-return" (catch_return @(Except Int) @Int)
  ok "catch-bind"   (catch_bind @(Except Int) @Int @Int)

checkExcept' :: IO ()
checkExcept' = do
  ko "bad-throwZero"     (bad_throwZero @(Except Int) @Int @Int)
  ko "bad-throw-catch"   (bad_throw_catch @(Except Int) @Int)
  ko "bad-catch-catch-1" (bad_catch_catch_1 @(Except Int) @Int)
  ko "bad-catch-catch-2" (bad_catch_catch_2 @(Except Int) @Int)
  ko "bad-catch-bind"    (bad_catch_bind @(Except Int) @Int @Int)

  ko "mut-1-throw-catch"       (throw_catch @(MutantExcept1 Int) @Int)
  ok "mut-1-bad-throw-catch"   (bad_throw_catch @(MutantExcept1 Int) @Int)
  ko "mut-1-bad-catch-catch-1" (bad_catch_catch_1 @(MutantExcept1 Int) @Int)
  ko "mut-1-bad-catch-catch-2" (bad_catch_catch_2 @(MutantExcept1 Int) @Int)
  ko "mut-1-bad-catch-bind"    (bad_catch_bind @(MutantExcept1 Int) @Int @Int)

  ko "mut-2-throw-catch" (throw_catch @(MutantExcept2 Int) @Int)
  ko "mut-2-catch-catch" (catch_catch @(MutantExcept2 Int) @Int)
  ko "mut-2-catch-bind"  (catch_bind @(MutantExcept2 Int) @Int @Int)
