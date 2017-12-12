{-# LANGUAGE TypeApplications #-}

module Test.Monad.Except.Checkers where

import Control.Monad.Except
import Test.QuickCheck

import Test.Checkers
import Test.Mutants
import Test.Monad.Except
import Test.Monad.Except.Mutants

checkExcept :: IO ()
checkExcept = do
  ok "throwZero" $ \e (Fn k) -> throwZero @(Except Int) @Int @Int e k
  ok "throw-catch" $ \e (Fn k) -> throw_catch @(Except Int) @Int e k
  ok "catch-throw" (catch_throw @(Except Int) @Int)
  ok "catch-catch" $ \e (Fn h1) (Fn h2) -> catch_catch @(Except Int) @Int e h1 h2
  ok "catch-return" $ \a (Fn h) -> catch_return @(Except Int) @Int a h
  ok "catch-bind" $ \a (Fn k) (Fn h) -> catch_bind @(Except Int) @Int @Int a k h

checkExcept' :: IO ()
checkExcept' = do
  ko "bad-throwZero" $ \e (Fn k) -> bad_throwZero @(Except Int) @Int @Int e k
  ko "bad-throw-catch" $ \e (Fn k) -> bad_throw_catch @(Except Int) @Int e k
  ko "bad-catch-catch-1" $ \e (Fn h1) (Fn h2) ->
    bad_catch_catch_1 @(Except Int) @Int e h1 h2
  ko "bad-catch-catch-2" $ \e (Fn h1) (Fn h2) ->
    bad_catch_catch_2 @(Except Int) @Int e h1 h2
  ko "bad-catch-bind" $ \a (Fn k) (Fn h) -> bad_catch_bind @(Except Int) @Int @Int a k h

  ko "mut-1-throw-catch" $ \e (Fn k) -> throw_catch @(MutantExcept1 Int) @Int e k
  ok "mut-1-bad-throw-catch" $ \e (Fn k) -> bad_throw_catch @(MutantExcept1 Int) @Int e k
  ko "mut-1-bad-catch-catch-1" $ \e (Fn h1) (Fn h2) ->
    bad_catch_catch_1 @(MutantExcept1 Int) @Int e h1 h2
  ko "mut-1-bad-catch-catch-2" $ \e (Fn h1) (Fn h2) ->
    bad_catch_catch_2 @(MutantExcept1 Int) @Int e h1 h2
  ko "mut-1-bad-catch-bind" $ \a (Fn k) (Fn h) ->
    bad_catch_bind @(MutantExcept1 Int) @Int @Int a k h

  ko "mut-2-throw-catch" $ \e (Fn k) -> throw_catch @(MutantExcept2 Int) @Int e k
  ko "mut-2-catch-catch" $ \e (Fn h1) (Fn h2) -> catch_catch @(MutantExcept2 Int) @Int e h1 h2
  ko "catch-bind" $ \a (Fn k) (Fn h) -> catch_bind @(MutantExcept2 Int) @Int @Int a k h
