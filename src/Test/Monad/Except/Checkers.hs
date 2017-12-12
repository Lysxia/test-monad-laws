{-# LANGUAGE TypeApplications #-}

module Test.Monad.Except.Checkers where

import Control.Monad.Except
import Test.QuickCheck

import Test.Checkers
import Test.Mutants
import Test.Monad.Except

checkExcept :: IO ()
checkExcept = do
  ok "throwZero" $ \e (Fn k) -> throwZero @(Except Int) @Int @Int e k
  ok "throw-catch" $ \e (Fn k) -> throw_catch @(Except Int) @Int e k
  ok "catch-throw" (catch_throw @(Except Int) @Int)
  ok "catch-catch" $ \e (Fn h1) (Fn h2) -> catch_catch @(Except Int) @Int e h1 h2
  ok "catch-return" $ \a (Fn h) -> catch_return @(Except Int) @Int a h
  ok "catch-bind" $ \a (Fn k) (Fn h) -> catch_bind @(Except Int) @Int @Int a k h

