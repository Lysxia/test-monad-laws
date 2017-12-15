{-# LANGUAGE TypeApplications #-}

module Test.Monad.Reader.Checkers where

import Control.Monad.Reader
import Test.QuickCheck

import Test.Checkers
import Test.Mutants
import Test.Monad.Morph
import Test.Monad.Reader

checkReader :: IO ()
checkReader = do
  ok "ask-ask" (ask_ask @(Reader Int))
  ok "local-ask" (\(Fn f) -> local_ask @(Reader Int) f)
  ok "local-local" (\(Fn f) (Fn g) (Fn m) ->
    local_local @(Reader Int) @Int f g (reader m))
  ok "bindHom-local" (\(Fn f) (Fn m) (Fn2 k) ->
    bindHom @(Reader Int) @_ @Int @Int (local f) (reader m) (reader . k))
  ok "returnHom-local" (\(Fn f) -> returnHom @(Reader Int) @_ @Int (local f))
