{-# LANGUAGE TypeApplications #-}

module Test.Monad.Reader.Checkers where

import Control.Monad.Reader
import Test.QuickCheck

import Test.Checkers
import Test.Mutants
import Test.Monad.Morph
import Test.Monad.Reader
import Test.Monad.Reader.Mutants

checkReader :: IO ()
checkReader = do
  ok "ask-ask" (ask_ask @(Reader Int))
  ok "local-ask" (\(Fn f) -> local_ask @(Reader Int) f)
  ok "local-local" (\(Fn f) (Fn g) (Fn m) ->
    local_local @(Reader Int) @Int f g (reader m))
  ok "bindHom-local" (\(Fn f) (Fn m) (Fn2 k) ->
    bindHom @(Reader Int) @_ @Int @Int (local f) (reader m) (reader . k))
  ok "returnHom-local" (\(Fn f) -> returnHom @(Reader Int) @_ @Int (local f))

checkReader' :: IO ()
checkReader' = do
  ok "mut-1-ask-ask"         (ask_ask @(MutantReader LocalId Int))
  ko "mut-1-local-ask"       (\(Fn f) -> local_ask @(MutantReader LocalId Int) f)
  ok "mut-1-local-local"     (\(Fn f) (Fn g) (Fn m) ->
    local_local @(MutantReader LocalId Int) @Int f g (reader m))
  ok "mut-1-bindHom-local"   (\(Fn f) (Fn m) (Fn2 k) ->
    bindHom @(MutantReader LocalId Int) @_ @Int @Int (local f) (reader m) (reader . k))
  ok "mut-1-returnHom-local" (\(Fn f) ->
    returnHom @(MutantReader LocalId Int) @_ @Int (local f))

  ok "mut-2-ask-ask"         (ask_ask @(MutantReaderT LocalRunsTwice Int []))
  ok "mut-2-local-ask"       (\(Fn f) -> local_ask @(MutantReaderT LocalRunsTwice Int []) f)
  ko "mut-2-local-local"     (\(Fn f) (Fn g) (Fn m) ->
    local_local @(MutantReaderT LocalRunsTwice Int []) @Int f g (Mutant (ReaderT m)))
  ko "mut-2-bindHom-local"   (\(Fn f) (Fn m) (Fn2 k) ->
    bindHom @(MutantReaderT LocalRunsTwice Int []) @_ @Int @Int (local f) (Mutant (ReaderT m)) (Mutant . ReaderT . k))
  ok "mut-2-returnHom-local" (\(Fn f) ->
    returnHom @(MutantReaderT LocalRunsTwice Int []) @_ @Int (local f))
