{-# LANGUAGE TypeApplications #-}

module Test.Monad.Writer.Checkers where

import Control.Monad.Writer
import Test.QuickCheck (Property)

import Test.Checkers
import Test.Checkers.Instances ()
import Test.Monad.Writer
import Test.Monad.Writer.Mutants

checkWriter :: [(String, Property)]
checkWriter =
  [ ok "tell-tell"     (tell_tell     @(Writer (Sum Int)))
  , ok "tell-mempty"   (tell_mempty   @(Writer (Sum Int)))
  , ok "listen-return" (listen_return @(Writer (Sum Int)) @Int)
  , ok "listen-bind"   (listen_bind   @(Writer (Sum Int)) @Int @Int)
  , ok "listen-tell"   (listen_tell   @(Writer (Sum Int)))
  , ok "listen-listen" (listen_listen @(Writer (Sum Int)) @Int)
  , ok "listen-pass"   (listen_pass   @(Writer (Sum Int)) @Int)
  ]

checkWriter' :: [(String, Property)]
checkWriter' =
  [ ko "bad-listen-tell" (bad_listen_tell @(Writer (Sum Int)))

  , ko "mut-1-listen-tell" (listen_tell   @(MutantWriter TellDoesNothing (Sum Int)))

  , ko "mut-2-listen-tell" (listen_tell   @(MutantWriter ListenDoesNothing (Sum Int)))
  , ko "mut-2-listen-pass" (listen_pass   @(MutantWriter ListenDoesNothing (Sum Int)) @Int)

  , ko "mut-3-listen-tell"     (listen_tell     @(MutantWriter ListenResets (Sum Int)))
  , ko "mut-3-listen-listen"   (listen_listen   @(MutantWriter ListenResets (Sum Int)) @Int)
  , ko "mut-3-listen-pass"     (listen_pass     @(MutantWriter ListenResets (Sum Int)) @Int)
  , ok "mut-3-bad-listen-tell" (bad_listen_tell @(MutantWriter ListenResets (Sum Int)))

  , ko "mut-4-listen-pass"   (listen_pass     @(MutantWriter PassDoesNothing (Sum Int)) @Int)
  ]
