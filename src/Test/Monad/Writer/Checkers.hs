{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Writer.Checkers where

import Control.Monad.Writer
import Test.QuickCheck (CoArbitrary, Function, Property)
import Test.QuickCheck.HigherOrder (Constructible, TestEq, ok, ko)

import Test.Monad.Instances ()
import Test.Monad.Writer
import Test.Monad.Writer.Mutants

checkWriter
  :: forall m a b w
  .  ( MonadWriter w m
     , CoArbitrary b, Function b, Show b
     , CoArbitrary w, Function w, Show w
     , Constructible a, Constructible w, Constructible (m a), Constructible (m b)
     , Constructible (m (a, w -> w))
     , TestEq (m ()), TestEq (m (a, w)), TestEq (m w), TestEq (m ((a, w), w)) )
  => [(String, Property)]
checkWriter =
  [ ok "tell-tell"     (tell_tell     @m)
  , ok "tell-mempty"   (tell_mempty   @m)
  , ok "listen-return" (listen_return @m @a)
  , ok "listen-bind"   (listen_bind   @m @b @a)
  , ok "listen-tell"   (listen_tell   @m)
  , ok "listen-listen" (listen_listen @m @a)
  , ok "listen-pass"   (listen_pass   @m @a)
  , ok "pass-tell"     (pass_tell   @m)
  ]

checkWriter_ :: [(String, Property)]
checkWriter_ = checkWriter @(Writer (Sum Int)) @Int @Int

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
