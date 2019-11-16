{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Monad.Cont.Checkers where

import Control.Monad.Cont
import Control.Monad.State
import Test.QuickCheck (Gen, Property)
import Test.QuickCheck.HigherOrder (CoArbitrary, Constructible, TestEq, ok, ko)

import Test.Monad.Instances ()
import Test.Monad.Cont
-- import Test.Monad.Cont.Mutants

checkCont ::
  forall m a b.
  ( MonadCont m
  , TestEq (m a)
  , CoArbitrary Gen b, CoArbitrary Gen (m b)
  , Constructible a, Constructible (m a), Constructible (m b)
  ) =>
  [(String, Property)]
checkCont =
  [ ok "callCC-const"     (callCC_const @m @a)
  , ok "callCC-id"        (callCC_id @m @a)
  , ok "callCC-bind"      (callCC_bind @m @a)
  , ok "callCC-phantom"   (callCC_phantom @m @a @b)
  , ok "callCC-left-zero" (callCC_left_zero @m @a @b)
  ]
{-# NOINLINE checkCont #-}

checkCont_ :: [(String, Property)]
checkCont_ =
     checkCont @(Cont Int) @Int @Int
  ++ checkCont @(ContT Int (State Int)) @Int @Int
  ++ checkCont @(ContT Int []) @Int @Int
  ++ checkCont @(ContT Int (StateT Int [])) @Int @Int
