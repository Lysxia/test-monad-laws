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
  ]

checkWriter' :: [(String, Property)]
checkWriter' =
  [ ko "bad-listen-tell" (bad_listen_tell @(Writer (Sum Int)))
  ]
