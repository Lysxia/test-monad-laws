{-# LANGUAGE ScopedTypeVariables #-}

module Test.Monad.Writer.Mutants where

import Control.Monad.Writer

import Test.Checkers

bad_listen_tell
  :: forall m w
  .  MonadWriter w m
  => w -> Equation (m w)
bad_listen_tell w =
  fmap snd (listen (tell w)) :=: return w
