-- | Monad homomorphisms.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Test.Monad.Morph where

import Test.QuickCheck

import Test.Checkers

bindHom
  :: forall m n a b
  .  (Monad m, Monad n)
  => (forall t. m t -> n t) -> m a -> (a -> m b) -> Equation (n b)
bindHom hom m k = hom (m >>= k) :=: (hom m >>= hom . k)

returnHom
  :: forall m n a
  .  (Monad m, Monad n)
  => (forall t. m t -> n t) -> a -> Equation (n a)
returnHom hom a = hom (return a) :=: return a
