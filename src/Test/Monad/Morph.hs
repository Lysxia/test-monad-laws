-- | Monad homomorphisms.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Test.Monad.Morph where

import Test.QuickCheck

import Test.Checkers

bindHom
  :: forall m n a b
  .  (Monad m, Monad n, EqProp (n b))
  => (forall t. m t -> n t) -> m a -> (a -> m b) -> Property
bindHom hom m k = hom (m >>= k) =-= (hom m >>= hom . k)

returnHom
  :: forall m n a
  .  (Monad m, Monad n, EqProp (n a))
  => (forall t. m t -> n t) -> a -> Property
returnHom hom a = hom (return a) =-= return a
