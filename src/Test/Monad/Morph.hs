-- | Monad homomorphisms.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Test.Monad.Morph where

import Test.QuickCheck.HigherOrder (Equation(..))

-- | Natural transformation.
type m ~> n = forall t. m t -> n t

-- | Monad morphisms commute with @(>>=)@ and @(return)@.
-- @
-- hom (m '>>=' k) = hom m '>>=' hom . k
-- @
bindHom
  :: forall m n a b
  .  (Monad m, Monad n)
  => (m ~> n) -> m a -> (a -> m b) -> Equation (n b)
bindHom hom m k = hom (m >>= k) :=: (hom m >>= hom . k)

-- | A monad morphism maps @('return' a)@ to @('return' a)@.
-- @
-- hom ('return' a) = 'return' a
-- @
returnHom
  :: forall m n a
  .  (Monad m, Monad n)
  => (m ~> n) -> a -> Equation (n a)
returnHom hom a = hom (return a) :=: return a
