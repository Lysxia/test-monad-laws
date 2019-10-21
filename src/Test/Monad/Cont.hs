{-# LANGUAGE ScopedTypeVariables #-}

module Test.Monad.Cont where

import Control.Monad.Cont
import Data.Void (absurd)
import Test.QuickCheck.HigherOrder (Equation(..))

-- * 'MonadCont' laws

-- Reference: https://mail.haskell.org/pipermail/libraries/2019-October/030041.html

callCC_const :: forall m a. MonadCont m => m a -> Equation (m a)
callCC_const m = callCC (const m) :=: m

callCC_id :: forall m a. MonadCont m => a -> Equation (m a)
callCC_id a = callCC ($ a) :=: pure a

callCC_bind :: forall m a. MonadCont m => m a -> Equation (m a)
callCC_bind m = callCC ((>>=) m) :=: m

callCC_phantom ::
  forall m a b.
  MonadCont m =>
  ((a -> m b) -> m a) ->
  Equation (m a)
callCC_phantom f = callCC f :=: callCC (f . (fmap . fmap) absurd)

callCC_left_zero ::
  forall m a b.
  MonadCont m =>
  ((a -> m b) -> m a) ->
  ((a -> m b) -> b -> m a) ->
  Equation (m a)
callCC_left_zero f g = callCC (\k -> f k >>= (\a -> k a >>= g k)) :=: callCC f
