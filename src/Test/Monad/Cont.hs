{-# LANGUAGE ScopedTypeVariables #-}

module Test.Monad.Cont where

import Control.Monad.Cont
import Data.Void (absurd)
import Test.QuickCheck.HigherOrder (Equation(..))

-- * 'MonadCont' laws
-- These are derived from [here](https://mail.haskell.org/pipermail/libraries/2019-October/030041.html)

-- | 'callCC' has no effects other than passing the continuation to the provided
-- function.
--
-- @
-- callCC' ('const' x) = x
-- @
callCC_const :: forall m a. MonadCont m => m a -> Equation (m a)
callCC_const m = callCC (const m) :=: m

-- | The continuation given returns the value passed to it, and not some other
-- one.
--
-- @
-- 'callCC' ('$' x) = 'pure' x
-- @
callCC_id :: forall m a. MonadCont m => a -> Equation (m a)
callCC_id a = callCC ($ a) :=: pure a

-- | The continuation given returns the value passed to it, whether it's pure or
-- in the \'context\' of some effect.
--
-- @
-- 'callCC' (('>>=') m) = m
-- @
callCC_bind :: forall m a. MonadCont m => m a -> Equation (m a)
callCC_bind m = callCC ((>>=) m) :=: m

-- | The return type of the continuation is effectively 'm Void'; it will never
-- actually \'return\'.
--
-- @
-- 'callCC' f = 'callCC' (f . ('fmap' . 'fmap') 'absurd')
-- @
callCC_phantom ::
  forall m a b.
  MonadCont m =>
  ((a -> m b) -> m a) ->
  Equation (m a)
callCC_phantom f = callCC f :=: callCC (f . (fmap . fmap) absurd)

-- | The continuation never returns, so @g@ does not matter.
-- @
-- 'callCC' (\\k -> f k '>>=' (\\a -> k a '>>=' g k)) = 'callCC' f
-- @
callCC_left_zero ::
  forall m a b.
  MonadCont m =>
  ((a -> m b) -> m a) ->
  ((a -> m b) -> b -> m a) ->
  Equation (m a)
callCC_left_zero f g = callCC (\k -> f k >>= (\a -> k a >>= g k)) :=: callCC f
