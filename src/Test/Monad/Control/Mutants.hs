{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Monad.Control.Mutants where

import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Functor
import Test.QuickCheck

import Test.Monad.Control
import Test.Checkers
import Test.Mutants

mutantLiftWith
  :: (Monad m, MonadTransControl t)
  => (RunDefault (Mutant v t) t -> m a) -> Mutant v t m a
mutantLiftWith = defaultLiftWith Mutant mutate

mutantRestoreT :: (Monad m, MonadTransControl t) => m (StT t a) -> Mutant v t m a
mutantRestoreT = defaultRestoreT Mutant

-- | An general way to get 'MonadTransControl' wrong is to run the base
-- computation twice when lifting...
data LiftWithTwice

-- | There should be no blanket @OVERLAPPABLE@ instance because of the type
-- family.
instance
  MonadTransControl t => MonadTransControl (Mutant LiftWithTwice t) where
  type StT (Mutant LiftWithTwice t) a = StT t a
  restoreT = mutantRestoreT

  liftWith f = mutantLiftWith (\run -> f run >> f run)

data RunTwice

instance
  MonadTransControl t => MonadTransControl (Mutant RunTwice t) where
  type StT (Mutant RunTwice t) a = StT t a
  restoreT = mutantRestoreT

  liftWith f = mutantLiftWith (\run -> f (\t -> run t >> run t))


-- | ... or run it twice when restoring.
data RestoreTwice

instance
  MonadTransControl t => MonadTransControl (Mutant RestoreTwice t) where
  type StT (Mutant RestoreTwice t) a = StT t a
  liftWith = mutantLiftWith

  restoreT m = mutantRestoreT (m >> m)

-- * State

data LiftWithDropState

instance MonadTransControl (Mutant LiftWithDropState (StateT s)) where
  type StT (Mutant LiftWithDropState (StateT s)) a = StT (StateT s) a
  restoreT = mutantRestoreT

  liftWith f = Mutant . StateT $ \s ->
    fmap
      (\a -> (a, s))
      (f (\(Mutant (StateT m)) ->
        fmap
          (\(a, _) -> (a, s))
          (m s)))
          -- Drop the state that should have been returned.

data RestoreDropState

instance MonadTransControl (Mutant RestoreDropState (StateT s)) where
  type StT (Mutant RestoreDropState (StateT s)) a = StT (StateT s) a
  liftWith = mutantLiftWith

  restoreT m = Mutant . StateT $ \s -> fmap (\(a, _) -> (a, s)) m

