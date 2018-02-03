{-# LANGUAGE FlexibleInstances #-}

module Test.Monad.Trans.Mutants where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Functor (($>))

import Test.Mutants

-- | An general way to get 'MonadTrans' wrong is to run the base
-- computation twice.
data LiftTwice

instance {-# OVERLAPPING #-}
  MonadTrans t => MonadTrans (Mutant LiftTwice t) where
  lift m = lift (m >> m)

-- Other kinds of mistakes are difficult to make, by parametricity.

-- State:
-- lift :: m a -> (s -> m (a, s))

-- Except:
-- lift :: m a -> m (Either e a)

-- Writer:
-- lift :: m a -> m (w, a)

-- Reader:
-- lift :: m a -> (r -> m a)

-- Maybe:
-- lift :: m a -> m (Maybe a)

-- | Forget the computation.
data LiftMaybeNothing

instance {-# OVERLAPPING #-}
  MonadTrans (Mutant LiftMaybeNothing MaybeT) where
  lift _ = Mutant . MaybeT $ return Nothing

-- | Run the computation but forget the result.
data LiftMaybeDiscard

instance {-# OVERLAPPING #-}
  MonadTrans (Mutant LiftMaybeDiscard MaybeT) where
  lift m = Mutant . MaybeT $ m $> Nothing
