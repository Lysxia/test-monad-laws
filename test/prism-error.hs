{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Monad.Except.Checkers
import Test.QuickCheck
import Test.QuickCheck.HigherOrder

import Control.Monad.State
import Control.Monad.Except

newtype PrismErrorT f e m a = PrismErrorT { runPrismErrorT :: m a }
  deriving (Functor, Applicative, Monad, Eq, Ord, Show, TestEq, Constructible)

class Prism e f where
  match :: e -> Either e f
  build :: f -> e

instance Prism (Either a b) a where
  match (Left a) = Right a
  match e = Left e
  build = Left

instance (MonadError e m, Prism e f) => MonadError f (PrismErrorT f e m) where
  throwError f = PrismErrorT (throwError (build f))
  catchError (PrismErrorT m) handleF = PrismErrorT (catchError m (\e ->
    case match e of
      Left e -> throwError e
      Right f -> runPrismErrorT (handleF f)))

trans :: forall m e f a. (MonadError e m, Prism e f)
  => (forall n. MonadError f n => n a) -> m a
trans g = runPrismErrorT g

type M = PrismErrorT Int (Either Int Word) (ExceptT (Either Int Word) (StateT Int []))

main :: IO ()
main = defaultMain . testProperties "PrismError" $ checkExcept @M @Int @Int
