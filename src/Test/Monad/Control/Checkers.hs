-- {-# OPTIONS_GHC -ddump-

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Monad.Control.Checkers where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control (MonadTransControl, MonadBaseControl)
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits
import Test.QuickCheck

import Test.Checkers
import Test.Checkers.Instances ()
import Test.Monad.Control
import Test.SmallList

-- | All lists of length @n@ /or less/ with (possibly duplicate)
-- elements from @xs@.
type family Replicate (n :: Nat) (xs :: [k]) :: [[k]] where
  Replicate 0 _ = '[ '[] ]
  Replicate n xs = Extend' xs (Replicate (n - 1) xs)

type Extend' xs ys = ys ++ Extend xs ys

type family Extend (xs :: [k]) (ys :: [[k]]) :: [[k]] where
  Extend '[] ys = '[]
  Extend (x ': xs) ys = MapCons x ys ++ Extend xs ys

type family MapCons (x :: k) (ys :: [[k]]) :: [[k]] where
  MapCons x '[] = '[]
  MapCons x (y ': ys) = (x ': y) ': MapCons x ys

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

class TestControl (xs :: [[(Type -> Type) -> (Type -> Type)]]) (m :: Type -> Type) where
  testControl :: IO ()

instance TestControl '[] m where
  testControl = return ()

instance
  (  TestTransControl ts m, TestBaseControl ts m, TestControl tss m
  ,  Typeable (StackT ts m)
  )
  => TestControl (ts ': tss) m where
  testControl = do
    print (typeRep (Proxy @(StackT ts m)))
    testTransControl @ts @m
    testBaseControl @ts @m
    testControl @tss @m

type family StackT (ts :: [(Type -> Type) -> (Type -> Type)]) (m :: Type -> Type) :: Type -> Type where
  StackT '[] m = m
  StackT (t ': ts) m = t (StackT ts m)

type Stack ts = StackT ts Identity

class TestTransControl (ts :: [(Type -> Type) -> (Type -> Type)]) (m :: Type -> Type) where
  testTransControl :: IO ()

instance TestTransControl '[] m where
  testTransControl = return ()

instance
  (  MonadTransControl t, Monad (StackT (t ': ts) m), Monad (StackT ts m)
  ,  Example (StackT (t ': ts) m Int), Example (StackT ts m Int)
  ,  TestEq (StackT (t ': ts) m Int))
  => TestTransControl (t ': ts) m where
  testTransControl = (do
    ok "liftWith-return"   (liftWith_return @t @n @Int)
    ok "liftWith-bind"     (\m (Fn k) -> liftWith_bind @t @n @Int @Int m k)
    ok "liftWith-lift"     (liftWith_lift @t @n @Int)
    ok "liftWith-restoreT" (liftWith_restoreT @t @n @Int)
    ) :: forall n. (n ~ StackT ts m) => IO ()

class TestBaseControl (ts :: [(Type -> Type) -> (Type -> Type)]) (m :: Type -> Type) where
  testBaseControl :: IO ()

instance
  (  MonadBaseControl m (StackT ts m)
  ,  Example (StackT ts m Int), Example (m Int)
  ,  TestEq (StackT ts m Int))
  => TestBaseControl ts m where
  testBaseControl = (do
    ok "liftBaseWith-liftBase" (liftBaseWith_liftBase @n @Int)
    ok "liftBaseWith-restoreM" (liftBaseWith_restoreM @n @Int)
    ) :: forall n. (n ~ StackT ts m) => IO ()

type StdTrans = '[ ReaderT Int, StateT Int, ExceptT Int ]

type StdStacks = Replicate 2 StdTrans

checkControl :: IO ()
checkControl = do
  testControl @StdStacks @Identity
  testControl @StdStacks @SmallList
