-- | A manual mutation testing framework for monads and transformers.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Mutants where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Kind (Type)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.HigherOrder (TestEq(..), Constructible(..))

-- | The type @Mutant v t m a@ is isomorphic to @t m a@, and inherits
-- various instances from it.
--
-- > class MyClass m where
-- >   myMethod :: m ()
-- >
-- > deriving instance MyClass (t m) => MyClass (Mutant v t m)
--
-- The first parameter @v@ is phantom, and identifies a "mutation".
--
-- A new mutation can be declared as a datatype.
--
-- > data BugInMyClass
--
-- Use overlapping instances to "mutate" an implementation.
--
-- > instance {-# OVERLAPPING #-} MyClass (Mutant BugInMyClass t m) where
-- >   myMethod = wrongMethod
newtype Mutant v t (m :: Type -> Type) a = Mutant { mutate :: t m a }
  deriving (
    Eq, Ord, Show,
    Functor, Applicative, Alternative, Monad, MonadPlus,
    MonadState s, MonadReader r, MonadError e, MonadWriter w,
    Arbitrary)

deriving instance MonadTrans t => MonadTrans (Mutant v t)
deriving instance TestEq (t m a) => TestEq (Mutant v t m a)

instance Constructible (t m a) => Constructible (Mutant v t m a) where
  type Repr (Mutant v t m a) = Repr (t m a)
  fromRepr = Mutant . fromRepr
