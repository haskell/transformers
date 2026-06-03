{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Compose
-- Copyright   :  (C) 2026 The transformers Authors
-- License     :  BSD-style (see the file LICENSE)
--
-- This combines two transformers into a single compound transformer.
-- Potentially useful for when a single transformer is required as a type
-- argument.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Compose (
    ComposeT (..),
) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Zip (MonadZip)
import Data.Data (Data)
import Data.Functor.Classes (Eq1, Ord1)
import Data.Functor.Compose (Compose)
import Data.Functor.Contravariant (Contravariant)
import Data.Kind (Type)
#ifdef __GLASGOW_HASKELL__
import GHC.Generics (Generic)
#endif

infixr 9 `ComposeT`

-- | Like its analogue `Compose`, @ComposeT@ is polykinded; typically it will
-- have kind
--
-- > ((Type -> Type) -> Type -> Type) -> ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
--
-- After enabling @{-# LANGUAGE TypeOperators #-}@, the `ComposeT` type
-- constructor may be written in infix notation in signatures and is
-- right-associative, mirroring `(.)`.  Example:
--
-- > type FallibleCountT = ExceptT String `ComposeT` StateT Int
-- >
-- > checkNonNeg :: (Monad m) => FallibleCountT m ()
-- > checkNonNeg = ComposeT $ do
-- >     count <- lift get
-- >     when (count < 0) $ throwE $ "count is negative (" ++ show count ++ ")"
--
type ComposeT :: forall k1 k2 k3.
    (k3 -> k2 -> Type) -> (k1 -> k3) -> (k1 -> k2 -> Type)
newtype ComposeT t1 t2 m a = ComposeT { runComposeT :: t1 (t2 m) a }
    deriving stock (
        Functor,
        Traversable,
        Foldable,
        Eq,
        Ord,
        Read,
        Show,
#ifdef __GLASGOW_HASKELL__
        Generic,
#endif
        Data)
    deriving newtype (
        Contravariant,
        Applicative,
        Monad,
        MonadIO,
        Alternative,
        MonadFail,
        MonadPlus,
        MonadFix,
        MonadZip,
        Semigroup,
        Monoid,
        Eq1,
        Ord1,
        Bounded,
        Enum,
        Fractional,
        Floating,
        Real,
        RealFrac,
        RealFloat,
        Integral,
        Num)

instance (MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
    lift = ComposeT . lift . lift
    {-# INLINE lift #-}
