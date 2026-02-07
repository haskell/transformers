{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Control.Monad.Trans.Compose where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Kind (Type)

infixr 9 `ComposeT`

#if __GLASGOW_HASKELL__ >= 810
type ComposeT :: (k3 -> k2 -> Type) -> (k1 -> k3) -> (k1 -> k2 -> Type)
#endif
newtype ComposeT trans1 trans2 m a = ComposeT (trans1 (trans2 m) a)
#if __GLASGOW_HASKELL__ >= 806
    deriving newtype (Functor, Applicative, Monad)
#endif

#if __GLASGOW_HASKELL__ < 806
instance (Functor (trans1 (trans2 m))) => Functor (ComposeT trans1 trans2 m) where
    fmap f (ComposeT x) = ComposeT (fmap f x)

instance (Applicative (trans1 (trans2 m))) => Applicative (ComposeT trans1 trans2 m) where
    pure x = ComposeT (pure x)
    ComposeT a <*> ComposeT b = ComposeT (a <*> b)

instance (Monad (trans1 (trans2 m))) => Monad (ComposeT trans1 trans2 m) where
    return x = ComposeT (return x)
    (ComposeT x) >>= f = ComposeT (x >>= (\(ComposeT x') -> x') . f)
#endif

instance (MonadTrans trans1, MonadTrans trans2) => MonadTrans (ComposeT trans1 trans2) where
    lift = ComposeT . lift . lift
