{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
#endif
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Constant
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The constant functor.
-----------------------------------------------------------------------------

module Data.Functor.Constant {-# DEPRECATED "Use Data.Functor.Const; will be removed in transformers 0.8" #-} (
    Constant(..),
  ) where

import Data.Functor.Classes
import Data.Functor.Contravariant

import Control.Applicative
import Data.Foldable
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Prelude hiding (null, length)
#ifdef __GLASGOW_HASKELL__
import Data.Data
#endif
#ifdef __GLASGOW_HASKELL__
import GHC.Generics
#endif

-- | Constant functor.
newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord
#ifdef __GLASGOW_HASKELL__
        , Data
#endif
#ifdef __GLASGOW_HASKELL__
        , Generic, Generic1
#endif
        )

-- These instances would be equivalent to the derived instances of the
-- newtype if the field were removed.

instance (Read a) => Read (Constant a b) where
    readsPrec = readsData $
         readsUnaryWith readsPrec "Constant" Constant

instance (Show a) => Show (Constant a b) where
    showsPrec d (Constant x) = showsUnaryWith showsPrec "Constant" d x

-- Instances of lifted Prelude classes

instance Eq2 Constant where
    liftEq2 eq _ (Constant x) (Constant y) = eq x y
    {-# INLINE liftEq2 #-}

instance Ord2 Constant where
    liftCompare2 comp _ (Constant x) (Constant y) = comp x y
    {-# INLINE liftCompare2 #-}

instance Read2 Constant where
    liftReadsPrec2 rp _ _ _ = readsData $
         readsUnaryWith rp "Constant" Constant

instance Show2 Constant where
    liftShowsPrec2 sp _ _ _ d (Constant x) = showsUnaryWith sp "Constant" d x

instance (Eq a) => Eq1 (Constant a) where
    liftEq = liftEq2 (==)
    {-# INLINE liftEq #-}
instance (Ord a) => Ord1 (Constant a) where
    liftCompare = liftCompare2 compare
    {-# INLINE liftCompare #-}
instance (Read a) => Read1 (Constant a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList
    {-# INLINE liftReadsPrec #-}
instance (Show a) => Show1 (Constant a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
    {-# INLINE liftShowsPrec #-}

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x
    {-# INLINE fmap #-}

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty
    {-# INLINE foldMap #-}
    null (Constant _) = True
    length (Constant _) = 0

instance Traversable (Constant a) where
    traverse _ (Constant x) = pure (Constant x)
    {-# INLINE traverse #-}

instance (Semigroup a) => Semigroup (Constant a b) where
    Constant x <> Constant y = Constant (x <> y)
    {-# INLINE (<>) #-}

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    {-# INLINE pure #-}
    Constant x <*> Constant y = Constant (x `mappend` y)
    {-# INLINE (<*>) #-}
    liftA2 _ (Constant x) (Constant y) = Constant (x `mappend` y)
    {-# INLINE liftA2 #-}

instance (Monoid a) => Monoid (Constant a b) where
    mempty = Constant mempty
    {-# INLINE mempty #-}

instance Bifunctor Constant where
    first f (Constant x) = Constant (f x)
    {-# INLINE first #-}
    second _ (Constant x) = Constant x
    {-# INLINE second #-}

instance Bifoldable Constant where
    bifoldMap f _ (Constant a) = f a
    {-# INLINE bifoldMap #-}

instance Bitraversable Constant where
    bitraverse f _ (Constant a) = Constant <$> f a
    {-# INLINE bitraverse #-}

instance Contravariant (Constant a) where
    contramap _ (Constant a) = Constant a
    {-# INLINE contramap #-}
