{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Backwards
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors with an 'Applicative' instance that performs actions
-- in the reverse order.
-----------------------------------------------------------------------------

module Control.Applicative.Backwards (
    Backwards(..),
  ) where

#if MIN_VERSION_base(4,18,0)
import Data.Foldable1 (Foldable1(foldMap1))
#endif
import Data.Functor.Classes
import Data.Functor.Contravariant
#ifdef __GLASGOW_HASKELL__
import GHC.Generics
#endif

import Prelude hiding (foldr, foldr1, foldl, foldl1, null, length)
import Control.Applicative
import Data.Foldable
import Data.Traversable (Traversable(traverse, sequenceA))

-- | The same functor, but with an 'Applicative' instance that performs
-- actions in the reverse order.
newtype Backwards f a = Backwards { forwards :: f a }
#ifdef __GLASGOW_HASKELL__
    deriving (Generic, Generic1)
#endif

instance (Eq1 f) => Eq1 (Backwards f) where
    liftEq eq (Backwards x) (Backwards y) = liftEq eq x y
    {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (Backwards f) where
    liftCompare comp (Backwards x) (Backwards y) = liftCompare comp x y
    {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (Backwards f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Backwards" Backwards

instance (Show1 f) => Show1 (Backwards f) where
    liftShowsPrec sp sl d (Backwards x) =
        showsUnaryWith (liftShowsPrec sp sl) "Backwards" d x

instance (Eq1 f, Eq a) => Eq (Backwards f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Backwards f a) where compare = compare1
instance (Read1 f, Read a) => Read (Backwards f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Backwards f a) where showsPrec = showsPrec1

-- | Derived instance.
instance (Functor f) => Functor (Backwards f) where
    fmap f (Backwards a) = Backwards (fmap f a)
    {-# INLINE fmap #-}
    x <$ Backwards a = Backwards (x <$ a)
    {-# INLINE (<$) #-}

-- | Apply @f@-actions in the reverse order.
instance (Applicative f) => Applicative (Backwards f) where
    pure a = Backwards (pure a)
    {-# INLINE pure #-}
    Backwards f <*> Backwards a = Backwards (a <**> f)
    {-# INLINE (<*>) #-}
    liftA2 f (Backwards m) (Backwards n) = Backwards $ liftA2 (flip f) n m
    {-# INLINE liftA2 #-}
    Backwards xs *> Backwards ys = Backwards (ys <* xs)
    {-# INLINE (*>) #-}
    Backwards ys <* Backwards xs = Backwards (xs *> ys)
    {-# INLINE (<*) #-}

-- | Try alternatives in the same order as @f@.
instance (Alternative f) => Alternative (Backwards f) where
    empty = Backwards empty
    {-# INLINE empty #-}
    Backwards x <|> Backwards y = Backwards (x <|> y)
    {-# INLINE (<|>) #-}

-- | Derived instance.
instance (Foldable f) => Foldable (Backwards f) where
    foldMap f (Backwards t) = foldMap f t
    {-# INLINE foldMap #-}
    foldr f z (Backwards t) = foldr f z t
    {-# INLINE foldr #-}
    foldl f z (Backwards t) = foldl f z t
    {-# INLINE foldl #-}
    foldr1 f (Backwards t) = foldr1 f t
    {-# INLINE foldr1 #-}
    foldl1 f (Backwards t) = foldl1 f t
    {-# INLINE foldl1 #-}
    null (Backwards t) = null t
    length (Backwards t) = length t

#if MIN_VERSION_base(4,18,0)
-- | Derived instance.
instance (Foldable1 f) => Foldable1 (Backwards f) where
    foldMap1 f (Backwards t) = foldMap1 f t
    {-# INLINE foldMap1 #-}
#endif

-- | Derived instance.
instance (Traversable f) => Traversable (Backwards f) where
    traverse f (Backwards t) = fmap Backwards (traverse f t)
    {-# INLINE traverse #-}
    sequenceA (Backwards t) = fmap Backwards (sequenceA t)
    {-# INLINE sequenceA #-}

-- | Derived instance.
instance (Contravariant f) => Contravariant (Backwards f) where
    contramap f = Backwards . contramap f . forwards
    {-# INLINE contramap #-}
