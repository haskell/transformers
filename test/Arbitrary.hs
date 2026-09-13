{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Arbitrary
  ( Bot (..),
    BaseMonad (..),
    isStrictMonad,
    F1 (..),
    F1Bot (..),
    withBaseMonad,
  )
where

import           Control.Exception (evaluate)

import           Data.Data
import           Data.Functor.Identity (Identity (..))
import           Data.Tuple.Solo

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck

bottom :: forall a. a
bottom = error "<bottom>"

-- | Arbitrary (Bot a) values may be bottom.
--
-- Borrowed from container tests: https://github.com/haskell/containers/
newtype Bot a
  = Bot { unBot :: a }

data BaseMonad
  = forall m. (Typeable m, Monad m) => LazyBaseMonad (forall a. m a -> IO a)
  | forall m. (Typeable m, Monad m) => StrictBaseMonad (forall a. m a -> IO a)

-- Use the underlying Monad of a BaseMonad.
withBaseMonad :: BaseMonad -> (forall m. (Monad m) => m a) -> IO a
withBaseMonad (LazyBaseMonad v)   = v
withBaseMonad (StrictBaseMonad v) = v

isStrictMonad :: BaseMonad -> Bool
isStrictMonad (LazyBaseMonad _)   = False
isStrictMonad (StrictBaseMonad _) = True

instance Arbitrary BaseMonad where
  arbitrary =
    elements
      [ LazyBaseMonad id,                           -- IO
        StrictBaseMonad (evaluate . runIdentity),   -- Identity
        LazyBaseMonad (evaluate . getSolo),         -- Solo
        LazyBaseMonad (\f -> evaluate $ f ())       -- constant function () -> a
      ]

instance Show BaseMonad where
  show v = case v of
    (StrictBaseMonad m) -> "StrictBaseMonad " <> getName m
    (LazyBaseMonad m)   -> "LazyBaseMonad " <> getName m
    where
      getName :: forall m. (Typeable m) => (forall a. m a -> IO a) -> String
      getName _ = show $ typeRep (Proxy @m)

instance Show a => Show (Bot a) where
  show (Bot x) = if isBottom x then "<bottom>" else show x

instance Arbitrary a => Arbitrary (Bot a) where
  arbitrary =
    frequency
      [ (1, pure bottom),
        (4, Bot <$> arbitrary)
      ]

-- | Arbitrary function of one argument.
newtype F1 a b = F1 { unF1 :: a -> b }
  deriving newtype (Arbitrary)

instance (Typeable a, Typeable b) => Show (F1 a b) where
  show :: F1 a b -> String
  show _ = a <> " -> " <> b
    where
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @b)

-- | Arbitrary function that may bottom in the output.
--
-- To be precise, the function is either
-- a) a valid arbitrary function i.e. never bottoms, or
-- b) a constant function which always bottoms.
--
-- In particular, the QuickCheck built-in Func cannot be used for this, since
-- Func a (Bot b) generates a function which may or may not bottom depending on the input.
newtype F1Bot a b = F1Bot { unF1Bot :: a -> b }

instance (CoArbitrary a, Arbitrary b) => Arbitrary (F1Bot a b) where
  arbitrary = do
    useBottomFunc <- arbitrary :: Gen Bool
    F1Bot <$>
      if useBottomFunc
        then return (const bottom)
        else (arbitrary :: Gen (a -> b))

instance (Typeable a, Typeable b) => Show (F1Bot a b) where
  show :: F1Bot a b -> String
  show _ = a <> " -> " <> b
    where
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @b)
