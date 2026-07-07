{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Arbitrary
  (
    Bot(..),
    BaseMonad(..),
    F1 (..),
  )
where

import           Data.Data
import           Data.Functor.Identity (Identity (..))

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck

bottom :: forall a. a
bottom = error "<bottom>"

-- | Arbitrary (Bot a) values may be bottom.
--
-- Borrowed from container tests: https://github.com/haskell/containers/
newtype Bot a = Bot a

data BaseMonad = forall m. Monad m => BaseMonad (forall a. m a -> IO a)

identityBaseMonad:: BaseMonad
identityBaseMonad = BaseMonad (return . runIdentity)

ioBaseMonad :: BaseMonad
ioBaseMonad = BaseMonad id

instance Arbitrary BaseMonad where
  arbitrary = elements [identityBaseMonad, ioBaseMonad]

instance Show a => Show (Bot a) where
  show (Bot x) = if isBottom x then "<bottom>" else show x

instance Arbitrary a => Arbitrary (Bot a) where
  arbitrary =
    frequency
      [ (1, pure bottom),
        (4, Bot <$> arbitrary)
      ]

-- | Arbitrary function of 1 argument
newtype F1 a b = F1 { unF1::a -> b }
  deriving newtype (Arbitrary)

instance (Typeable a, Typeable b) => Show (F1 a b) where
  show :: F1 a b -> String
  show _ = a <> " -> " <> b
    where
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @b)



