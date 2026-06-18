{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Needed since Typeable is redundant in GHC 9.14
{-# OPTIONS_GHC -Wwarn=redundant-constraints #-}

module Utils
  ( Bot (..),
    bottom,
    F1 (..),
    unF1,
  )
where

import Data.Data
import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck

bottom :: forall a. a
bottom = error "<bottom>"

-- | Arbitrary (Bot a) values may be bottom.
newtype Bot a = Bot a

instance (Show a) => Show (Bot a) where
  show (Bot x) = if isBottom x then "<bottom>" else show x

instance (Arbitrary a) => Arbitrary (Bot a) where
  arbitrary =
    frequency
      [ (1, pure bottom),
        (4, Bot <$> arbitrary)
      ]

newtype F1 a b = F1 (a -> b)
  deriving newtype (Arbitrary)

unF1 :: F1 a b -> a -> b
unF1 (F1 f) = f

instance forall a b. (Typeable a, Typeable b) => Show (F1 a b) where
  show :: F1 a b -> String
  show _ = a <> " -> " <> b
    where
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @a)
