{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils
  ( Bot (..),
  )
where

import Test.ChasingBottoms (bottom)
import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck

instance Show (String -> String) where
  show _ = "string function"

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
