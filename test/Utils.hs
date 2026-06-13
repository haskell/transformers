module Utils(
  Bot(..),
  ) where

import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck

-- | Arbitrary (Bot a) values may be bottom.
newtype Bot a = Bot a

instance Show a => Show (Bot a) where
  show (Bot x) = if isBottom x then "<bottom>" else show x

instance Arbitrary a => Arbitrary (Bot a) where
  arbitrary = frequency
    [ (1, pure (error "<bottom>"))
    , (4, Bot <$> arbitrary)
    ]
