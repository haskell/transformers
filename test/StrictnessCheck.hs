module StrictnessCheck (
  bottomLabel,
  notBottomLabel,
  bottomLabelFor,
  isLazy,
  isValueLazy,
  isStrictIn,
  isBiStrictIn,
  shouldBeBottom,
  shouldBeBottomIO,
  ) where

import           Arbitrary

import           Test.ChasingBottoms (isBottom)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assertExceptionIO)

bottomLabel :: String
bottomLabel = "_|_"

notBottomLabel :: String
notBottomLabel = "Not _|_"

bottomLabelFor :: String -> Bool -> String
bottomLabelFor s x = s <> ": " <> (if x then bottomLabel else notBottomLabel)

-- Never bottom
isLazy :: a -> Property
isLazy = shouldBeBottom False

-- Never bottom not just in the outer constructor but in the inner value.
-- This function ensures that we do not accidentally check only the outer constructor.
isValueLazy :: (m a -> a) -> m a -> Property
isValueLazy unWrap = shouldBeBottom False . unWrap

-- Strictness in one argument:
-- The result should be bottom whenever arg1 is bottom.
isStrictIn :: arg1 -> a -> Property
isStrictIn x  =
  let bottomX = isBottom x
  in label (bottomLabelFor "arg" bottomX)
    . shouldBeBottom bottomX

-- Strictness in two arguments:
-- The result (normalized to IO) should be bottom whenever arg1 OR arg2 is bottom.
isBiStrictIn :: arg1 -> arg2 -> o -> Property
isBiStrictIn x y  =
    let bottomX = isBottom x
        bottomY = isBottom y
    in label (bottomLabelFor "arg1" bottomX <> ", " <> bottomLabelFor "arg2" bottomY)
      . shouldBeBottom (bottomX || bottomY)

shouldBeBottom :: Bool -> o -> Property
shouldBeBottom expectBottom result = classify expectBottom bottomLabel $
  isBottom result === expectBottom

shouldBeBottomIO :: Bool -> IO o -> Property
shouldBeBottomIO expectBottom result = classify expectBottom bottomLabel $
  if expectBottom
    -- NOTE: TODO explain why assertExceptionIO is needed here
    then assertExceptionIO isBottomError result
    else ioProperty $ do
      v <- result
      v `seq` return ()

