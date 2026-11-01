{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Arbitrary
  ( Bot (..),
    BaseMonad,
    baseMonadName,
    F1 (..),
    F1Bot (..),
    withBaseMonad,
    isBottomError,
  )
where

import           Control.Exception (ErrorCall, Exception (displayException),
                                    evaluate, try)

import           Data.Data
import           Data.Functor.Identity (Identity (..))
import           Data.List (isInfixOf)
import           Data.Tuple.Solo

import           GHC.IO (unsafePerformIO)

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck

bottom :: forall a. a
bottom = error "<bottom>"

-- Check error message only i.e. the prefix, ignoring the stacktrace.
isBottomError :: ErrorCall -> Bool
isBottomError e = "<bottom>" `isInfixOf` displayException e

-- | Arbitrary (Bot a) values may be bottom.
--
-- Borrowed from container tests: https://github.com/haskell/containers/
newtype Bot a = Bot { unBot :: a }

data BaseMonad
  = forall m. (Typeable m, Monad m) => BaseMonad (forall a. m a -> IO a)

-- Use the underlying Monad of a BaseMonad.
withBaseMonad :: BaseMonad -> (forall m. (Monad m) => m a) -> IO a
withBaseMonad (BaseMonad v)   = v

baseMonadName :: BaseMonad -> String
baseMonadName (BaseMonad m) = getName m
  where
      getName :: forall m. (Typeable m) => (forall a. m a -> IO a) -> String
      getName _ = show $ typeRep (Proxy @m)

instance Arbitrary BaseMonad where
  arbitrary =
    elements
      [ BaseMonad id,                         -- IO
        BaseMonad (evaluate . runIdentity),   -- Identity: newtype, lazy sequencing
        BaseMonad (evaluate . getSolo),       -- Solo: lazy type, strict sequencing
        BaseMonad (\f -> evaluate $ f ())     -- constant function () -> a: lazy type, lazy sequencing
      ]

instance Show BaseMonad where
  show v@(BaseMonad {}) = "BaseMonad " <> baseMonadName v

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

isBottomFunc :: F1Bot a b -> Bool
isBottomFunc (F1Bot f) = unsafePerformIO $ do
    result <- try (evaluate (f undefined))
    case result of
      Left (e :: ErrorCall) -> return $ isBottomError e
      _                         -> return False

instance (CoArbitrary a, Arbitrary b) => Arbitrary (F1Bot a b) where
  arbitrary = do
    useBottomFunc <- arbitrary :: Gen Bool
    F1Bot <$>
      if useBottomFunc
        then return (const bottom)
        else (arbitrary :: Gen (a -> b))

instance (Typeable a, Typeable b) => Show (F1Bot a b) where
  show :: F1Bot a b -> String
  show f = if isBottomFunc f
      then a <> " -> bottom"
      else a <> " -> " <> b
    where
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @b)
