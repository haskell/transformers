{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WriterStrictness (test) where

import Control.Exception (SomeException)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assertExceptionIO, monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (Bot (..))

type Output = [()]

------------------------------------------------------------------------

-- * Test list

test :: IO ()
test = do
  defaultMain $ testGroup "Writer" tests

tests :: [TestTree]
tests =
  [ testFunction "execWriter" (prop_strictExecWriter @String @String) prop_lazyExecWriter prop_cpsExecWriter,
    testFunction "tell" prop_strictTellBottomOutput prop_lazyTellBottomOutput prop_cpsTell,
    testFunction "listen" (prop_strictListen @String @()) prop_lazyListen prop_cpsListen,
    testFunction "listens" (prop_strictListens @String @()) prop_lazyListens prop_cpsListens,
    testFunction "pass" (prop_strictPass @String @()) prop_lazyPass prop_cpsPass,
    testFunction "censor" (prop_strictCensor @String @String) prop_lazyCensor prop_cpsCensor,
    testProperty "listens" (prop_lazyListensIO @String @()),
    testProperty "l" (prop_strictListensIO @String @())
  ]

instance Show (String -> String) where
  show _ = "string function"

testFunction :: (Testable a) => String -> a -> a -> a -> TestTree
testFunction name strict lazy cps =
  testGroup
    name
    [ testProperty "strict" strict,
      testProperty "lazy" lazy,
      testProperty "cps" cps
    ]

prop_lazyExecWriter :: Bot (a, w) -> Property
prop_lazyExecWriter (Bot i) =
  isBottom
    ( Lazy.execWriter
        (Lazy.writer i)
    )
    === isBottom i

prop_strictExecWriter :: Bot (a, w) -> Property
prop_strictExecWriter (Bot i) =
  isBottom
    ( Strict.execWriter
        (Strict.writer i)
    )
    === isBottom i

prop_cpsExecWriter :: (Monoid w) => Bot (a, w) -> Property
prop_cpsExecWriter (Bot w) =
  isBottom
    ( CPS.execWriter
        (CPS.writer w)
    )
    === isBottom w

prop_lazyTellBottomOutput :: Bot a -> Property
prop_lazyTellBottomOutput (Bot w) = test_strictness_lazy False $ Lazy.tell w

prop_strictTellBottomOutput :: Bot a -> Property
prop_strictTellBottomOutput (Bot w) =
  -- NOTE: tell involves no binding so result is the same as lazy.
  test_strictness_strict False $ Strict.tell w

prop_cpsTell :: Bot Output -> Property
prop_cpsTell (Bot w) = test_strictness_cps (isBottom w) $ CPS.tell w

prop_lazyListen :: Bot (a, w) -> Property
prop_lazyListen (Bot w) =
  isBottom
    ( Lazy.runWriter $
        Lazy.listen
          (Lazy.writer w)
    )
    === False

prop_strictListen :: Bot (a, w) -> Property
prop_strictListen (Bot i) =
  isBottom
    ( Strict.runWriter $
        Strict.listen
          (Strict.writer i)
    )
    === isBottom i

prop_cpsListen :: (Monoid w) => Bot (a, w) -> Property
prop_cpsListen (Bot i) =
  isBottom
    ( CPS.runWriter $
        CPS.listen
          (CPS.writer i)
    )
    === isBottom i

prop_lazyListensIO :: Bot (a, w) -> Property
prop_lazyListensIO (Bot w) =
  test_strictness_lazy_IO False $ Lazy.listens id $ Lazy.writer w

prop_strictListensIO :: Bot (a, w) -> Property
prop_strictListensIO (Bot w) =
  test_strictness_strict_IO (isBottom w) $ Strict.listens id $ Strict.writer w

prop_lazyListens :: Bot (a, w) -> Property
prop_lazyListens (Bot w) =
  test_strictness_lazy False $ Lazy.listens id $ Lazy.writer w

prop_strictListens :: Bot (a, w) -> Property
prop_strictListens (Bot w) =
  test_strictness_strict (isBottom w) $ Strict.listens id $ Strict.writer w

prop_cpsListens :: (Monoid w) => Bot (a, w) -> Property
prop_cpsListens (Bot w) =
  test_strictness_cps (isBottom w) $ CPS.listens id $ CPS.writer w

prop_lazyPass :: (Monoid w) => Bot (a, w -> w) -> Property
prop_lazyPass (Bot v) =
  test_strictness_lazy False $ Lazy.pass $ return v

prop_strictPass :: (Monoid w) => Bot (a, w -> w) -> Property
prop_strictPass (Bot v) =
  test_strictness_strict (isBottom v) $ Strict.pass $ return v

prop_cpsPass :: (Monoid w) => Bot (a, w -> w) -> Property
prop_cpsPass (Bot v) =
  test_strictness_cps (isBottom v) $ CPS.pass $ return v

prop_lazyCensor :: Bot (a, w) -> Property
prop_lazyCensor (Bot v) =
  test_strictness_lazy False $ Lazy.censor id $ Lazy.writer v

prop_strictCensor :: Bot (a, w) -> Property
prop_strictCensor (Bot v) =
  test_strictness_strict (isBottom v) $ Strict.censor id $ Strict.writer v

prop_cpsCensor :: (Monoid w) => Bot (a, w) -> Property
prop_cpsCensor (Bot v) =
  test_strictness_cps (isBottom v) $ CPS.censor id $ CPS.writer v

test_strictness_lazy :: Bool -> Lazy.Writer w a -> Property
test_strictness_lazy expected w = isBottom (Lazy.runWriter w) === expected

test_strictness_lazy_IO :: Bool -> Lazy.WriterT w IO a -> Property
test_strictness_lazy_IO expected w = monadicIO $ run $ do
  (\v -> isBottom v === expected) <$> Lazy.runWriterT w

-- return $ isBottom ( Lazy.runWriterT w) === expected
-- v <- Lazy.runWriterT w
-- return $ expected === isBottom v
--
test_strictness_strict_IO :: Bool -> Strict.WriterT w IO a -> Property
-- test_strictness_strict_IO expected w = monadicIO $ run $ do
test_strictness_strict_IO True w =
  assertExceptionIO @SomeException (const True) $ Strict.runWriterT w
test_strictness_strict_IO False w = monadicIO $ run $ do
  v <- Strict.runWriterT w
  return $ not $ isBottom v

-- try
-- v <- Strict.runWriterT w
-- return $ isBottom v === expected
-- (\v -> isBottom v === expected) <$> Strict.runWriterT w
-- return $ isBottom ( Strict.runWriterT w) === expected

test_strictness_strict :: Bool -> Strict.Writer w a -> Property
test_strictness_strict expected w = isBottom (Strict.runWriter w) === expected

test_strictness_cps :: (Monoid w) => Bool -> CPS.Writer w a -> Property
test_strictness_cps expected w = isBottom (CPS.runWriter w) === expected
