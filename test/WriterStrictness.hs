{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module WriterStrictness (test) where

import Control.Applicative
import Control.Exception.Base (ErrorCall)
import Control.Monad.Fix (mfix)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Contravariant (Contravariant (contramap), Predicate (..))
import GHC.IO (evaluate)
import Test.ChasingBottoms (bottom)
import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assertExceptionIO, monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (Bot (..))

test :: IO ()
test = do
  defaultMain $ testGroup "Writer" tests

tests :: [TestTree]
tests =
  -- Basic methods
  [ testFunction
      "execWriter"
      ( \(Bot v :: (Bot (String, ()))) ->
          isBottom (Strict.execWriter $ Strict.writer v)
            === isBottom v
      )
      ( \(Bot v :: (Bot (String, ()))) ->
          isBottom (Lazy.execWriter $ Lazy.writer v)
            === isBottom v
      )
      ( \(Bot v :: (Bot (String, ()))) ->
          isBottom (CPS.execWriter $ CPS.writer v)
            === isBottom v
      ),
    testFunction
      "tell"
      -- NOTE: tell involves no binding so result is the same as lazy.
      (\(Bot v :: (Bot String)) -> test_strictness_strict False $ Strict.tell v)
      (\(Bot v :: (Bot String)) -> test_strictness_lazy False $ Lazy.tell v)
      (\(Bot v :: (Bot String)) -> test_strictness_cps (isBottom v) $ CPS.tell v),
    testFunction
      "tellIO"
      -- NOTE: tell involves no binding so result is the same as lazy.
      (\(Bot v :: (Bot String)) -> test_strictness_strict_IO False $ Strict.tell v)
      (\(Bot v :: (Bot String)) -> test_strictness_lazy_IO False $ Lazy.tell v)
      (\(Bot v :: (Bot String)) -> test_strictness_cps_IO (isBottom v) $ CPS.tell v),
    testFunction
      "listen"
      (\(Bot v :: (Bot ((), String))) -> test_strictness_strict (isBottom v) $ Strict.listen $ Strict.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_lazy False $ Lazy.listen $ Lazy.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_cps (isBottom v) $ CPS.listen $ CPS.writer v),
    testFunction
      "listenIO"
      (\(Bot v :: (Bot ((), String))) -> test_strictness_strict_IO (isBottom v) $ Strict.listen $ Strict.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_lazy_IO False $ Lazy.listen $ Lazy.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_cps_IO (isBottom v) $ CPS.listen $ CPS.writer v),
    testFunction
      "listens"
      (\(Bot v :: (Bot ((), String))) -> test_strictness_strict (isBottom v) $ Strict.listens id $ Strict.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_lazy False $ Lazy.listens id $ Lazy.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_cps (isBottom v) $ CPS.listens id $ CPS.writer v),
    testFunction
      "listensIO"
      (\(Bot v :: (Bot ((), String))) -> test_strictness_strict_IO (isBottom v) $ Strict.listens id $ Strict.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_lazy_IO False $ Lazy.listens id $ Lazy.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_cps_IO (isBottom v) $ CPS.listens id $ CPS.writer v),
    testFunction
      "pass"
      (\(Bot v :: (Bot ((), String -> String))) -> test_strictness_strict (isBottom v) $ Strict.pass $ return v)
      (\(Bot v :: (Bot ((), String -> String))) -> test_strictness_lazy False $ Lazy.pass $ return v)
      (\(Bot v :: (Bot ((), String -> String))) -> test_strictness_cps (isBottom v) $ CPS.pass $ return v),
    testFunction
      "censor"
      (\(Bot v :: (Bot ((), String))) -> test_strictness_strict (isBottom v) $ Strict.censor id $ Strict.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_lazy False $ Lazy.censor id $ Lazy.writer v)
      (\(Bot v :: (Bot ((), String))) -> test_strictness_cps (isBottom v) $ CPS.censor id $ CPS.writer v)
  ]
    -- Type classes
    ++ [ testFunction
           "Functor"
           (\(Bot v :: (Bot ((), String))) -> test_strictness_strict_IO (isBottom v) $ (\w -> w <> w) <$> Strict.writer v)
           (\(Bot v :: (Bot ((), String))) -> test_strictness_lazy_IO False $ (\w -> w <> w) <$> Lazy.writer v)
           (\(Bot v :: (Bot ((), String))) -> test_strictness_cps_IO (isBottom v) $ (\w -> w <> w) <$> CPS.writer v),
         testFunction
           "Applicative map"
           ( \(Bot v :: (Bot ((), String))) (Bot u :: (Bot ())) -> do
               let mapper =
                     if isBottom u
                       then
                         Strict.writer bottom
                       else
                         Strict.writer (id, "")
               test_strictness_strict_IO (isBottom v || isBottom u) $ mapper <*> Strict.writer v
           )
           ( \(Bot v :: (Bot ((), String))) (Bot u :: (Bot ())) -> do
               let mapper =
                     if isBottom u
                       then
                         Lazy.writer bottom
                       else
                         Lazy.writer (id, "")
               test_strictness_lazy_IO False $ mapper <*> Lazy.writer v
           )
           ( \(Bot v :: (Bot ((), String))) (Bot u :: (Bot ())) -> do
               let mapper =
                     if isBottom u
                       then
                         CPS.writer bottom
                       else
                         CPS.writer (id, "")
               test_strictness_cps_IO (isBottom v || isBottom u) $ mapper <*> CPS.writer v
           ),
         testFunction
           "Monad bind"
           (\(Bot v :: (Bot (Int, String))) (Bot w :: (Bot ((), String))) -> test_strictness_strict_IO (isBottom v || isBottom w) $ Strict.writer v >>= \_ -> Strict.writer w)
           (\(Bot v :: (Bot (Int, String))) (Bot w :: (Bot ((), String))) -> test_strictness_lazy_IO False $ Lazy.writer v >>= \_ -> Lazy.writer w)
           (\(Bot v :: (Bot (Int, String))) (Bot w :: (Bot ((), String))) -> test_strictness_cps_IO (isBottom v || isBottom w) $ CPS.writer v >>= \_ -> CPS.writer w),
         -- TODO
         -- Foldable
         -- Traversable
         -- MonadFix
         -- MonadFail
         -- MonadPlus
         -- MonadIO
         -- MonadZip
         -- MonadTrans
         testFunction
           ""
           (True === True)
           (True === True)
           (True === True),
         testFunction
           "Alternative"
           -- TODO: why does this only depend on v?
           (\(Bot v :: (Bot ((), String))) (Bot w :: (Bot ((), String))) -> test_strictness_strict_IO (isBottom v) $ Strict.writer v <|> Strict.writer w)
           (\(Bot v :: (Bot ((), String))) (Bot w :: (Bot ((), String))) -> test_strictness_lazy_IO (isBottom v) $ Lazy.writer v <|> Lazy.writer w)
           (\(Bot v :: (Bot ((), String))) (Bot w :: (Bot ((), String))) -> test_strictness_cps_IO (isBottom v) $ CPS.writer v <|> CPS.writer w),
         testGroup
           "Contravariant"
           [ testProperty
               "strict"
               ( \(Bot v :: (Bot (Int, String))) -> do
                   let writer = Strict.WriterT $ Predicate (== (1, ""))
                   isBottom (getPredicate (Strict.runWriterT $ contramap (+ 2) writer) v)
                     === isBottom v
               ),
             testProperty
               "lazy"
               ( \(Bot v :: (Bot (Int, String))) -> do
                   let writer = Lazy.WriterT $ Predicate (== (1, ""))
                   -- TODO:
                   isBottom (getPredicate (Lazy.runWriterT $ contramap (+ 2) writer) v)
                     === isBottom v
               )
           ]
       ]

testFunction :: (Testable a) => String -> a -> a -> a -> TestTree
testFunction name strict lazy cps =
  testGroup
    name
    [ testProperty "strict" strict,
      testProperty "lazy" lazy,
      testProperty "cps" cps
    ]

test_strictness_monad :: forall k writer w (m :: k) a. (writer w m a -> (a, w)) -> Bool -> writer w m a -> Property
test_strictness_monad runW True writer =
  assertExceptionIO @ErrorCall (const True) (evaluate $ runW writer)
test_strictness_monad runW False writer =
  monadicIO $ run $ runW writer `seq` return ()

test_strictness_IO :: (writer w IO a -> IO (a, w)) -> Bool -> writer w IO a -> Property
test_strictness_IO runW True writer =
  assertExceptionIO @ErrorCall (const True) (runW writer)
test_strictness_IO runW False writer = monadicIO $ run $ do
  v <- runW writer
  v `seq` return ()

test_strictness_lazy :: Bool -> Lazy.Writer w a -> Property
test_strictness_lazy = test_strictness_monad Lazy.runWriter

test_strictness_lazy_IO :: Bool -> Lazy.WriterT w IO a -> Property
test_strictness_lazy_IO = test_strictness_IO Lazy.runWriterT

test_strictness_strict_IO :: Bool -> Strict.WriterT w IO a -> Property
test_strictness_strict_IO = test_strictness_IO Strict.runWriterT

test_strictness_cps_IO :: (Monoid w) => Bool -> CPS.WriterT w IO a -> Property
test_strictness_cps_IO = test_strictness_IO CPS.runWriterT

test_strictness_strict :: Bool -> Strict.Writer w a -> Property
test_strictness_strict = test_strictness_monad Strict.runWriter

test_strictness_cps :: (Monoid w) => Bool -> CPS.Writer w a -> Property
test_strictness_cps = test_strictness_monad CPS.runWriter
