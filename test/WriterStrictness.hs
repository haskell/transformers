{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module WriterStrictness (test) where

import           Arbitrary

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Exception (ErrorCall (..), SomeException (..),
                                    evaluate)
import           Control.Exception.Base (displayException)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Signatures (Catch)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont (ContT (..), callCC, runContT)
import           Control.Monad.Trans.Except (ExceptT (..), catchE, throwE)
import           Control.Monad.Trans.Reader (ReaderT (..), ask, reader)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Zip

import           Data.Coerce
import           Data.Either (fromRight)
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.List (isPrefixOf)
import           Data.Maybe
import           Data.Monoid

import           GHC.IO (unsafePerformIO)

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assertExceptionIO, monadicIO, run)
import           Test.Tasty
import           Test.Tasty.QuickCheck

test :: IO ()
test = defaultMain $ testGroup "Writer"
  [
    testGroup "Pair (a, w)" strictPairTests2,
    testGroup "Log w" strictLogTest,
    testGroup "Pair OLD" strictPairTests
  ]

data WriterBase writer
  = WriterBase
      { writer    :: forall w m a. m (a, w) -> writer w m a,
        runWriter :: forall w m a. writer w m a -> m (a, w)
      }

-- CPS Writer requires Functor
data WriterBaseF writer
  = WriterBaseF
      { writerF    :: forall w m a. (Functor m, Monoid w) => m (a, w) -> writer w m a,
        runWriterF :: forall w m a. (Functor m, Monoid w) => writer w m a -> m (a, w)
      }

data WriterMethods writer w m
  = WriterMethods
      { execWriterT :: forall a. writer w m a -> m w,
        mapWriter   :: forall a w' n b. (Monad n, Monoid w') => (m (a, w) -> n (b, w')) -> writer w m a -> writer w' n b,
        tell        :: w -> writer w m (),
        listen      :: forall a. writer w m a -> writer w m (a, w),
        pass        :: forall a. writer w m (a, w -> w) -> writer w m a,
        censor      :: forall a. (w -> w) -> writer w m a -> writer w m a
      }

data WriterLifts writer
  = WriterLifts
      { liftCatch :: forall w m a e. Monoid w => Catch e m (a, w) -> Catch e (writer w m) a
      }

lazyBaseF :: WriterBaseF Lazy.WriterT
lazyBaseF = WriterBaseF {writerF = Lazy.WriterT, runWriterF = Lazy.runWriterT}

lazyBase :: WriterBase Lazy.WriterT
lazyBase = WriterBase {writer = Lazy.WriterT, runWriter = Lazy.runWriterT}

lazyWriterMethods :: (Monad m) => WriterMethods Lazy.WriterT w m
lazyWriterMethods =
  WriterMethods
    { execWriterT = Lazy.execWriterT,
      mapWriter = Lazy.mapWriterT,
      tell = Lazy.tell,
      listen = Lazy.listen,
      pass = Lazy.pass,
      censor = Lazy.censor
    }

lazyWriterLifts :: WriterLifts Lazy.WriterT
lazyWriterLifts = WriterLifts { liftCatch = Lazy.liftCatch }

strictBaseF :: WriterBaseF Strict.WriterT
strictBaseF = WriterBaseF {writerF = Strict.WriterT, runWriterF = Strict.runWriterT}

strictBase :: WriterBase Strict.WriterT
strictBase = WriterBase {writer = Strict.WriterT, runWriter = Strict.runWriterT}

strictWriterMethods :: (Monad m) => WriterMethods Strict.WriterT w m
strictWriterMethods =
  WriterMethods
    { execWriterT = Strict.execWriterT,
      mapWriter = Strict.mapWriterT,
      tell = Strict.tell,
      listen = Strict.listen,
      pass = Strict.pass,
      censor = Strict.censor
    }

strictWriterLifts :: WriterLifts Strict.WriterT
strictWriterLifts =
  WriterLifts
    { liftCatch = Strict.liftCatch
    }

cpsBase :: WriterBaseF CPS.WriterT
cpsBase = WriterBaseF {writerF = CPS.writerT, runWriterF = CPS.runWriterT}

cpsWriterMethods :: (Monad m, Monoid w) => WriterMethods CPS.WriterT w m
cpsWriterMethods =
  WriterMethods
    { execWriterT = CPS.execWriterT,
      mapWriter = CPS.mapWriterT,
      tell = CPS.tell,
      listen = CPS.listen,
      pass = CPS.pass,
      censor = CPS.censor
    }

cpsWriterLifts :: WriterLifts CPS.WriterT
cpsWriterLifts =
  WriterLifts
    { liftCatch = CPS.liftCatch
    }


data WriterProp a = WriterProp
  { lazy   :: Prop a,
    strict :: Prop a,
    cps    :: Prop a
  }

data Prop a = Prop a | Unsupported

runWriterProps :: Testable a => String -> WriterProp a -> TestTree
runWriterProps name WriterProp {..} =
  testGroup name $ catMaybes [
    runProp "Lazy" lazy,
    runProp "Strict" strict,
    runProp "CPS" cps
  ]
    where
      runProp monadName (Prop a) = Just $ testProperty monadName a
      runProp _ Unsupported     = Nothing

strictPairTests2 :: [TestTree]
strictPairTests2 = [
  runWriterProps "execWriter" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: (Int, String))) -> isStrict v $ withBaseMonad m $ Lazy.execWriterT $ Lazy.writer v,
      strict = Prop $ \(m :: BaseMonad, Bot (v :: (Int, String))) -> isStrict v $ withBaseMonad m $ Strict.execWriterT $ Strict.writer v,
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: (Int, String))) -> isStrict v $ withBaseMonad m $ CPS.execWriterT $ CPS.writer v
  },
  runWriterProps "mapWriter" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: (Int, String))) -> isStrict v $ withBaseMonad m $ Lazy.runWriterT $ Lazy.mapWriterT id $ Lazy.writer v,
      strict = Prop $ \(m :: BaseMonad, Bot (v :: (Int, String))) -> isStrict v $ withBaseMonad m $ Strict.runWriterT $ Strict.mapWriterT id $ Strict.writer v,
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: (Int, String))) -> isStrict v $ withBaseMonad m $ CPS.runWriterT $ CPS.mapWriterT id $ CPS.writer v
  },
  runWriterProps "listen" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: (Float, Sum Int))) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.listen $ Lazy.writer v,
      strict = Prop $ \(m :: BaseMonad, Bot (v :: (Float, Sum Int))) -> isStrict v $ withBaseMonad m $ Strict.runWriterT $ Strict.listen $ Strict.writer v,
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: (Float, Sum Int))) -> isStrict v $ withBaseMonad m $ CPS.runWriterT $ CPS.listen $ CPS.writer v
  },
  runWriterProps "pass" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: ((), F1 String String))) ->
        let v' = coerce v :: ((), String -> String)
        in isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.pass $ Lazy.writer (v', []),
      strict = Prop $ \(m :: BaseMonad, Bot (v :: ((), F1 String String))) ->
        let v' = coerce v :: ((), String -> String)
        in isStrict v $ withBaseMonad m $ Strict.runWriterT $ Strict.pass $ Strict.writer (v', []),
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: ((), F1 String String))) ->
        let v' = coerce v :: ((), String -> String)
        in isStrict v $ withBaseMonad m $ CPS.runWriterT $ CPS.pass $ CPS.writer (v', [])
  },
  runWriterProps "censor" WriterProp {
      lazy   = Prop $ \(m :: BaseMonad, Bot (v :: ((), [Int]))) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.censor id $ Lazy.writer v,
      strict = Prop $ \(m :: BaseMonad, Bot (v :: ((), [Int]))) -> isStrict v $ withBaseMonad m $ Strict.runWriterT $ Strict.censor id $ Strict.writer v,
      cps    = Prop $ \(m :: BaseMonad, Bot (v :: ((), [Int]))) -> isStrict v $ withBaseMonad m $ CPS.runWriterT $ CPS.censor id $ CPS.writer v
  },

  -- == Functor/Applicative/Monad ==
  runWriterProps "Monad: >>=" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot v :: (Bot ((), String)), Bot w :: (Bot (Int, String))) ->
          isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer v >>= const (Lazy.writer w),
      strict = Prop $ \(m :: BaseMonad, Bot v :: (Bot ((), String)), Bot w :: (Bot (Int, String))) ->
          isStrict2 v w $ withBaseMonad m $ Strict.runWriterT $ Strict.writer v >>= const (Strict.writer w),
      cps =    Prop $ \(m :: BaseMonad, Bot v :: (Bot ((), String)), Bot w :: (Bot (Int, String))) ->
          isStrict2 v w $ withBaseMonad m $ CPS.runWriterT $ CPS.writer v >>= const (CPS.writer w)
    },
  runWriterProps "Alternative: <|>" WriterProp {
      lazy     = Prop $ \(v :: [Bot (Int, String)], w :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)]
              w' = coerce w :: [(Int, String)]
           -- same as underlying alternative 
           in (isBottom <$> Lazy.runWriterT (Lazy.WriterT v' <|> Lazy.WriterT w')) === (isBottom <$> v' ++ w'),
      strict   = Prop $ \(v :: [Bot (Int, String)], w :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)]
              w' = coerce w :: [(Int, String)]
           in (isBottom <$> Strict.runWriterT (Strict.WriterT v' <|> Strict.WriterT w')) === (isBottom <$> v' ++ w'),
      cps      = Prop $ \(v :: [Bot (Int, String)], w :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)]
              w' = coerce w :: [(Int, String)]
           in (isBottom <$> CPS.runWriterT (CPS.writerT v' <|> CPS.writerT w')) === (isBottom <$> v' ++ w')
    },

  -- == Other type classes ==
  runWriterProps "Foldable: foldMap" WriterProp {
      -- NOTE: foldMap is lazy in both Lazy and Strict Writers due to the use of fst to pick the value.
      lazy   = Prop $ \(v :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)] in isLazyValue $ getSum $ foldMap (const (Sum (0 :: Int))) (Lazy.WriterT v'),
      strict = Prop $ \(v :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)] in isLazyValue $ getSum $ foldMap (const (Sum (0 :: Int))) (Strict.WriterT v'),
      cps    = Unsupported
  },
  runWriterProps "contramap" WriterProp {
      lazy   = Prop $ \(Bot (v :: (Int, String))) ->
          let f = getOp $ Lazy.runWriterT $ contramap (+1) $ Lazy.WriterT (Op id) in isLazyValue $ f v,
      strict = Prop $ \(Bot (v :: (Int, String))) ->
          let f = getOp $ Strict.runWriterT $ contramap (+1) $ Strict.WriterT (Op id) in isStrictValue v $ f v,
      cps    = Unsupported
  }
  ]

strictLogTest :: [TestTree]
strictLogTest = [
  runWriterProps "runWriter" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: Sum Int)) -> isLazy (withBaseMonad m $ Lazy.runWriterT $ Lazy.writer ((), v)),
      strict = Prop $ \(m :: BaseMonad, Bot (v :: Sum Int)) -> isLazy (withBaseMonad m $ Strict.runWriterT $ Strict.writer ((), v)),
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: Sum Int)) -> isStrict v (withBaseMonad m $ CPS.runWriterT $ CPS.writer ((), v))
  },
  runWriterProps "mapWriter" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: String)) -> isLazy (withBaseMonad m $ Lazy.runWriterT $ Lazy.mapWriterT id $ Lazy.writer (0 :: Int, v)),
      strict = Prop $ \(m :: BaseMonad, Bot (v :: String)) -> isLazy (withBaseMonad m $ Strict.runWriterT $ Strict.mapWriterT id $ Strict.writer(0 :: Int, v)),
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: String)) -> isStrict v (withBaseMonad m $ CPS.runWriterT $ CPS.mapWriterT id $ CPS.writer (0 :: Int, v))
  },
  runWriterProps "listen" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: Sum Int)) -> isLazy (withBaseMonad m $ Lazy.runWriterT $ Lazy.listen $ Lazy.writer ((), v)),
      strict = Prop $ \(m :: BaseMonad, Bot (v :: Sum Int)) -> isLazy (withBaseMonad m $ Strict.runWriterT $ Strict.listen $ Strict.writer ((), v)),
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: Sum Int)) -> isStrict v (withBaseMonad m $ CPS.runWriterT $ CPS.listen $ CPS.writer ((), v))
  },
  runWriterProps "tell" WriterProp {
      lazy =   Prop $ \(m :: BaseMonad, Bot (v :: String)) -> isLazy (withBaseMonad m $ Lazy.runWriterT $ Lazy.tell v),
      strict = Prop $ \(m :: BaseMonad, Bot (v :: String)) -> isLazy (withBaseMonad m $ Strict.runWriterT $ Strict.tell v),
      cps =    Prop $ \(m :: BaseMonad, Bot (v :: String)) -> isStrict v (withBaseMonad m $ CPS.runWriterT $ CPS.tell v)
  }
  ]

-- | Strictness tests for the value (a, w) of Writers
--
-- Test whether the pair (a, w) is handled lazily/strictly
-- in Writer functions as expected.
-- In particular, whether pairs patterns are handled correctly
-- in the respective Writers.
--
-- Please note that tests DO NOT test strictness in the log w
-- of the CPS writer.
--
strictPairTests :: [TestTree]
strictPairTests =
  testStrictPairTypeClass
    "Lazy"
    lazyBase
    StrictPairTypeClassExpectations
      { expect_bot_foldMap = const False,
        expect_bot_traverse = id,           -- NOTE: does not use lazy pattern match
        expect_bot_mzipWith = const2 False,
        expect_bot_contramap = const False
      }
    ++ testStrictPairTypeClass
      "Strict"
      strictBase
      StrictPairTypeClassExpectations
        { expect_bot_foldMap = const False, -- NOTE: lazy due to use of fst
          expect_bot_traverse = id,
          expect_bot_mzipWith = (||),
          expect_bot_contramap = id
        }
    ++ testStrictPairMonadic
      "Lazy"
      lazyBaseF
      lazyWriterMethods
      StrictPairMonadicExpectations
        { expect_bot_functor_fmap = const False,
          expect_bot_monad_bind = const2 False,
          expect_bot_applicative_apply = const2 False,
          expect_bot_alternative_list = \x y -> or $ x <> y,
          expect_bot_alternative_maybe = \x y -> fromMaybe False $ x <|> y,
          expect_bot_mfix = const False,
          expect_bot_stack = const2 False
        }
    ++ testStrictPairMonadic
      "Strict"
      strictBaseF
      strictWriterMethods
      StrictPairMonadicExpectations
        { expect_bot_functor_fmap = id,
          expect_bot_monad_bind = (||),
          expect_bot_applicative_apply = (||),
          expect_bot_alternative_list = \x y -> or $ x <> y,
          expect_bot_alternative_maybe = \x y -> fromMaybe False $ x <|> y,
          expect_bot_mfix = const False,       -- NOTE: fix must be lazy
          expect_bot_stack = (||)
        }
    ++ testStrictPairMonadic
      "CPS"
      cpsBase
      cpsWriterMethods
      StrictPairMonadicExpectations
        { expect_bot_functor_fmap = id,
          expect_bot_monad_bind = (||),
          expect_bot_applicative_apply = (||),
          expect_bot_alternative_list = \x y -> or $ x <> y,
          expect_bot_alternative_maybe = \x y -> fromMaybe False $ x <|> y,
          expect_bot_mfix = id,
          expect_bot_stack = (||)
        }
    ++ testStrictPairMethods
      "Lazy + Identity"
      runIdentity
      lazyBaseF
      lazyWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,                -- NOTE: same as strict since it reduces to the log w
          expect_bot_mapWriter_strict = id,          -- NOTE: mapWriter depends only on the strictness of the map function provided by caller
          expect_bot_mapWriter_lazy = const False,
          expect_bot_listen = const False,
          expect_bot_pass = const False,
          expect_bot_censor = const False,
          expect_bot_combination = const2 . const2 False
        }
    ++ testStrictPairMethods
      "Strict + Identity"
      runIdentity
      strictBaseF
      strictWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter_strict = id,          -- NOTE: mapWriter depends only on the strictness of the map function provided by caller
          expect_bot_mapWriter_lazy = const False,
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }
    ++ testStrictPairMethods
      "CPS + Identity"
      runIdentity
      cpsBase
      cpsWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter_strict = id,
          expect_bot_mapWriter_lazy = id,
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }
    ++ testStrictPairMethods
      "Lazy + IO"
      unsafePerformIO
      lazyBaseF
      lazyWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,                -- NOTE: same as strict since it reduces to the log w
          expect_bot_mapWriter_strict = id,          -- NOTE: mapWriter depends only on the strictness of the map function provided by caller
          expect_bot_mapWriter_lazy = const False,
          expect_bot_listen = const False,
          expect_bot_pass = const False,
          expect_bot_censor = const False,
          expect_bot_combination = const2 . const2 False
        }
    ++ testStrictPairMethods
      "Strict + IO"
      unsafePerformIO
      strictBaseF
      strictWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter_strict = id,          -- NOTE: mapWriter depends only on the strictness of the map function provided by caller
          expect_bot_mapWriter_lazy = const False,
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }
    ++ testStrictPairMethods
      "CPS + IO"
      unsafePerformIO
      cpsBase
      cpsWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter_strict = id,
          expect_bot_mapWriter_lazy = id,
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }
    ++ testStrictPairLifts
      "Lazy"
      lazyBaseF
      lazyWriterLifts
      StrictPairLiftsExpectations
        { expect_bot_liftCatch = id  -- NOTE: same as strict since the value is just passed through.
        }
    ++ testStrictPairLifts
      "Strict"
      strictBaseF
      strictWriterLifts
      StrictPairLiftsExpectations
        { expect_bot_liftCatch = id
        }
    ++ testStrictPairLifts
      "CPS"
      cpsBase
      cpsWriterLifts
      StrictPairLiftsExpectations
        { expect_bot_liftCatch = id
        }

-- Expected values of tests.
data StrictPairMethodsExpectations
  = StrictPairMethodsExpectations
      { expect_bot_execWriter       :: Bool -> Bool,
        expect_bot_mapWriter_strict :: Bool -> Bool,
        expect_bot_mapWriter_lazy   :: Bool -> Bool,
        expect_bot_listen           :: Bool -> Bool,
        expect_bot_pass             :: Bool -> Bool,
        expect_bot_censor           :: Bool -> Bool,
        expect_bot_combination      :: Bool -> Bool -> Bool -> Bool -> Bool
      }

-- | Value (a, w) strictness tests for the core Writer methods.
--
-- Functions that do not involve handling pairs are not tested:
-- * tell
testStrictPairMethods ::
  forall writer m.
  (Monad m, Monad (writer String m)) =>
  String ->
  (forall a. m a -> a) ->
  WriterBaseF writer ->
  WriterMethods writer String m ->
  StrictPairMethodsExpectations ->
  [TestTree]
testStrictPairMethods testLabel runMonad WriterBaseF {..} WriterMethods {..} StrictPairMethodsExpectations {..} =
  [ testProperty
      (prop_name "execWriter")
      ( \(Bot v :: (Bot ((), String))) ->
          let result = runMonad $ execWriterT $ writerF $ return @m v
           in isBottom result === expect_bot_execWriter (isBottom v)
      ),

    -- NOTE: strictness depends only on the strictness of the provided map
    testProperty
      (prop_name "mapWriter - strict map")
      ( \(Bot v :: (Bot ((), String))) ->
          let result = runIdentity $ runWriterF $ mapWriter fstrict $ writerF $ returnM v
              fstrict = Identity . runMonad
           in isBottom result === expect_bot_mapWriter_strict (isBottom v)
      ),
    testProperty
      (prop_name "mapWriter - lazy map")
      ( \(Bot v :: (Bot ((), String))) ->
          let result = runWriterF $ mapWriter flazy $ writerF $ returnM v
              flazy x = let ~(a, w) = runMonad x in Identity (a, w)
           in isBottom result === expect_bot_mapWriter_lazy (isBottom v)
      ),

    -- NOTE: no test for tell since it does not involve pair values

    testProperty
      (prop_name "listen")
      ( \(Bot v :: (Bot ((), String))) ->
          test_pair_strictness_monad runW (expect_bot_listen (isBottom v)) $
            listen $ writerF $ returnM v
      ),
    testProperty
      (prop_name "pass")
      ( \(Bot v :: (Bot ((), F1 String String))) ->
          let v' = coerce v :: ((), String -> String)
           in test_pair_strictness_monad runW (expect_bot_pass (isBottom v)) $
                pass $
                  writerF $
                    return (v', "")
      ),
    testProperty
      (prop_name "censor")
      ( \(Bot v :: (Bot ((), String))) ->
          test_pair_strictness_monad runW (expect_bot_censor (isBottom v)) $
            censor (\w -> w <> w) $
              writerF $
                returnM v
      ),
    testProperty
      (prop_name "combination")
      ( \(Bot t :: (Bot (Int, String)))
         (Bot u :: (Bot (Int, String)))
         (Bot v :: (Bot (Int, String)))
         (Bot w :: (Bot (Int, String))) ->
            let mt = writerF $ returnM t
                mu = writerF $ returnM u
                mv = writerF $ returnM v
                mw = writerF $ returnM w
                result = do
                  a <- mt
                  (b, x) <- listen mu
                  tell x
                  c <- censor (drop 0) mv
                  d <- mapWriter id mw
                  return $ a + b + c + d
                expected = expect_bot_combination (isBottom t) (isBottom u) (isBottom v) (isBottom w)
             in isBottom (runW result) === expected
      )
  ]
  where
    prop_name methodName = "[" <> testLabel <> "] " <> methodName
    runW :: (Monoid w) => writer w m a -> (a, w)
    runW = runMonad . runWriterF
    returnM :: (a, w) -> m (a, w)
    returnM = return @m

data StrictPairLiftsExpectations
  = StrictPairLiftsExpectations
      { expect_bot_liftCatch :: Bool -> Bool
      }

-- | Value (a, w) strictness tests for lifts.
--
-- In general, lifts do not involve pairs (a, w), since the log w is
-- not visible to other monads in the stack.
-- Hence, tests are omitted for:
-- * lift
-- * liftIO
-- * liftCallCC
--
testStrictPairLifts ::
  forall writer.
  String ->
  WriterBaseF writer ->
  WriterLifts writer ->
  StrictPairLiftsExpectations ->
  [TestTree]
testStrictPairLifts testLabel WriterBaseF {..} WriterLifts {..} StrictPairLiftsExpectations {..} =
  [
    testProperty
      (prop_name "liftCatch - no error")
      ( \(Bot v :: (Bot ((), String))) ->
        let
           catch :: Catch SomeException (writer String (ExceptT SomeException IO)) ()
           catch = liftCatch catchE
           writer = catch (writerF $ return v) e
        in test_pair_strictness_IOBase (expect_bot_liftCatch (isBottom v))
              $ fromRight e <$> runExceptT (runWriterF writer)
      ),
    testProperty
      (prop_name "liftCatch - handle error")
      ( \(Bot v :: (Bot ((), String))) ->
        let
           catch :: Catch SomeException (writer String (ExceptT SomeException IO)) ()
           catch = liftCatch catchE
           writer = catch (writerF $ throwE e) (const (writerF $ return v))
        in test_pair_strictness_IOBase (expect_bot_liftCatch (isBottom v))
              $ fromRight e <$> runExceptT (runWriterF writer)
      )
  ]
  where
    e = error "this should never happen"
    prop_name methodName = "[" <> testLabel <> "] " <> methodName

data StrictPairTypeClassExpectations
  = StrictPairTypeClassExpectations
      { expect_bot_foldMap   :: Bool -> Bool,
        expect_bot_traverse  :: Bool -> Bool,
        expect_bot_mzipWith  :: Bool -> Bool -> Bool,
        expect_bot_contramap :: Bool -> Bool
      }

-- | Value (a, w) strictness tests for type class functions other than Monads.
--
testStrictPairTypeClass ::
  forall writer.
  ( Traversable (writer String []),
    Contravariant (writer String (Op (Int, String))),
    MonadZip (writer String Identity)
  ) =>
  String ->
  WriterBase writer ->
  StrictPairTypeClassExpectations ->
  [TestTree]
testStrictPairTypeClass testLabel WriterBase {..} StrictPairTypeClassExpectations {..} =
  [ testProperty
      (prop_name "foldMap")
      ( \(v :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)]
              result = getSum $ foldMap (const (Sum (0 :: Int))) (writer v')
           in isBottom result === expect_bot_foldMap (any isBottom v)
      ),
    testProperty
      (prop_name "traverse")
      ( \(v :: [Bot (Int, String)]) ->
          let v' = coerce v :: [(Int, String)]
              result = traverse Just (writer v')
           in isBottom (isBottom result) === expect_bot_foldMap (any isBottom v)
      ),
    testProperty
      (prop_name "contramap")
      ( \(Bot v :: (Bot (Int, String))) ->
          let f = getOp $ runWriter $ contramap (+ 1) $ writer (Op id)
           in isBottom (f v) === expect_bot_contramap (isBottom v)
      ),
    testProperty
      (prop_name "mzipWith")
      ( \(Bot v :: (Bot (Int, String)))
         (Bot w :: (Bot (Int, String))) ->
            let f = runWriter $ mzipWith (+) (writer (Identity v)) (writer (Identity w))
             in isBottom f === expect_bot_mzipWith (isBottom v) (isBottom w)
      )
  ]
  where
    prop_name methodName = "[" <> testLabel <> "] " <> methodName

data StrictPairMonadicExpectations
  = StrictPairMonadicExpectations
      { expect_bot_functor_fmap      :: Bool -> Bool,
        expect_bot_monad_bind        :: Bool -> Bool -> Bool,
        expect_bot_applicative_apply :: Bool -> Bool -> Bool,
        expect_bot_alternative_list  :: [Bool] -> [Bool] -> Bool,
        expect_bot_alternative_maybe :: Maybe Bool -> Maybe Bool -> Bool,
        expect_bot_mfix              :: Bool -> Bool,
        expect_bot_stack             :: Bool -> Bool -> Bool
      }

-- | Value (a, w) strictness tests for Monadic typeclass functions.
--
testStrictPairMonadic ::
  forall writer.
  ( MonadFix (writer String Identity),
    MonadPlus (writer String []),
    MonadPlus (writer String Maybe),
    MonadIO (writer String IO)
  ) =>
  String ->
  WriterBaseF writer ->
  WriterMethods writer String IO ->
  StrictPairMonadicExpectations ->
  [TestTree]
testStrictPairMonadic testLabel WriterBaseF {..} WriterMethods {..} StrictPairMonadicExpectations {..} =
  [ testProperty
      (prop_name "Functor fmap")
      ( \(Bot v :: (Bot ((), String))) ->
          let expected = expect_bot_functor_fmap (isBottom v)
           in test_pair_strictness_monad runWriterF expected $
                (\w -> w <> w) <$> writerF (Identity v)
      ),
    testProperty
      (prop_name "Monad >>=")
      ( \(Bot v :: (Bot (Int, String)))
         (Bot w :: (Bot ((), String))) -> do
            let expected = expect_bot_monad_bind (isBottom v) (isBottom w)
             in test_pair_strictness_monad runWriterF expected $
                  writerF (Identity v) >>= const (writerF (Identity w))
      ),
    testProperty
      (prop_name "Applicative <*>")
      ( \(Bot v :: (Bot ((), String)))
         (Bot u :: (Bot (F1 () (), String))) -> do
            let expected = expect_bot_applicative_apply (isBottom v) (isBottom u)
                u' :: (() -> (), String)
                u' = coerce u
             in test_pair_strictness_monad runWriterF expected $
                  writerF (Identity u') <*> writerF (Identity v)
      ),
    testProperty
      (prop_name "Applicative liftA2")
      ( \(Bot v :: (Bot (Int, String)))
         (Bot u :: (Bot (Int, String))) -> do
            let expected = expect_bot_applicative_apply (isBottom v) (isBottom u)
            test_pair_strictness_monad runWriterF expected $
              liftA2 (+) (writerF (Identity v)) (writerF (Identity u))
      ),
    testProperty
      -- NOTE: implementation of any fixed point functions must be lazy, so
      -- mfix is lazy for all Writer types.
      (prop_name "MonadFix mfix")
      ( \(Bot v :: (Bot ([Int], String))) ->
          let expected = expect_bot_mfix (isBottom v)
           in test_pair_strictness_monad runWriterF expected $
                mfix (\x -> writerF $ Identity $ first (x <>) v)
      ),
    testProperty
      (prop_name "Alternative <|> - []")
      ( \(v :: [Bot (Int, String)])
         (w :: [Bot (Int, String)]) ->
            let v' = coerce v :: [(Int, String)]
                w' = coerce w :: [(Int, String)]
                result = runWriterF $ writerF v' <|> writerF w'
                expected = expect_bot_alternative_list (isBottom <$> v') (isBottom <$> w')
             in any isBottom result === expected
      ),
    testProperty
      (prop_name "Alternative <|> - Maybe")
      ( \(v :: Maybe (Bot (Int, String)))
         (w :: Maybe (Bot (Int, String))) ->
            let v' = coerce v :: Maybe (Int, String)
                w' = coerce w :: Maybe (Int, String)
                result = runWriterF $ writerF v' `mplus` writerF w'
             in any isBottom result === expect_bot_alternative_maybe (isBottom <$> v') (isBottom <$> w')
      ),
    testProperty
      (prop_name "MonadPlus mplus - []")
      ( \(v :: [Bot (Int, String)])
         (w :: [Bot (Int, String)]) ->
            let v' = coerce v :: [(Int, String)]
                w' = coerce w :: [(Int, String)]
                result = runWriterF $ writerF v' `mplus` writerF w'
                expected = expect_bot_alternative_list (isBottom <$> v') (isBottom <$> w')
             in any isBottom result === expected
      ),

    testProperty
      (prop_name "Monad stack")
      ( \(Bot v :: (Bot (Int, String)))
         (Bot w :: (Bot (Int, String)))
         ->
            let expected = expect_bot_stack (isBottom v) (isBottom w)
                result :: TestMonadStack String (writer String IO) Int
                result = do
                  w' <- lift $ lift $ writerF $ return w
                  a <- lift ask
                  b <- callCC $ \next -> do
                    when (even a) $ next (10 :: Int)
                    return 20
                  v' <- lift $ lift $ writerF $ return v
                  lift $ lift $ tell "hello"
                  return $ w' + a + v' + b
                writer = censor (filter (/= 'l')) $ runTestMonadStack result show
             in test_pair_strictness_IO (runWriterF @String) expected writer
      )
  ]
  where
    prop_name methodName = "[" <> testLabel <> "]" <> methodName

type TestMonadStack r m a = (ContT r (ReaderT Int m) a)

runTestMonadStack :: (Monad m) => TestMonadStack r m a -> (a -> r) -> m r
runTestMonadStack m cont = runReaderT rd 0
  where rd = runContT m $ \x -> reader $ const (cont x)

const2 :: a -> b -> c -> a
const2 = const . const

test_pair_strictness_monad :: (writer w m a -> b) -> Bool -> writer w m a -> Property
test_pair_strictness_monad runWriter expected writer = test_pair_strictness_IOBase expected $ evaluate (runWriter writer)

test_pair_strictness_IO :: (writer w IO a -> IO b) -> Bool -> writer w IO a -> Property
test_pair_strictness_IO runW expected = test_pair_strictness_IOBase expected . runW

test_pair_strictness_IOBase :: Bool -> IO a -> Property
test_pair_strictness_IOBase True v = assertExceptionIO @ErrorCall (const True) v
test_pair_strictness_IOBase False v = monadicIO $ run $ do
  v' <- v
  v' `seq` return ()

isStrictValue :: arg1 -> a -> Property
isStrictValue x  = isStrict x . evaluate

isLazyValue :: a -> Property
isLazyValue = isLazy . evaluate

-- Strictness in one argument:
-- The result (normalized to IO) should be bottom whenever arg1 is bottom.
isStrict :: arg1 -> IO a -> Property
isStrict x  = shouldBeBottom (isBottom x)

-- Strictness in two arguments:
-- The result (normalized to IO) should be bottom whenever arg1 OR arg2 is bottom.
isStrict2 :: arg1 -> arg2 -> IO a -> Property
isStrict2 x y  = shouldBeBottom (isBottom x || isBottom y)

isLazy :: IO a -> Property
isLazy = shouldBeBottom False

shouldBeBottom :: Bool -> IO a -> Property
shouldBeBottom True result = assertExceptionIO isBottomError result
  where
    -- Check error message only i.e. prefix, ignoring stacktrace.
    isBottomError :: ErrorCall -> Bool
    isBottomError e = "<bottom>" `isPrefixOf` displayException e
shouldBeBottom False result = monadicIO $ run $ do
  v <- result
  v `seq` return ()
