{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module WriterStrictness (test) where

import           Control.Applicative
import           Control.Exception (ErrorCall (..), Exception,
                                    SomeException (..), evaluate, throw)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Signatures (CallCC, Catch)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT (..), throwE)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Zip

import           Data.Coerce
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Monoid

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assertExceptionIO, monadicIO, run)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Utils

test :: IO ()
test = defaultMain $ testGroup "Writer" strictPairTests

data WriterBase writer = WriterBase {
    writer    :: forall w m a. m (a, w) -> writer w m a,
    runWriter :: forall w m a. writer w m a -> m (a, w)
  }

-- CPS Writer requires Functor
data WriterBaseF writer = WriterBaseF {
    writerF    :: forall w m a. (Functor m, Monoid w) => m (a, w) -> writer w m a,
    runWriterF :: forall w m a. (Functor m, Monoid w) => writer w m a -> m (a, w)
  }

data WriterMethods writer w m = WriterMethods {
    execWriterT :: forall a.   writer w m a -> m w,
    mapWriter   :: forall a w' n b. (Monad n, Monoid w') => (m (a, w) -> n (b, w')) -> writer w m a -> writer w' n b,
    tell        ::              w -> writer w m (),
    listen      :: forall  a.   writer w m a -> writer w m (a, w),
    pass        :: forall  a.   writer w m (a, w -> w) -> writer w m a,
    censor      :: forall  a.   (w -> w) -> writer w m a -> writer w m a,
    liftCallCC  :: forall  a b. CallCC m (a, w) (b, w) -> CallCC (writer w m) a b,
    liftCatch   :: forall  a e. Catch e m (a, w) -> Catch e (writer w m) a
  }

data StrictPairMethodsExpectations = StrictPairMethodsExpectations {
    expect_bot_execWriter  :: Bool -> Bool,
    expect_bot_mapWriter   :: Bool -> Bool,
    expect_bot_tell        :: Bool -> Bool,
    expect_bot_listen      :: Bool -> Bool,
    expect_bot_pass        :: Bool -> Bool,
    expect_bot_censor      :: Bool -> Bool,
    expect_bot_liftCallCC  :: Bool -> Bool,
    expect_bot_liftCatch   :: Bool -> Bool,
    expect_bot_combination :: Bool -> Bool -> Bool -> Bool -> Bool
  }

data StrictPairMonadicExpectations = StrictPairMonadicExpectations {
    expect_bot_functor_fmap      :: Bool -> Bool,
    expect_bot_monad_bind        :: Bool -> Bool -> Bool,
    expect_bot_applicative_apply :: Bool -> Bool -> Bool,
    expect_bot_alternative_list  :: [Bool] -> [Bool] -> Bool,
    expect_bot_alternative_maybe :: Maybe Bool -> Maybe Bool -> Bool,
    expect_bot_mfix              :: Bool -> Bool,
    expect_bot_lift              :: Bool -> Bool,
    expect_bot_nested            :: Bool -> Bool -> Bool 
  }

data StrictPairTypeClassExpectations = StrictPairTypeClassExpectations {
    expect_bot_foldMap   :: Bool -> Bool,
    expect_bot_traverse  :: Bool -> Bool,
    expect_bot_mzipWith  :: Bool -> Bool -> Bool,
    expect_bot_contramap :: Bool -> Bool
  }

lazyBaseF :: WriterBaseF Lazy.WriterT
lazyBaseF = WriterBaseF {writerF = Lazy.WriterT, runWriterF = Lazy.runWriterT}

lazyBase :: WriterBase Lazy.WriterT
lazyBase = WriterBase {writer = Lazy.WriterT, runWriter = Lazy.runWriterT}

lazyWriterMethods :: (Monad m, Monoid w) => WriterMethods Lazy.WriterT w m
lazyWriterMethods =
  WriterMethods {
      execWriterT = Lazy.execWriterT,
      mapWriter = Lazy.mapWriterT,
      tell = Lazy.tell,
      listen = Lazy.listen,
      pass = Lazy.pass,
      censor = Lazy.censor,
      liftCallCC = Lazy.liftCallCC,
      liftCatch = Lazy.liftCatch
    }

strictBaseF :: WriterBaseF Strict.WriterT
strictBaseF = WriterBaseF {writerF = Strict.WriterT, runWriterF = Strict.runWriterT}

strictBase :: WriterBase Strict.WriterT
strictBase = WriterBase {writer = Strict.WriterT, runWriter = Strict.runWriterT}

strictWriterMethods :: (Monad m, Monoid w) => WriterMethods Strict.WriterT w m
strictWriterMethods =
  WriterMethods
    { execWriterT = Strict.execWriterT,
      mapWriter = Strict.mapWriterT,
      tell = Strict.tell,
      listen = Strict.listen,
      pass = Strict.pass,
      censor = Strict.censor,
      liftCallCC = Strict.liftCallCC,
      liftCatch = Strict.liftCatch
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
      censor = CPS.censor,
      liftCallCC = CPS.liftCallCC,
      liftCatch = CPS.liftCatch
    }

-- Strictness tests for the *pair* (a, w)
--
strictPairTests :: [TestTree]
strictPairTests =
  testStrictPairTypeClass
    "Lazy"
    lazyBase
    StrictPairTypeClassExpectations
      { expect_bot_foldMap = const False,
        expect_bot_traverse = id, -- ISSUE?: not lazy pattern
        expect_bot_mzipWith = const2 False,
        expect_bot_contramap = const False
      }
    ++ testStrictPairTypeClass
      "Strict"
      strictBase
      StrictPairTypeClassExpectations
        { expect_bot_foldMap = const False, -- NOTE: same as lazy since it uses fst
          expect_bot_traverse = id,
          expect_bot_mzipWith = (||),
          expect_bot_contramap = id
        }
    ++ testStrictPairMonadic
      "Lazy"
      lazyBaseF
      StrictPairMonadicExpectations
        { expect_bot_functor_fmap = const False,
          expect_bot_monad_bind = const2 False,
          expect_bot_applicative_apply = const2 False,
          expect_bot_alternative_list = \x y -> or $ x <> y,
          expect_bot_alternative_maybe = \x y -> fromMaybe False $ x <|> y,
          expect_bot_mfix = id, -- NOTE: same as strict
          expect_bot_lift = const False,
          expect_bot_nested = const2  False
        }
    ++ testStrictPairMonadic
      "Strict"
      strictBaseF
      StrictPairMonadicExpectations
        { expect_bot_functor_fmap = id,
          expect_bot_monad_bind = (||),
          expect_bot_applicative_apply = (||),
          expect_bot_alternative_list = \x y -> or $ x <> y,
          expect_bot_alternative_maybe = \x y -> fromMaybe False $ x <|> y,
          expect_bot_mfix = id,
          expect_bot_lift = const False, -- NOTE: same as lazy since it's just a wrapping
          expect_bot_nested = (||)
        }
    ++ testStrictPairMonadic
      "CPS"
      cpsBase
      StrictPairMonadicExpectations
        { expect_bot_functor_fmap = id,
          expect_bot_monad_bind = (||),
          expect_bot_applicative_apply = (||),
          expect_bot_alternative_list = \x y -> or $ x <> y,
          expect_bot_alternative_maybe = \x y -> fromMaybe False $ x <|> y,
          expect_bot_mfix = id,
          expect_bot_lift = const False, -- NOTE: same as lazy since it's just a wrapping
          expect_bot_nested = (||)
        }
    ++ testStrictPairMethods
      "Lazy + Identity"
      runIdentity
      lazyBaseF
      lazyWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id, -- NOTE: same as strict since reduces to the log w
          expect_bot_mapWriter = id, -- NOTE: same as strict since it just applies a map on the pair (a, w)
          expect_bot_tell = const False,
          expect_bot_listen = const False,
          expect_bot_pass = const False,
          expect_bot_censor = const False,
          expect_bot_liftCallCC = const False,
          expect_bot_liftCatch = const False,
          expect_bot_combination = const2 . const2 False
        }
    ++ testStrictPairMethods
      "Strict + Identity"
      runIdentity
      strictBaseF
      strictWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter = id,
          expect_bot_tell = const False, -- NOTE: same as lazy since same implementation
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_liftCallCC = id,
          expect_bot_liftCatch = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }
    ++ testStrictPairMethods
      "CPS + Identity"
      runIdentity
      cpsBase
      cpsWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter = id,
          expect_bot_tell = id,
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_liftCallCC = id,
          expect_bot_liftCatch = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }
    -- Maybe
    ++ testStrictPairMethods
      "Lazy + Maybe"
      fromJust
      lazyBaseF
      lazyWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id, -- NOTE: same as strict since reduces to the log w
          expect_bot_mapWriter = id, -- NOTE: same as strict since it just applies a map on the pair (a, w)
          expect_bot_tell = const False,
          expect_bot_listen = const False,
          expect_bot_pass = const False,
          expect_bot_censor = const False,
          expect_bot_liftCallCC = const False,
          expect_bot_liftCatch = const False,
          expect_bot_combination = const2 . const2 False
        }
    ++ testStrictPairMethods
      "CPS + Identity"
      fromJust
      cpsBase
      cpsWriterMethods
      StrictPairMethodsExpectations
        { expect_bot_execWriter = id,
          expect_bot_mapWriter = id,
          expect_bot_tell = id,
          expect_bot_listen = id,
          expect_bot_pass = id,
          expect_bot_censor = id,
          expect_bot_liftCallCC = id,
          expect_bot_liftCatch = id,
          expect_bot_combination = \t u v w -> or [t, u, v, w]
        }

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
          let result = runW $ writerF $ return @m v
           in isBottom result === expect_bot_execWriter (isBottom v)
      ),
    testProperty
      (prop_name "mapWriter - Identity")
      ( \(Bot v :: (Bot ((), String))) ->
          let result = runIdentity $ runWriterF $ mapWriter f $ writerF $ returnM v
              f = Identity . runMonad
           in isBottom result === expect_bot_mapWriter (isBottom v)
      ),
    testProperty
      (prop_name "mapWriter - Maybe")
      ( \(Bot v :: (Bot ((), String))) ->
          let result = fromMaybe ((), "_") $ runWriterF $ mapWriter f $ writerF $ returnM v
              f = Just . runMonad
           in isBottom result === expect_bot_mapWriter (isBottom v)
      ),
    testProperty
      (prop_name "tell")
      ( \(Bot v :: (Bot String)) ->
          test_pair_strictness_monad runW (expect_bot_tell (isBottom v)) $
            tell v
      ),
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
            censor copyMonoid $
              writerF $
                returnM v
      ),
    testProperty
      (prop_name "liftCallCC")
      (label "TODO" $ property ()),
    testProperty
      (prop_name "liftCatch")
      (label "TODO" $ property ()),
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

copyMonoid :: (Monoid w) => w -> w
copyMonoid w = w <> w

const2 :: a -> b -> c -> a
const2 = const . const

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

testStrictPairMonadic ::
  forall writer.
  ( MonadFix (writer String Maybe),
    MonadPlus (writer String []),
    MonadPlus (writer String Maybe),
    MonadTrans (writer String),
    MonadIO (writer String IO)
  ) =>
  String ->
  WriterBaseF writer ->
  StrictPairMonadicExpectations ->
  [TestTree]
testStrictPairMonadic testLabel WriterBaseF {..} StrictPairMonadicExpectations {..} =
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
      (prop_name "MonadFix mfix")
      ( \(Bot v :: (Bot (Int, String))) ->
          let expected = expect_bot_mfix (isBottom v)
           in test_pair_strictness_monad (fromJust . runWriterF) expected $
                mfix (const (writerF $ Just v))
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
      (prop_name "MonadTrans lift")
      ( \(Bot v :: (Bot ())) ->
          let expected = expect_bot_lift (isBottom v)
           in test_pair_strictness_IO (runWriterF @String) expected $
                lift (return v)
      ),
    testProperty
      (prop_name "MonadIO liftIO")
      ( \(Bot v :: (Bot ())) ->
          let expected = expect_bot_lift (isBottom v)
           in test_pair_strictness_IO (runWriterF @String) expected $
                liftIO (return v)
      ),
    testProperty
      (prop_name "Monad nested")
      ( \
         (Bot u :: (Bot (Int, String)))
         (Bot v :: (Bot (Int, String)))
         (Bot w :: (Bot (Int, String)))
                ->
            let expected = expect_bot_nested (isBottom v) (isBottom w)
                result :: TestMonad TestException (writer String IO) Int
                result = TestMonad $ do
                  s <- lift $ lift $ writerF $ return v
                  a <- lift $ lift  $ writerF $ return w
                  t <- lift ask
                  when (isBottom u) $ throwE TestException
                  return $ s + t * a
             in test_pair_strictness_IO (runWriterF @String) (expected || isBottom u) $
               runTestMonad result 
      )
  ]
  where
    prop_name methodName = "[" <> testLabel <> "]" <> methodName

newtype TestMonad e m a = TestMonad (ExceptT e (ReaderT Int m) a)
  deriving newtype (Functor, Applicative, Monad)

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException where

instance MonadTrans (TestMonad e) where
  lift m = TestMonad $ lift $ lift m

-- runTestMonad :: (MonadIO m, Exception e) => TestMonad e m a -> m a
runTestMonad :: (MonadIO m) => TestMonad e m a -> m a
runTestMonad (TestMonad m)  = do
    result <- x
    case result of
      Right v -> return v
      -- Left _  -> liftIO $ evaluate $ error "OOP"
      -- Left _  -> liftIO $ evaluate $ error "OOP"
      Left _  -> liftIO $ evaluate bottom
      -- Left _  -> return y
  where x = (runReaderT $ runExceptT m) 0

-- testMonad :: (Monad m) => m a -> TestMonad m a
-- testMonad = TestMonad . lift . lift

-- runTestMonad :: (Monad m) => TestMonad m Int -> (Int -> m Int) -> Int -> m Int
-- runTestMonad (TestMonad m) f = runReaderT (runContT m (lift . f))

test_pair_strictness_monad :: forall k writer w (m :: k) a b. (writer w m a -> b) -> Bool -> writer w m a -> Property
test_pair_strictness_monad runW True wr =
  assertExceptionIO @ErrorCall (const True) (evaluate $ runW wr)
test_pair_strictness_monad runW False wr = monadicIO $ run $ runW wr `seq` return ()

test_pair_strictness_IO :: (writer w IO a -> IO b) -> Bool -> writer w IO a -> Property
test_pair_strictness_IO runW True wr =
  assertExceptionIO @ErrorCall (const True) (runW wr)
test_pair_strictness_IO runW False wr = monadicIO $ run $ (`seq` ()) <$> runW wr
