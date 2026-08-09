{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module WriterStrictness (test) where

import           Arbitrary

import           Control.Applicative
import           Control.Exception (ErrorCall (..))
import           Control.Exception.Base (displayException)
import           Control.Monad.Fix (MonadFix (..))
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Zip

import           Data.Coerce
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.List (isPrefixOf)
import           Data.Monoid

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assertExceptionIO)
import           Test.Tasty
import           Test.Tasty.QuickCheck

test :: IO ()
test = defaultMain $ testGroup "Writer" strictnessTest

-- | NOTE: Monoid choice
--
-- For the test, we use a monoid that is strict in both arguments of mappend :: a -> a -> a
--
-- This is needed in particular to test a strictness property of the CPS writer, which
-- is that it evaluates the log w strictly as it is accumulated.
-- If mappend is lazy in an argument, then outwardly the result appears the same with or without evaluation
-- and hence cannot be used for validation, which will complicate the tests.
-- e.g.
--   List mappend is strict only in the first argument:
--     writer ((), _|_) >> writer ((), "a")      _|_
--     writer ((), "a") >> writer ((), _|_)      ((), ('a':_|_))
--
--   Sum mappend is strict in both:
--     writer ((), Sum 0) >> writer ((), _|_)    _|_
--     writer ((), _|_) >> writer ((), Sum 0)    _|_
type SumInt = Sum Int

-- | NOTE: Strictness test
--
-- Each writer variation is strict in the following ways:
--
--  Writer.Lazy     non-strict in spine (a,w)
--
--  Writer.Strict   strict in spine (a,w)
--
--  Writer.CPS      strict in spine (a,w) and log w
--
strictnessTest :: [TestTree]
strictnessTest = [
  testGroup "writer" [
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) -> isLazy $ Lazy.runWriterT $ Lazy.writer @LazyIdentity (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) -> isLazy $ Strict.runWriterT $ Strict.writer @LazyIdentity (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ CPS.writer @SumInt @LazyIdentity (unBotDeeper p)
  ],

  testGroup "execWriterT" [
      -- NOTE: TODO explain why we use LazyIdentity here
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) -> isLazy $ Lazy.execWriterT $ Lazy.WriterT $ LazyIdentity (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) -> isStrictIn p $ Strict.execWriterT $ Strict.WriterT $ LazyIdentity (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.execWriterT $ CPS.writer @SumInt @LazyIdentity (unBotDeeper p)
  ],

  -- Lazy, Strict Writer    output of map is used directly without any intervention by the Writer itself
  --                        i.e. spine strict
  --
  -- CPS Writer             output of map is evaluated in the log w
  testGroup "mapWriter" [
      testProperty "Lazy"   $ \(f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = unF1Bot f
            result = f' ((), mempty)
        in isStrictIn result $ runLazyIdentity $ Lazy.runWriterT $ Lazy.mapWriterT @LazyIdentity (f'<$>) $ pure (),
      testProperty "Strict" $ \(f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = unF1Bot f
            result = f' ((), mempty)
        in isStrictIn result $ runLazyIdentity $ Strict.runWriterT $ Strict.mapWriterT @LazyIdentity (f'<$>) $ pure (),
      testProperty "CPS"    $ \(f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = coerce f :: ((), SumInt) -> ((), SumInt)
            result = f' ((), mempty)
        in isBiStrictIn result (snd result) $ CPS.runWriterT $ CPS.mapWriterT @LazyIdentity (f'<$>) $ pure ()
  ],

  testGroup "listen" [
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) -> isLazy $ Lazy.runWriterT $ Lazy.listen $ Lazy.WriterT $ LazyIdentity (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) -> isStrictIn p $ Strict.runWriterT $ Strict.listen $ Strict.WriterT $ LazyIdentity (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ CPS.listen $ CPS.writer @SumInt @LazyIdentity (unBotDeeper p)
  ],

  testGroup "listens" [
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) -> isLazy $ runLazyIdentity $ Lazy.runWriterT $ Lazy.listens id $ Lazy.WriterT $ LazyIdentity (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) -> isStrictIn p $ Strict.runWriterT $ Strict.listens id $ Strict.WriterT $ LazyIdentity (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ CPS.listens id $ CPS.writer @SumInt @LazyIdentity (unBotDeeper p)
  ],

  testGroup "tell" [
      testProperty "Lazy"   $ \(Bot (w :: SumInt)) -> isLazy $ runLazyIdentity $ Lazy.runWriterT $ Lazy.tell w,
      testProperty "Strict" $ \(Bot (w :: SumInt)) -> isLazy $ runLazyIdentity $ Strict.runWriterT $ Strict.tell w,
      testProperty "CPS"    $ \(Bot (w :: SumInt)) -> isStrictIn w $ CPS.runWriterT $ CPS.tell @SumInt @LazyIdentity w
  ],

  -- NOTE: strictness in the log w for censor (CPS only)
  -- censor should be strict in the *final* value of the log w after applying the log censor (w -> w),
  -- not the initial value in the given writer.
  -- Thus, for the tests below, a bottom log value is tested in the output of the log censor f, instead of the log w in the initial (a, w).
  testGroup "censor" [
      testProperty "Lazy"   $ \(f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
        in isLazy $ runLazyIdentity $ Lazy.runWriterT $ Lazy.censor f' $ Lazy.WriterT $ LazyIdentity p,
      testProperty "Strict" $ \(f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
        in isStrictIn p $ Strict.runWriterT $ Strict.censor f' $ Strict.WriterT $ LazyIdentity p,
      testProperty "CPS"    $ \(f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
            result = f' mempty
        in isBiStrictIn p result $ CPS.runWriterT $ CPS.censor f' $ CPS.writer @SumInt @LazyIdentity p
  ],

  -- See note on censor above.
  testGroup "pass" [
      testProperty "Lazy"   $ \(p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
        in isLazy $ runLazyIdentity $ Lazy.runWriterT $ Lazy.pass $ Lazy.WriterT $ LazyIdentity p',
      testProperty "Strict" $ \(p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
        in isStrictIn p $ Strict.runWriterT $ Strict.pass $ Strict.WriterT $ LazyIdentity p',
      testProperty "CPS"    $ \(p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
            result = (snd $ fst p') mempty
        in isBiStrictIn p result $ CPS.runWriterT $ CPS.pass $ CPS.writer @SumInt @LazyIdentity p'
  ],

  -- == Functor/Applicative/Monad ==
  testGroup "Functor: fmap" [
      testProperty "Lazy"    $ \(p :: Bot (Int, Bot SumInt)) -> isLazy $ runLazyIdentity $ Lazy.runWriterT $ (+1) <$> Lazy.WriterT (LazyIdentity (unBotDeeper p)),
      testProperty "Strict"  $ \(p :: Bot (Int, Bot SumInt)) -> isStrictIn p $ runLazyIdentity $ Strict.runWriterT $ (+1) <$> Strict.WriterT (LazyIdentity (unBotDeeper p)),
      testProperty "CPS"     $ \(p :: Bot (Int, Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ (+1) <$> CPS.writer @SumInt @LazyIdentity (unBotDeeper p)
    ],

  testGroup "Applicative: <*>" [
      testProperty "Lazy"    $ \(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isLazy $ runLazyIdentity $ Lazy.runWriterT $ Lazy.writer f' <*> Lazy.writer (unBotDeeper p),
      testProperty "Strict"  $ \(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isBiStrictIn p wf $ runLazyIdentity $ Strict.runWriterT $ Strict.writer f' <*> Strict.writer (unBotDeeper p),
      testProperty "CPS"     $ \(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isBiStrictDeeperIn p wf $ CPS.runWriterT $ CPS.writer @SumInt @LazyIdentity f' <*> CPS.writer (unBotDeeper p)
    ],

  testGroup "Applicative: liftA2" [
      testProperty "Lazy"    $ \(p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isLazy $ runLazyIdentity $ Lazy.runWriterT $ liftA2 (+) (Lazy.writer $ unBotDeeper p) (Lazy.writer $ unBotDeeper q),
      testProperty "Strict"  $ \(p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isBiStrictIn p q $ runLazyIdentity $ Strict.runWriterT $ liftA2 (+) (Strict.writer $ unBotDeeper p) (Strict.writer $ unBotDeeper q),
      testProperty "CPS"     $ \(p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isBiStrictDeeperIn p q $ CPS.runWriterT $ liftA2 (+) (CPS.writer @SumInt @LazyIdentity $ unBotDeeper p) (CPS.writer $ unBotDeeper q)
    ],

  testGroup "Monad: >>=" [
      testProperty "Lazy"    $ \(p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isLazy $ runLazyIdentity $ Lazy.runWriterT $ Lazy.writer (unBotDeeper p) >>= const (Lazy.writer $ unBotDeeper q),
      testProperty "Strict"  $ \(p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isBiStrictIn p q $ runLazyIdentity $ Strict.runWriterT $ Strict.writer (unBotDeeper p) >>= const (Strict.writer $ unBotDeeper q),
      testProperty "CPS"     $ \(p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isBiStrictDeeperIn p q $ CPS.runWriterT $ CPS.writer @SumInt @LazyIdentity (unBotDeeper p) >>= const (CPS.writer (unBotDeeper q))
    ],


  -- == Other typeclasses ==
  testGroup "Foldable: foldMap" [
      -- NOTE: foldMap is lazy for both Writers, since it only involves the value `a` of Writer w m a.
      testProperty "Lazy"    $ \(p :: [Bot (Int, SumInt)]) ->
          let p' = unBot <$> p
          in isLazy $ foldMap (const (Sum (0 :: Int))) (Lazy.WriterT p'),
      testProperty "Strict"  $ \(p :: [Bot (Int, SumInt)]) ->
          let p' = unBot <$> p
          in isLazy $ foldMap (const (Sum (0 :: Int))) (Strict.WriterT p')
      -- NOTE: no Foldable for CPS
  ],

  testGroup "Traversable: traverse" [
      testProperty "Lazy"    $ \(p :: [Bot (Int, SumInt)]) ->
          let p' = unBot <$> p
              result = traverse Identity (Lazy.WriterT p')
          in (isBottom <$> p') === (isBottom <$> Lazy.runWriterT (runIdentity result)),
      testProperty "Strict"  $ \(p :: [Bot (Int, SumInt)]) ->
          let p' = unBot <$> p
              result = traverse Identity (Strict.WriterT p')
          in (isBottom <$> p') === (isBottom <$> Strict.runWriterT (runIdentity result))

      -- NOTE: no Traversable for CPS
  ],

  testGroup "MonadZip: mzipWith" [
      testProperty "Lazy"  $
        \(Bot (p :: (Int, SumInt)))
         (Bot (q :: (Int, SumInt))) ->
           isLazy $ Lazy.runWriterT $ mzipWith (+) (Lazy.WriterT (Identity p)) (Lazy.WriterT (Identity q)),
      testProperty "Strict"  $
        \(Bot (p :: (Int, SumInt)))
         (Bot (q :: (Int, SumInt))) ->
           isBiStrictIn p q $ Strict.runWriterT $ mzipWith (+) (Strict.WriterT (Identity p)) (Strict.WriterT (Identity q))
      -- NOTE: no MonadZip for CPS
  ],

  testGroup "Contravariant: contramap" [
      testProperty "Lazy"   $ \(Bot (p :: (Int, SumInt))) ->
        let f = getOp $ Lazy.runWriterT $ contramap (+1) $ Lazy.WriterT (Op id)
        in shouldBeBottom False $ f p,
      testProperty "Strict" $ \(Bot (p :: (Int, SumInt))) ->
        let f = getOp $ Strict.runWriterT $ contramap (+1) $ Strict.WriterT (Op id)
        in isStrictIn p $ f p
      -- NOTE: no Contravariant for CPS
  ],

  testGroup "MonadFix: mfix" [
      -- TODO: documentation
      testProperty "Lazy"   $ \(Bot (p :: (Int, SumInt))) ->
        isStrictIn p $ Lazy.runWriterT $ mfix (const $ Lazy.WriterT (Identity p)),
      testProperty "Strict" $ \(Bot (p :: (Int, SumInt))) ->
        isStrictIn p $ Strict.runWriterT $ mfix (const $ Strict.WriterT (Identity p)),
      testProperty "CPS" $ \(Bot (p :: (Int, SumInt))) ->
        isStrictIn p $ CPS.runWriterT $ mfix (const $ CPS.writer @SumInt @Identity p)
  ],

  testGroup "combination" [
     testProperty "Lazy" $
       \ m
         (f :: F1Bot (Int, SumInt) (Int, SumInt))
         (Bot (r :: SumInt))
         (Bot (s :: SumInt))
         (Bot (t :: (Int, SumInt)))
         (Bot (u :: (Int, SumInt)))
         (Bot (v :: (Int, SumInt))) ->
           shouldBeBottomIO False $ withBaseMonad m $ Lazy.runWriterT $ do
             a <- Lazy.WriterT (return t)
             (b, x) <- Lazy.listen $ Lazy.WriterT $ return u
             Lazy.tell $ s <> x
             c <- Lazy.censor (<>r) $ Lazy.WriterT $ return v
             d <- Lazy.mapWriterT (unF1Bot f<$>) $ pure 0

             return $ a + b + c + d,

     testProperty "Strict" $
       \ m
         (f :: F1Bot (Int, SumInt) (Int, SumInt))
         (Bot (r :: SumInt))
         (Bot (s :: SumInt))
         (Bot (t :: (Int, SumInt)))
         (Bot (u :: (Int, SumInt)))
         (Bot (v :: (Int, SumInt))) ->
           let
             f' = unF1Bot f
             expected = isBottom t || isBottom u || isBottom v
              || isBottom (f' (0, mempty))
           in shouldBeBottomIO expected $ withBaseMonad m $ Strict.runWriterT $ do
             a <- Strict.WriterT (return t)
             (b, x) <- Strict.listen $ Strict.WriterT $ return u
             Strict.tell $ s <> x
             c <- Strict.censor (<> r) $ Strict.WriterT $ return v
             d <- Strict.mapWriterT (f'<$>) $ pure 0
             return $ a + b + c + d,

     testProperty "CPS" $
       \ m
         (f :: F1Bot (Int, SumInt) (Int, Bot SumInt))
         (Bot (r :: SumInt))
         (Bot (s :: SumInt))
         (t :: Bot (Int, Bot SumInt))
         (u :: Bot (Int, Bot SumInt))
         (v :: Bot (Int, Bot SumInt)) ->
           let
             f' = coerce f :: (Int, SumInt) -> (Int, SumInt)
             expected = isBottomDeeper t || isBottomDeeper u || isBottomDeeper v
              || isBottom s || isBottom r
              || isBottom (f' (0, mempty)) || isBottom (snd $ f' (0, mempty))
           in shouldBeBottomIO expected $ withBaseMonad m $ CPS.runWriterT $ do
             a <- CPS.writer $ unBotDeeper t
             (b, x) <- CPS.listen $ CPS.writer $ unBotDeeper u
             CPS.tell $ s <> x
             c <- CPS.censor (<>r) $ CPS.writer $ unBotDeeper v
             d <- CPS.mapWriterT (f'<$>) $ pure 0
             return $ a + b + c + d
    ]
  ]

bottomLabel :: String
bottomLabel = "_|_"

notBottomLabel :: String
notBottomLabel = "Not _|_"

bottomLabelFor :: String -> Bool -> String
bottomLabelFor s x = s <> ": " <> (if x then bottomLabel else notBottomLabel)

-- Never bottom
isLazy :: a -> Property
isLazy = shouldBeBottom False

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
  where
    -- Check error message only i.e. the prefix, ignoring the stacktrace.
    isBottomError :: ErrorCall -> Bool
    isBottomError e = "<bottom>" `isPrefixOf` displayException e


-- | NOTE: Deeper strictness assertions for CPS
--
-- CPS Writer is strict in multiple levels, namely:
-- a) strict in the spine (a, w)
-- b) strict in the log w
--
-- Thus, for CPS tests, we expect the output to bottom whenever the input is bottom in either of the above ways.
-- The "Deeper" assertions below are a deeper analogue of the assertions above which only check up to WHNF of the argument.

-- Deeper unBot for CPS.
-- See NOTE on deeper strictness above for details.
unBotDeeper :: Bot (a, Bot w) -> (a, w)
unBotDeeper = coerce

-- Deeper isBottom for CPS.
-- See NOTE on deeper strictness above for details.
isBottomDeeper :: Bot (a, Bot w) -> Bool
isBottomDeeper (Bot p) = isBottom p || isBottom (snd p)

-- Deeper strictness in one argument.
-- The result (normalized to IO) should be bottom whenever (a, w) or w is bottom.
isStrictDeeperIn :: Bot (a, Bot w) -> o -> Property
isStrictDeeperIn p =
  let bottomP = isBottomDeeper p
  in label (bottomLabelFor "arg" bottomP)
    . shouldBeBottom bottomP

-- Deeper strictness in two arguments:
-- The result (normalized to IO) should be bottom whenever (a, w), (a', w'), w or w' is bottom.
isBiStrictDeeperIn :: Bot (a, Bot w) -> Bot (a', Bot w') -> o -> Property
isBiStrictDeeperIn p q  =
  let bottomP = isBottomDeeper p
      bottomQ = isBottomDeeper q
   in label (bottomLabelFor "arg1" bottomP <> ", " <> bottomLabelFor "arg2" bottomQ)
     . shouldBeBottom (bottomP || bottomQ)
