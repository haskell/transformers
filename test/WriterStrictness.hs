{-# LANGUAGE ScopedTypeVariables #-}

module WriterStrictness (test) where

import           Arbitrary

import           Control.Applicative
import           Control.Exception (ErrorCall (..), evaluate)
import           Control.Exception.Base (displayException)
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

-- | Monoid choice
--
-- For the test, we use a monoid that is strict in both arguments of mappend :: a -> a -> a
--
-- This is needed in particular to test a strictness property of the CPS writer, which
-- is that it evaluates the log w strictly as it is accumulated.
-- If mappend is lazy in an argument, then outwardly the result appears the same with or without evaluation
-- and hence cannot be validated.
-- e.g.
--   List mappend is only strict in the first argument:
--     writer ((), _|_) >> writer ((), "a")      _|_
--     writer ((), "a") >> writer ((), _|_)      ((), ('a':_|_))
--
--   Sum mappend is strict in both:
--     writer ((), Sum 0) >> writer ((), _|_)    _|_
--     writer ((), _|_) >> writer ((), Sum 0)    _|_
type SumInt = Sum Int

-- | Strictness test
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
  testGroup "Strictness" [
      testProperty "Lazy"   $ \m (p :: Bot ((), Bot SumInt)) -> isStrictIn p $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer (unBotDeeper p),
      testProperty "Strict" $ \m (p :: Bot ((), Bot SumInt)) -> isStrictIn p $ withBaseMonad m $ Strict.runWriterT $ Strict.writer (unBotDeeper p),
      testProperty "CPS"    $ \m (p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ withBaseMonad m $ CPS.runWriterT $ CPS.writer (unBotDeeper p)
  ],

  -- Lazy, Strict Writer    output of map is used directly without any intervention by the Writer itself
  --                        i.e. spine strict
  --
  -- CPS Writer             output of map is evaluated in the log w
  testGroup "mapWriter" [
      testProperty "Lazy"   $ \m (f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = unF1Bot f
            result = f' ((), mempty)
        in isStrictIn result $ withBaseMonad m $ Lazy.runWriterT $ Lazy.mapWriterT (f'<$>) $ pure (),
      testProperty "Strict" $ \m (f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = unF1Bot f
            result = f' ((), mempty)
        in isStrictIn result $ withBaseMonad m $ Strict.runWriterT $ Strict.mapWriterT (f'<$>) $ pure (),
      testProperty "CPS"    $ \m (f  :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = coerce f :: ((), SumInt) -> ((), SumInt)
            result = f' ((), mempty)
        in isBiStrictIn result (snd result) $ withBaseMonad m $ CPS.runWriterT $ CPS.mapWriterT (f'<$>) $ pure ()
  ],

  testGroup "listen" [
      testProperty "Lazy"   $ \m (p :: Bot ((), Bot SumInt)) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.listen $ Lazy.WriterT $ return (unBotDeeper p),
      testProperty "Strict" $ \m (p :: Bot ((), Bot SumInt)) -> isStrictIn p $ withBaseMonad m $ Strict.runWriterT $ Strict.listen $ Strict.WriterT $ return (unBotDeeper p),
      testProperty "CPS"    $ \m (p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ withBaseMonad m $ CPS.runWriterT $ CPS.listen $ CPS.writer (unBotDeeper p)
  ],

  testGroup "listens" [
      testProperty "Lazy"   $ \m (p :: Bot ((), Bot SumInt)) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.listens id $ Lazy.WriterT $ return (unBotDeeper p),
      testProperty "Strict" $ \m (p :: Bot ((), Bot SumInt)) -> isStrictIn p $ withBaseMonad m $ Strict.runWriterT $ Strict.listens id $ Strict.WriterT $ return (unBotDeeper p),
      testProperty "CPS"    $ \m (p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ withBaseMonad m $ CPS.runWriterT $ CPS.listens id $ CPS.writer (unBotDeeper p)
  ],

  testGroup "tell" [
      testProperty "Lazy"   $ \(m, Bot (w :: SumInt)) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.tell w,
      testProperty "Strict" $ \(m, Bot (w :: SumInt)) -> isLazy $ withBaseMonad m $ Strict.runWriterT $ Strict.tell w,
      testProperty "CPS"    $ \(m, Bot (w :: SumInt)) -> isStrictIn w $ withBaseMonad m $ CPS.runWriterT $ CPS.tell w
  ],

  -- There are 3 levels of strictness to test:
  -- 1) spine (a, w)
  -- 2) log w
  -- 3) log map w -> w
  --
  -- Lazy, Strict Writers are only concerned with 1.
  -- CPS Writer is strict in all.
  testGroup "censor" [
      testProperty "Lazy"   $ \m (f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
        in isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.censor f' $ Lazy.WriterT $ return p,
      testProperty "Strict" $ \m (f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
        in isStrictIn p $ withBaseMonad m $ Strict.runWriterT $ Strict.censor f' $ Strict.WriterT $ return p,
      testProperty "CPS"    $ \m (f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
            result = f' mempty
        in isBiStrictIn p result $ withBaseMonad m $ CPS.runWriterT $ CPS.censor f' $ CPS.writer p
  ],

  -- See note on censor above.
  testGroup "pass" [
      testProperty "Lazy"   $ \m (p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
        in isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.pass $ Lazy.WriterT $ return p',
      testProperty "Strict" $ \m (p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
        in isStrictIn p $ withBaseMonad m $ Strict.runWriterT $ Strict.pass $ Strict.WriterT $ return p',
      testProperty "CPS"    $ \m (p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
            result  = (snd $ fst p') mempty
        in isBiStrictIn p result $ withBaseMonad m $ CPS.runWriterT $ CPS.pass $ CPS.writer p'
  ],

  -- == Functor/Applicative/Monad ==
  testGroup "Functor: fmap" [
      testProperty "Lazy"    $ \m (p :: Bot (Int, Bot SumInt)) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ (+1) <$> Lazy.WriterT (return (unBotDeeper p)),
      testProperty "Strict"  $ \m (p :: Bot (Int, Bot SumInt)) -> isStrictIn p $ withBaseMonad m $ Strict.runWriterT $ (+1) <$> Strict.WriterT (return (unBotDeeper p)),
      testProperty "CPS"     $ \m (p :: Bot (Int, Bot SumInt)) -> isStrictDeeperIn p $ withBaseMonad m $ CPS.runWriterT $ (+1) <$> CPS.writer (unBotDeeper p)
    ],

  testGroup "Applicative: <*>" [
      testProperty "Lazy"    $ \m(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer f' <*> Lazy.writer (unBotDeeper p),
      testProperty "Strict"  $ \m(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isBiStrictIn p wf $ withBaseMonad m $ Strict.runWriterT $ Strict.writer f' <*> Strict.writer (unBotDeeper p),
      testProperty "CPS"     $ \m (wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isBiStrictDeeperIn p wf $ withBaseMonad m $ CPS.runWriterT $ CPS.writer f' <*> CPS.writer (unBotDeeper p)
    ],

  testGroup "Applicative: liftA2" [
      testProperty "Lazy"    $ \m (p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isLazy $ withBaseMonad m $ Lazy.runWriterT $ liftA2 (+) (Lazy.writer $ unBotDeeper p) (Lazy.writer $ unBotDeeper q),
      testProperty "Strict"  $ \m (p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isBiStrictIn p q $ withBaseMonad m $ Strict.runWriterT $ liftA2 (+) (Strict.writer $ unBotDeeper p) (Strict.writer $ unBotDeeper q),
      testProperty "CPS"     $ \m (p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isBiStrictDeeperIn p q $ withBaseMonad m $ CPS.runWriterT $ liftA2 (+) (CPS.writer $ unBotDeeper p) (CPS.writer $ unBotDeeper q)
    ],

  testGroup "Monad: >>=" [
      testProperty "Lazy"      $ \m (p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer (unBotDeeper p) >>= const (Lazy.writer (unBotDeeper q)),
      testProperty "Strict"      $ \m (p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isBiStrictIn p q $ withBaseMonad m $ Strict.runWriterT $ Strict.writer (unBotDeeper p) >>= const (Strict.writer (unBotDeeper q)),
      testProperty "CPS"         $ \m (p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isBiStrictDeeperIn p q $ withBaseMonad m $ CPS.runWriterT $ CPS.writer (unBotDeeper p) >>= const (CPS.writer (unBotDeeper q))
    ],


  -- == Other typeclasses ==
  testGroup "Foldable: foldMap" [
      -- NOTE: foldMap is lazy for both Writers, since it only involves the value `a` of Writer w m a.
      testProperty "Lazy"    $ \(p :: [Bot (Int, SumInt)]) ->
          let p' = unBot <$> p
          in isLazyValue $ getSum $ foldMap (const (Sum (0 :: Int))) (Lazy.WriterT p'),
      testProperty "Strict"  $ \(p :: [Bot (Int, SumInt)]) ->
          let p' = unBot <$> p
          in isLazyValue $ getSum $ foldMap (const (Sum (0 :: Int))) (Strict.WriterT p')

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
           isLazyValue $ evaluate $ Lazy.runWriterT $ mzipWith (+) (Lazy.WriterT (Identity p)) (Lazy.WriterT (Identity q)),
      testProperty "Strict"  $
        \(Bot (p :: (Int, SumInt)))
         (Bot (q :: (Int, SumInt))) ->
           isBiStrictIn p q $ evaluate $ Strict.runWriterT $ mzipWith (+) (Strict.WriterT (Identity p)) (Strict.WriterT (Identity q))
      -- NOTE: no MonadZip for CPS
  ],

  testGroup "Contravariant: contramap" [
      testProperty "Lazy"   $ \(Bot (p :: (Int, SumInt))) ->
        let f = getOp $ Lazy.runWriterT $ contramap (+1) $ Lazy.WriterT (Op id)
        in isLazyValue $ f p,
      testProperty "Strict" $ \(Bot (p :: (Int, SumInt))) ->
        let f = getOp $ Strict.runWriterT $ contramap (+1) $ Strict.WriterT (Op id)
        in isStrictValue p $ f p
      -- NOTE: no Contravariant for CPS
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
           isLazy $ withBaseMonad m $ Lazy.runWriterT $ do
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
           in shouldBeBottom expected $ withBaseMonad m $ Strict.runWriterT $ do
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
           in shouldBeBottom expected $ withBaseMonad m $ CPS.runWriterT $ do
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

isStrictValue :: arg1 -> a -> Property
isStrictValue x  = isStrictIn x . evaluate

isLazyValue :: a -> Property
isLazyValue = isLazy . evaluate

-- Never bottom
isLazy :: IO a -> Property
isLazy = shouldBeBottom False

-- Strictness in one argument:
-- The result (normalized to IO) should be bottom whenever arg1 is bottom.
isStrictIn :: arg1 -> IO a -> Property
isStrictIn x  =
  let bottomX = isBottom x
  in label (bottomLabelFor "arg" bottomX)
    . shouldBeBottom bottomX

-- Strictness in two arguments:
-- The result (normalized to IO) should be bottom whenever arg1 OR arg2 is bottom.
isBiStrictIn :: arg1 -> arg2 -> IO o -> Property
isBiStrictIn x y  =
    let bottomX = isBottom x
        bottomY = isBottom y
   in label (bottomLabelFor "arg1" bottomX <> ", " <> bottomLabelFor "arg2" bottomY)
    . shouldBeBottom (bottomX || bottomY)

shouldBeBottom :: Bool -> IO o -> Property
shouldBeBottom expectBottom result = classify expectBottom bottomLabel $
  if expectBottom
    then assertExceptionIO isBottomError result
    else ioProperty $ do
      v <- result
      v `seq` return ()
  where
    -- Check error message only i.e. prefix, ignoring stacktrace.
    isBottomError :: ErrorCall -> Bool
    isBottomError e = "<bottom>" `isPrefixOf` displayException e

-- unBot for CPS which is strict in both the spine (a,w) and log w.
unBotDeeper :: Bot (a, Bot w) -> (a, w)
unBotDeeper = coerce

-- isBottom for CPS, which is strict in both the spine (a,w) and log w.
isBottomDeeper :: Bot (a, Bot w) -> Bool
isBottomDeeper (Bot p) = isBottom p || isBottom (snd p)

-- isStrictIn analogue for deeper (CPS).
isStrictDeeperIn :: Bot (a, Bot w) -> IO o -> Property
isStrictDeeperIn p =
  let bottomP = isBottomDeeper p
  in label (bottomLabelFor "arg" bottomP)
    . shouldBeBottom bottomP

isBiStrictDeeperIn :: Bot (a, Bot w) -> Bot (a', Bot w') -> IO o -> Property
isBiStrictDeeperIn p q  =
  let bottomP = isBottomDeeper p
      bottomQ = isBottomDeeper q
   in label (bottomLabelFor "arg1" bottomP <> ", " <> bottomLabelFor "arg2" bottomQ)
     . shouldBeBottom (bottomP || bottomQ)
