{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module WriterStrictness (test) where

import           Arbitrary

import           Control.Applicative
import           Control.Exception (ErrorCall (..))
import           Control.Exception.Base (displayException)
import           Control.Monad.Fix
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Zip

import           Data.Coerce
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Data.List (isPrefixOf)
import           Data.Monoid
import           Data.Tuple.Solo

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assertExceptionIO)
import           Test.Tasty
import           Test.Tasty.QuickCheck

test :: IO ()
test = defaultMain $ testGroup "Writer" strictnessTest

-- | NOTE: Monoid choice
--
-- For the tests, we use a monoid that is strict in both arguments of mappend :: a -> a -> a
--
-- This is needed in particular to test a strictness property of the CPS writer, which
-- is that it evaluates the log w strictly as it is accumulated.
-- If mappend is lazy in either argument, then there would be no observable signal of
-- strict evaluation in the result value, which would complicate the tests.
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
--  Writer.Lazy     lazy in the computed value (a,w)
--
--  Writer.Strict   strict in the computed value (a,w)
--
--  Writer.CPS      strict in the computed value (a,w) and log w
--
--
-- = Choice of the underlying monad
--
-- For the purpose of the test, we use a monad with the following properties:
-- a) strict in the monad bind i.e. a monad such that
--
--   x >>= k
--
-- is strict in x if k is strict and lazy if k is lazy.
--
-- b) lazy as a data type.
--
-- We employ Solo since it is a simple monad that has both properties.
--
-- The reasoning is as follows.
-- The distinguishing feature of the strict writer is that it is strict in the sequencing of computations.
-- To be precise, this means that when binding in the underlying monad, the map k used
-- is strict with the strict writer and lazy with the lazy writer.
-- The map k of the CPS writer is further strict in the log w of (a, w) >>= k.
-- Thus, in order to observe this effect in tests, we need to use a monad that preserves this property.
--
-- However, for the data type itself, we use one that is lazy.
-- This is because otherwise, it becomes impossible to distinguish between a bottom of the monad itself or the computed value.
-- e.g there is no difference between `Identity undefined` and `undefined`
-- Writer function that require a Monad are typically the former, whereas functions that require only a Functor or an Applicative are the latter,
-- since the caller has no control over the outer data constructor.
--
strictnessTest :: [TestTree]
strictnessTest = [
  testGroup "writer" [
      -- NOTE: Lazy and Strict are the same since there is no computational sequence involved.
      -- The output is exactly the same as the input.
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) ->
        isStrictIn p $ getSolo $ Lazy.runWriterT $ Lazy.writer @Solo (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) ->
        isStrictIn p $ getSolo $ Strict.runWriterT $ Strict.writer @Solo (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ CPS.writer @SumInt @Solo (unBotDeeper p)
  ],

  testGroup "execWriterT" [
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) ->
        let result = Lazy.execWriterT $ Lazy.WriterT $ MkSolo (unBotDeeper p)
         in isLazy result .&. isStrictDeeperIn p (getSolo result),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) ->
        let result = Strict.execWriterT $ Strict.WriterT $ MkSolo (unBotDeeper p)
         in isStrictIn p result .&. isStrictDeeperIn p (getSolo result),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.execWriterT $ CPS.writer @SumInt @Solo (unBotDeeper p)
  ],

  -- Lazy, Strict Writer    output of map is used directly without any intervention by the Writer itself
  --
  -- CPS Writer             output of map is evaluated in the log w
  testGroup "mapWriter" [
      testProperty "Lazy"   $ \(f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = unF1Bot f
            v = f' ((), mempty)
            result = Lazy.runWriterT $ Lazy.mapWriterT @Solo (f'<$>) $ pure ()
         -- Never bottoms in the outer constructor since the result of the map is used as is.
         in isLazy result .&. isStrictIn v (getSolo result),
      testProperty "Strict" $ \(f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = unF1Bot f
            v = f' ((), mempty)
            result = Strict.runWriterT $ Strict.mapWriterT @Solo (f'<$>) $ pure ()
         -- Never bottoms in the outer constructor since the result of the map is used as is.
         in isLazy result .&. isStrictIn v (getSolo result),
      testProperty "CPS"    $ \(f :: F1Bot ((), SumInt) ((), Bot SumInt)) ->
        let f' = coerce f :: ((), SumInt) -> ((), SumInt)
            v = f' ((), mempty)
        in isBiStrictIn v (snd v) $ CPS.runWriterT $ CPS.mapWriterT @Solo (f'<$>) $ pure ()
  ],

  testGroup "listen" [
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) -> isValueLazy getSolo $ Lazy.runWriterT $ Lazy.listen $ Lazy.WriterT $ MkSolo (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) -> isStrictIn p $ Strict.runWriterT $ Strict.listen $ Strict.WriterT $ MkSolo (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ CPS.listen $ CPS.writer @SumInt @Solo (unBotDeeper p)
  ],

  testGroup "listens" [
      testProperty "Lazy"   $ \(p :: Bot ((), Bot SumInt)) -> isValueLazy getSolo $ Lazy.runWriterT $ Lazy.listens id $ Lazy.WriterT $ MkSolo (unBotDeeper p),
      testProperty "Strict" $ \(p :: Bot ((), Bot SumInt)) -> isStrictIn p $ Strict.runWriterT $ Strict.listens id $ Strict.WriterT $ MkSolo (unBotDeeper p),
      testProperty "CPS"    $ \(p :: Bot ((), Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ CPS.listens id $ CPS.writer @SumInt @Solo (unBotDeeper p)
  ],

  testGroup "tell" [
      -- NOTE: Lazy and Strict here are both lazy since there is no computational sequence involved here.
      testProperty "Lazy"   $ \(Bot (w :: SumInt)) -> isValueLazy getSolo $ Lazy.runWriterT $ Lazy.tell @Solo w,
      testProperty "Strict" $ \(Bot (w :: SumInt)) -> isValueLazy getSolo $ Strict.runWriterT $ Strict.tell @Solo w,
      testProperty "CPS"    $ \(Bot (w :: SumInt)) -> isStrictIn w $ CPS.runWriterT $ CPS.tell @SumInt @Solo w
  ],

  -- NOTE: strictness in the log w for censor (CPS only)
  -- censor should be strict in the *final* value of the log w after applying the log censor (w -> w),
  -- not the initial value in the given writer.
  -- Thus, for the tests below, a bottom log value is tested in the output of the log censor f, instead of the log w in the initial (a, w).
  testGroup "censor" [
      testProperty "Lazy"   $ \(f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
        in isValueLazy getSolo $ Lazy.runWriterT $ Lazy.censor f' $ Lazy.WriterT $ MkSolo p,
      testProperty "Strict" $ \(f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
        in isStrictIn p $ Strict.runWriterT $ Strict.censor f' $ Strict.WriterT $ MkSolo p,
      testProperty "CPS"    $ \(f :: F1Bot SumInt SumInt) (Bot (p :: ((), SumInt))) ->
        let f' = coerce f :: SumInt -> SumInt
            result = f' mempty
        in isBiStrictIn p result $ CPS.runWriterT $ CPS.censor f' $ CPS.writer @SumInt @Solo p
  ],

  -- See note on censor above.
  testGroup "pass" [
      testProperty "Lazy"   $ \(p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
        in isValueLazy getSolo $ Lazy.runWriterT $ Lazy.pass $ Lazy.WriterT $ MkSolo p',
      testProperty "Strict" $ \(p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
        in isStrictIn p $ Strict.runWriterT $ Strict.pass $ Strict.WriterT $ MkSolo p',
      testProperty "CPS"    $ \(p :: Bot (((), F1Bot SumInt SumInt), SumInt)) ->
        let p' = coerce p :: (((), SumInt -> SumInt), SumInt)
            result = (snd $ fst p') mempty
        in isBiStrictIn p result $ CPS.runWriterT $ CPS.pass $ CPS.writer @SumInt @Solo p'
  ],

  -- == Functor/Applicative/Monad ==
  testGroup "Functor: fmap" [
      testProperty "Lazy"    $ \(p :: Bot (Int, Bot SumInt)) -> isValueLazy getSolo $ Lazy.runWriterT $ (+1) <$> Lazy.WriterT (MkSolo (unBotDeeper p)),
      -- Never bottoms in the outer constructor since Functor only exposes control over the inner value.
      testProperty "Strict"  $ \(p :: Bot (Int, Bot SumInt)) -> isStrictIn p $ getSolo $ Strict.runWriterT $ (+1) <$> Strict.WriterT (MkSolo (unBotDeeper p)),
      testProperty "CPS"     $ \(p :: Bot (Int, Bot SumInt)) -> isStrictDeeperIn p $ CPS.runWriterT $ (+1) <$> CPS.writer @SumInt @Solo (unBotDeeper p)
    ],

  testGroup "Applicative: <*>" [
      testProperty "Lazy"    $ \(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isValueLazy getSolo $ Lazy.runWriterT $ Lazy.writer @Solo f' <*> Lazy.writer(unBotDeeper p),
      testProperty "Strict"  $ \(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
          -- Never bottoms in the outer constructor since Applicative only exposes control over the inner value.
           in isBiStrictIn p f' $ getSolo $ Strict.runWriterT $ Strict.writer @Solo f' <*> Strict.writer(unBotDeeper p),
      testProperty "CPS"     $ \(wf :: Bot (F1 () (), Bot SumInt)) (p :: Bot ((), Bot SumInt)) ->
          let f' = coerce wf :: (() -> (), SumInt)
         in isBiStrictDeeperIn p wf $ CPS.runWriterT $ CPS.writer @SumInt @Solo f' <*> CPS.writer (unBotDeeper p)
    ],

  testGroup "Applicative: liftA2" [
      testProperty "Lazy"    $ \(p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isValueLazy getSolo $ Lazy.runWriterT $ liftA2 (+) (Lazy.writer @Solo $ unBotDeeper p) (Lazy.writer $ unBotDeeper q),
      testProperty "Strict"  $ \(p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          -- Never bottoms in the outer constructor since Applicative only exposes control over the inner value.
          isBiStrictIn p q $ getSolo $ Strict.runWriterT $ liftA2 (+) (Strict.writer @Solo $ unBotDeeper p) (Strict.writer $ unBotDeeper q),
      testProperty "CPS"     $ \(p :: Bot (Int, Bot SumInt)) (q :: Bot (Int, Bot SumInt)) ->
          isBiStrictDeeperIn p q $ CPS.runWriterT $ liftA2 (+) (CPS.writer @SumInt @Solo $ unBotDeeper p) (CPS.writer $ unBotDeeper q)
    ],

  testGroup "Monad: >>=" [
      testProperty "Lazy"    $ \(p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isValueLazy getSolo $ Lazy.runWriterT $ Lazy.writer @Solo (unBotDeeper p) >>= const (Lazy.writer $ unBotDeeper q),
      testProperty "Strict"  $ \(p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isBiStrictIn p q $ Strict.runWriterT $ Strict.writer @Solo (unBotDeeper p) >>= const (Strict.writer $ unBotDeeper q),
      testProperty "CPS"     $ \(p :: Bot ((), Bot SumInt)) (q :: Bot ((), Bot SumInt)) ->
          isBiStrictDeeperIn p q $ CPS.runWriterT $ CPS.writer @SumInt @Solo (unBotDeeper p) >>= const (CPS.writer (unBotDeeper q))
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
           isValueLazy getSolo $ Lazy.runWriterT $ mzipWith (+) (Lazy.WriterT (MkSolo p)) (Lazy.WriterT (MkSolo q)),
      testProperty "Strict"  $
        \(Bot (p :: (Int, SumInt)))
         (Bot (q :: (Int, SumInt))) ->
           let result = Strict.runWriterT $ mzipWith (+) (Strict.WriterT (MkSolo p)) (Strict.WriterT (MkSolo q))
            in isLazy result .&. isBiStrictIn p q (getSolo result)
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

  -- NOTE: sufficient to just validate that mfix terminates
  testGroup "MonadFix: mfix" [
      testProperty "Lazy"   $ \(p :: (Int, SumInt)) ->
        p === runIdentity (Lazy.runWriterT $ mfix (const $ Lazy.WriterT (Identity p))),
      testProperty "Strict" $ \(p :: (Int, SumInt)) ->
        p === runIdentity (Strict.runWriterT $ mfix (const $ Strict.WriterT (Identity p))),
      testProperty "CPS"    $ \(p :: (Int, SumInt)) ->
        p === runIdentity (CPS.runWriterT $ mfix (const $ CPS.writer @SumInt @Identity p))
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
-- The result (normalized to IO) should be bottom whenever (a, w), (a', w'), w, or w' is bottom.
isBiStrictDeeperIn :: Bot (a, Bot w) -> Bot (a', Bot w') -> o -> Property
isBiStrictDeeperIn p q  =
  let bottomP = isBottomDeeper p
      bottomQ = isBottomDeeper q
   in label (bottomLabelFor "arg1" bottomP <> ", " <> bottomLabelFor "arg2" bottomQ)
     . shouldBeBottom (bottomP || bottomQ)
