{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module StateStrictness (test) where

import           Arbitrary

import           Control.Applicative
import           Control.Monad.Fix (mfix)
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Writer

import           Data.Coerce (coerce)
import           Data.Functor.Contravariant
import           Data.Functor.Identity (Identity (..))
import           Data.Tuple.Solo

import           StrictnessCheck

import           Test.ChasingBottoms (isBottom)
import           Test.Tasty
import           Test.Tasty.QuickCheck

test :: TestTree
test = testGroup "State" strictnessTest

-- | NOTE: Strictness test
--
-- See note in WriterStrictness.hs regarding the choice of Solo as the monad used for testing.
--
strictnessTest :: [TestTree]
strictnessTest = [
  -- NOTE: Lazy and Strict are the same since there is no computation involved.
  -- i.e. just returns the input.
  testGroup "evalStateT" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Lazy.evalStateT () $ Lazy.StateT (MkSolo . x)
       in isLazy result .&. isStrictIn (x ()) (getSolo result),
    testProperty "Strict" $ \(F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Strict.evalStateT () $ Strict.StateT (MkSolo . x)
       in isLazy result .&. isStrictIn (x ()) (getSolo result)
  ],
  testGroup "execStateT" [
    -- NOTE: See note on evalStateT.
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Lazy.execStateT () $ Lazy.StateT (MkSolo . x)
       in isLazy result .&. isStrictIn (x ()) (getSolo result),
    testProperty "Strict" $ \(F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Strict.execStateT () $ Strict.StateT (MkSolo . x)
       in isLazy result .&. isStrictIn (x ()) (getSolo result)
  ],

  testGroup "withStateT" [
    -- NOTE: f is not evaluated strictly, so the result depends only on the original input.
    testProperty "Lazy"   $ \(F1Bot (f :: () -> ())) (F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Lazy.runStateT () $ Lazy.withStateT f (Lazy.StateT (MkSolo . x))
       in isLazy result .&. isStrictIn (x ()) (getSolo result),
    testProperty "Strict" $ \(F1Bot (f :: () -> ())) (F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Strict.runStateT () $ Strict.withStateT f (Strict.StateT (MkSolo . x))
       in isLazy result .&. isStrictIn (x ()) (getSolo result)
  ],

  testGroup "mapStateT" [
    -- NOTE: f is applied directly on the underlying monad without any intervention by the State, so the result
    -- depends only on the result of f.
    testProperty "Lazy"   $ \(F1Bot (f :: (Int, ()) -> (Int, ()))) ->
      let result = flip Lazy.runStateT () $ Lazy.mapStateT (f <$>) (Lazy.StateT (MkSolo . const (0, ())))
       in isLazy result .&. isStrictIn (f (0, ())) (getSolo result),
    testProperty "Strict" $ \(F1Bot (f :: (Int, ()) -> (Int, ())) ) ->
      let result = flip Strict.runStateT () $ Strict.mapStateT (f <$>) (Strict.StateT (MkSolo . const (0, ())))
       in isLazy result .&. isStrictIn (f (0, ())) (getSolo result)
  ],

  testGroup "put" [
    -- NOTE: lazy since there is no computation involved.
    testProperty "Lazy"   $ \(Bot (x :: Int)) ->
      isValueLazy getSolo $ flip Lazy.runStateT 0 $ Lazy.put x,
    testProperty "Strict" $ \(Bot (x :: Int)) ->
      isValueLazy getSolo $ flip Strict.runStateT 0 $ Strict.put x
  ],

  -- NOTE: modify and get variants, with the exception of modify', 
  -- only act on the state s of the State's context (a, s).
  -- Hence, the mapping is evaluated lazily.
  testGroup "modify" [
    testProperty "Lazy"   $ \(F1Bot (f :: Int -> Int)) ->
      isValueLazy getSolo $ flip Lazy.runStateT 0 $ Lazy.modify f,
    testProperty "Strict" $ \(F1Bot (f :: Int -> Int)) ->
      isValueLazy getSolo $ flip Strict.runStateT 0 $ Strict.modify f
  ],
  testGroup "modify'" [
    testProperty "Lazy"   $ \(F1Bot (f :: Int -> Int)) ->
      isStrictIn (f 0) $ flip Lazy.runStateT 0 $ Lazy.modify' @Solo f,
    testProperty "Strict" $ \(F1Bot (f :: Int -> Int)) ->
      isStrictIn (f 0) $ flip Strict.runStateT 0 $ Strict.modify' @Solo f
  ],
  testGroup "modifyM" [
    testProperty "Lazy"   $ \(F1Bot (f :: Int -> Int)) ->
      isValueLazy getSolo $ flip Lazy.runStateT 0 $ Lazy.modifyM $ MkSolo . f,
    testProperty "Strict" $ \(F1Bot (f :: Int -> Int)) ->
      isValueLazy getSolo $ flip Strict.runStateT 0 $ Strict.modifyM $ MkSolo . f
  ],
  testGroup "gets" [
    testProperty "Lazy"   $ \(F1Bot (f :: Int -> ())) ->
      isValueLazy getSolo $ flip Lazy.runStateT 0 $ Lazy.gets f,
    testProperty "Strict" $ \(F1Bot (f :: Int -> ())) ->
      isValueLazy getSolo $ flip Strict.runStateT 0 $ Strict.gets f
  ],

  -- == Functor/Applicative/Monad ==
  testGroup "Functor: fmap" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) ->
      isValueLazy getSolo $ flip Lazy.runStateT () $  (+1) <$> Lazy.StateT (MkSolo. x),
    testProperty "Strict" $ \(F1Bot (x :: () -> (Int, ()))) ->
      let result = flip Strict.runStateT () $ (+1) <$> Strict.StateT (MkSolo . x)
       -- Does not bottom in the outer constructor since Functor only exposes control over the inner value.
       in isLazy result .&. isStrictIn (x ()) (getSolo result)
  ],

  testGroup "Applicative: <*>" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) (F1Bot (y :: () -> (F1 Int Int, ()))) ->
      let f' = coerce y :: () -> (Int -> Int, ())
       in isValueLazy getSolo $ flip Lazy.runStateT () $ Lazy.StateT (MkSolo . f') <*> Lazy.StateT (MkSolo . x),
    testProperty "Strict" $ \(F1Bot (x :: () -> (Int, ()))) (F1Bot (y :: () -> (F1 Int Int, ()))) ->
      let f' = coerce y :: () -> (Int -> Int, ())
          result = flip Strict.runStateT () $ Strict.StateT (MkSolo . f') <*> Strict.StateT (MkSolo . x)
       in isBiStrictIn (x ()) (f' ()) result
  ],
  testGroup "Applicative: liftA2" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) (F1Bot (y :: () -> (Int, ()))) ->
      isValueLazy getSolo $ Lazy.runStateT (liftA2 (+) (Lazy.StateT $ MkSolo . x)  (Lazy.StateT $ MkSolo . y)) (),
    testProperty "Strict" $ \(F1Bot (x :: () -> (Int, ()))) (F1Bot (q :: () -> (Int, ()))) ->
      isBiStrictIn (x ()) (q ()) $ Strict.runStateT (liftA2 (+) (Strict.StateT $ MkSolo . x)  (Strict.StateT $ MkSolo . q)) ()
  ],

  testGroup "Monad: >>=" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) (F1Bot (k :: () -> (Int, ()))) ->
      let result = flip Lazy.runStateT () $ Lazy.StateT (MkSolo . x) >>= const (Lazy.StateT (MkSolo . k))
       in isLazy result .&. isStrictIn (k ()) (getSolo result),
    testProperty "Strict" $ \(F1Bot (x :: () -> (Int, ()))) (F1Bot (k :: () -> (Int, ()))) ->
      let result = flip Strict.runStateT () $ Strict.StateT (MkSolo . x) >>= const (Strict.StateT (MkSolo . k))
       in isStrictIn (x ()) result .&. isBiStrictIn (x ()) (k ()) (getSolo result)
  ],


  -- == Other typeclasses ==
  testGroup "Contravariant: contramap" [
    testProperty "Lazy"   $ \(Bot (x :: (Int, ()))) ->
      let f = getOp $ flip Lazy.runStateT () $ contramap (+1) $ Lazy.StateT (const $ Op id)
      in isLazy $ f x,
    testProperty "Strict" $ \(Bot (x :: (Int, ()))) ->
      let f = getOp $ flip Strict.runStateT () $ contramap (+1) $ Strict.StateT (const $ Op id)
      in isStrictIn x $ f x
  ],

  -- NOTE: sufficient to just validate that mfix terminates
  testGroup "MonadFix: mfix" [
    testProperty "Lazy"   $ \(x :: F1 () (Int, ())) ->
      let x' = unF1 x
       in x' () === runIdentity (Lazy.runStateT (mfix (const (Lazy.StateT $ Identity . x'))) ()),
    testProperty "Strict" $ \(x :: F1 () (Int, ())) ->
      let x' = unF1 x
       in x' () === runIdentity (Strict.runStateT (mfix (const (Strict.StateT $ Identity . x'))) ())
  ],


  -- == Lift ==
  testGroup "liftListen" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> (Int, ()))) ->
      let listen = Lazy.liftListen $ Writer.listen @Solo
          value = Lazy.StateT (Writer.writer . (,"a") <$> x)
          result = Writer.runWriterT $ Lazy.runStateT (listen value) ()
       in isValueLazy getSolo result,
    testProperty "Strict" $ \(F1Bot (x :: () -> ((Int, ()), ()))) ->
      let listen = Strict.liftListen $ Writer.listen @Solo
          value = Strict.StateT (Writer.writer . (,"a") <$> x)
          result = Writer.runWriterT $ Strict.runStateT (listen value) ()
       in isStrictIn (x ()) result
  ],

  testGroup "liftPass" [
    testProperty "Lazy"   $ \(F1Bot (x :: () -> ((Int, String -> String), ()))) ->
      let pass = Lazy.liftPass $ Writer.pass @Solo
          value = pass $ Lazy.StateT (Writer.writer . (,"a") <$> x)
          result = Writer.runWriterT $ Lazy.runStateT value ()
       in isValueLazy getSolo result,
    testProperty "Strict" $ \(F1Bot (x :: () -> ((Int, String -> String), ()))) ->
      let pass = Strict.liftPass $ Writer.pass @Solo
          value = pass $ Strict.StateT (Writer.writer . (,"a") <$> x)
          result = Writer.runWriterT $ Strict.runStateT value ()
       in isStrictIn (x ()) result
  ],


  testGroup "combination" [
    testProperty "Lazy" $
      \m
      (F1Bot (x :: Int -> (String, Int)))
      (F1Bot (f :: Int -> Int))
      (Bot (s :: String))
      (Bot (t :: Int)) ->
        -- In general, the computations are lazy since this is the Lazy State.
        --
        -- However, one exception is modify', which enforces strictness on the state value.
        -- Thus, modify' will bottom for monads which are strict in the underlying sequencing.
        -- i.e. Solo and IO
        --
        -- In particular, note that Identity is lazy whereas Solo is strict:
        --
        --   m >>= k    = k (runIdentity m)     lazy runIdentity
        --   MkSolo x >>= k = k x               strict pattern match
        --
        let expected = (baseMonadName m == "Solo" || baseMonadName m == "IO") && isBottom (f (snd (x 0)))
         in shouldBeBottomIO expected $ withBaseMonad m $ flip Lazy.runStateT 0 $ do
          p <- Lazy.StateT $ return . x
          q <- Lazy.get

          Lazy.modify' f -- may throw depending on the underlying monad. See above.

          Lazy.put $ q + length (s <> p)
          Lazy.gets (+ t),

    testProperty "Strict" $
      \m
      (F1Bot (x :: Int -> (String, Int)))
      (F1Bot (f :: Int -> Int))
      (Bot (s :: String))
      (Bot (t :: Int)) ->
        let expected = isBottom (x 0) || isBottom (f 0)
         in shouldBeBottomIO expected $ withBaseMonad m $ flip Strict.runStateT 0 $ do
          p <- Strict.StateT $ return . x
          q <- Strict.get
          Strict.modify' f
          Strict.put $ q + length (s <> p)
          Strict.gets (+ t)
  ]
 ]
