{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module WriterStrictness (test) where

import           Arbitrary

import           Control.Applicative
import           Control.Arrow (Arrow (second), first)
import           Control.Exception (ErrorCall (..), evaluate)
import           Control.Exception.Base (displayException)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont (ContT (..), callCC, runContT)
import           Control.Monad.Trans.Reader (ReaderT (..), ask, reader)
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
import           Test.QuickCheck.Monadic (assertExceptionIO, monadicIO, run)
import           Test.Tasty
import           Test.Tasty.QuickCheck

test :: IO ()
test = defaultMain $ testGroup "Writer" strictSpineTest

-- | Strictness test
--
--  TODO: documentation
--
--  TODO: CPS
--
strictSpineTest :: [TestTree]
strictSpineTest = [
  testGroup "writer" [
      testProperty "Lazy"   $ \m (Bot (p :: (Int, String))) -> isStrict p $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer p,
      testProperty "Strict" $ \m (Bot (p :: (Int, String))) -> isStrict p $ withBaseMonad m $ Strict.runWriterT $ Strict.writer p,
      testProperty "CPS"    $ \m (p :: Bot (Int, Bot String)) -> isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ CPS.writer (unBotDeep p)
  ],

 -- TODO: 
  testGroup "mapWriter" [
      testProperty "Lazy - lazy map"    $ \m (Bot (p :: (Int, String))) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.mapWriterT idLazy $ Lazy.WriterT $ return p,
      testProperty "Lazy - strict map"  $ \m (Bot (p :: (Int, String))) -> isStrict p $ withBaseMonad m $ Lazy.runWriterT $ Lazy.mapWriterT id $ Lazy.WriterT $ return p,

      testProperty "Strict - lazy map"  $ \m (Bot (p :: (Int, String))) -> isLazy $ withBaseMonad m $ Strict.runWriterT $ Strict.mapWriterT idLazy $ Strict.WriterT $ return p,
      testProperty "Strict - strict map"$ \m (Bot (p :: (Int, String))) -> isStrict p $ withBaseMonad m $ Strict.runWriterT $ Strict.mapWriterT id $ Strict.WriterT $ return p,

      testProperty "CPS - lazy map"     $ \m (p :: Bot (Int, Bot String)) -> isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ CPS.mapWriterT idLazy $ CPS.writer (unBotDeep p),
      testProperty "CPS - strict map"   $ \m (p :: Bot (Int, Bot String)) -> isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ CPS.mapWriterT id $ CPS.writer (unBotDeep p)
  ],

  testGroup "listen" [
      testProperty "Lazy"   $ \m (Bot (p :: (Float, String))) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.listen $ Lazy.WriterT $ return p,
      testProperty "Strict" $ \m (Bot (p :: (Float, String))) -> isStrict p $ withBaseMonad m $ Strict.runWriterT $ Strict.listen $ Strict.WriterT $ return p,
      testProperty "CPS"    $ \m (p :: Bot (Float, Bot String)) -> isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ CPS.listen $ CPS.writer (unBotDeep p)
  ],

  testGroup "listens" [
      testProperty "Lazy"   $ \m (Bot (p :: (Float, String))) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.listens id $ Lazy.writer p,
      testProperty "Strict" $ \m (Bot (p :: (Float, String))) -> isStrict p $ withBaseMonad m $ Strict.runWriterT $ Strict.listens id $ Strict.writer p,
      testProperty "CPS"    $ \m (p :: Bot (Float, Bot String)) -> isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ CPS.listens id $ CPS.writer (unBotDeep p)
  ],

  testGroup "tell" [
      testProperty "Lazy"   $ \(m, Bot (w :: String)) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.tell w,
      testProperty "Strict" $ \(m, Bot (w :: String)) -> isLazy $ withBaseMonad m $ Strict.runWriterT $ Strict.tell w,
      testProperty "CPS"    $ \(m, Bot (w :: String)) -> isStrict w $ withBaseMonad m $ CPS.runWriterT $ CPS.tell w
  ],

  -- TODO: revisit function
  testGroup "pass" [
      testProperty "Lazy"   $ \m (p :: Bot ((), Bot String)) ->
        let p' = p `seq` second (<>) $ unBotDeep p
        in isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.pass $ return p',
      testProperty "Strict" $ \m (p :: Bot ((), Bot String)) ->
        let p' = p `seq` second (<>) $ unBotDeep p
        in isStrict p' $ withBaseMonad m $ Strict.runWriterT $ Strict.pass $ return p',
      testProperty "CPS"    $ \m (p :: Bot ((), Bot String)) ->
        let p' = p `seq` second (<>) $ unBotDeep p
        in isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ CPS.pass $ return p'
  ],

  -- NOTE: TODO note on Sum Int
  testGroup "censor" [
      testProperty "Lazy"   $ \m (Bot (w :: Sum Int)) (Bot (p :: ((), Sum Int))) ->
        isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.censor (<> w) $ Lazy.WriterT $ return p,
      testProperty "Strict" $ \m (Bot (w :: Sum Int)) (Bot (p :: ((), Sum Int))) ->
        isStrict p $ withBaseMonad m $ Strict.runWriterT $ Strict.censor (<> w) $ Strict.WriterT $ return p,
      testProperty "CPS"    $ \m (Bot (w :: Sum Int)) (p :: Bot ((), Bot (Sum Int))) ->
        isDeepStrict1 p w $ withBaseMonad m $ CPS.runWriterT $ CPS.censor (<> w) $ CPS.writer (unBotDeep p)
  ],


  -- == Functor/Applicative/Monad ==
  testGroup "Functor: fmap" [
      testProperty "Lazy"    $ \m (Bot (p :: (Int, String))) -> isLazy $ withBaseMonad m $ Lazy.runWriterT $ (+1) <$> Lazy.WriterT (return p),
      testProperty "Strict"  $ \m (Bot (p :: (Int, String))) -> isStrict p $ withBaseMonad m $ Strict.runWriterT $ (+1) <$> Strict.WriterT (return p),
      testProperty "CPS"     $ \m (p :: Bot (Int, Bot String)) -> isDeepStrict p $ withBaseMonad m $ CPS.runWriterT $ (+1) <$> CPS.writer (unBotDeep p)
    ],

  testGroup "Applicative: <*>" [
      testProperty "Lazy"    $ \m (Bot (p :: ((), String))) (Bot (wf :: (F1 () (), String))) ->
          let f' = coerce wf :: (() -> (), String)
         in isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer f' <*> Lazy.writer p ,
      testProperty "Strict"  $ \m (Bot (p :: ((), String))) (Bot (wf :: (F1 () (), String))) ->
          let f' = coerce wf :: (() -> (), String)
         in isStrict2 p wf $ withBaseMonad m $ Strict.runWriterT $ Strict.writer f' <*> Strict.writer p,
      testProperty "CPS"     $ \m (Bot (p :: ((), String))) (Bot (wf :: (F1 () (), String))) ->
          let f' = coerce wf :: (() -> (), String)
         in isStrict2 p wf $ withBaseMonad m $ CPS.runWriterT $ CPS.writer f' <*> CPS.writer p
    ],

  -- NOTE: TODO note on Sum Int
  testGroup "Applicative: liftA2" [
      testProperty "Lazy"    $ \m (Bot (p :: (Int, Sum Int))) (Bot (q :: (Int, Sum Int))) ->
          isLazy $ withBaseMonad m $ Lazy.runWriterT $ liftA2 (+) (Lazy.writer p) (Lazy.writer q),
      testProperty "Strict"  $ \m (Bot (p :: (Int, Sum Int))) (Bot (q :: (Int, Sum Int))) ->
          isStrict2 p q $ withBaseMonad m $ Strict.runWriterT $ liftA2 (+) (Strict.writer p) (Strict.writer q),
      testProperty "CPS"     $ \m (p :: Bot (Int, Bot (Sum Int))) (q :: Bot (Int, Bot (Sum Int))) ->
          isDeepStrict2 p q $ withBaseMonad m $ CPS.runWriterT $ liftA2 (+) (CPS.writer $ unBotDeep p) (CPS.writer $ unBotDeep q)
    ],

  -- NOTE: TODO note on Sum Int
  testGroup "Monad: >>=" [
      testProperty "Lazy"        $ \m (Bot (p :: ((), Sum Int))) (Bot (q :: (Int, Sum Int))) ->
          isLazy $ withBaseMonad m $ Lazy.runWriterT $ Lazy.writer p >>= const (Lazy.writer q),
      testProperty "Strict"      $ \m (Bot (p :: ((), Sum Int))) (Bot (q :: (Int, Sum Int))) ->
          isStrict2 p q $ withBaseMonad m $ Strict.runWriterT $ Strict.writer p >>= const (Strict.writer q),
      testProperty "CPS"         $ \m (p :: Bot ((), Bot (Sum Int))) (q :: Bot (Int, Bot (Sum Int))) ->
          isDeepStrict2 p q $ withBaseMonad m $ CPS.runWriterT $ CPS.writer (unBotDeep p) >>= const (CPS.writer (unBotDeep q))
    ],

  -- TODO
  testGroup "Alternative: <|>" [
      testProperty "Lazy"      $ \(p :: [Bot (Int, String)]) (q :: [Bot (Int, String)]) ->
          let p' = unBot <$> p
              q' = unBot <$> q
           -- same as underlying alternative, so non-strict
           in (isBottom <$> Lazy.runWriterT (Lazy.WriterT p' <|> Lazy.WriterT q')) === (isBottom <$> p' ++ q'),
      testProperty "Strict"   $ \(p :: [Bot (Int, String)]) (q :: [Bot (Int, String)]) ->
          let p' = unBot <$> p 
              q' = unBot <$> q
           in (isBottom <$> Strict.runWriterT (Strict.WriterT p' <|> Strict.WriterT q')) === (isBottom <$> p' ++ q'),
      testProperty "CPS"      $ \(p :: [Bot (Int, Bot String)]) (q :: [Bot (Int, Bot String)]) ->
          let p' = unBotDeep <$> p
              q' = unBotDeep <$> q
           in (isBottom <$> CPS.runWriterT (CPS.writerT p' <|> CPS.writerT q')) === (isBottomDeep <$> p ++ q)
    ],

  -- TODO
  testGroup "MonadPlus: mplus" [
      testProperty "Lazy"      $ \(p :: [Bot (Int, String)]) (q :: [Bot (Int, String)]) ->
          let p' = unBot <$> p
              q' = unBot <$> q
           -- same as underlying alternative, so non-strict
           in (isBottom <$> Lazy.runWriterT (Lazy.WriterT p' `mplus` Lazy.WriterT q')) === (isBottom <$> p' ++ q'),
      testProperty "Strict"   $ \(p :: [Bot (Int, String)]) (q :: [Bot (Int, String)]) ->
          let p' = unBot <$> p
              q' = unBot <$> q 
           in (isBottom <$> Strict.runWriterT (Strict.WriterT p' `mplus` Strict.WriterT q')) === (isBottom <$> p' ++ q'),
      testProperty "CPS"      $ \(p :: [Bot (Int, Bot String)]) (q :: [Bot (Int, Bot String)]) ->
          let p' = unBotDeep <$> p
              q' = unBotDeep <$> q
           in (isBottom <$> CPS.runWriterT (CPS.writerT p' `mplus` CPS.writerT q')) === (isBottomDeep <$> p ++ q)
    ],

  testGroup "MonadFix: >>=" [
      -- NOTE: implementation of any fixed point functions must be lazy, so
      -- mfix is lazy for all Writer types.
      testProperty "Lazy" $ \(Bot (w :: String)) ->
          isStrict w $ Lazy.runWriterT $ mfix (\x -> Lazy.writer $ w `seq` (w, x)),
      testProperty "Strict" $ \(Bot (w :: String)) ->
          isStrict w $ Strict.runWriterT $ mfix (\x -> Strict.writer $ w `seq` (w, x)),

      -- TODO:
      testProperty "CPS"    $ \(p :: Bot (Int, Bot String)) ->
          isDeepStrict p $ CPS.runWriterT $ mfix (\x -> CPS.writer $ p `seq` first (x +) (unBotDeep p))
    ],

  -- TODO
  testGroup "MonadStack" [
    testProperty "Lazy" $
       \(Bot (p :: (Int, String)))
         (Bot (q :: (Int, String)))
         ->
            let
                result :: TestMonadStack String (Lazy.WriterT String IO) Int
                result = do
                  q' <- lift $ lift $ Lazy.WriterT $ return q
                  a <- lift ask
                  b <- callCC $ \next -> do
                    when (even a) $ next (10 :: Int)
                    return 20
                  p' <- lift $ lift $ Lazy.WriterT $ return p
                  lift $ lift $ Lazy.tell "hello"
                  return $ q' + a + p' + b
                writer = Lazy.censor (filter (/= 'l')) $ runTestMonadStack result show
             in isLazy $ Lazy.runWriterT writer,
    testProperty "Strict" $
       \(Bot (p :: (Int, String)))
         (Bot (q :: (Int, String)))
         ->
            let
                result :: TestMonadStack String (Strict.WriterT String IO) Int
                result = do
                  q' <- lift $ lift $ Strict.WriterT $ return q
                  a <- lift ask
                  b <- callCC $ \next -> do
                    when (even a) $ next (10 :: Int)
                    return 20
                  p' <- lift $ lift $ Strict.WriterT $ return p
                  lift $ lift $ Strict.tell "hello"
                  return $ q' + a + p' + b
                writer = Strict.censor (filter (/= 'l')) $ runTestMonadStack result show
             in isStrict2 p q $ Strict.runWriterT writer
  ],


  -- == Other type classes ==
  testGroup "Foldable: foldMap" [
      -- NOTE: foldMap is lazy in both Lazy and Strict Writers due to the use of fst to pick the value.
      testProperty "Lazy"    $ \(p :: [Bot (Int, String)]) ->
          let p' = unBot <$> p in isLazyValue $ getSum $ foldMap (const (Sum (0 :: Int))) (Lazy.WriterT p'),
      testProperty "Strict"  $ \(p :: [Bot (Int, String)]) ->
          let p' = unBot <$> p in isLazyValue $ getSum $ foldMap (const (Sum (0 :: Int))) (Strict.WriterT p')
      -- NOTE: no Foldable for CPS
  ],

  testGroup "Traversable: traverse" [
      -- NOTE: traverse
      testProperty "Lazy"    $ \(p :: [Bot (Int, String)]) ->
          let p' = unBot <$> p in isStrictAny p' $ evaluate $ traverse Just (Lazy.WriterT p'),
      testProperty "Strict"  $ \(p :: [Bot (Int, String)]) ->
          let p' = unBot <$> p in isStrictAny p' $ evaluate $ traverse Just (Strict.WriterT p')
      -- NOTE: no Traversable for CPS
  ],

  testGroup "MonadZip: mzipWith" [
      testProperty "Lazy"  $
        \(Bot (p :: (Int, String)))
         (Bot (q :: (Int, String))) ->
            isLazyValue $ evaluate $ Lazy.runWriterT $ mzipWith (+) (Lazy.WriterT (Identity p)) (Lazy.WriterT (Identity q)),
      testProperty "Strict"  $
        \(Bot (p :: (Int, String)))
         (Bot (q :: (Int, String))) ->
           isStrict2 p q $ evaluate $ Strict.runWriterT $ mzipWith (+) (Strict.WriterT (Identity p)) (Strict.WriterT (Identity q))
      -- NOTE: no MonadZip for CPS
  ],

  testGroup "Contravariant: contramap" [
      testProperty "Lazy"   $ \(Bot (p :: (Int, String))) -> 
        let f = getOp $ Lazy.runWriterT $ contramap (+1) $ Lazy.WriterT (Op id) in isLazyValue $ f p,
      testProperty "Strict" $ \(Bot (p :: (Int, String))) -> 
        let f = getOp $ Strict.runWriterT $ contramap (+1) $ Strict.WriterT (Op id) in isStrictValue p $ f p
      -- NOTE: no MonadZip for CPS
  ],

  -- TODO
  testGroup "combination" [
     testProperty "Lazy" $
       \ m
         (Bot (t :: (Int, String)))
         (Bot (u :: (Int, String)))
         (Bot (v :: (Int, String)))
         (Bot (w :: (Int, String))) ->
           isLazy $ withBaseMonad m $ Lazy.runWriterT $ do
                a <- Lazy.WriterT (return t)
                (b, x) <- Lazy.listen $ Lazy.WriterT $ return u
                Lazy.tell x
                c <- Lazy.censor (drop 0) $ Lazy.WriterT $ return v
                d <- Lazy.mapWriterT id $ Lazy.WriterT $ return w
                return $ a + b + c + d,
     testProperty "Strict" $
       \ m
         (Bot (t :: (Int, String)))
         (Bot (u :: (Int, String)))
         (Bot (v :: (Int, String)))
         (Bot (w :: (Int, String)))
         (s :: String) ->
           let expected = isBottom t || isBottom u || isBottom v || isBottom w
           in shouldBeBottom expected $ withBaseMonad m $ Strict.runWriterT $ do
                a <- Strict.WriterT (return t)
                (b, x) <- Strict.listen $ Strict.WriterT $ return u
                Strict.tell (s ++ x)
                c <- Strict.censor (drop 0) $ Strict.WriterT $ return v
                d <- Strict.mapWriterT id $ Strict.WriterT $ return w
                return $ a + b + c + d,
     testProperty "CPS" $
       \ m
         (t :: Bot ((), Bot (Sum Int)))
         (u :: Bot ((), Bot (Sum Int)))
         (v :: Bot ((), Bot (Sum Int)))
         (w :: Bot ((), Bot (Sum Int)))
         (Bot (s :: (Sum Int))) ->
           let expected = isBottomDeep t || isBottomDeep u || isBottomDeep v || isBottomDeep w || isBottom s
               result =  withBaseMonad m $ CPS.runWriterT $ do
                a <- CPS.writer $ unBotDeep t
                (b, x) <- CPS.listen $ CPS.writer $ unBotDeep u
                c <- CPS.censor id $ CPS.writer $ unBotDeep v
                d <- CPS.mapWriterT id $ CPS.writer $ unBotDeep w
                -- CPS.tell $ show $ a <> b <> c <> d
                CPS.tell $ x <> s
                return $ a <> b <> c <> d
           in shouldBeBottom expected result
  ]
  ]
  where
    idLazy :: Functor f => f (a, w) -> f (a, w)
    idLazy = ((\ ~(a, w) -> (a, w)) <$>)

-- -- | Strictness tests for the value (a, w) of Writers
-- --
-- -- Test whether the pair (a, w) is handled lazily/strictly
-- -- in Writer functions as expected.
-- -- In particular, whether pairs patterns are handled correctly
-- -- in the respective Writers.
-- --
-- -- Please note that tests DO NOT test strictness in the log w
-- -- of the CPS writer.
-- --
-- strictPairTests :: [TestTree]
-- strictPairTests =
--     testStrictPairLifts
--       "Lazy"
--       lazyBaseF
--       lazyWriterLifts
--       StrictPairLiftsExpectations
--         { expect_bot_liftCatch = id  -- NOTE: same as strict since the value is just passed through.
--         }
--     ++ testStrictPairLifts
--       "Strict"
--       strictBaseF
--       strictWriterLifts
--       StrictPairLiftsExpectations
--         { expect_bot_liftCatch = id
--         }
--     ++ testStrictPairLifts
--       "CPS"
--       cpsBase
--       cpsWriterLifts
--       StrictPairLiftsExpectations
--         { expect_bot_liftCatch = id
--         }

-- | Value (a, w) strictness tests for the core Writer methods.
--
-- Functions that do not involve handling pairs are not tested:
-- * tell
-- testStrictPairMethods ::
--   forall writer m.
--   (Monad m, Monad (writer String m)) =>
--   String ->
--   (forall a. m a -> a) ->
--   WriterBaseF writer ->
--   WriterMethods writer String m ->
--   StrictPairMethodsExpectations ->
--   [TestTree]
-- testStrictPairMethods testLabel runMonad WriterBaseF {..} WriterMethods {..} StrictPairMethodsExpectations {..} =
--   [
    -- testProperty
    --   (prop_name "execWriter")
    --   ( \(Bot v :: (Bot ((), String))) ->
    --       let result = runMonad $ execWriterT $ writerF $ return @m v
    --        in isBottom result === expect_bot_execWriter (isBottom v)
    --   ),

    -- NOTE: strictness depends only on the strictness of the provided map
    -- testProperty
    --   (prop_name "mapWriter - strict map")
    --   ( \(Bot v :: (Bot ((), String))) ->
    --       let result = runIdentity $ runWriterF $ mapWriter fstrict $ writerF $ returnM v
    --           fstrict = Identity . runMonad
    --        in isBottom result === expect_bot_mapWriter_strict (isBottom v)
    --   ),
    -- testProperty
    --   (prop_name "mapWriter - lazy map")
    --   ( \(Bot v :: (Bot ((), String))) ->
    --       let result = runWriterF $ mapWriter flazy $ writerF $ returnM v
    --           flazy x = let ~(a, w) = runMonad x in Identity (a, w)
    --        in isBottom result === expect_bot_mapWriter_lazy (isBottom v)
    --   ),

    -- -- NOTE: no test for tell since it does not involve pair values

    -- testProperty
    --   (prop_name "listen")
    --   ( \(Bot v :: (Bot ((), String))) ->
    --       test_pair_strictness_monad runW (expect_bot_listen (isBottom v)) $
    --         listen $ writerF $ returnM v
    --   ),
    -- testProperty
    --   (prop_name "pass")
    --   ( \(Bot v :: (Bot ((), F1 String String))) ->
    --       let p' = coerce v :: ((), String -> String)
    --        in test_pair_strictness_monad runW (expect_bot_pass (isBottom v)) $
    --             pass $
    --               writerF $
    --                 return (p', "")
    --   ),
    -- testProperty
    --   (prop_name "censor")
    --   ( \(Bot v :: (Bot ((), String))) ->
    --       test_pair_strictness_monad runW (expect_bot_censor (isBottom v)) $
    --         censor (\w -> w <> w) $
    --           writerF $
    --             returnM v
    --   ),
    -- testProperty
    --   (prop_name "combination")
    --   ( \(Bot t :: (Bot (Int, String)))
    --      (Bot u :: (Bot (Int, String)))
    --      (Bot v :: (Bot (Int, String)))
    --      (Bot w :: (Bot (Int, String))) ->
    --         let mt = writerF $ returnM t
    --             mu = writerF $ returnM u
    --             mv = writerF $ returnM v
    --             mw = writerF $ returnM w
    --             result = do
    --               a <- mt
    --               (b, x) <- listen mu
    --               tell x
    --               c <- censor (drop 0) mv
    --               d <- mapWriter id mw
    --               return $ a + b + c + d
    --             expected = expect_bot_combination (isBottom t) (isBottom u) (isBottom v) (isBottom w)
    --          in isBottom (runW result) === expected
    --   )
  -- ]
  -- where
    -- prop_name methodName = "[" <> testLabel <> "] " <> methodName
    -- runW :: (Monoid w) => writer w m a -> (a, w)
    -- runW = runMonad . runWriterF
    -- returnM :: (a, w) -> m (a, w)
    -- returnM = return @m

-- | Value (a, w) strictness tests for lifts.
--
-- In general, lifts do not involve pairs (a, w), since the log w is
-- not visible to other monads in the stack.
-- Hence, tests are omitted for:
-- * lift
-- * liftIO
-- * liftCallCC
--
-- testStrictPairLifts ::
--   forall writer.
--   String ->
--   WriterBaseF writer ->
--   WriterLifts writer ->
--   StrictPairLiftsExpectations ->
--   [TestTree]
-- testStrictPairLifts testLabel WriterBaseF {..} WriterLifts {..} StrictPairLiftsExpectations {..} =
--   [
--     testProperty
--       (prop_name "liftCatch - no error")
--       ( \(Bot v :: (Bot ((), String))) ->
--         let
--            catch :: Catch SomeException (writer String (ExceptT SomeException IO)) ()
--            catch = liftCatch catchE
--            writer = catch (writerF $ return v) e
--         in test_pair_strictness_IOBase (expect_bot_liftCatch (isBottom v))
--               $ fromRight e <$> runExceptT (runWriterF writer)
--       ),
--     testProperty
--       (prop_name "liftCatch - handle error")
--       ( \(Bot v :: (Bot ((), String))) ->
--         let
--            catch :: Catch SomeException (writer String (ExceptT SomeException IO)) ()
--            catch = liftCatch catchE
--            writer = catch (writerF $ throwE e) (const (writerF $ return v))
--         in test_pair_strictness_IOBase (expect_bot_liftCatch (isBottom v))
--               $ fromRight e <$> runExceptT (runWriterF writer)
--       )
--   ]
--   where
--     e = error "this should never happen"
--     prop_name methodName = "[" <> testLabel <> "] " <> methodName

-- | Value (a, w) strictness tests for type class functions other than Monads.
--
-- testStrictPairTypeClass ::
--   forall writer.
--   ( Traversable (writer String []),
--     Contravariant (writer String (Op (Int, String))),
--     MonadZip (writer String Identity)
--   ) =>
--   String ->
--   WriterBase writer ->
--   StrictPairTypeClassExpectations ->
--   [TestTree]
-- testStrictPairTypeClass testLabel WriterBase {..} StrictPairTypeClassExpectations {..} =
--   [ testProperty
--       (prop_name "foldMap")
--       ( \(v :: [Bot (Int, String)]) ->
--           let p' = coerce v :: [(Int, String)]
--               result = getSum $ foldMap (const (Sum (0 :: Int))) (writer p')
--            in isBottom result === expect_bot_foldMap (any isBottom v)
--       ),
--     testProperty
--       (prop_name "traverse")
--       ( \(v :: [Bot (Int, String)]) ->
--           let p' = coerce v :: [(Int, String)]
--               result = traverse Just (writer p')
--            in isBottom (isBottom result) === expect_bot_foldMap (any isBottom v)
--       ),
--     testProperty
--       (prop_name "contramap")
--       ( \(Bot v :: (Bot (Int, String))) ->
--           let f = getOp $ runWriter $ contramap (+ 1) $ writer (Op id)
--            in isBottom (f v) === expect_bot_contramap (isBottom v)
--       ),
--     testProperty
--       (prop_name "mzipWith")
--       ( \(Bot v :: (Bot (Int, String)))
--          (Bot w :: (Bot (Int, String))) ->
--             let f = runWriter $ mzipWith (+) (writer (Identity v)) (writer (Identity w))
--              in isBottom f === expect_bot_mzipWith (isBottom v) (isBottom w)
--       )
--   ]
--   where
--     prop_name methodName = "[" <> testLabel <> "] " <> methodName

-- | Value (a, w) strictness tests for Monadic typeclass functions.
--
-- testStrictPairMonadic ::
--   forall writer.
--   (
--     -- MonadPlus (writer String []),
--     -- MonadPlus (writer String Maybe),
--     MonadIO (writer String IO)
--   ) =>
--   String ->
--   WriterBaseF writer ->
--   WriterMethods writer String IO ->
--   StrictPairMonadicExpectations ->
--   [TestTree]
-- testStrictPairMonadic testLabel WriterBaseF {..} WriterMethods {..} StrictPairMonadicExpectations {..} =
--   [
  -- testProperty
  --     (prop_name "Functor fmap")
  --     ( \(Bot v :: (Bot ((), String))) ->
  --         let expected = expect_bot_functor_fmap (isBottom v)
  --          in test_pair_strictness_monad runWriterF expected $
  --               (\w -> w <> w) <$> writerF (Identity v)
  --     ),
  --   testProperty
  --     (prop_name "Monad >>=")
  --     ( \(Bot v :: (Bot (Int, String)))
  --        (Bot w :: (Bot ((), String))) -> do
  --           let expected = expect_bot_monad_bind (isBottom v) (isBottom w)
  --            in test_pair_strictness_monad runWriterF expected $
  --                 writerF (Identity v) >>= const (writerF (Identity w))
  --     ),
  --   testProperty
  --     (prop_name "Applicative <*>")
  --     ( \(Bot v :: (Bot ((), String)))
  --        (Bot u :: (Bot (F1 () (), String))) -> do
  --           let expected = expect_bot_applicative_apply (isBottom v) (isBottom u)
  --               u' :: (() -> (), String)
  --               u' = coerce u
  --            in test_pair_strictness_monad runWriterF expected $
  --                 writerF (Identity u') <*> writerF (Identity v)
  --     ),
    -- testProperty
    --   (prop_name "Applicative liftA2")
    --   ( \(Bot v :: (Bot (Int, String)))
    --      (Bot u :: (Bot (Int, String))) -> do
    --         let expected = expect_bot_applicative_apply (isBottom v) (isBottom u)
    --         test_pair_strictness_monad runWriterF expected $
    --           liftA2 (+) (writerF (Identity v)) (writerF (Identity u))
    --   ),
    -- testProperty
    --   -- NOTE: implementation of any fixed point functions must be lazy, so
    --   -- mfix is lazy for all Writer types.
    --   (prop_name "MonadFix mfix")
    --   ( \(Bot v :: (Bot ([Int], String))) ->
    --       let expected = expect_bot_mfix (isBottom v)
    --        in test_pair_strictness_monad runWriterF expected $
    --             mfix (\x -> writerF $ Identity $ first (x <>) v)
    --   ),
    -- testProperty
    --   (prop_name "Alternative <|> - []")
    --   ( \(v :: [Bot (Int, String)])
    --      (w :: [Bot (Int, String)]) ->
    --         let p' = coerce v :: [(Int, String)]
    --             q' = coerce w :: [(Int, String)]
    --             result = runWriterF $ writerF p' <|> writerF q'
    --             expected = expect_bot_alternative_list (isBottom <$> p') (isBottom <$> q')
    --          in any isBottom result === expected
    --   ),
    -- testProperty
    --   (prop_name "Alternative <|> - Maybe")
    --   ( \(v :: Maybe (Bot (Int, String)))
    --      (w :: Maybe (Bot (Int, String))) ->
    --         let p' = coerce v :: Maybe (Int, String)
    --             q' = coerce w :: Maybe (Int, String)
    --             result = runWriterF $ writerF p' `mplus` writerF q'
    --          in any isBottom result === expect_bot_alternative_maybe (isBottom <$> p') (isBottom <$> q')
    --   ),
    -- testProperty
    --   (prop_name "MonadPlus mplus - []")
    --   ( \(v :: [Bot (Int, String)])
    --      (w :: [Bot (Int, String)]) ->
    --         let p' = coerce v :: [(Int, String)]
    --             q' = coerce w :: [(Int, String)]
    --             result = runWriterF $ writerF p' `mplus` writerF q'
    --             expected = expect_bot_alternative_list (isBottom <$> p') (isBottom <$> q')
    --          in any isBottom result === expected
    --   ),

--     testProperty
--       (prop_name "Monad stack")
--       ( \(Bot v :: (Bot (Int, String)))
--          (Bot w :: (Bot (Int, String)))
--          ->
--             let expected = expect_bot_stack (isBottom v) (isBottom w)
--                 result :: TestMonadStack String (writer String IO) Int
--                 result = do
--                   q' <- lift $ lift $ writerF $ return w
--                   a <- lift ask
--                   b <- callCC $ \next -> do
--                     when (even a) $ next (10 :: Int)
--                     return 20
--                   p' <- lift $ lift $ writerF $ return v
--                   lift $ lift $ tell "hello"
--                   return $ q' + a + p' + b
--                 writer = censor (filter (/= 'l')) $ runTestMonadStack result show
--              in test_pair_strictness_IO (runWriterF @String) expected writer
--       )
--   ]
--   where
--     prop_name methodName = "[" <> testLabel <> "]" <> methodName

type TestMonadStack r m a = (ContT r (ReaderT Int m) a)

runTestMonadStack :: (Monad m) => TestMonadStack r m a -> (a -> r) -> m r
runTestMonadStack m cont = runReaderT rd 0
  where rd = runContT m $ \x -> reader $ const (cont x)

unBotDeep :: Bot (a, Bot w) -> (a, w)
unBotDeep = coerce

isStrictValue :: arg1 -> a -> Property
isStrictValue x  = isStrict x . evaluate

isLazyValue :: a -> Property
isLazyValue = isLazy . evaluate

-- Strictness in one argument:
-- The result (normalized to IO) should be bottom whenever arg1 is bottom.
isStrict :: arg1 -> IO a -> Property
isStrict x  = shouldBeBottom (isBottom x)
--   where p' = unBot p

-- Strictness in two arguments:
-- The result (normalized to IO) should be bottom whenever arg1 OR arg2 is bottom.
isStrict2 :: arg1 -> arg2 -> IO a -> Property
isStrict2 x y  = shouldBeBottom (isBottom x || isBottom y)

isBottomDeep :: Bot (a, Bot w) -> Bool
isBottomDeep (Bot p) = isBottom p || isBottom (snd p)

isDeepStrict :: Bot (a, Bot w) -> IO b -> Property
isDeepStrict p  = shouldBeBottom (isBottomDeep p)

isDeepStrict2 :: Bot (a, Bot w) -> Bot (a', Bot w') -> IO b -> Property
isDeepStrict2 p q  = shouldBeBottom (isBottomDeep p || isBottomDeep q)

isDeepStrict1 :: Bot (a, Bot w) -> arg1 -> IO b -> Property
isDeepStrict1 p y  = shouldBeBottom (isBottomDeep p || isBottom y)

isStrictAny :: [arg] -> IO a -> Property
isStrictAny x = shouldBeBottom (any isBottom x)

isLazy :: IO a -> Property
isLazy = shouldBeBottom False

shouldBeBottom :: Bool -> IO a -> Property
shouldBeBottom True result = label "Bottom" $ assertExceptionIO isBottomError result
  where
    -- Check error message only i.e. prefix, ignoring stacktrace.
    isBottomError :: ErrorCall -> Bool
    isBottomError e = "<bottom>" `isPrefixOf` displayException e
shouldBeBottom False result = label "not Bottom" $ monadicIO $ run $ do
  v <- result
  v `seq` return ()
