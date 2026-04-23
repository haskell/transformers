{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Select
-- Copyright   :  (c) Ross Paterson 2017
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Selection monad transformer, modelling search algorithms.
--
-- * Martin Escardo and Paulo Oliva.
--   "Selection functions, bar recursion and backward induction",
--   /Mathematical Structures in Computer Science/ 20:2 (2010), pp. 127-168.
--   <https://www.cs.bham.ac.uk/~mhe/papers/selection-escardo-oliva.pdf>
--
-- * Jules Hedges. "Monad transformers for backtracking search".
--   In /Proceedings of MSFP 2014/. <https://arxiv.org/abs/1406.2058>
-----------------------------------------------------------------------------

module Control.Monad.Trans.Select (
    -- * The Select monad
    Select,
    select,
    runSelect,
    mapSelect,
    -- * The SelectT monad transformer
    SelectT(..),
    mapSelectT,
    -- * Monad transformation
    selectToContT,
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Functor.Identity
#ifdef __GLASGOW_HASKELL__
import GHC.Generics
#endif

-- | The selection monad, which is non-strict.
type Select r = SelectT r Identity

-- | Constructor for computations in the selection monad.
select :: ((a -> r) -> a) -> Select r a
select f = SelectT $ \ k -> Identity (f (runIdentity . k))
{-# INLINE select #-}

-- | Runs a @Select@ computation with a function for evaluating answers
-- to select a particular answer.  (The inverse of 'select'.)
runSelect :: Select r a -> (a -> r) -> a
runSelect m k = runIdentity (runSelectT m (Identity . k))
{-# INLINE runSelect #-}

-- | Apply a function to transform the result of a selection computation.
--
-- * @'runSelect' ('mapSelect' f m) = f . 'runSelect' m@
mapSelect :: (a -> a) -> Select r a -> Select r a
mapSelect f = mapSelectT (Identity . f . runIdentity)
{-# INLINE mapSelect #-}

-- | Selection monad transformer.
--
-- 'SelectT' is not a functor on the category of monads, and many operations
-- cannot be lifted through it.
--
-- @SelectT r m@ is strict if and only if @m@ is.
newtype SelectT r m a = SelectT {
    -- | Runs a @SelectT@ computation with a function for evaluating
    -- answers to select a particular answer.
    runSelectT :: (a -> m r) -> m a }
#ifdef __GLASGOW_HASKELL__
    deriving (Generic)
#endif

-- | Apply a function to transform the result of a selection computation.
-- This has a more restricted type than the @map@ operations for other
-- monad transformers, because 'SelectT' does not define a functor in
-- the category of monads.
--
-- * @'runSelectT' ('mapSelectT' f m) = f . 'runSelectT' m@
mapSelectT :: (m a -> m a) -> SelectT r m a -> SelectT r m a
mapSelectT f m = SelectT $ f . runSelectT m
{-# INLINE mapSelectT #-}

instance (Functor m) => Functor (SelectT r m) where
    fmap f (SelectT g) = SelectT (fmap f . g . (. f))
    {-# INLINE fmap #-}

instance (Monad m) => Applicative (SelectT r m) where
    pure = lift . return
    {-# INLINE pure #-}
    SelectT gf <*> SelectT gx = SelectT $ \ k -> do
        let h f = liftM f (gx (k . f))
        f <- gf ((>>= k) . h)
        h f
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Alternative m, Monad m) => Alternative (SelectT r m) where
    empty = empty
    {-# INLINE empty #-}
    (<|>) = (<|>)
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (SelectT r m) where
    SelectT g >>= f = SelectT $ \ k -> do
        let h x = runSelectT (f x) k
        y <- g ((>>= k) . h)
        h y
    {-# INLINE (>>=) #-}

instance (Fail.MonadFail m) => Fail.MonadFail (SelectT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (SelectT r m) where
    mzero = SelectT (const mzero)
    {-# INLINE mzero #-}
    SelectT f `mplus` SelectT g = SelectT $ \ k -> f k `mplus` g k
    {-# INLINE mplus #-}

instance MonadTrans (SelectT r) where
    lift = SelectT . const
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (SelectT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- | Convert a selection computation to a continuation-passing computation.
selectToContT :: (Monad m) => SelectT r m a -> ContT r m a
selectToContT (SelectT g) = ContT $ \ k -> g k >>= k
{-# INLINE selectToContT #-}
