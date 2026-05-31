{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Compose
import System.Exit

type FallibleCountT = ExceptT String `ComposeT` StateT Int

checkNonNeg :: (Monad m) => FallibleCountT m ()
checkNonNeg = ComposeT $ do
    count <- lift get
    when (count < 0) $ throwE $ "count is negative (" ++ show count ++ ")"

main :: IO ()
main = do
    let negateAndCheck = lift (modify negate) >> runComposeT checkNonNeg
        unitOrE = runIdentity $ evalStateT (runExceptT negateAndCheck) (-10)

    either die return unitOrE
