module Main (main) where

import qualified ComposeT

import qualified StateStrictness

import           Test.Tasty

import qualified WriterStrictness

main :: IO ()
main = do
  ComposeT.test
  defaultMain $ testGroup "Transformers" [
    StateStrictness.test,
    WriterStrictness.test
   ]
