module Main (main) where

import qualified ComposeT
import qualified WriterStrictness

main :: IO ()
main = do
  ComposeT.test
  WriterStrictness.test
