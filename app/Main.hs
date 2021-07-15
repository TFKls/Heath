module Main (main) where

import qualified System.IO    as IO

import           Heath.Parser (interactRaw)

main :: IO ()
main = do
  IO.interact interactExpr
