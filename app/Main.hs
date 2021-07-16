module Main (main) where

import qualified System.IO  as IO

import           Heath.Eval (evalInteractive)

main :: IO ()
main = do
  IO.interact evalInteractive
