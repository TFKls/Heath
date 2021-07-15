{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE OverloadedStrings #-}

module Heath.Eval
  ( eval
  ) where

import qualified Data.Map     as M
import           Heath.Parser

emptyQuotList :: HeathVal
emptyQuotList = List [Atom "quote", List []]

eval :: HeathVal -> HeathVal
eval val@(String _)             = val
eval val@(Integer _)            = val
eval val@(Floating _)           = val
eval val@(Boolean _)            = val
eval (List [Atom "quote", val]) = val

eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [HeathVal] -> HeathVal
apply func args = maybe emptyQuotList ($ args) $ M.lookup func functions

functions :: M.Map String ([HeathVal] -> HeathVal)
functions = M.fromList [
  ("+", makeBinop (+))
  ]
