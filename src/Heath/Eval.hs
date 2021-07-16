{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Heath.Eval
  ( eval
  , evalInteractive
  ) where

import           Data.List    (foldl')

import           Heath.Func   (getPrim)
import           Heath.Parser (readExpr)
import           Heath.Types

eval :: HErrVal -> HErrVal
eval err@(Left _) = err
eval (Right val)  = evalRaw val

evalRaw :: HeathVal -> HErrVal
evalRaw val@(String _)             = pure val
evalRaw val@(Integer _)            = pure val
evalRaw val@(Floating _)           = pure val
evalRaw val@(Boolean _)            = pure val
evalRaw (List [Atom "quote", val]) = pure val
evalRaw (List [Atom "eval", (List [Atom "quote", val])]) = evalRaw val
evalRaw (List [Atom "eval", val])  = evalRaw val
evalRaw (IntError err) = Left err
evalRaw (List (Atom func : args))  = getPrim func $ map (toInternalErr . evalRaw) args
evalRaw val@(Atom _) = pure val
evalRaw (List xs) = case foldl' (\acc x -> do
                                    rAcc <- acc
                                    rx <- evalRaw x
                                    return $ rx : rAcc
                                    ) (Right []) xs of
                      Left err -> Left err
                      Right ls -> Right . List . reverse $ ls
evalRaw expr = Left $ EvalErr "Evaluation of this expression is not implemented yet" expr

evalInteractive :: String -> String
evalInteractive = showHErrVal . eval . readExpr


