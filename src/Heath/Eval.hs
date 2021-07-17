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

import           Data.List       (foldl')

import           Heath.Func      (getPrim)
import           Heath.Func.Bool (eqv)
import qualified Heath.Func.Type as T
import           Heath.Parser    (readExpr)
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
evalRaw (List [Atom "if", predicate, yes]) = evalRaw (List [Atom "if", predicate, yes, List [Atom "quote", List []]])
evalRaw (List [Atom "if", predicate, yes, no]) = evalRaw predicate >>= (\case
                                                              Boolean False -> evalRaw no
                                                              _          -> evalRaw yes)
evalRaw (List (Atom "cond" : rest)) = evalCond rest
evalRaw (List [Atom "case"]) = evalCaseError "case cannot be empty" $ List []
evalRaw (List [Atom "case", _]) = evalCaseError "case needs to contain at least one clause" $ List []
evalRaw (List (Atom "case" : case' : rest)) = evalCase case' rest
evalRaw (List (Atom "case" : case' : rest)) = evalCase case' rest
evalRaw (List (Atom func : args))  = mapM evalRaw args >>= getPrim func
-- evalRaw val@(Atom _) = pure val
evalRaw (List xs) = mapM evalRaw xs >>= Right . List
evalRaw (ImList xs x) = do
  xs' <- mapM evalRaw xs
  x' <- evalRaw x
  Right $ ImList (reverse xs') x'

evalRaw expr = Left $ EvalErr "Unrecognized expression" expr

evalInteractive :: String -> String
evalInteractive = showHErrVal . eval . readExpr

evalCondError :: String -> HeathVal -> HErrVal
evalCondError reason val = Left $ EvalErr ("Incorrect `cond` form: " ++ reason) val

evalCond :: HPrimitive
evalCond [List clause, List (Atom "else" : elseClause)] =
  if null clause
  then evalCondError "clause cannot be empty" (List clause)
  else do test <- evalRaw $ head clause
          if case test of
            Boolean False -> True
            _             -> False
            then do res <- mapM evalRaw elseClause
                    Right $ last res
            else if length clause /= 1
            then do res <- mapM evalRaw $ tail clause
                    Right $ last res
                 else Right $ test

evalCond [List clause] = evalCond [List clause, List [Atom "else", List []]]
evalCond (List clause : xs) = if null clause
  then evalCondError "clause cannot be empty" (List clause)
  else do test <- evalRaw $ head clause
          if case test of
               Boolean False -> True
               _             -> False
            then evalCond xs
            else if length clause /= 1
            then do res <- mapM evalRaw $ tail clause
                    Right $ last res
                 else Right $ test

evalCond (val:_) = evalCondError "element isn't a list" val
evalCond [] = evalCondError "cond cannot be empty" $ List []

evalCaseError :: String -> HeathVal -> HErrVal
evalCaseError reason val = Left $ EvalErr ("Incorrect `case` form: " ++ reason) val

evalCase :: HeathVal -> HPrimitive
evalCase val xs = do
  val' <- evalRaw val
  evalCase' val' xs

evalCase' :: HeathVal -> HPrimitive
evalCase' val [List (List dat : clause), List (Atom "else" : elseClause)] = if val `elem` dat
  then do clause' <- mapM evalRaw clause
          Right $ if null clause' then List [] else last clause'
  else do clause' <- mapM evalRaw elseClause
          Right $ if null clause' then List [] else last clause'

evalCase' val [List clause@(List _ : _)] = evalCase' val [List clause, List [Atom "else", List []]]
evalCase' val (List (List dat : clause) : xs) = if val `elem` dat
    then do clause' <- mapM evalRaw clause
            Right $ if null clause' then List [] else last clause'
    else evalCase' val xs

evalCase' _ (val:_) = evalCaseError "improper clause form" val
evalCase' _ [] = evalCaseError "case needs to contain at least one clause" $ List []
