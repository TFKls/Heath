{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

module Heath.Types (
  HeathVal (..)
  , HPrimitive
  , HErrVal
  , HError (..)
  , showHErrVal
  ) where

import qualified Text.Parsec as P

data HeathVal = Atom String
              | List [HeathVal]
              | ImList [HeathVal] HeathVal
              | Integer Integer
              | String String
              | Boolean Bool
              | Floating Double
              deriving stock (Eq)

type HPrimitive = [HeathVal] -> HErrVal
type HErrVal = Either HError HeathVal
data HError = TypeErr String HeathVal
            | ParseErr P.ParseError
            | FuncErr String String
            | EvalErr String HeathVal
            | ArgNumErr Ordering String

instance Show HeathVal where
  show expr = case expr of
    String val                   -> show val
    Atom val                     -> val
    Boolean val                  -> '#' : if val then "t" else "f"
    Integer val                  -> show val
    Floating val                 -> show val
    List [Atom "quote", rest]    -> '\'' : (show rest)
    List val -> '(' : unwords (map show val) ++ ")"
    ImList head' tail' -> '(' : unwords (map show head')
      ++ " . " ++ show tail' ++ ")"
instance Show HError where
  show (TypeErr str val)  = "TypeError:\n" ++ str ++ " "++ show val
  show (ParseErr err)     = "ParseError:\n" ++ show err
  show (FuncErr str name) = "PrimitiveError:\n" ++ str ++ " @ " ++ name
  show (EvalErr str val)  = "EvalError:\n" ++ str ++ " " ++ show val
  show (ArgNumErr ord ctx) = "ArgNumError:\n" ++ (case ord of
    LT -> "Not enough arguments: "
    EQ -> "Wrong number of arguments: "
    GT -> "Too much arguments: ") ++ ctx

showHErrVal :: HErrVal -> String -- Could make HErrVal a newtype but this works for now
showHErrVal = \case
  Left err  -> show err
  Right val -> show val

