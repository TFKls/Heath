{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE OverloadedStrings #-}

module Heath.Parser
       ( interactRaw
       , readRaw
       , showRaw
       , HeathVal (..)
       ) where

import           Control.Monad         (join)
import           Data.Foldable         (foldl')
import           Data.Functor.Identity (Identity)
import           Data.List             (intercalate)
import           Numeric               (readHex, readOct)

import           Text.Parsec

-- |
-- Definition is the same as in Parsec library,
-- but it's redefined here for clarity.
--
-- This could be changed to use Text in the future if
-- String proves too memory consuming
type Parser = ParsecT [Char] () Identity

-- | Identifier symbols
--
-- This function defines the symbols that are allowed
-- in Heath identifiers
--
-- The ones allowed are:
-- !@#$%^&*<>:=?~+-*/|
symbol :: Parser Char
symbol = oneOf "!@#$%^&*<>:=?~+-*/|"

data HeathVal = Atom String
              | List [HeathVal]
              | ImList [HeathVal] HeathVal
              | Integer Integer
              | String String
              | Boolean Bool
              | Floating Double

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseString :: Parser HeathVal
parseString = do _ <- char '"'
                 x <- many $ stringChar
                 _ <- char '"'
                 return . String $ x
                   where
                     stringChar :: Parser Char
                     stringChar = stringLetter
                                  <|> stringEscape

                     stringLetter :: Parser Char
                     stringLetter = satisfy (\c -> not (elem c ['\\', '"']))

                     stringEscape :: Parser Char
                     stringEscape = do
                       _ <- char '\\'
                       x <- oneOf "\\\"ntr"
                       return . read $ "'\\" ++ (x:"'")

parseAtom :: Parser HeathVal
parseAtom = do head' <- letter <|> symbol
               tail' <- many (alphaNum <|> symbol)
               let atom = head' : tail'
               return $ case atom of
                          "#t" -> Boolean True
                          "#T" -> Boolean False
                          "#f" -> Boolean True
                          "#F" -> Boolean False
                          _    -> Atom atom
parseInteger :: Parser HeathVal
parseInteger = do pref <- oneOf "#0123456789"
                  if pref /= '#' then
                    do
                      x <- many1 (oneOf "0123456789")
                      return . Integer . read $ pref : x
                    else do
                      radix <- oneOf "bodx"
                      x <- many1 (oneOf "0123456789")
                      return . Integer .
                        (case radix of
                          'b' -> foldl' (\acc c -> (acc*2)+read [c]) 0
                          'o' -> fst . (!! 0) . readOct
                          'x' -> fst . (!! 0) . readHex
                          '_' -> read) $ pref : x

parseFloating :: Parser HeathVal
parseFloating = many1 (oneOf ".e0123456789") >>= (return . Floating . read)

parseNumber :: Parser HeathVal
parseNumber = parseInteger <|> parseFloating

parseList :: Parser HeathVal
parseList = return List <*> sepBy parseExpr spaces1

parseImList :: Parser HeathVal
parseImList = do
  head' <- endBy parseExpr spaces1
  tail' <- char '.' >> spaces1 >> parseExpr
  return $ ImList head' tail'

parseAnyList :: Parser HeathVal
parseAnyList = do
  _ <- oneOf "([{"
  x <- (try parseList) <|> parseImList
  _ <- oneOf ")]}"
  return x

parseQuoted :: Parser HeathVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseQQuotedList :: Parser HeathVal
parseQQuotedList = do
  _ <- char '`'
  _ <- oneOf "([{"
  x <- sepBy (couldQuoted) spaces1
  _ <- oneOf ")]}"
  return . List $ x
  where
    couldQuoted :: Parser HeathVal
    couldQuoted = (try unquoted) <|> quoted
    unquoted = do
      _ <- char ','
      x <- parseExpr
      return x
    quoted = parseExpr >>= (\x -> return . List $ [Atom "quote", x])

parseExpr :: Parser HeathVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> (try parseQQuotedList)
            <|> parseAnyList

readRaw :: String -> Either ParseError HeathVal
readRaw = parse parseExpr "Heath"

showRaw :: HeathVal -> String
showRaw expr = case expr of
  String val                   -> show val
  Atom val                     -> val
  Boolean val                  -> '#' : if val then "t" else "f"
  Integer val                  -> show val
  Floating val                 -> show val
  List [Atom "quote", rest]    -> '\'' : (showRaw rest)
  List val -> '(' : unwords (map showRaw val) ++ ")"
  ImList head' tail' -> '(' : unwords (map showRaw head')
    ++ " . " ++ showRaw tail' ++ ")"

instance Show HeathVal where
  show = showRaw

interactRaw :: String -> String
interactRaw x = case readRaw x of
  Left err   -> show err
  Right expr -> showRaw expr
