{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

module Heath.Func.Bool
  ( primitives
  , eqv ) where

import qualified Data.Map        as M

import qualified Heath.Func.Type as T
import           Heath.Types

primitives :: M.Map String HPrimitive
primitives = M.fromList [ ("=", makeNumBinop (==) (==))
                        , ("<", makeNumBinop (<) (<))
                        , (">", makeNumBinop (>) (>))
                        , ("<=", makeNumBinop (<=) (<=))
                        , (">=", makeNumBinop (>=) (>=))
                        , ("/=", makeNumBinop (/=) (/=))
                        , ("||", makeBoolBinop (||))
                        , ("&&", makeBoolBinop (&&))
                        , ("string=?", makeStrBinop (==))
                        , ("string<?", makeStrBinop (<))
                        , ("string>?", makeStrBinop (>))
                        , ("string<=?", makeStrBinop (<=))
                        , ("string>=?", makeStrBinop (>=))
                        , ("eqv?", eqv)
                        , ("eq?", eqv)
                        , ("equal?", equal)
                        ]


makeBinop :: (HeathVal -> Either HError a) -> (a -> a -> Bool) -> HPrimitive
makeBinop ext func args = if length args /= 2
  then Left $ ArgNumErr (compare (length args) 2) "should be 2"
  else do fst' <- ext (args !! 0)
          snd' <- ext (args !! 1)
          Right . Boolean $ func fst' snd'

makeBoolBinop :: (Bool -> Bool -> Bool) -> HPrimitive
makeBoolBinop = makeBinop T.asBoolean
makeStrBinop :: (String -> String -> Bool) -> HPrimitive
makeStrBinop = makeBinop T.asString
_makeIntBinop :: (Integer -> Integer -> Bool) -> HPrimitive
_makeIntBinop = makeBinop T.asInteger
_makeFltBinop :: (Double -> Double -> Bool) -> HPrimitive
_makeFltBinop = makeBinop T.asFloating
makeNumBinop :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> HPrimitive
makeNumBinop funcI funcF args
  | length args /= 2 = Left $ ArgNumErr (compare (length args) 2) "should be 2"
  | all T.isInteger args = do fst' <- T.asInteger (args !! 0)
                              snd' <- T.asInteger (args !! 1)
                              Right . Boolean $ funcI fst' snd'
  | all T.isNumber args = do fst' <- (if T.isInteger (args !! 0) then (T.asInteger (args !! 0) >>= (Right . fromIntegral)) else T.asFloating (args !! 0))
                             snd' <- (if T.isInteger (args !! 1) then (T.asInteger (args !! 1) >>= (Right . fromIntegral)) else T.asFloating (args !! 1))
                             Right . Boolean $ funcF fst' snd'
  | T.isInteger (args !! 0) = Left $ TypeErr "Could not coerce value to Number" (args !! 1)
  | otherwise = Left $ TypeErr "Could not coerce value to Number" (args !! 0)


eqv :: HPrimitive
eqv [Boolean x, Boolean y]     = Right . Boolean $ x == y
eqv [String x, String y]       = Right . Boolean $ x == y
eqv [Integer x, Integer y]     = Right . Boolean $ x == y
eqv [Floating x, Floating y]   = Right . Boolean $ x == y
eqv [Atom x, Atom y]           = Right . Boolean $ x == y
eqv [ImList xs x, ImList ys y] = eqv [List (x:xs), List (y:ys)]
eqv [List xs, List ys] = Right . Boolean $ (length xs == length ys)
  && (and $ zipWith (\x y -> case eqv [x,y] of { Right (Boolean val) -> val; _ -> False}) xs ys)
eqv [_, _] = Right . Boolean $ False
eqv ls = Left $ ArgNumErr (compare (length ls) 2) "eqv takes 2 arguments"

data AsTypeFunc = forall a. Eq a => AnyATF (HeathVal -> Either HError a)

primEqual :: HeathVal -> HeathVal -> AsTypeFunc -> Bool
primEqual x y (AnyATF func) = case (do x' <- func x
                                       y' <- func y
                                       Right $ x' == y') of
                                Left _    -> False
                                Right val -> val

atfs :: [AsTypeFunc]
atfs = [AnyATF T.weakAsBoolean, AnyATF T.asInteger, AnyATF T.weakAsFloating, AnyATF T.weakAsString, AnyATF T.asAtom]

equal :: HPrimitive
equal [List xs, List ys] = Right . Boolean $ (length xs == length ys) && and (map (\(x, y) -> (\case
                                                                     Right x' -> x'
                                                                     Left _ -> False) $ T.asBoolean =<< equal [x,y]) (zip xs ys))
equal [ImList xs x, ImList ys y] = do
  xys <- equal [x,y] >>= T.asBoolean
  xy <- equal [List xs, List ys] >>= T.asBoolean
  Right . Boolean $ xy && xys

equal [x, y] = Right . Boolean . or $ map (primEqual x y) atfs

equal ls = Left $ ArgNumErr (compare (length ls) 2) "equal takes 2 arguments"
