{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Heath.Func.Bool
  (  primitives  ) where

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
                        ]


makeBinop :: (HeathVal -> Either HError a) -> (a -> a -> Bool) -> HPrimitive
makeBinop ext func args = if length args /= 2
  then Left $ ArgNumErr (if (length args) > 2 then GT else LT) "should be 2"
  else do fst' <- ext (args !! 0)
          snd' <- ext (args !! 1)
          Right . Boolean $ func fst' snd'

makeBoolBinop :: (Bool -> Bool -> Bool) -> HPrimitive
makeBoolBinop = makeBinop T.asBoolean
makeStrBinop :: (String -> String -> Bool) -> HPrimitive
makeStrBinop = makeBinop T.asString
makeIntBinop :: (Integer -> Integer -> Bool) -> HPrimitive
makeIntBinop = makeBinop T.asInteger
makeFltBinop :: (Double -> Double -> Bool) -> HPrimitive
makeFltBinop = makeBinop T.asFloating
makeNumBinop :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> HPrimitive
makeNumBinop funcI funcF args
  | length args /= 2 = Left $ ArgNumErr (if (length args) > 2 then GT else LT) "should be 2"
  | all T.isInteger args = do fst' <- T.asInteger (args !! 0)
                              snd' <- T.asInteger (args !! 1)
                              Right . Boolean $ funcI fst' snd'
  | all T.isNumber args = do fst' <- (if T.isInteger (args !! 0) then (T.asInteger (args !! 0) >>= (Right . fromIntegral)) else T.asFloating (args !! 0))
                             snd' <- (if T.isInteger (args !! 1) then (T.asInteger (args !! 1) >>= (Right . fromIntegral)) else T.asFloating (args !! 1))
                             Right . Boolean $ funcF fst' snd'
  | T.isInteger (args !! 0) = Left $ TypeErr "Could not coerce value to Number" (args !! 1)
  | otherwise = Left $ TypeErr "Could not coerce value to Number" (args !! 0)
