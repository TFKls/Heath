{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE LambdaCase #-}

module Heath.Func.Number
  (  primitives  ) where

import           Data.List       (foldl1')
import qualified Data.Map        as M

import qualified Heath.Func.Type as T
import           Heath.Types

primitives :: M.Map String HPrimitive
primitives = M.fromList [ ("+", makeNumBinop (+) (+))
                        , ("-", makeNumBinop (-) (-))
                        , ("*", makeNumBinop (*) (*))
                        , ("/", makeNumBinop div (/))
                        , ("mod", makeIntBinop mod)
                        , ("quotient", makeIntBinop quot)
                        , ("remainder", makeIntBinop rem)
                        ]

makeIntBinop :: (Integer -> Integer -> Integer) -> HPrimitive
makeIntBinop func args = if all T.isInteger args
                         then Right . Integer $ foldl1' func (map (\case
                                                                      Integer x -> x
                                                                      _         -> 0 {-should be unreachable-}) args)
                         else Left $ TypeErr "Mismatched types for numeric operator" (List args)

_makeFltBinop :: (Double -> Double -> Double) -> HPrimitive
_makeFltBinop func args = if all T.isFloating args
                         then Right . Floating $ foldl1' func (map (\case
                                                                       Floating x -> x
                                                                       _          -> 0 {-should be unreachable-}) args)
                         else Left $ TypeErr "Mismatched types for numeric operator" (List args)
makeNumBinop :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> HPrimitive
makeNumBinop funcI funcF args = if all T.isInteger args
                                then Right . Integer $ foldl1' funcI (map (\case
                                                                              Integer x -> x
                                                                              _         -> 0 {-should be unreachable-}) args)
                                else
                                  if all T.isNumber args
                                  then Right . Floating $ foldl1' funcF (map (\case
                                                                                 Integer x  -> fromIntegral x
                                                                                 Floating x -> x
                                                                                 _          -> 0 {-should be unreachable-}) args)
                                  else Left $ TypeErr "Mismatched types for numeric operator" (List args)

