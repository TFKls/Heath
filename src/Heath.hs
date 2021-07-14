{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>

A scheme interpreter, made to better understand Haskell
-}

module Heath
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)