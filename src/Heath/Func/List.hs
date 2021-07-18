{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

module Heath.Func.List
  (  primitives  ) where

import qualified Data.Map    as M

import           Heath.Types

primitives :: M.Map String HPrimitive
primitives = M.fromList [ ("car", car)
                        , ("cdr", cdr)
                        , ("cons", cons)
                        ]

car :: HPrimitive
car [List (x : _)] = Right x
car [ImList (x : _) _] = Right x
car [val] = Left $ TypeErr "Could not coerce value to a Pair" val
car ls = Left $ ArgNumErr (compare (length ls) 1) "car takes 1 argument"

cdr :: HPrimitive
cdr [List (_ : xs)] = Right $ List xs
cdr [ImList [_] x] = Right x
cdr [ImList (_ : xs) x] = Right $ ImList xs x
cdr [val] = Left $ TypeErr "Could not coerce value to a Pair" val
cdr ls = Left $ ArgNumErr (compare (length ls) 1) "cdr takes 1 argument"

cons :: HPrimitive
cons [x, List xs] = Right $ List (x : xs)
cons [x, ImList xs xl] = Right $ ImList (x : xs) xl
cons [x, y] = Right $ ImList [x] y
cons ls = Left $ ArgNumErr (compare (length ls) 2) "cons takes 2 arguments"

