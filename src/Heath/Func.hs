{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

module Heath.Func
  ( primitives
  , getPrim
  ) where

import qualified Data.Map          as M

import qualified Heath.Func.Bool   as Bool
import qualified Heath.Func.List   as Lst
import qualified Heath.Func.Number as Num
import qualified Heath.Func.System as Sys
import qualified Heath.Func.Type   as Typ
import           Heath.Types


getPrim :: String -> HPrimitive
getPrim func args = maybe (Left $ FuncErr "Could not find primitive function" func) ($ args) $ M.lookup func primitives

primitives :: M.Map String HPrimitive
primitives = M.unions [ Bool.primitives
                      , Lst.primitives
                      , Num.primitives
                      , Typ.primitives
                      , Sys.primitives
                      ]
