module Heath.Func
  ( primitives
  , getPrim
  ) where

import qualified Data.Map          as M

import qualified Heath.Func.Bool   as Bool
import qualified Heath.Func.Number as Num
import qualified Heath.Func.Type   as Typ
import           Heath.Types


getPrim :: String -> HPrimitive
getPrim func args = maybe (Left $ PrimErr "Could not find primitive function" func) ($ args) $ M.lookup func primitives

primitives :: M.Map String HPrimitive
primitives = M.unions [ Num.primitives
                      , Typ.primitives
                      , Bool.primitives
                      ]
