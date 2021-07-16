module Heath.Func.Type
  ( primitives
  , isString
  , isNumber
  , isFloating
  , isInteger
  , isList
  , isAtom
  , isBoolean
  , isImList
  , isAnyList
  , isReal
  ) where

import qualified Data.Map    as M

import           Heath.Types

primitives :: M.Map String HPrimitive
primitives = M.fromList [ ("list?", makeTypeFunc isList)
                        , ("imlist?", makeTypeFunc isImList)
                        , ("bool?", makeTypeFunc isBoolean)
                        , ("string?", makeTypeFunc isString)
                        , ("number?", makeTypeFunc isNumber)
                        , ("integer?", makeTypeFunc isInteger)
                        , ("real?", makeTypeFunc isReal)
                        , ("float?", makeTypeFunc isFloating)
                        , ("symbol?", makeTypeFunc isAtom)
                        ]

type IsTypeFunc = HeathVal -> Bool
makeTypeFunc :: IsTypeFunc -> HPrimitive
makeTypeFunc func args = Right . Boolean . (all func) $ args

isString :: IsTypeFunc
isString (String _) = True
isString _          = False
isNumber :: IsTypeFunc
isNumber x = isInteger x || isFloating x
isReal :: IsTypeFunc
isReal x = isInteger x || isFloating x
isFloating :: IsTypeFunc
isFloating (Floating _) = True
isFloating _            = False
isInteger :: IsTypeFunc
isInteger (Integer _) = True
isInteger _           = False
isAtom :: IsTypeFunc
isAtom (Atom _) = True
isAtom _        = False
isBoolean :: IsTypeFunc
isBoolean (Boolean _) = True
isBoolean _           = False
isList :: IsTypeFunc
isList (List _) = True
isList _        = False
isImList :: IsTypeFunc
isImList (ImList _ _) = True
isImList _            = False
isAnyList :: IsTypeFunc
isAnyList x = isList x || isImList x

