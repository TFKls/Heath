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
  , asString
  , asBoolean
  , asInteger
  , asFloating
  , asAtom
  , weakAsString
  , weakAsBoolean
  , weakAsFloating
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

type ConvTypeFunc a = HeathVal -> Either HError a
asString :: ConvTypeFunc String
asString (String val) = Right val
asString val          = Left $ TypeErr "Could not coerce value to String" val
asBoolean :: ConvTypeFunc Bool
asBoolean (Boolean val) = Right val
asBoolean val           = Left $ TypeErr "Could not coerce value to Boolean" val
asInteger :: ConvTypeFunc Integer
asInteger (Integer val) = Right val
asInteger val           = Left $ TypeErr "Could not coerce value to Integer" val
asFloating :: ConvTypeFunc Double
asFloating (Floating val) = Right val
asFloating val           = Left $ TypeErr "Could not coerce value to Floating" val
asAtom :: ConvTypeFunc String
asAtom (Atom val) = Right val
asAtom val        = Left $ TypeErr "Could not coerce value to Atom" val


weakAsString :: ConvTypeFunc String
weakAsString (String val) = Right val
weakAsString (Integer val) = Right $ show val
weakAsString (Floating val) = Right $ show val
weakAsString (Boolean val) = Right $ if val then "#t" else "#f"
weakAsString (Atom val) = Right val
weakAsString val          = Left $ TypeErr "Could not coerce value to String" val
weakAsBoolean :: ConvTypeFunc Bool
weakAsBoolean (Boolean val) = Right val
weakAsBoolean (String val) = case val of
                               "#t" -> Right True
                               "#T" -> Right True
                               "#f" -> Right False
                               "#F" -> Right False
                               _ -> Left $ TypeErr "Could not coerce value to Boolean" (String val)
weakAsBoolean (Integer val) = Right $ val /= 0
weakAsBoolean val           = Left $ TypeErr "Could not coerce value to Boolean" val
weakAsFloating :: ConvTypeFunc Double
weakAsFloating (Floating val) = Right val
weakAsFloating (Integer val) = Right $ fromIntegral val
weakAsFloating val           = Left $ TypeErr "Could not coerce value to Floating" val
