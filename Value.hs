{-# LANGUAGE LambdaCase #-}
module Value
    ( Value(..)
    , isNull
    , getBool
    , getNum
    , getStr
    , getArr
    , getObj

    -- Pretty printing
    , render
    , putValue
    ) where

import           Pretty
type Map k v = [(k, v)]

data Value
    = Null
    | Bool Bool
    | Num Double
    | Str String
    | Arr [Value]
    | Obj (Map String Value)
    deriving (Eq)

isNull :: Value -> Bool
isNull Null = True
isNull _    = False

getBool :: Value -> Maybe Bool
getBool (Bool b) = Just b
getBool _        = Nothing

getNum :: Value -> Maybe Double
getNum (Num n) = Just n
getNum _       = Nothing

getStr :: Value -> Maybe String
getStr (Str s) = Just s
getStr _       = Nothing

getArr :: Value -> Maybe [Value]
getArr (Arr a) = Just a
getArr _       = Nothing

getObj :: Value -> Maybe (Map String Value)
getObj (Obj o) = Just o
getObj _       = Nothing

render :: Value -> Doc
render = \case
    Null   -> text "null"
    Bool t -> text $ if t then "true" else "false"
    Num  n -> double n
    Str  s -> string s
    Arr  a -> delimited '[' ']' render a
    Obj  o -> delimited '{' '}' field o
        where field (k, v) = string k <> text ": " <> render v

instance Show Value where
    show = show . render

putValue :: Value -> IO ()
putValue = print
