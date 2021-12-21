module Util where

import           Data.Maybe
import           Numeric

escapes :: [(Char, String)]
escapes = zipWith f es cs
 where
  es = "\b\n\f\r\t\\\"/"
  cs = "bnfrt\\\"/"
  f a b = (a, ['\\', b])

mustEscape :: Char -> Bool
mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

-- | If a character 'c' exists in the associated list of escapes, apply f to the resulting key value r; otherwise, if the character must be escaped regardless, apply g to c, otherwise apply h to c.
escaped :: Char -> (String -> p) -> (Char -> p) -> (Char -> p) -> p
escaped c f g h = case lookup c escapes of
  Just r -> f r
  Nothing | mustEscape c -> g c
          | otherwise    -> h c

isEscapable :: Char -> Bool
isEscapable c = isJust $ lookup c escapes
