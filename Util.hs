module Util where

import           Numeric

escapes :: [(Char, String)]
escapes = zipWith f es cs
  where
    es = "\b\n\f\r\t\\\"/"
    cs = "bnfrt\\\"/"
    f a b = (a, ['\\', b])

mustEscape :: Char -> Bool
mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
