module Pretty where

import           Data.Char

import           Data.Bits
import           Numeric

data Doc = X
    deriving Show

instance Semigroup Doc where
    a <> b = undefined

char :: Char -> Doc
char c = undefined

string :: String -> Doc
string = surround '"' '"' . hcat . map character

text :: String -> Doc
text t = undefined

double :: Double -> Doc
double d = undefined

-- | Circumscribe with prefix and suffix
surround :: Char -> Char -> Doc -> Doc
surround left right x = char left <> x <> char right

-- | Concatenates multiple 'Doc' values, i.e., 'concat' for lists
hcat :: [Doc] -> Doc
hcat xs = undefined

-- DEALING WITH CHARACTERS AND ESCAPES
character :: Char -> Doc
character c = case lookup c escaped of
    Just r -> text r
    Nothing | mustEscape c -> hexEscape c
            | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

escaped :: [(Char, String)]
escaped = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

-- | Represent Unicode characters up to 0xffffff
smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
    where h = showHex x ""

-- | Helper (bit-manipulation) function to represent characters with Unicode values above 0xfffff
bigHex :: Int -> Doc
bigHex n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = bigHex (d - 0x10000)
    where d = ord c
