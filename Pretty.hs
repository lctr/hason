{-# LANGUAGE LambdaCase #-}
module Pretty where

import           Data.Char

import           Data.Bits
import           Numeric
import           Util

data Doc =
    Empty
    -- | *Hard* line breaks, which always appear in output. *Soft* breaks are handled by the function @softline@
    | Line
    | Char Char
    | Text String
    | Concat Doc Doc
    | Union Doc Doc
    deriving (Eq, Show)

instance Semigroup Doc where
    (<>) = append

instance Monoid Doc where
    mempty = Empty

append :: Doc -> Doc -> Doc
append a b = case (a, b) of
    (Empty, y    ) -> y
    (x    , Empty) -> x
    (a'   , b'   ) -> Concat a' b'

-- | Circumscribe with prefix and suffix
surround :: Char -> Char -> Doc -> Doc
surround left right x = char left <> x <> char right

-- | Concatenates multiple 'Doc' values, i.e., 'concat' for lists
hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f mempty

-- | Given a list of Doc values, if there is more than 1 element, separate with given punctuator Doc value, e.g., a:b:[] -> a, b
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p = \case
    []       -> []
    [d     ] -> [d]
    (d : ds) -> (d <> p) : punctuate p ds

--
-- Escapes moved to Util.hs
--

character :: Char -> Doc
character c = escaped c text hexEscape char

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


-- | Prettify a delimited structure surrounded by a prefix and suffix, e.g.,  arrays and objects
delimited :: Char -> Char -> (a -> Doc) -> [a] -> Doc
delimited left right item =
    surround left right . fsep . punctuate (char ',') . map item

-- | Combine a list of Doc values into one, wrapping lines if necessary
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- | Inserts a new line if current line is too wide, or a space otherwise. @group@ provides alternative representations of line for rendering purposes.
softline :: Doc
softline = group line

-- | Since Doc doesn't have any rendering information, when we encounter a softline, we will maintain two alternative representations of the Doc using the @Union@ constructor.
group :: Doc -> Doc
group x = flatten x `Union` x

-- | Combine two lines into a single longer line by replacing a @Line@ with a space. *Note* that the left of each @Union@ has a length at least as long as the right.
flatten :: Doc -> Doc
flatten = \case
    x `Concat` y -> flatten x `Concat` flatten y
    Line         -> char ' '
    x `Union` _  -> flatten x
    other        -> other

-- | Represent document with minimal spacing
compact :: Doc -> String
compact x = transform [x]
  where
    transform = \case
        []       -> ""
        (d : ds) -> case d of
            Empty        -> transform ds
            Char c       -> c : transform ds
            Text t       -> t ++ transform ds
            Line         -> '\n' : transform ds
            a `Concat` b -> transform (a : b : ds)
            _ `Union`  b -> transform (b : ds)

-- | Format Doc given max column size (i.e., line width)
pretty :: Int -> Doc -> [Char]
pretty width x = best 0 [x]
  where
    best col (d : ds) = case d of
        Empty        -> best col ds
        Char c       -> c : best (col + 1) ds
        Text t       -> t ++ best (col + length t) ds
        Line         -> '\n' : best 0 ds
        a `Concat` b -> best col (a : b : ds)
        a `Union`  b -> nicest col (best col (a : ds)) (best col (d : ds))
    best _ _ = ""
    nicest col a b | (width - least) >- a = a
                   | otherwise            = b
        where least = min width col

-- | Whether a single line of a rendered Doc value will fit into a given number of columns
(>-) :: Int -> String -> Bool
w >- _ | w < 0   = False
w >- ""          = True
w >- ('\n' : _ ) = True
w >- (c    : cs) = (w - 1) >- cs

-- CONSTRUCTOR FUNCTIONS
line :: Doc
line = Line

char :: Char -> Doc
char = Char

string :: String -> Doc
string = surround '"' '"' . hcat . map character

text :: String -> Doc
text = \case
    "" -> Empty
    s  -> Text s

double :: Double -> Doc
double d = text (show d)
