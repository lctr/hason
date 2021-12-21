{-# LANGUAGE LambdaCase #-}
module Parser where

import           Control.Applicative
import           Data.Char
import           Util
import           Value

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
        (x, s') <- p s
        Just (f x, s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (Parser a) <*> (Parser b) = Parser $ \s -> do
        (f, rest ) <- a s
        (x, rest') <- b rest
        Just (f x, rest')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

-- | Consume spaces, discarding result
spaces :: Parser String
spaces = satisfy isSpace

-- | Consume characters that satisfy a given predicate.
satisfy :: (Char -> Bool) -> Parser String
satisfy pred =
    Parser $ \s -> let (token, rest) = span pred s in Just (token, rest)

-- | Consume a specific character
char :: Char -> Parser Char
char x = Parser $ \case
    parsed : rest | x == parsed -> Just (parsed, rest)
    _                           -> Nothing

-- | TODO: explain
symbol :: String -> Parser String
symbol = traverse char

-- | Ensure that a list of parsers is non-empty
nonempty :: Parser [a] -> Parser [a]
nonempty (Parser p) = Parser $ \s -> do
    (xs, rest) <- p s
    if null xs then Nothing else Just (xs, rest)

-- | Parse internals of a string
word :: Parser String
word = satisfy (/= '"') <|> satisfy (/= '\"') <|> satisfy isEscapable

string :: Parser String
string = enclose '"' '"' word

trim :: Parser a -> Parser a
trim f = spaces *> f <* spaces

enclose :: Char -> Char -> Parser a -> Parser a
enclose left right f = char left *> f <* char right

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (trim sep *> item) <|> pure []

parseNull :: Parser Value
parseNull = Null <$ symbol "null"

parseBool :: Parser Value
parseBool = f <$> (symbol "true" <|> symbol "false")
    where f x = Bool $ x == "true"

parseNum :: Parser Value
parseNum = f <$> nonempty (satisfy isDigit) where f ds = Num $ read ds

parseStr :: Parser Value
parseStr = Str <$> string

parseArr :: Parser Value
parseArr = Arr <$> (open *> trim elements <* close)
  where
    open     = char '[' *> spaces
    close    = spaces <* char ']'
    elements = sepBy (trim (char ',')) parseJson

parseObj :: Parser Value
parseObj = Obj <$> (open *> entry pair <* close)
  where
    open  = char '{' *> spaces
    close = char '}' <* spaces
    entry = sepBy (trim (char ','))
    pair  = (\k _ v -> (k, v)) <$> string <*> trim (char ':') <*> parseJson

parseJson :: Parser Value
parseJson = parseNull <|> parseNum <|> parseStr <|> parseArr <|> parseObj

