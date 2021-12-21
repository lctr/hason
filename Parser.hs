{-# LANGUAGE LambdaCase #-}
module Parser where

import           Control.Applicative
import           Data.Char

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

string :: String -> Parser String
string = traverse char

-- | Ensure that a list of parsers is non-empty
nonempty :: Parser [a] -> Parser [a]
nonempty (Parser p) = Parser $ \s -> do
    (xs, rest) <- p s
    if null xs then Nothing else Just (xs, rest)

