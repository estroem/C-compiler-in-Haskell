import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))

import Data.Maybe

data Parser a = P (String -> [(a, String)])

instance Functor (Parser) where
    fmap f p = P $ \ inp -> [(f a, b) | (a, b) <- parse p inp]

instance Applicative (Parser) where
    pure a = P $ \ b -> [(a, b)]
    (<*>) p1 p2 = P $ \ inp -> [(f a, b) | (f, inp') <- parse p1 inp,
                                           (a, b) <- parse p2 inp']

instance Monad (Parser) where
    return = pure
    (>>=) p f = P $ \ inp -> case parse p inp of
                                [] -> []
                                [(v, inp')] -> parse (f v) inp'

(<|>) ::  Parser a -> Parser a -> Parser a
(<|>) p1 p2 = P $ \ inp -> case parse p1 inp of
                               [] -> parse p2 inp
                               a -> a

(<$>) :: (a -> b) -> Parser a -> Parser b
(<$>) = fmap

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

yst = fromJust $ pure 4

many1 :: Parser a -> Parser [a]
many1 e = (:) <$> e <*> many0 e 

many0 :: Parser a -> Parser [a]
many0 e = many1 e <|> return []

sepBy1 :: Parser b -> Parser a -> Parser [a]
sepBy1 sep p = (:) <$> p <*> many0 (sep >> p)

sepBy0 :: Parser b -> Parser a -> Parser [a]
sepBy0 sep p = sepBy1 sep p <|> return [] 

parens :: Parser a -> Parser a
parens p = do
    char '('
    r <- p
    char ')'
    return r

char :: Char -> Parser Char
char ch = P $ \ inp ->
    if null inp || head inp /= ch
        then []
        else [(ch, tail inp)]