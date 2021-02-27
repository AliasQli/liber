{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Parser.Helper where

import Control.Monad
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Parser.Type
import Text.Parsec

type Parse t = forall m. forall s. (Monad m, Stream s m Char) => ParsecT s St m t

incIndent n = modifyState (\st@St{..} -> st{indent = indent + n})

decIndent n = modifyState (\st@St{..} -> st{indent = indent - n})

number parser = length <$> many (try parser)

number1 parser = length <$> many1 (try parser)

whites :: Parse Int
whites = number (char ' ')

whites1 :: Parse Int
whites1 = number1 (char ' ')

whiteLine :: Parse ()
whiteLine = whites >> endOfLine $> ()

oneOrTwoLine :: Parse ()
oneOrTwoLine = whiteLine >> optional whiteLine

multiLines' prefix p = do
  St{..} <- getState
  h <- p 0
  t <-
    many $
      try
        ( do
            n <- number (whiteLine >> string prefix)
            string (replicate indent ' ')
            p (n -1)
        )
  endOfLine
  return (h : t)

multiStrings' s p = multiLines' s (\n -> (replicate n '\n' <>) <$> p)

multiLines :: Stream s m Char => (Int -> ParsecT s St m a) -> ParsecT s St m [a]
multiLines = multiLines' ""

multiStrings p = multiLines (\n -> (replicate n '\n' <>) <$> p)

str :: Parse String
str = try exposed <|> enclosed
  where
    exposed = many1 $ noneOf "\" \n"
    quoted =
      try (char '\\' >> char '"') <|> noneOf "\""
    enclosed = do
      char '"'
      s <- many quoted
      char '"'
      return s

name :: Parse String
name = do
  h <- letter
  t <- many (alphaNum <|> char '_')
  return (h : t)

varName :: Parse String
varName = do
  nm <- name
  St{..} <- getState
  unless (isJust $ M.lookup nm vars) $ error ("Variable " <> nm <> " is not defined.")
  return nm

expr :: Parse Expr
expr =
  try (Var <$> varName)
    <|> ( do
            sign <- option 1 $ (char '+' $> 1) <|> (char '-' $> -1)
            n <- foldl (\n x -> 10 * n + ord x - ord '0') 0 <$> many1 digit
            return $ Lit (sign * n)
        )
