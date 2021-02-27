{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Parser.Parse where

import Control.Monad
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Functor
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Parser.Helper
import Parser.Type
import Text.Parsec

actores :: Parse ()
actores = do
  string "Actores"
  oneOrTwoLine
  actors' <-
    many
      ( do
          n <- name
          whites1
          v <- str
          oneOrTwoLine
          return (n, v)
      )
  modifyState (\st@St{..} -> st{actors = actors'})

status :: Parse ()
status = do
  string "Status"
  oneOrTwoLine
  status' <-
    M.fromList
      <$> many
        ( do
            n <- name
            whites1
            v <- str
            oneOrTwoLine
            return (n, read v :: Int)
        )
  modifyState (\st@St{..} -> st{vars = status'})

narrate :: Parse Sentence
narrate = do
  char '>'
  n <- whites
  incIndent n
  text <- multiStrings' ">" $ many1 (noneOf "\n")
  decIndent n
  optional whiteLine
  return $ Speak Nothing text

speak :: Parse Sentence
speak = do
  St{..} <- getState
  nm <- name
  n <- whites1
  let ind = length nm + n
  incIndent ind
  text <- multiStrings $ many1 (noneOf "\n")
  decIndent ind
  optional whiteLine
  let str = fromMaybe nm $ lookup nm actors
  return $ Speak (Just str) text

electio :: Parse Sentence
electio = do
  string "Electio"
  whites1
  nm <- varName
  whiteLine
  n <- whites1
  incIndent n
  text <- multiStrings $ many1 (noneOf "\n")
  decIndent n
  optional whiteLine
  return $ Electio nm text

command :: Parse Command
command = do
  nm <- name
  whites1
  optionMaybe (try $ string "I")
    >>= \case
      Just _ -> return $ I nm
      Nothing -> do
        St{..} <- getState
        unless (isJust $ M.lookup nm vars) $ error ("Variable " <> nm <> " is not defined.")
        e <- expr
        whites1
        let parsers = (\(a, b) -> try $ string a $> b) <$> commandTable
        con <- foldr1 (<|>) parsers
        return $ con nm e

age :: Parse Sentence
age = do
  string "Age"
  St{..} <- getState
  optional . try $ whiteLine >> string (replicate indent ' ')
  n <- whites1
  incIndent n
  t <- multiLines $ const command
  decIndent n
  optional whiteLine
  return $ Age t

symbol :: Parse Symbol
symbol =
  try
    ( do
        let parsers = (\(a, b) -> try $ string a $> Pred b) <$> predTable
        foldr1 (<|>) parsers
    )
    <|> try
      ( do
          let parsers = (\(a, b) -> try $ string a $> Op2 b) <$> op2Table
          foldr1 (<|>) parsers
      )
    <|> try (string "Non" $> Op1 Non)
    <|> Expr <$> expr

data Elem
  = BoolElem Boolean
  | ExprElem Expr
  deriving (Show, Eq)

cond :: Parse Boolean
cond = do
  syms <- symbol `sepBy1` whites1
  let f (Expr e) stack = ExprElem e : stack
      f (Pred p) (ExprElem e1 : ExprElem e2 : stack) = BoolElem (p e1 e2) : stack
      f (Op2 op) (BoolElem e1 : BoolElem e2 : stack) = BoolElem (op e1 e2) : stack
      f (Op1 op) (BoolElem e : stack) = BoolElem (op e) : stack
      [BoolElem bool] = foldl' (flip f) [] syms
  return bool

si :: Parse Sentence
si = do
  string "Si"
  St{..} <- getState
  optional . try $ whiteLine >> string (replicate indent ' ')
  n <- whites1
  incIndent n
  c <- cond
  whiteLine
  string (replicate (indent + n) ' ')
  a <- try electio <|> try age <|> si
  decIndent n
  optional whiteLine
  return $ Si c a

caput :: Parse (String, [Sentence])
caput = do
  string "Caput"
  whites1
  nm <- name
  oneOrTwoLine
  (nm,) <$> many (try electio <|> try age <|> try si <|> try speak <|> narrate)

script :: Parse Script
script = do
  many whiteLine
  let whiteLine1 = many1 whiteLine
  optional $ try (actores >> whiteLine1)
  optional $ try (status >> whiteLine1)
  chapters <- caput `sepBy1` whiteLine1
  spaces
  eof
  St{..} <- getState
  return $ Script vars chapters

parseScriptFile fname = runParser script initState fname <$> readFile fname

parseScript :: Stream s Identity Char => s -> Either ParseError Script
parseScript = runParser script initState ""
