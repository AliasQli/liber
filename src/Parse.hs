{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Parse where

import Control.Monad
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import GHC.IO (unsafePerformIO)
import Text.Parsec
import Data.Text(Text)

data St = St
  { indent :: Int
  , actors :: [(String, String)]
  , vars :: Status
  }
  deriving (Show, Eq)

incIndent n st@St{..} = st{indent = indent + n}

decIndent n st@St{..} = st{indent = indent - n}

initState = St 0 [] M.empty

type Status = Map String Int

data Script = Script Status [Chapter]
  deriving (Show, Eq)

type Chapter = (String, [Sentence])

data Sentence
  = Speak (Maybe String) [String]
  | Electio String [String]
  | Si Boolean Sentence
  | Age [Command]
  deriving (Show, Eq)

data Expr = Var String | Lit Int
  deriving (Show, Eq)

fetch map (Var s) = fromJust $ M.lookup s map
fetch _ (Lit i) = i

data Boolean
  = Est Expr Expr
  | Plus Expr Expr
  | Infra Expr Expr
  | Et Boolean Boolean
  | Aut Boolean Boolean
  | Non Boolean
  deriving (Show, Eq)

eval map = \case
  Est e1 e2 -> fetch' e1 == fetch' e2
  Plus e1 e2 -> fetch' e1 > fetch' e2
  Infra e1 e2 -> fetch' e1 < fetch' e2
  Et b1 b2 -> eval' b1 && eval' b2
  Aut b1 b2 -> eval' b1 || eval' b2
  Non b -> not (eval' b)
  where
    fetch' = fetch map
    eval' = eval map

data Symbol
  = Pred (Expr -> Expr -> Boolean)
  | Op1 (Boolean -> Boolean)
  | Op2 (Boolean -> Boolean -> Boolean)
  | Expr Expr

instance Show Symbol where
  show (Pred _) = "Pred"
  show (Op1 _) = "Op1"
  show (Op2 _) = "Op2"
  show (Expr e) = show e

data Elem = BoolElem Boolean | ExprElem Expr
  deriving (Show, Eq)

predTable = [("Est", Est), ("Plus", Plus), ("Infra", Infra)]

op2Table = [("Et", Et), ("Aut", Aut)]

data Command
  = I String
  | Aug String Expr
  | Min String Expr
  | Mul String Expr
  | Div String Expr
  | Ads String Expr
  deriving (Show, Eq)

action map = foldl' f (map, Nothing)
  where
    update' f k map = M.update (Just . f) k map
    fetch' = fetch map
    f tup@(_, Just _) _ = tup
    f (map, Nothing) (I s) = (map, Just s)
    f (map, Nothing) (Aug s e) = (update' (+ fetch' e) s map, Nothing)
    f (map, Nothing) (Min s e) = (update' (\a -> a - fetch' e) s map, Nothing)
    f (map, Nothing) (Mul s e) = (update' (* fetch' e) s map, Nothing)
    f (map, Nothing) (Div s e) = (update' (`div` fetch' e) s map, Nothing)
    f (map, Nothing) (Ads s e) = (update' (const $ fetch' e) s map, Nothing)

commandTable = [("Aug", Aug), ("Min", Min), ("Mul", Mul), ("Div", Div), ("Ads", Ads)]

number parser = length <$> many (try parser)

number1 parser = length <$> many1 (try parser)

whites = number (char ' ')

whites1 = number1 (char ' ')

whiteLine = whites >> endOfLine $> ()

oneOrTwoLine = whiteLine >> optional whiteLine

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

name = do
  h <- letter
  t <- many (alphaNum <|> char '_')
  return (h : t)

varName = do
  nm <- name
  St{..} <- getState
  unless (isJust $ M.lookup nm vars) $ error ("Variable " <> nm <> " is not defined.")
  return nm

expr =
  try (Var <$> varName)
    <|> ( do
            sign <- option 1 $ (char '+' $> 1) <|> (char '-' $> -1)
            n <- foldl (\n x -> 10 * n + ord x - ord '0') 0 <$> many1 digit
            return $ Lit (sign * n)
        )

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

multiLines = multiLines' ""

multiStrings p = multiLines (\n -> (replicate n '\n' <>) <$> p)

narrate = do
  char '>'
  n <- whites
  modifyState (incIndent n)
  text <- multiStrings' ">" $ many1 (noneOf "\n")
  modifyState (decIndent n)
  optional whiteLine
  return $ Speak Nothing text

speak = do
  St{..} <- getState
  nm <- name
  n <- whites1
  let ind = length nm + n
  modifyState (incIndent ind)
  text <- multiStrings $ many1 (noneOf "\n")
  modifyState (decIndent ind)
  optional whiteLine
  let str = fromMaybe nm $ lookup nm actors
  return $ Speak (Just str) text

electio = do
  string "Electio"
  whites1
  nm <- varName
  whiteLine
  n <- whites1
  modifyState (incIndent n)
  text <- multiStrings $ many1 (noneOf "\n")
  modifyState (decIndent n)
  optional whiteLine
  return $ Electio nm text

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

age = do
  string "Age"
  St{..} <- getState
  optional . try $ whiteLine >> string (replicate indent ' ')
  n <- whites1
  modifyState (incIndent n)
  t <- multiLines $ const command
  modifyState (decIndent n)
  optional whiteLine
  return $ Age t

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

cond = do
  syms <- symbol `sepBy1` whites1
  let f (Expr e) stack = ExprElem e : stack
      f (Pred p) (ExprElem e1 : ExprElem e2 : stack) = BoolElem (p e1 e2) : stack
      f (Op2 op) (BoolElem e1 : BoolElem e2 : stack) = BoolElem (op e1 e2) : stack
      f (Op1 op) (BoolElem e : stack) = BoolElem (op e) : stack
      [BoolElem bool] = foldl' (flip f) [] syms
  return bool

si = do
  string "Si"
  St{..} <- getState
  optional . try $ whiteLine >> string (replicate indent ' ')
  n <- whites1
  modifyState (incIndent n)
  c <- cond
  whiteLine
  string (replicate (indent + n) ' ')
  a <- try electio <|> try age <|> si
  modifyState (decIndent n)
  optional whiteLine
  return $ Si c a

caput = do
  string "Caput"
  whites1
  nm <- name
  oneOrTwoLine
  (nm,) <$> many (try electio <|> try age <|> try si <|> try speak <|> narrate)

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

-- parseScriptFile fname = runParser script initState fname <$> readFile fname

parseScript :: Text -> Either ParseError Script
parseScript = runParser script initState ""
