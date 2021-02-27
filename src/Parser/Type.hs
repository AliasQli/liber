{-# LANGUAGE RecordWildCards #-}

module Parser.Type where

import Data.Map (Map)
import qualified Data.Map as M

type Status = Map String Int

data St = St
  { indent :: Int
  , actors :: [(String, String)]
  , vars :: Status
  }
  deriving (Show, Eq)

initState = St 0 [] M.empty

data Script = Script Status [Chapter]
  deriving (Show, Eq)

type Chapter = (String, [Sentence])

data Sentence
  = Speak (Maybe String) [String]
  | Electio String [String]
  | Si Boolean Sentence
  | Age [Command]
  deriving (Show, Eq)

data Expr
  = Var String
  | Lit Int
  deriving (Show, Eq)

data Boolean
  = Est Expr Expr
  | Plus Expr Expr
  | Infra Expr Expr
  | Et Boolean Boolean
  | Aut Boolean Boolean
  | Non Boolean
  deriving (Show, Eq)

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

data Command
  = I String
  | Aug String Expr
  | Min String Expr
  | Mul String Expr
  | Div String Expr
  | Ads String Expr
  deriving (Show, Eq)

predTable = [("Est", Est), ("Plus", Plus), ("Infra", Infra)]

op2Table = [("Et", Et), ("Aut", Aut)]

commandTable = [("Aug", Aug), ("Min", Min), ("Mul", Mul), ("Div", Div), ("Ads", Ads)]