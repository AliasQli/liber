{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Parser.Type where

import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Parser.ToTemplate as Export
import Data.Vector (Vector)

type Status = Map String Int

data St = St
  { indent :: Int
  , actors :: [(String, String)]
  , vars :: Status
  }
  deriving (Show, Eq, Generic)

instance ToTemplate St

data Script = Script Status (Vector Chapter)
  deriving (Show, Eq, Generic)

instance ToTemplate Script

type Chapter = (String, Vector Sentence)

data Sentence
  = Speak (Maybe String) [String]
  | Electio String [String]
  | Si Boolean Sentence
  | Age [Command]
  deriving (Show, Eq, Generic)

instance ToTemplate Sentence

data Expr
  = Var String
  | Lit Int
  deriving (Show, Eq, Generic)

instance ToTemplate Expr

data Boolean
  = Est Expr Expr
  | Plus Expr Expr
  | Infra Expr Expr
  | Et Boolean Boolean
  | Aut Boolean Boolean
  | Non Boolean
  deriving (Show, Eq, Generic)

instance ToTemplate Boolean

data Command
  = I String
  | Aug String Expr
  | Min String Expr
  | Mul String Expr
  | Div String Expr
  | Ads String Expr
  deriving (Show, Eq, Generic)

instance ToTemplate Command

initState = St 0 [] M.empty

predTable = [("Est", Est), ("Plus", Plus), ("Infra", Infra)]

op2Table = [("Et", Et), ("Aut", Aut)]

commandTable = [("Aug", Aug), ("Min", Min), ("Mul", Mul), ("Div", Div), ("Ads", Ads)]

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