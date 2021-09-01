{-# LANGUAGE TemplateHaskell #-}

module MegaParser.Expr where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import           Data.Map                   (Map)
import qualified Data.Map                   as M

newtype VarName = VarName String deriving (Eq, Ord)

newtype Episode = Episode String

type Vars = Map VarName Int

data Expr
  = Var VarName
  | Lit Int
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr :% Expr
  | Expr :&& Expr
  | Expr :|| Expr
  | Not Expr

makeBaseFunctor ''Expr

evalExpr :: Map VarName Int -> Expr -> Either String Int
evalExpr map = cata go
  where
    and 0 _ = 0
    and _ 0 = 0
    and _ _ = 1
    or 0 0 = 0
    or _ _ = 1
    not 0 = 1
    not _ = 0
    go (VarF name@(VarName s)) = maybe (Left $ s <> " is not defined") Right $ M.lookup name map
    go (LitF lit) = Right lit
    go (a :+$ b) = (+) <$> a <*> b
    go (a :-$ b) = (-) <$> a <*> b
    go (a :*$ b) = (*) <$> a <*> b
    go (a :/$ b) = div <$> a <*> b
    go (a :%$ b) = mod <$> a <*> b
    go (a :&&$ b) = and <$> a <*> b
    go (a :||$ b) = or <$> a <*> b
    go (NotF a) = not <$> a

data Cmd
  = VarName := Expr
  | If Expr [Cmd]
  | While Expr [Cmd]
  | Goto Episode
  | Input VarName (Int, Int)
