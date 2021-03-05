{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Eval where

import Data.Foldable (foldl')
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Parser.Type

fetch map (Var s) = fromJust $ M.lookup s map
fetch _ (Lit i) = i

makeBaseFunctor ''Boolean

eval map = cata go
  where
    fetch' = fetch map
    go (EstF e1 e2) = fetch' e1 == fetch' e2
    go (PlusF e1 e2) = fetch' e1 > fetch' e2
    go (InfraF e1 e2) = fetch' e1 < fetch' e2
    go (EtF b1 b2) = b1 && b2
    go (AutF b1 b2) = b1 || b2
    go (NonF b) = not b

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
