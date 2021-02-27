{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.ToTemplate where

import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Language.Haskell.TH
import Parser.Type
import qualified Data.ByteString as B
import Display

class ToTemplate a where
  toTemplate :: a -> Q Exp

instance ToTemplate String where
  toTemplate s = litE (stringL s)

instance ToTemplate Int where
  toTemplate i = varE 'fromEnum `appE` litE (integerL (toInteger i))

instance ToTemplate a => ToTemplate (Maybe a) where
  toTemplate (Just a) = conE 'Just `appE` toTemplate a
  toTemplate Nothing = conE 'Nothing

instance {-# OVERLAPPABLE #-} ToTemplate a => ToTemplate [a] where
  toTemplate as = listE (toTemplate <$> as)

instance (ToTemplate a, ToTemplate b) => ToTemplate (a, b) where
  toTemplate (a, b) = tupE [toTemplate a, toTemplate b]

instance (ToTemplate k, ToTemplate a) => ToTemplate (Map k a) where
  toTemplate map = varE 'M.fromList `appE` toTemplate (M.toList map)

instance ToTemplate Expr where
  toTemplate (Var s) = conE 'Var `appE` toTemplate s
  toTemplate (Lit i) = conE 'Lit `appE` toTemplate i

instance ToTemplate Command where
  toTemplate (I s) = conE 'I `appE` toTemplate s
  toTemplate (Aug s e) = conE 'Aug `appE` toTemplate s `appE` toTemplate e
  toTemplate (Min s e) = conE 'Min `appE` toTemplate s `appE` toTemplate e
  toTemplate (Mul s e) = conE 'Mul `appE` toTemplate s `appE` toTemplate e
  toTemplate (Div s e) = conE 'Div `appE` toTemplate s `appE` toTemplate e
  toTemplate (Ads s e) = conE 'Ads `appE` toTemplate s `appE` toTemplate e

instance ToTemplate Boolean where
  toTemplate (Est e1 e2) = conE 'Est `appE` toTemplate e1 `appE` toTemplate e2
  toTemplate (Plus e1 e2) = conE 'Plus `appE` toTemplate e1 `appE` toTemplate e2
  toTemplate (Infra e1 e2) = conE 'Infra `appE` toTemplate e1 `appE` toTemplate e2
  toTemplate (Et b1 b2) = conE 'Et `appE` toTemplate b1 `appE` toTemplate b2
  toTemplate (Aut b1 b2) = conE 'Aut `appE` toTemplate b1 `appE` toTemplate b2
  toTemplate (Non b) = conE 'Non `appE` toTemplate b

instance ToTemplate Sentence where
  toTemplate (Speak may ss) = conE 'Speak `appE` toTemplate may `appE` toTemplate ss
  toTemplate (Electio s ss) = conE 'Electio `appE` toTemplate s `appE` toTemplate ss
  toTemplate (Si b sen) = conE 'Si `appE` toTemplate b `appE` toTemplate sen
  toTemplate (Age commands) = conE 'Age `appE` toTemplate commands

instance ToTemplate St where
  toTemplate (St indent actors vars) = conE 'St `appE` toTemplate indent `appE` toTemplate actors `appE` toTemplate vars

instance ToTemplate Script where
  toTemplate (Script status chapters) = conE 'Script `appE` toTemplate status `appE` toTemplate chapters
  
embedScript path = do
  file <- runIO $ B.readFile path
  let script = getScript file
  toTemplate script