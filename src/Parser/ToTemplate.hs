{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Parser.ToTemplate(ToTemplate, toTemplate) where

import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Language.Haskell.TH

class ToTemplate a where
  toTemplate :: a -> Q Exp
  default toTemplate :: (Generic a, GToTemplate (Rep a)) => a -> Q Exp
  toTemplate a = unExpX $ gtoTemplate (from a)

data ExpX
  = Con
      { unExpX :: ExpQ
      }
  | Fields
      { fields :: [ExpQ]
      }

class GToTemplate f where
  gtoTemplate :: f a -> ExpX

instance GToTemplate V1 where
  gtoTemplate _ = error "Type without any data constructor may not be an instance of ToTemplate."

instance GToTemplate U1 where
  gtoTemplate _ = Fields []

instance ToTemplate c => GToTemplate (K1 R c) where
  gtoTemplate (K1 x) = Fields [toTemplate x]

instance (GToTemplate a, GToTemplate b) => GToTemplate (a :+: b) where
  gtoTemplate (L1 x) = gtoTemplate x
  gtoTemplate (R1 x) = gtoTemplate x

instance (GToTemplate a, GToTemplate b) => GToTemplate (a :*: b) where
  gtoTemplate (a :*: b) = Fields $ fields (gtoTemplate a) ++ fields (gtoTemplate b)

instance (GToTemplate f, Constructor c) => GToTemplate (M1 C c f) where
  gtoTemplate (M1 (x :: f a)) = Con $ foldl appE cons xs
    where
      nm = mkName $ conName (undefined :: M1 C c f p)
      cons = conE nm
      xs = fields $ gtoTemplate x

instance {-# OVERLAPPABLE #-} (GToTemplate f) => GToTemplate (M1 i c f) where
  gtoTemplate (M1 x) = gtoTemplate x

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
