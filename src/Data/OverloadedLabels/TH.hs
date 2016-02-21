{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.OverloadedLabels.TH
  where

import Control.Applicative (Applicative((<*>)))
import Data.Foldable (concat)
import Data.Function ((.), ($))
import Data.Functor (Functor(fmap), (<$>))
import Data.String (String)
import Data.Traversable (mapM, sequenceA)
import GHC.Exts (Proxy#, proxy#)

import Language.Haskell.TH
    ( DecsQ
    , litT
    , mkName
    , normalB
    , sigD
    , strTyLit
    , valD
    , varP
    )

import Data.OverloadedLabels


label :: String -> DecsQ
label l = (:) <$> sig <*> labelNoSig l
  where
    sig = mkName l `sigD` [t|forall a. IsLabel $(litT $ strTyLit l) a => a|]

labelNoSig :: String -> DecsQ
labelNoSig l = sequenceA [varP (mkName l) `valD` normalB body $ []]
  where
    body = [| fromLabel (proxy# :: Proxy# $(litT $ strTyLit l)) |]

labels :: [String] -> DecsQ
labels = fmap concat . mapM label

labelsNoSig :: [String] -> DecsQ
labelsNoSig = fmap concat . mapM labelNoSig
