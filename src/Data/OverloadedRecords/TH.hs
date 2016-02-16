{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.OverloadedRecords.TH
  where

import Control.Applicative (Applicative((<*>)))
import Data.Foldable (concat)
import Data.Function ((.))
import Data.Functor (Functor(fmap), (<$>))
import Data.String (String)
import Data.Traversable (mapM, sequenceA)
import GHC.Exts (Proxy#, proxy#)

import Language.Haskell.TH
    ( DecQ
    , DecsQ
    , litT
    , strTyLit
    , varE
    , mkName
    , varP
    , sigD
    )

import Data.OverloadedRecords


label :: String -> DecsQ
label l = (:) <$> signature <*> labelNoSig l
  where
    signature =
        mkName l `sigD` [t|forall a. IsLabel => $(litT (strTyLit l)) a => a|]

labelNoSig :: String -> DecsQ
labelNoSig l =
    [d| $(varP (mkName l)) = fromLabel (proxy# :: Proxy# $(labelType)) |]
  where
    labelType = litT (strTyLit l)

labels :: [String] -> DecsQ
labels = fmap concat . mapM label

labelsNoSig :: [String] -> DecsQ
labelsNoSig = fmap concat . mapM labelNoSig
