{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:       $HEADER$
-- Description:  Derive instances of IsLabel magic class for OverloadedLabels.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Derive instances of 'IsLabel' magic class for OverloadedLabels.
--
-- If you are using GHC, then you should use OverloadedLabels language
-- extension and not this module.
module Data.OverloadedLabels.TH
    ( label
    , labelNoSig
    , labels
    , labelsNoSig
    )
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

import Data.OverloadedLabels (IsLabel(fromLabel))


-- | Define overloaded label:
--
-- @
-- \<label-name\> :: 'IsLabel' \"\<label-name\>\" a => a
-- \<label-name\> = 'fromLabel' ('proxy#' :: 'Proxy#' \"\<label-name\>\")
-- @
label :: String -> DecsQ
label l = (:) <$> sig <*> labelNoSig l
  where
    sig = mkName l `sigD` [t|forall a. IsLabel $(litT $ strTyLit l) a => a|]

-- | Define overloaded label, but without a type signature:
--
-- @
-- \<label-name\> = 'fromLabel' ('proxy#' :: 'Proxy#' \"\<label-name\>\")
-- @
labelNoSig :: String -> DecsQ
labelNoSig l = sequenceA [varP (mkName l) `valD` normalB body $ []]
  where
    body = [| fromLabel (proxy# :: Proxy# $(litT $ strTyLit l)) |]

-- | Same as 'label', but defines multiple overloaded labels at once.
labels :: [String] -> DecsQ
labels = fmap concat . mapM label

-- | Same as 'labelsNoSig', but defines multiple overloaded labels at once.
labelsNoSig :: [String] -> DecsQ
labelsNoSig = fmap concat . mapM labelNoSig
