{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}   -- For type equality constraint.
-- |
-- Module:       $HEADER$
-- Description:  Magic class for OverloadedLabels.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- This module defines the `IsLabel` class which is used by the OverloadedLabels
-- language extension.  See the
-- <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/OverloadedLabels wiki page>
-- for more details.
--
-- The key idea is that when GHC sees an occurrence of the new
-- overloaded label syntax @#foo@, it is replaced with
--
-- > fromLabel (proxy# :: Proxy# "foo") :: alpha
--
-- plus a wanted constraint @IsLabel "foo" alpha@.
--
-- On /GHC >=8.0.1/ we just reexport "GHC.OverloadedLabels" module.
module Data.OverloadedLabels
    (
    -- * Oveloaded Labels
#ifdef HAVE_OVERLOADED_LABELS
      module GHC.OverloadedLabels
#else
      IsLabel(..)
#endif

    , Label(..)
    , getLabel
    , showLabel
    , unLabel
    )
  where

import Data.Function ((.), id)
import Data.String (String)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal')
import GHC.Exts (Proxy#)
import Text.Show (Show(show, showsPrec), showChar, showString)

#ifdef HAVE_OVERLOADED_LABELS
import GHC.OverloadedLabels
#else


class IsLabel (l :: Symbol) a where
    fromLabel :: Proxy# l -> a
#endif

-- |
-- /Since 0.4.2.0/
data Label (l :: Symbol) = Label (Proxy# l)
  deriving Typeable

-- |
-- /Since 0.4.2.0/
instance KnownSymbol l => Show (Label l) where
    showsPrec _ (Label p) = showChar '#' . showString (symbolVal' p)

-- |
-- /Since 0.4.2.0/
instance (l1 ~ l2) => IsLabel l1 (Label l2) where
    fromLabel = Label

-- | Type restricted version of 'show'.
--
-- >>> showLabel #foo
-- #foo
--
-- /Since 0.4.2.0/
showLabel :: KnownSymbol l => Label l -> String
showLabel = show

-- | Type restricted identity function.
--
-- /Since 0.4.2.0/
getLabel :: Label l -> Label l
getLabel = id

-- | Same as 'fromLabel', but works for 'Label' data type.
--
-- /Since 0.4.2.0/
unLabel :: IsLabel l a => Label l -> a
unLabel (Label proxy) = fromLabel proxy
