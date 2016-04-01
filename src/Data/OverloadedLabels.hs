{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Magic class for OverloadedLabels.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
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
    )
  where

#ifdef HAVE_OVERLOADED_LABELS
import GHC.OverloadedLabels
#else
import GHC.TypeLits (Symbol)
import GHC.Exts (Proxy#)


class IsLabel (l :: Symbol) a where
    fromLabel :: Proxy# l -> a
#endif
