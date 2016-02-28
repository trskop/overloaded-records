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
-- This module defines the `IsLabel` class is used by the OverloadedLabels
-- extension.  See the
-- <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/OverloadedLabels wiki page>
-- for more details.
--
-- The key idea is that when GHC sees an occurrence of the new
-- overloaded label syntax @#foo@, it is replaced with
--
-- > fromLabel (proxy# :: Proxy# "foo") :: alpha
--
-- plus a wanted constraint @IsLabel "foo" alpha@.
module Data.OverloadedLabels
    (
    -- * Oveloaded Labels
      IsLabel(..)
    )
  where

#ifdef HAVE_OVERLOADED_LABELS
import GHC.OverloadedLabels (IsLabel(fromLabel))
#else
import GHC.TypeLits (Symbol)
import GHC.Exts (Proxy#)
#endif


#ifndef HAVE_OVERLOADED_LABELS
class IsLabel (l :: Symbol) a where
    fromLabel :: Proxy# l -> a
#endif
