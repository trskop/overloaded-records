{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
