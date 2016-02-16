{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.OverloadedRecords
    (
    -- * Oveloaded Labels
      IsLabel(..)

    -- * Overloaded Records
    , FieldType
    , HasField(..)

    , UpdateType
    , UpdateField(..)
    )
  where

import Data.Bool (Bool(False, True))
import Data.Functor (Functor, (<$>))
import GHC.TypeLits (Symbol)
import GHC.Exts (Proxy#)

#ifdef HAVE_OVERLOADED_LABELS
import GHC.OverloadedLabels (IsLabel(fromLabel))
#endif


#ifndef HAVE_OVERLOADED_LABELS
class IsLabel (l :: Symbol) a where
    fromLabel :: Proxy# l -> a
#endif

type family FieldType (l :: Symbol) (s :: *) :: *
type family UpdateType (l :: Symbol) (s :: *) (a :: *) :: *

-- Definition of this class is based on: https://phabricator.haskell.org/D1687
class HasField (l :: Symbol) s a | l s -> a where
    getField :: Proxy# l -> s -> a

class
    ( HasField l s a
    , FieldType l s ~ a
    , UpdateType l s a ~ s
    ) => UpdateField l s a
  where
    updateField :: Proxy# l -> s -> a -> UpdateType l s a

type family FromArrow (a :: *) :: Bool where
    FromArrow (x -> y) = 'True
    FromArrow t        = 'False

class
    ( z ~ FromArrow x
    ) => IsFieldFunction (l :: Symbol) x y (z :: Bool) | l y -> x
  where
    fieldFunction :: Proxy# l -> x -> y

instance IsFieldFunction l x y (FromArrow x) => IsLabel l (x -> y) where
    fromLabel = fieldFunction

-- @'Functor' f => 'Proxy#' l -> (a -> f b) -> s -> f t@
instance
    ( Functor f
    , HasField l s a
    , UpdateField l s b
    , FieldType l s ~ a
    , f (UpdateType l s b) ~ ft
    ) => IsFieldFunction l (a -> f b) (s -> ft) 'True
  where
    fieldFunction proxy f s = updateField proxy s <$> f (getField proxy s)

-- | @'Proxy#' l -> r -> a@
instance
    ( HasField l s a
    , FieldType l s ~ a
    , FromArrow s ~ 'False
    ) => IsFieldFunction l s a 'False
  where
    fieldFunction = getField
