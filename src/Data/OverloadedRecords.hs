{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.OverloadedRecords
    (
    -- * Oveloaded Labels
      module Data.OverloadedLabels

    -- * Overloaded Records
    , FieldType
    , HasField(..)

    , UpdateType
    , SetField(..)
    )
  where

import Data.Bool (Bool(False, True))
import Data.Functor (Functor, (<$>))
import GHC.TypeLits (Symbol)
import GHC.Exts (Proxy#)

import Data.OverloadedLabels


-- | When accessing field named @l :: Symbol@ of a record @s :: *@, then the
-- type of the value in that field is @'FieldType' l s@.
type family FieldType (l :: Symbol) (s :: *) :: *

-- | If field @l :: Symbol@ of a record @s :: *@ is set to new value which has
-- type @a :: *@, then the modified record will have type @'UpdateType' l s a@.
type family UpdateType (l :: Symbol) (s :: *) (a :: *) :: *

-- Definition of this class is based on: https://phabricator.haskell.org/D1687
class HasField (l :: Symbol) s a | l s -> a where
    -- | Get value of a field.
    getField :: Proxy# l -> s -> a

class
    ( HasField l s a
    , FieldType l s ~ a
    , UpdateType l s a ~ s
    ) => SetField l s a
  where
    setField :: Proxy# l -> s -> a -> UpdateType l s a

type family FromArrow (a :: *) :: Bool where
    FromArrow (x -> y) = 'True
    FromArrow t        = 'False

class
    ( z ~ FromArrow x
    ) => IsFieldAccessor (l :: Symbol) x y (z :: Bool) | l y -> x
  where
    fieldAccessor :: Proxy# l -> x -> y

instance IsFieldAccessor l x y (FromArrow x) => IsLabel l (x -> y) where
    fromLabel = fieldAccessor

-- | @'Functor' f => 'Proxy#' l -> (a -> f b) -> s -> f t@
instance
    ( Functor f
    , HasField l s a
    , SetField l s b
    , FieldType l s ~ a
    , UpdateType l s b ~ t
    ) => IsFieldAccessor l (a -> f b) (s -> f t) 'True
  where
    fieldAccessor proxy f s = setField proxy s <$> f (getField proxy s)

-- | @'Proxy#' l -> r -> a@
instance
    ( HasField l s a
    , FieldType l s ~ a
    , FromArrow s ~ 'False
    ) => IsFieldAccessor l s a 'False
  where
    fieldAccessor = getField
