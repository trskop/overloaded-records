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
-- |
-- Module:       $HEADER$
-- Description:  Magic classes for OverloadedRecordFields.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DataKinds, FlexibleInstances, FunctionalDependencies,
--               MagicHash, MultiParamTypeClasses, NoImplicitPrelude,
--               TypeFamilies, UndecidableInstances
--
-- Magic classes for OverloadedRecordFields.
--
-- Implementation is based on:
-- <https://github.com/adamgundry/records-prototype/blob/master/CoherentPrototype.hs>
-- by Adam Gundry under MIT License.
module Data.OverloadedRecords
    (
    -- * Oveloaded Labels
      module Data.OverloadedLabels

    -- * Overloaded Record Fields
    --
    -- ** Getter
    , FieldType
    , HasField(..)

    -- ** Setter
    , UpdateType
    , SetField(..)
    , Setter
    , set

    -- ** IsLabel For Getter and Lens
    , FromArrow
    , IsFieldAccessor(..)
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

-- | Definition of this class is based on:
-- <https://phabricator.haskell.org/D1687>
class HasField (l :: Symbol) s a | l s -> a where
    -- | Get value of a field.
    getField :: Proxy# l -> s -> a

class
    ( HasField l s b
    , FieldType l s ~ b
    ) => SetField l s b
  where
    -- | Set value of a field.
    setField :: Proxy# l -> s -> b -> UpdateType l s b

-- | Returns 'True' if type @a@ is a function.
type family FromArrow (a :: *) :: Bool where
    FromArrow (x -> y) = 'True
    FromArrow t        = 'False

-- | Distinguish between getter and lens.
class
    ( z ~ FromArrow x
    ) => IsFieldAccessor (l :: Symbol) x y (z :: Bool) | l y -> x
  where
    fieldAccessor :: Proxy# l -> x -> y

instance IsFieldAccessor l x y (FromArrow x) => IsLabel l (x -> y) where
    fromLabel = fieldAccessor

-- | Overloaded lens:
--
-- @
-- 'Functor' f => 'Proxy#' l -> (a -> f b) -> s -> f t
-- @
instance
    ( Functor f
    , HasField l s a
    , SetField l s b
    , FieldType l s ~ a
    , UpdateType l s b ~ t
    ) => IsFieldAccessor l (a -> f b) (s -> f t) 'True
  where
    fieldAccessor proxy f s = setField proxy s <$> f (getField proxy s)

-- | Overloaded getter:
--
-- @
-- 'Proxy#' l -> r -> a
-- @
instance
    ( HasField l s a
    , FieldType l s ~ a
    , FromArrow s ~ 'False
    ) => IsFieldAccessor l s a 'False
  where
    fieldAccessor = getField

-- | Wrapper for a set function, lens naming convention is used for type
-- variables. Its instance for 'IsLabel' forces overloaded label to behave as a
-- setter.
newtype Setter s t b = Setter (s -> b -> t)

-- | Extract set function from 'Setter'. Using 'Setter' instance for 'IsLabel'
-- forces overloaded label to behave as a setter.
--
-- Usage example:
--
-- @
-- newtype Bar a = Bar {_bar :: a}
--   deriving Show
--
-- overloadedRecord ''Bar
-- @
--
-- >>> set bar (Bar (Just False)) Nothing
-- Bar {_bar = Nothing}
set :: Setter s t b -> s -> b -> t
set (Setter f) = f

instance
    ( SetField l s b
    , UpdateType l s b ~ t
    ) => IsLabel l (Setter s t b)
  where
    fromLabel _proxy = Setter (setField _proxy)
