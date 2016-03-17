{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Portability:  ConstraintKinds, DataKinds, DeriveDataTypeable, DeriveGeneric,
--               FlexibleInstances, FlexibleContexts, FunctionalDependencies,
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

    -- ** Setter and Modifier
    , UpdateType
    , ModifyField(..)

    , Setter
    , set

    , Modifier
    , modify

    -- ** Simple Setter and Modifier
    , ModifyField'
    , fieldLens'
    , modifyField'
    , setField'

    , Setter'
    , set'

    , Modifier'
    , modify'

    -- ** IsLabel For Getter and Lens
    , FromArrow
    , IsFieldAccessor(..)
    )
  where

import Data.Bool (Bool(False, True))
import Data.Function (const)
import Data.Functor (Functor, (<$>))
import Data.Typeable (Typeable)
import GHC.Exts (Proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)

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

-- | Represents overloaded record fields that can be modified, i.e. updated.
--
-- @
-- forall l a b s t.
--     'ModifyField' l s t a b => a /= b <==> s /= t
-- @
--
-- In other words, if field is modified and its type has changed, then the type
-- of the record has to change as well, and vice versa. Functional dependencies
-- enforce this rule.
class (HasField l s a) => ModifyField (l :: Symbol) s t a b
    | l s -> a, l t -> b, l s b -> t, l t a -> s
  where
    {-# MINIMAL modifyField | setField #-}

    -- | Modify overloaded field @l :: 'Symbol'@ of record @s@ using pure
    -- function @a -> b@, and produce new record @t@.
    modifyField :: Proxy# l -> (a -> b) -> s -> t
    modifyField proxy f s = setField proxy s (f (getField proxy s))

    -- | Set overloaded field @l :: 'Symbol'@ of record @s@ to value of type
    -- @b@, and produce new record @t@. Please note that there is no mention of
    -- the type @a@, therefore the compiler may not be able to derive it in
    -- some cases.
    setField :: Proxy# l -> s -> b -> t
    setField proxy s b = modifyField proxy (const b) s

    -- | Lens for overloaded field @l :: 'Symbol'@ of record @s@.
    fieldLens :: Functor f => Proxy# l -> (a -> f b) -> s -> f t
    fieldLens proxy f s = setField proxy s <$> f (getField proxy s)

-- | Same as 'ModifyField', but type-changing assignment is prohibited.
type ModifyField' l s a = ModifyField l s s a a

-- | Same as 'setFiend', but the field type can not be changed.
setField' :: ModifyField' l s a => Proxy# l -> s -> a -> s
setField' = setField

-- | Same as 'modifyField', but the field type can not be changed.
modifyField' :: ModifyField' l s a => Proxy# l -> (a -> a) -> s -> s
modifyField' = modifyField

-- | Same as 'modifyField', but the field type can not be changed.
fieldLens'
    :: (Functor f, ModifyField' l s a)
    => Proxy# l
    -> (a -> f a)
    -> s -> f s
fieldLens' = fieldLens

-- | Returns 'True' if type @a :: \*@ is a function.
type family FromArrow (a :: *) :: Bool where
    FromArrow (x -> y) = 'True
    FromArrow t        = 'False

-- | Distinguish between getter and lens.
class
    ( z ~ FromArrow x
    ) => IsFieldAccessor (l :: Symbol) x y (z :: Bool) | l y -> x
  where
    fieldAccessor :: Proxy# l -> x -> y

-- | Overloaded lens:
--
-- @
-- 'Functor' f => 'Proxy#' l -> (a -> f b) -> s -> f t
-- @
instance
    ( Functor f
    , ModifyField l s t a b
    ) => IsFieldAccessor l (a -> f b) (s -> f t) 'True
  where
    fieldAccessor = fieldLens

-- | Overloaded getter:
--
-- @
-- 'Proxy#' l -> r -> a
-- @
instance
    ( HasField l s a
    , FromArrow s ~ 'False
    ) => IsFieldAccessor l s a 'False
  where
    fieldAccessor = getField

instance IsFieldAccessor l x y (FromArrow x) => IsLabel l (x -> y) where
    fromLabel = fieldAccessor

-- {{{ Setter -----------------------------------------------------------------

-- | Wrapper for a set function, lens naming convention is used for type
-- variables. Its instance for 'IsLabel' forces overloaded label to behave as a
-- setter.
newtype Setter s t b = Setter (s -> b -> t)
  deriving (Generic, Typeable)

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

-- | Simple 'Setter' which forbids changing the field type.
type Setter' s a = Setter s s a

-- | Same as 'set', but the field type can not be changed.
set' :: Setter' s a -> s -> a -> s
set' = set

instance (ModifyField l s t a b) => IsLabel l (Setter s t b) where
    fromLabel _proxy = Setter (setField _proxy)

-- }}} Setter -----------------------------------------------------------------

-- {{{ Modifier ---------------------------------------------------------------

newtype Modifier s t a b = Modifier ((a -> b) -> s -> t)
  deriving (Generic, Typeable)

instance (ModifyField l s t a b) => IsLabel l (Modifier s t a b) where
    fromLabel proxy = Modifier (modifyField proxy)

modify :: Modifier s t a b -> (a -> b) -> s -> t
modify (Modifier f) = f

-- | Simple 'Modifier' which forbids changing the field type.
type Modifier' s a = Modifier s s a a

-- | Same as 'modify', but the field type can not be changed.
modify' :: Modifier' s a -> (a -> a) -> s -> s
modify' = modify

-- {{{ Modifier ---------------------------------------------------------------
