{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
--               LambdaCase MagicHash, MultiParamTypeClasses,
--               NoImplicitPrelude, RankNTypes, TypeFamilies, TypeOperators,
--               UndecidableInstances
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
    , R
    , (:::)

    , Setting
    , setting

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
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy)
import Data.Typeable (Typeable)
import GHC.Exts (Constraint, Proxy#)
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
--   'ModifyField' l s t a b => a /= b \<==\> s /= t
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

-- | Using this type family provides more compact type signatures for multiple
-- record fields.
--
-- @
-- data V3 a = V3
--     { _x :: a
--     , _y :: a
--     , _z :: a
--     }
--   deriving Show
--
-- 'Data.OverloadedRecords.TH.overloadedRecord' def ''V3
--
-- setV3
--     :: 'R' [\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r
--     => a -> a -> a -> r -> r
-- setV3 x y z = 'set'' \#x x . 'set'' \#y y . 'set'' \#z z
-- @
--
-- >>> setV3 0 0 0 (V3 1 1 1 :: V3 Int)
-- V3 {_x = 0, _y = 0, _z = 0}
type family R (ts :: [(Symbol, *)]) (r :: *) :: Constraint where
    R '[] r             = ()
    R ('(l, a) ': ts) r = (ModifyField' l r a, R ts r)

-- | This type alias is used for more readable type signatures when using 'R'
-- type family.
type (:::) (l :: Symbol) (a :: *) = '(l, a)

-- {{{ Setter -----------------------------------------------------------------

-- | 'Setting' is just a form of a 'Modifier' that allows us to specify what
-- was the original type of the value we are changing.
--
-- See also 'Setter', 'Setter'', 'Modifier', and 'Modifier''.
type Setting a s t b = Modifier s t a b

-- | Same as 'set', but allows us to use phantom type to restrict the type of a
-- value before it was changed.
--
-- @
-- newtype Bar a = Bar {_bar :: a}
--   deriving Show
--
-- overloadedRecord ''Bar
-- @
--
-- Now we can use 'setting' to change the value stored in @Bar@. The type
-- signature in following example is not required, it is what type checker
-- would infer.
--
-- @
-- 'setting' \#bar ('Proxy' @Int) False :: Bar Int -> Bar Bool
-- @
setting :: Setting a s t b -> Proxy a -> b -> s -> t
setting x _proxy b = modify x (const b)

-- | Wrapper for a set function, lens naming convention is used for type
-- variables. Its instance for 'IsLabel' forces overloaded label to behave as a
-- setter. We could also define 'Setter' as:
--
-- @
-- type 'Setter' s t b = forall a. 'Setting' a s t b
-- @
--
-- Notice that the @forall a@ forbids us from stating what exactly it is,
-- therefore functional dependencies in 'ModifyField' type class have to be
-- able to uniquely identify it. If that is not possible, then we may have to
-- use explicit type signature.
--
-- See also 'Setting', 'Setter'', 'Modifier', and 'Modifier''.
type Setter s t b = forall a. Modifier s t a b

-- | Extract set function from 'Setter'. Using 'Modifier' instance for
-- 'IsLabel' forces overloaded label to behave as a setter.
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
-- >>> set #bar (Nothing :: Maybe Int) (Bar (Just False))
-- Bar {_bar = Nothing}
set :: Setter s t b -> b -> s -> t
set m b = modify m (const b)
{-# INLINE set #-}

-- | Simple 'Setter' which forbids changing the field type. It can be also
-- defined in terms of 'Setting':
--
-- @
-- type 'Setter'' s a = 'Setting' a s s a
-- @
--
-- See also 'Setting', 'Setter', 'Modifier', and 'Modifier''.
type Setter' s a = Modifier' s a

-- | Same as 'set', but the field type can not be changed.
set' :: Setter' s a -> a -> s -> s
set' m a = modify' m (const a)
{-# INLINE set' #-}

-- }}} Setter -----------------------------------------------------------------

-- {{{ Modifier ---------------------------------------------------------------

-- | Wrapper for a modification function, lens naming convention is used for
-- type variables. Its instance for 'IsLabel' forces overloaded label to behave
-- as a modification function.
--
-- See also 'Modifier'', 'Setting', 'Setter', and 'Setter''.
newtype Modifier s t a b = Modifier ((a -> b) -> s -> t)
  deriving (Generic, Typeable)

instance (ModifyField l s t a b) => IsLabel l (Modifier s t a b) where
    fromLabel proxy = Modifier (modifyField proxy)

-- | Modify field value using provided function.
modify :: Modifier s t a b -> (a -> b) -> s -> t
modify (Modifier f) = f

-- | Simple 'Modifier' which forbids changing the field type.
--
-- See also 'Modifier', 'Modifier'', 'Setting', 'Setter', and 'Setter''.
type Modifier' s a = Modifier s s a a

-- | Same as 'modify', but the field type can not be changed.
modify' :: Modifier' s a -> (a -> a) -> s -> s
modify' = modify

-- }}} Modifier ---------------------------------------------------------------

-- {{{ Instances --------------------------------------------------------------

-- {{{ Instances -- Tuples ----------------------------------------------------

type instance FieldType "fst" (a, b) = a
type instance UpdateType "fst" (a, b) a' = (a', b)

type instance FieldType "snd" (a, b) = b
type instance UpdateType "snd" (a, b) b' = (a, b')

type instance FieldType "curry" ((a, b) -> c) = a -> b -> c

instance HasField "fst" (a, b) a where
    getField _proxy (a, _) = a

instance HasField "snd" (a, b) b where
    getField _proxy (_, b) = b

instance HasField "curry" ((a, b) -> c) (a -> b -> c) where
    getField _proxy f a b = f (a, b)

instance ModifyField "fst" (a, b) (a', b) a a' where
    modifyField _proxy f (a, b) = (f a, b)
    setField _proxy (_, b) a = (a, b)

instance ModifyField "snd" (a, b) (a, b') b b' where
    modifyField _proxy f (a, b) = (a, f b)
    setField _proxy (a, _) b = (a, b)

type instance FieldType "fst" (a, b, c) = a
type instance UpdateType "fst" (a, b, c) a' = (a', b, c)

type instance FieldType "snd" (a, b, c) = b
type instance UpdateType "snd" (a, b, c) b' = (a, b', c)

type instance FieldType "thd" (a, b, c) = c
type instance UpdateType "thd" (a, b, c) c' = (a, b, c')

type instance FieldType "curry" ((a, b, c) -> d) = a -> b -> c -> d

instance HasField "fst" (a, b, c) a where
    getField _proxy (a, _, _) = a

instance HasField "snd" (a, b, c) b where
    getField _proxy (_, b, _) = b

instance HasField "thd" (a, b, c) c where
    getField _proxy (_, _, c) = c

instance HasField "curry" ((a, b, c) -> d) (a -> b -> c -> d) where
    getField _proxy f a b c = f (a, b, c)

instance ModifyField "fst" (a, b, c) (a', b, c) a a' where
    modifyField _proxy f (a, b, c) = (f a, b, c)
    setField _proxy (_, b, c) a = (a, b, c)

instance ModifyField "snd" (a, b, c) (a, b', c) b b' where
    modifyField _proxy f (a, b, c) = (a, f b, c)
    setField _proxy (a, _, c) b = (a, b, c)

instance ModifyField "thd" (a, b, c) (a, b, c') c c' where
    modifyField _proxy f (a, b, c) = (a, b, f c)
    setField _proxy (a, b, _) c = (a, b, c)

type instance FieldType "fst" (a1, a2, a3, a4) = a1
type instance UpdateType "fst" (a1, a2, a3, a4) a1' = (a1', a2, a3, a4)

type instance FieldType "snd" (a1, a2, a3, a4) = a2
type instance UpdateType "snd" (a1, a2, a3, a4) a2' = (a1, a2', a3, a4)

type instance FieldType "thd" (a1, a2, a3, a4) = a3
type instance UpdateType "thd" (a1, a2, a3, a4) a3' = (a1, a3', a3, a4)

type instance FieldType "curry" ((a1, a2, a3, a4) -> r) =
    a1 -> a2 -> a3 -> a4 -> r

instance HasField "fst" (a1, a2, a3, a4) a1 where
    getField _proxy (a1, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4) a2 where
    getField _proxy (_, a2, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4) a3 where
    getField _proxy (_, _, a3, _) = a3

instance ModifyField "fst" (a1, a2, a3, a4) (a1', a2, a3, a4) a1 a1' where
    modifyField _proxy f (a1, a2, a3, a4) = (f a1, a2, a3, a4)
    setField _proxy (_, a2, a3, a4) a1 = (a1, a2, a3, a4)

instance ModifyField "snd" (a1, a2, a3, a4) (a1, a2', a3, a4) a2 a2' where
    modifyField _proxy f (a1, a2, a3, a4) = (a1, f a2, a3, a4)
    setField _proxy (a1, _, a3, a4) a2 = (a1, a2, a3, a4)

instance ModifyField "thd" (a1, a2, a3, a4) (a1, a2, a3', a4) a3 a3' where
    modifyField _proxy f (a1, a2, a3, a4) = (a1, a2, f a3, a4)
    setField _proxy (a1, a2, _, a4) a3 = (a1, a2, a3, a4)

instance HasField "curry" ((a1, a2, a3, a4) -> r) (a1 -> a2 -> a3 -> a4 -> r)
  where
    getField _proxy f a1 a2 a3 a4 = f (a1, a2, a3, a4)

type instance FieldType "fst" (a1, a2, a3, a4, a5) = a1
type instance UpdateType "fst" (a1, a2, a3, a4, a5) a1' = (a1', a2, a3, a4, a5)

type instance FieldType "snd" (a1, a2, a3, a4, a5) = a2
type instance UpdateType "snd" (a1, a2, a3, a4, a5) a2' = (a1, a2', a3, a4, a5)

type instance FieldType "thd" (a1, a2, a3, a4, a5) = a3
type instance UpdateType "thd" (a1, a2, a3, a4, a5) a3' = (a1, a2, a3', a4, a5)

type instance FieldType "curry" ((a1, a2, a3, a4, a5) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> r

instance HasField "fst" (a1, a2, a3, a4, a5) a1 where
    getField _proxy (a1, _, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4, a5) a2 where
    getField _proxy (_, a2, _, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4, a5) a3 where
    getField _proxy (_, _, a3, _, _) = a3

instance ModifyField "fst" (a1, a2, a3, a4, a5) (a1', a2, a3, a4, a5) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5) = (f a1, a2, a3, a4, a5)
    setField _proxy (_, a2, a3, a4, a5) a1 = (a1, a2, a3, a4, a5)

instance
    ModifyField "snd" (a1, a2, a3, a4, a5) (a1, a2', a3, a4, a5) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5) = (a1, f a2, a3, a4, a5)
    setField _proxy (a1, _, a3, a4, a5) a2 = (a1, a2, a3, a4, a5)

instance
    ModifyField "thd" (a1, a2, a3, a4, a5) (a1, a2, a3', a4, a5) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5) = (a1, a2, f a3, a4, a5)
    setField _proxy (a1, a2, _, a4, a5) a3 = (a1, a2, a3, a4, a5)

instance
    HasField "curry" ((a1, a2, a3, a4, a5) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 = f (a1, a2, a3, a4, a5)

type instance FieldType "fst" (a1, a2, a3, a4, a5, a6) = a1
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6) a1' =
    (a1', a2, a3, a4, a5, a6)

type instance FieldType "snd" (a1, a2, a3, a4, a5, a6) = a2
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6) a2' =
    (a1, a2', a3, a4, a5, a6)

type instance FieldType "thd" (a1, a2, a3, a4, a5, a6) = a3
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6) a3' =
    (a1, a2, a3', a4, a5, a6)

type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r

instance HasField "fst" (a1, a2, a3, a4, a5, a6) a1 where
    getField _proxy (a1, _, _, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4, a5, a6) a2 where
    getField _proxy (_, a2, _, _, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4, a5, a6) a3 where
    getField _proxy (_, _, a3, _, _, _) = a3

instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6) (a1', a2, a3, a4, a5, a6) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6) = (f a1, a2, a3, a4, a5, a6)
    setField _proxy (_, a2, a3, a4, a5, a6) a1 = (a1, a2, a3, a4, a5, a6)

instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6) (a1, a2', a3, a4, a5, a6) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6) = (a1, f a2, a3, a4, a5, a6)
    setField _proxy (a1, _, a3, a4, a5, a6) a2 = (a1, a2, a3, a4, a5, a6)

instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6) (a1, a2, a3', a4, a5, a6) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6) = (a1, a2, f a3, a4, a5, a6)
    setField _proxy (a1, a2, _, a4, a5, a6) a3 = (a1, a2, a3, a4, a5, a6)

instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 = f (a1, a2, a3, a4, a5, a6)

type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7) = a1
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7) a1' =
    (a1', a2, a3, a4, a5, a6, a7)

type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7) = a2
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7) a2' =
    (a1, a2', a3, a4, a5, a6, a7)

type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7) = a3
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7) a3' =
    (a1, a2, a3', a4, a5, a6, a7)

type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6, a7) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r

instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7) a1 where
    getField _proxy (a1, _, _, _, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7) a2 where
    getField _proxy (_, a2, _, _, _, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7) a3 where
    getField _proxy (_, _, a3, _, _, _, _) = a3

instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7)
        (a1', a2, a3, a4, a5, a6, a7) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7) =
        (f a1, a2, a3, a4, a5, a6, a7)
    setField _proxy (_, a2, a3, a4, a5, a6, a7) a1 =
        (a1, a2, a3, a4, a5, a6, a7)

instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7)
        (a1, a2', a3, a4, a5, a6, a7) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7) =
        (a1, f a2, a3, a4, a5, a6, a7)
    setField _proxy (a1, _, a3, a4, a5, a6, a7) a2 =
        (a1, a2, a3, a4, a5, a6, a7)

instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7)
        (a1, a2, a3', a4, a5, a6, a7) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7) =
        (a1, a2, f a3, a4, a5, a6, a7)
    setField _proxy (a1, a2, _, a4, a5, a6, a7) a3 =
        (a1, a2, a3, a4, a5, a6, a7)

instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 = f (a1, a2, a3, a4, a5, a6, a7)

type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7, a8) = a1
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7, a8) a1' =
    (a1', a2, a3, a4, a5, a6, a7, a8)

type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7, a8) = a2
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7, a8) a2' =
    (a1, a2', a3, a4, a5, a6, a7, a8)

type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7, a8) = a3
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7, a8) a3' =
    (a1, a2, a3', a4, a5, a6, a7, a8)

type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6, a7, a8) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r

instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7, a8) a1 where
    getField _proxy (a1, _, _, _, _, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7, a8) a2 where
    getField _proxy (_, a2, _, _, _, _, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7, a8) a3 where
    getField _proxy (_, _, a3, _, _, _, _, _) = a3

instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7, a8)
        (a1', a2, a3, a4, a5, a6, a7, a8) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8) =
        (f a1, a2, a3, a4, a5, a6, a7, a8)
    setField _proxy (_, a2, a3, a4, a5, a6, a7, a8) a1 =
        (a1, a2, a3, a4, a5, a6, a7, a8)

instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7, a8)
        (a1, a2', a3, a4, a5, a6, a7, a8) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8) =
        (a1, f a2, a3, a4, a5, a6, a7, a8)
    setField _proxy (a1, _, a3, a4, a5, a6, a7, a8) a2 =
        (a1, a2, a3, a4, a5, a6, a7, a8)

instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7, a8)
        (a1, a2, a3', a4, a5, a6, a7, a8) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8) =
        (a1, a2, f a3, a4, a5, a6, a7, a8)
    setField _proxy (a1, a2, _, a4, a5, a6, a7, a8) a3 =
        (a1, a2, a3, a4, a5, a6, a7, a8)

instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7, a8) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 a8 =
        f (a1, a2, a3, a4, a5, a6, a7, a8)

type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a1' =
    (a1', a2, a3, a4, a5, a6, a7, a8, a9)

type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a2
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a2' =
    (a1, a2', a3, a4, a5, a6, a7, a8, a9)

type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a3
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a3' =
    (a1, a2, a3', a4, a5, a6, a7, a8, a9)

type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6, a7, a8, a9) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r

instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a1 where
    getField _proxy (a1, _, _, _, _, _, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a2 where
    getField _proxy (_, a2, _, _, _, _, _, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a3 where
    getField _proxy (_, _, a3, _, _, _, _, _, _) = a3

instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (a1', a2, a3, a4, a5, a6, a7, a8, a9) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        (f a1, a2, a3, a4, a5, a6, a7, a8, a9)
    setField _proxy (_, a2, a3, a4, a5, a6, a7, a8, a9) a1 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (a1, a2', a3, a4, a5, a6, a7, a8, a9) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        (a1, f a2, a3, a4, a5, a6, a7, a8, a9)
    setField _proxy (a1, _, a3, a4, a5, a6, a7, a8, a9) a2 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (a1, a2, a3', a4, a5, a6, a7, a8, a9) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        (a1, a2, f a3, a4, a5, a6, a7, a8, a9)
    setField _proxy (a1, a2, _, a4, a5, a6, a7, a8, a9) a3 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7, a8, a9) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
        f (a1, a2, a3, a4, a5, a6, a7, a8, a9)

type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a1
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a1' =
    (a1', a2, a3, a4, a5, a6, a7, a8, a9, a10)

type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a2
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a2' =
    (a1, a2', a3, a4, a5, a6, a7, a8, a9, a10)

type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a3
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a3' =
    (a1, a2, a3', a4, a5, a6, a7, a8, a9, a10)

type instance FieldType "curry"
    ((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) -> r) =
        a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> r

instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a1 where
    getField _proxy (a1, _, _, _, _, _, _, _, _, _) = a1

instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a2 where
    getField _proxy (_, a2, _, _, _, _, _, _, _, _) = a2

instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a3 where
    getField _proxy (_, _, a3, _, _, _, _, _, _, _) = a3

instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (a1', a2, a3, a4, a5, a6, a7, a8, a9, a10) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
        (f a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    setField _proxy (_, a2, a3, a4, a5, a6, a7, a8, a9, a10) a1 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (a1, a2', a3, a4, a5, a6, a7, a8, a9, a10) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
        (a1, f a2, a3, a4, a5, a6, a7, a8, a9, a10)
    setField _proxy (a1, _, a3, a4, a5, a6, a7, a8, a9, a10) a2 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (a1, a2, a3', a4, a5, a6, a7, a8, a9, a10) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
        (a1, a2, f a3, a4, a5, a6, a7, a8, a9, a10)
    setField _proxy (a1, a2, _, a4, a5, a6, a7, a8, a9, a10) a3 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
        f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- }}} Instances -- Tuples ----------------------------------------------------

-- {{{ Instances -- Lists -----------------------------------------------------

type instance FieldType "head" [a] = Maybe a
type instance UpdateType "head" [a] (Maybe a) = [a]

-- |
-- >>> #head []
-- Nothing
--
-- >>> #head [1, 2, 3]
-- Just 1
instance HasField "head" [a] (Maybe a) where
    getField _proxy []      = Nothing
    getField _proxy (a : _) = Just a

-- |
-- >>> set' #head [] Nothing :: [Int]
-- []
--
-- >>> set' #head [] (Just 1)
-- [1]
--
-- >>> set' #head [1, 2, 3] Nothing
-- [2, 3]
--
-- >>> set' #head [1, 2, 3] (Just 4)
-- [4, 2, 3]
instance ModifyField "head" [a] [a] (Maybe a) (Maybe a) where
    modifyField _proxy f = \case
        [] -> case f Nothing of
            Nothing -> []
            Just a -> [a]
        a : as -> case f (Just a) of
            Nothing -> as
            Just a' -> a' : as

    setField _proxy = \case
        [] -> \case
            Nothing -> []
            Just a -> [a]
        _ : as -> \case
            Nothing -> as
            Just a -> a : as

type instance FieldType "tail" [a] = Maybe [a]
type instance UpdateType "tail" [a] (Maybe [a]) = [a]

-- |
-- >>> #tail []
-- Nothing
--
-- >>> #tail [1, 2, 3]
-- Just [2, 3]
instance HasField "tail" [a] (Maybe [a]) where
    getField _proxy []       = Nothing
    getField _proxy (_ : as) = Just as

-- |
-- >>> set' #tail [] Nothing :: [Int]
-- []
--
-- >>> set' #tail [] (Just [2, 3])
-- [2, 3]
--
-- >>> set' #tail [1, 2, 3] Nothing
-- [1]
--
-- >>> set' #tail [1, 2, 3] (Just [4, 5, 6])
-- [1, 4, 5, 6]
instance ModifyField "tail" [a] [a] (Maybe [a]) (Maybe [a]) where
    modifyField _proxy f = \case
        [] -> case f Nothing of
            Nothing -> []
            Just as -> as
        a : as -> case f (Just as) of
            Nothing -> [a]
            Just as' -> a : as'

    setField _proxy = \case
        [] -> \case
            Nothing -> []
            Just as -> as
        a : _ -> \case
            Nothing -> [a]
            Just as -> a : as

-- }}} Instances -- Lists -----------------------------------------------------

-- }}} Instances --------------------------------------------------------------
