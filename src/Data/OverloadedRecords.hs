{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
--               GADTs, LambdaCase, MagicHash, MultiParamTypeClasses,
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
    -- * Usage Examples
    --
    -- $usageExamples

    -- * Oveloaded Labels
      module Data.OverloadedLabels

    -- * Overloaded Record Fields
    --
    -- ** Getter
    , FieldType
    , HasField(..)

    , Getter
    , get

    -- ** Setter and Modifier
    , UpdateType
    , ModifyField(..)
    , R
    , (:::)
    , Rec(..)

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
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (Symbol)

import Data.OverloadedLabels


-- | When accessing field named @l :: Symbol@ of a record @s :: *@, then the
-- type of the value in that field is @'FieldType' l s@.
type family FieldType (l :: Symbol) (s :: *) :: *

-- | If field @l :: Symbol@ of a record @s :: *@ is set to new value which has
-- type @b :: *@, then the modified record will have type @'UpdateType' l s b@.
type family UpdateType (l :: Symbol) (s :: *) (b :: *) :: *

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
--
-- /Since 0.4.0.0/
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
--
-- /Since 0.4.0.0/
type ModifyField' l s a = ModifyField l s s a a

-- | Same as 'setFiend', but the field type can not be changed.
--
-- /Since 0.4.0.0/
setField' :: ModifyField' l s a => Proxy# l -> s -> a -> s
setField' = setField

-- | Same as 'modifyField', but the field type can not be changed.
--
-- /Since 0.4.0.0/
modifyField' :: ModifyField' l s a => Proxy# l -> (a -> a) -> s -> s
modifyField' = modifyField

-- | Same as 'modifyField', but the field type can not be changed.
--
-- /Since 0.4.0.0/
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
--     :: 'R' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r
--     => a -> a -> a -> r -> r
-- setV3 x y z = 'set'' \#x x . 'set'' \#y y . 'set'' \#z z
-- @
--
-- >>> setV3 0 0 0 (V3 1 1 1 :: V3 Int)
-- V3 {_x = 0, _y = 0, _z = 0}
--
-- /Since 0.4.0.0/
type family R (ts :: [(Symbol, *)]) (r :: *) :: Constraint where
    R '[] r             = ()
    R ('(l, a) ': ts) r = (ModifyField' l r a, R ts r)

-- | This type alias is used for more readable type signatures when using 'R'
-- type family.
--
-- /Since 0.4.0.0/
type (:::) (l :: Symbol) (a :: *) = '(l, a)

-- | Pass polymorphic record as a value along with all necessary instances. By
-- pattern matching on 'Rec' data constructor all those instances come in to
-- scope.
--
-- Example:
--
-- @
-- {-\# LANGUAGE GADTs \#-}
--     -- May be required in addition to the basic set of language extensions.
--
-- data V3 a = V3
--     { _x :: a
--     , _y :: a
--     , _z :: a
--     }
--   deriving Show
--
-- 'Data.OverloadedRecords.TH.overloadedRecord' def ''V3
--
-- zeroV3 :: 'Rec' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r -> r -> r
-- zeroV3 ('Rec' r) = 'set'' \#x 0 . 'set'' \#y 0 $ 'set'' \#z 0 r
-- @
--
-- >>> zeroV3 (V3 1 1 1 :: V3 Int)
-- V3 {_x = 0, _y = 0, _z = 0}
--
-- /Since 0.4.1.0/
data Rec ts r where
    Rec :: R ts r => r -> Rec ts r
  deriving (Typeable)

-- {{{ Getter -----------------------------------------------------------------

-- | Provides alternative to the \"native\" 'IsLabel' instance for getter.
-- Since mixing getter instance and lens instance for 'IsLabel' on polymorphic
-- records is not possible, one may want to use 'Getter' as an alternative.
--
-- /Since 0.4.1.0/
newtype Getter s a = Getter (s -> a)
  deriving (Generic, Generic1, Typeable)

-- | /Since 0.4.1.0/
instance (HasField l s a) => IsLabel l (Getter s a) where
    fromLabel proxy = Getter (getField proxy)

-- | Extract a getter function from overloaded label.
--
-- Example:
--
-- @
-- newtype Bar a = Bar {_bar :: a}
--
-- overloadedRecord ''Bar
-- @
--
-- >>> get #bar (Bar False)
-- False
--
-- /Since 0.4.1.0/
get :: Getter s a -> s -> a
get (Getter f) = f
{-# INLINE get #-}

-- }}} Getter -----------------------------------------------------------------

-- {{{ Setter -----------------------------------------------------------------

-- | 'Setting' is just a form of a 'Modifier' that allows us to specify what
-- was the original type of the value we are changing.
--
-- See also 'Setter', 'Setter'', 'Modifier', and 'Modifier''.
--
-- /Since 0.4.0.0/
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
--
-- /Since 0.4.0.0/
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
--
-- /Definition changed in 0.4.0.0/
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
--
-- /Definition changed in 0.4.0.0/
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
--
-- /Since 0.4.0.0/
newtype Modifier s t a b = Modifier ((a -> b) -> s -> t)
  deriving (Generic, Typeable)

-- | /Since 0.4.0.0/
instance (ModifyField l s t a b) => IsLabel l (Modifier s t a b) where
    fromLabel proxy = Modifier (modifyField proxy)

-- | Modify field value using provided function.
--
-- /Since 0.4.0.0/
modify :: Modifier s t a b -> (a -> b) -> s -> t
modify (Modifier f) = f

-- | Simple 'Modifier' which forbids changing the field type.
--
-- See also 'Modifier', 'Modifier'', 'Setting', 'Setter', and 'Setter''.
--
-- /Since 0.4.0.0/
type Modifier' s a = Modifier s s a a

-- | Same as 'modify', but the field type can not be changed.
--
-- /Since 0.4.0.0/
modify' :: Modifier' s a -> (a -> a) -> s -> s
modify' = modify

-- }}} Modifier ---------------------------------------------------------------

-- {{{ Instances --------------------------------------------------------------

-- {{{ Instances -- Tuples ----------------------------------------------------

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a, b) = a

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a, b) a' = (a', b)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a, b) = b

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a, b) b' = (a, b')

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a, b) -> c) = a -> b -> c

-- | /Since 0.4.0.0/
instance HasField "fst" (a, b) a where
    getField _proxy (a, _) = a

-- | /Since 0.4.0.0/
instance HasField "snd" (a, b) b where
    getField _proxy (_, b) = b

-- | /Since 0.4.0.0/
instance HasField "curry" ((a, b) -> c) (a -> b -> c) where
    getField _proxy f a b = f (a, b)

-- | /Since 0.4.0.0/
instance ModifyField "fst" (a, b) (a', b) a a' where
    modifyField _proxy f (a, b) = (f a, b)
    setField _proxy (_, b) a = (a, b)

-- | /Since 0.4.0.0/
instance ModifyField "snd" (a, b) (a, b') b b' where
    modifyField _proxy f (a, b) = (a, f b)
    setField _proxy (a, _) b = (a, b)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a, b, c) = a

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a, b, c) a' = (a', b, c)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a, b, c) = b

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a, b, c) b' = (a, b', c)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a, b, c) = c

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a, b, c) c' = (a, b, c')

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a, b, c) -> d) = a -> b -> c -> d

-- | /Since 0.4.0.0/
instance HasField "fst" (a, b, c) a where
    getField _proxy (a, _, _) = a

-- | /Since 0.4.0.0/
instance HasField "snd" (a, b, c) b where
    getField _proxy (_, b, _) = b

-- | /Since 0.4.0.0/
instance HasField "thd" (a, b, c) c where
    getField _proxy (_, _, c) = c

-- | /Since 0.4.0.0/
instance HasField "curry" ((a, b, c) -> d) (a -> b -> c -> d) where
    getField _proxy f a b c = f (a, b, c)

-- | /Since 0.4.0.0/
instance ModifyField "fst" (a, b, c) (a', b, c) a a' where
    modifyField _proxy f (a, b, c) = (f a, b, c)
    setField _proxy (_, b, c) a = (a, b, c)

-- | /Since 0.4.0.0/
instance ModifyField "snd" (a, b, c) (a, b', c) b b' where
    modifyField _proxy f (a, b, c) = (a, f b, c)
    setField _proxy (a, _, c) b = (a, b, c)

-- | /Since 0.4.0.0/
instance ModifyField "thd" (a, b, c) (a, b, c') c c' where
    modifyField _proxy f (a, b, c) = (a, b, f c)
    setField _proxy (a, b, _) c = (a, b, c)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4) a1' = (a1', a2, a3, a4)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4) a2' = (a1, a2', a3, a4)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4) a3' = (a1, a3', a3, a4)

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a1, a2, a3, a4) -> r) =
    a1 -> a2 -> a3 -> a4 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4) a1 where
    getField _proxy (a1, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4) a2 where
    getField _proxy (_, a2, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4) a3 where
    getField _proxy (_, _, a3, _) = a3

-- | /Since 0.4.0.0/
instance ModifyField "fst" (a1, a2, a3, a4) (a1', a2, a3, a4) a1 a1' where
    modifyField _proxy f (a1, a2, a3, a4) = (f a1, a2, a3, a4)
    setField _proxy (_, a2, a3, a4) a1 = (a1, a2, a3, a4)

-- | /Since 0.4.0.0/
instance ModifyField "snd" (a1, a2, a3, a4) (a1, a2', a3, a4) a2 a2' where
    modifyField _proxy f (a1, a2, a3, a4) = (a1, f a2, a3, a4)
    setField _proxy (a1, _, a3, a4) a2 = (a1, a2, a3, a4)

-- | /Since 0.4.0.0/
instance ModifyField "thd" (a1, a2, a3, a4) (a1, a2, a3', a4) a3 a3' where
    modifyField _proxy f (a1, a2, a3, a4) = (a1, a2, f a3, a4)
    setField _proxy (a1, a2, _, a4) a3 = (a1, a2, a3, a4)

-- | /Since 0.4.0.0/
instance HasField "curry" ((a1, a2, a3, a4) -> r) (a1 -> a2 -> a3 -> a4 -> r)
  where
    getField _proxy f a1 a2 a3 a4 = f (a1, a2, a3, a4)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4, a5) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4, a5) a1' = (a1', a2, a3, a4, a5)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4, a5) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4, a5) a2' = (a1, a2', a3, a4, a5)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4, a5) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4, a5) a3' = (a1, a2, a3', a4, a5)

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a1, a2, a3, a4, a5) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4, a5) a1 where
    getField _proxy (a1, _, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4, a5) a2 where
    getField _proxy (_, a2, _, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4, a5) a3 where
    getField _proxy (_, _, a3, _, _) = a3

-- | /Since 0.4.0.0/
instance ModifyField "fst" (a1, a2, a3, a4, a5) (a1', a2, a3, a4, a5) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5) = (f a1, a2, a3, a4, a5)
    setField _proxy (_, a2, a3, a4, a5) a1 = (a1, a2, a3, a4, a5)

-- | /Since 0.4.0.0/
instance
    ModifyField "snd" (a1, a2, a3, a4, a5) (a1, a2', a3, a4, a5) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5) = (a1, f a2, a3, a4, a5)
    setField _proxy (a1, _, a3, a4, a5) a2 = (a1, a2, a3, a4, a5)

-- | /Since 0.4.0.0/
instance
    ModifyField "thd" (a1, a2, a3, a4, a5) (a1, a2, a3', a4, a5) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5) = (a1, a2, f a3, a4, a5)
    setField _proxy (a1, a2, _, a4, a5) a3 = (a1, a2, a3, a4, a5)

-- | /Since 0.4.0.0/
instance
    HasField "curry" ((a1, a2, a3, a4, a5) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 = f (a1, a2, a3, a4, a5)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4, a5, a6) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6) a1' =
    (a1', a2, a3, a4, a5, a6)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4, a5, a6) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6) a2' =
    (a1, a2', a3, a4, a5, a6)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4, a5, a6) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6) a3' =
    (a1, a2, a3', a4, a5, a6)

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4, a5, a6) a1 where
    getField _proxy (a1, _, _, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4, a5, a6) a2 where
    getField _proxy (_, a2, _, _, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4, a5, a6) a3 where
    getField _proxy (_, _, a3, _, _, _) = a3

-- | /Since 0.4.0.0/
instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6) (a1', a2, a3, a4, a5, a6) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6) = (f a1, a2, a3, a4, a5, a6)
    setField _proxy (_, a2, a3, a4, a5, a6) a1 = (a1, a2, a3, a4, a5, a6)

-- | /Since 0.4.0.0/
instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6) (a1, a2', a3, a4, a5, a6) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6) = (a1, f a2, a3, a4, a5, a6)
    setField _proxy (a1, _, a3, a4, a5, a6) a2 = (a1, a2, a3, a4, a5, a6)

-- | /Since 0.4.0.0/
instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6) (a1, a2, a3', a4, a5, a6) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6) = (a1, a2, f a3, a4, a5, a6)
    setField _proxy (a1, a2, _, a4, a5, a6) a3 = (a1, a2, a3, a4, a5, a6)

-- | /Since 0.4.0.0/
instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 = f (a1, a2, a3, a4, a5, a6)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7) a1' =
    (a1', a2, a3, a4, a5, a6, a7)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7) a2' =
    (a1, a2', a3, a4, a5, a6, a7)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7) a3' =
    (a1, a2, a3', a4, a5, a6, a7)

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6, a7) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7) a1 where
    getField _proxy (a1, _, _, _, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7) a2 where
    getField _proxy (_, a2, _, _, _, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7) a3 where
    getField _proxy (_, _, a3, _, _, _, _) = a3

-- | /Since 0.4.0.0/
instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7)
        (a1', a2, a3, a4, a5, a6, a7) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7) =
        (f a1, a2, a3, a4, a5, a6, a7)
    setField _proxy (_, a2, a3, a4, a5, a6, a7) a1 =
        (a1, a2, a3, a4, a5, a6, a7)

-- | /Since 0.4.0.0/
instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7)
        (a1, a2', a3, a4, a5, a6, a7) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7) =
        (a1, f a2, a3, a4, a5, a6, a7)
    setField _proxy (a1, _, a3, a4, a5, a6, a7) a2 =
        (a1, a2, a3, a4, a5, a6, a7)

-- | /Since 0.4.0.0/
instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7)
        (a1, a2, a3', a4, a5, a6, a7) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7) =
        (a1, a2, f a3, a4, a5, a6, a7)
    setField _proxy (a1, a2, _, a4, a5, a6, a7) a3 =
        (a1, a2, a3, a4, a5, a6, a7)

-- | /Since 0.4.0.0/
instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 = f (a1, a2, a3, a4, a5, a6, a7)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7, a8) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7, a8) a1' =
    (a1', a2, a3, a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7, a8) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7, a8) a2' =
    (a1, a2', a3, a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7, a8) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7, a8) a3' =
    (a1, a2, a3', a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6, a7, a8) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7, a8) a1 where
    getField _proxy (a1, _, _, _, _, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7, a8) a2 where
    getField _proxy (_, a2, _, _, _, _, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7, a8) a3 where
    getField _proxy (_, _, a3, _, _, _, _, _) = a3

-- | /Since 0.4.0.0/
instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7, a8)
        (a1', a2, a3, a4, a5, a6, a7, a8) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8) =
        (f a1, a2, a3, a4, a5, a6, a7, a8)
    setField _proxy (_, a2, a3, a4, a5, a6, a7, a8) a1 =
        (a1, a2, a3, a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7, a8)
        (a1, a2', a3, a4, a5, a6, a7, a8) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8) =
        (a1, f a2, a3, a4, a5, a6, a7, a8)
    setField _proxy (a1, _, a3, a4, a5, a6, a7, a8) a2 =
        (a1, a2, a3, a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7, a8)
        (a1, a2, a3', a4, a5, a6, a7, a8) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8) =
        (a1, a2, f a3, a4, a5, a6, a7, a8)
    setField _proxy (a1, a2, _, a4, a5, a6, a7, a8) a3 =
        (a1, a2, a3, a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7, a8) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 a8 =
        f (a1, a2, a3, a4, a5, a6, a7, a8)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a1' =
    (a1', a2, a3, a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a2' =
    (a1, a2', a3, a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a3' =
    (a1, a2, a3', a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
type instance FieldType "curry" ((a1, a2, a3, a4, a5, a6, a7, a8, a9) -> r) =
    a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a1 where
    getField _proxy (a1, _, _, _, _, _, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a2 where
    getField _proxy (_, a2, _, _, _, _, _, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9) a3 where
    getField _proxy (_, _, a3, _, _, _, _, _, _) = a3

-- | /Since 0.4.0.0/
instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (a1', a2, a3, a4, a5, a6, a7, a8, a9) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        (f a1, a2, a3, a4, a5, a6, a7, a8, a9)
    setField _proxy (_, a2, a3, a4, a5, a6, a7, a8, a9) a1 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (a1, a2', a3, a4, a5, a6, a7, a8, a9) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        (a1, f a2, a3, a4, a5, a6, a7, a8, a9)
    setField _proxy (a1, _, a3, a4, a5, a6, a7, a8, a9) a2 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (a1, a2, a3', a4, a5, a6, a7, a8, a9) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        (a1, a2, f a3, a4, a5, a6, a7, a8, a9)
    setField _proxy (a1, a2, _, a4, a5, a6, a7, a8, a9) a3 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7, a8, a9) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
        f (a1, a2, a3, a4, a5, a6, a7, a8, a9)

-- | /Since 0.4.0.0/
type instance FieldType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a1

-- | /Since 0.4.0.0/
type instance UpdateType "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a1' =
    (a1', a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | /Since 0.4.0.0/
type instance FieldType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a2

-- | /Since 0.4.0.0/
type instance UpdateType "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a2' =
    (a1, a2', a3, a4, a5, a6, a7, a8, a9, a10)

-- | /Since 0.4.0.0/
type instance FieldType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a3

-- | /Since 0.4.0.0/
type instance UpdateType "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a3' =
    (a1, a2, a3', a4, a5, a6, a7, a8, a9, a10)

-- | /Since 0.4.0.0/
type instance FieldType "curry"
    ((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) -> r) =
        a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> r

-- | /Since 0.4.0.0/
instance HasField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a1 where
    getField _proxy (a1, _, _, _, _, _, _, _, _, _) = a1

-- | /Since 0.4.0.0/
instance HasField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a2 where
    getField _proxy (_, a2, _, _, _, _, _, _, _, _) = a2

-- | /Since 0.4.0.0/
instance HasField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a3 where
    getField _proxy (_, _, a3, _, _, _, _, _, _, _) = a3

-- | /Since 0.4.0.0/
instance
    ModifyField "fst" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (a1', a2, a3, a4, a5, a6, a7, a8, a9, a10) a1 a1'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
        (f a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    setField _proxy (_, a2, a3, a4, a5, a6, a7, a8, a9, a10) a1 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | /Since 0.4.0.0/
instance
    ModifyField "snd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (a1, a2', a3, a4, a5, a6, a7, a8, a9, a10) a2 a2'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
        (a1, f a2, a3, a4, a5, a6, a7, a8, a9, a10)
    setField _proxy (a1, _, a3, a4, a5, a6, a7, a8, a9, a10) a2 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | /Since 0.4.0.0/
instance
    ModifyField "thd" (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
        (a1, a2, a3', a4, a5, a6, a7, a8, a9, a10) a3 a3'
  where
    modifyField _proxy f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
        (a1, a2, f a3, a4, a5, a6, a7, a8, a9, a10)
    setField _proxy (a1, a2, _, a4, a5, a6, a7, a8, a9, a10) a3 =
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | /Since 0.4.0.0/
instance
    HasField "curry" ((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) -> r)
        (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> r)
  where
    getField _proxy f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
        f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- }}} Instances -- Tuples ----------------------------------------------------

-- {{{ Instances -- Lists -----------------------------------------------------

-- | /Since 0.4.0.0/
type instance FieldType "head" [a] = Maybe a

-- | /Since 0.4.0.0/
type instance UpdateType "head" [a] (Maybe a) = [a]

-- |
-- >>> #head []
-- Nothing
--
-- >>> #head [1, 2, 3]
-- Just 1
--
-- /Since 0.4.0.0/
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
--
-- /Since 0.4.0.0/
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

-- | /Since 0.4.0.0/
type instance FieldType "tail" [a] = Maybe [a]
-- | /Since 0.4.0.0/
type instance UpdateType "tail" [a] (Maybe [a]) = [a]

-- |
-- >>> #tail []
-- Nothing
--
-- >>> #tail [1, 2, 3]
-- Just [2, 3]
--
-- /Since 0.4.0.0/
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
--
-- /Since 0.4.0.0/
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

-- $usageExamples
--
-- @
-- -- Basic set of language extensions required when defining instances for
-- -- classes and type families from "Data.OverloadedRecords".
-- {-\# LANGUAGE DataKinds \#-}
-- {-\# LANGUAGE FlexibleInstances \#-}
-- {-\# LANGUAGE MultiParamTypeClasses \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE TypeFamilies \#-}
--
-- -- Following language extensions are required by code like this:
--
-- {-\# LANGUAGE ConstraintKinds \#-}
--     -- Codomain of type family 'R' is a 'Constraint' kind.
--
-- {-\# LANGUAGE FlexibleContexts \#-}
--     -- Required in example when field type (second argument of ':::') is a
--     -- specific type instead of a polymorphic type.
--
-- {-\# LANGUAGE TypeOperators \#-}
--     -- Required due to usage of ':::' type alias.
--
-- -- Following language extensions are available only in GHC >=8:
--
-- {-\# LANGUAGE OverloadedLabels \#-}
--     -- Enables #label syntactic sugar.
--
-- module Example
--   where
--
-- import Data.Default (Default(def))
--     -- Provided by one of these packages:
--     --
--     -- * <https://hackage.haskell.org/package/data-default data-default>
--     -- * <https://hackage.haskell.org/package/data-default data-default-extra>
--
-- import "Data.OverloadedRecords"
-- import "Data.OverloadedRecords.TH" ('Data.OverloadedRecords.TH.overloadedRecord')
--
--
-- data V3 a = V3
--     { v3x :: !a
--     , v3y :: !a
--     , v3z :: !a
--     }
--   deriving Show
--
-- -- Following line derives instances for various type classes and type
-- -- families that are provided by the overloaded-records library.
-- --
-- -- However with def (default settings) this is done only for fields that
-- -- start with type name, data constructor name, or underscore. Prefix is
-- -- stripped. In example \"v3x\" is transformed in to \"x\" and so would be
-- -- \"_x\".
-- 'Data.OverloadedRecords.TH.overloadedRecord' def ''V3
--
-- data V4 a = V4
--     { v4x :: !a
--     , v4y :: !a
--     , v4z :: !a
--     , v4t :: !a
--     }
--   deriving Show
--
-- 'Data.OverloadedRecords.TH.overloadedRecord' def ''V4
--
-- zeroV3
--     :: (Num a, 'R' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r)
--     => r -> r
-- zeroV3 = 'set'' \#x 0 . 'set'' \#y 0 . 'set'' \#z 0
-- @
--
-- The following type signatures for @zeroV3@ are equivalent:
--
-- @
-- zeroV3
--     :: (Num a, 'R' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r)
--     => r -> r
-- @
--
-- @
-- zeroV3
--     ::  ( Num a
--         , 'ModifyField'' \"x\" r a
--         , 'ModifyField'' \"y\" r a
--         , 'ModifyField'' \"z\" r a
--         )
--     => r -> r
-- @
--
-- One of the biggest features of /Overloaded Records/ is the possibility to
-- define functions that do not depend on concrete data types, but on the
-- \"fields\" they provide. In example function @zeroV3@ can be applied to
-- anything that has fields @\"x\"@, @\"y\"@, and @\"z\"@ that reference values
-- of some @Num@ type:
--
-- >>> zeroV3 (V3 1 1 1 :: V3 Int)
-- V3 {_x = 0, _y = 0, _z = 0}
--
-- >>> zeroV3 (V4 1 1 1 1 :: V4 Int)
-- V4 {_x = 0, _y = 0, _z = 0, _t = 1}
--
-- Function @zeroV3@ can be also defined using operators from
-- <https://hackage.haskell.org/package/lens lens> library:
--
-- @
-- import Control.Lens ((.~), simple)
--
-- zeroV3
--     :: (Num a, 'R' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r)
--     => r -> r
-- zeroV3 r = r
--     & \#x . simple .~ 0
--     & \#y . simple .~ 0
--     & \#z . simple .~ 0
-- @
--
-- However, following function would fail to compile:
--
-- @
-- incV3
--     :: (Num a, 'R' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r)
--     => r -> r
-- incV3 r = r
--     & \#x . simple .~ \#x r + 1
--     & \#y . simple .~ \#y r + 1
--     & \#z . simple .~ \#z r + 1
-- @
--
-- The problem is that we have two 'IsLabel' instances at play. One is for a
-- lens and the other one is for getter. Unfortunatelly these two instances are
-- mutually exclusive in case of polymorphic value. There are multiple
-- solutions to this. Use lenses all the time, e.g. in general by using @^.@
-- for getting the value, or in this case by using @+~@ operator for
-- incrementing. Example of using @+~@:
--
-- @
-- import Control.Lens ((.~), (+~), simple)
--
-- incV3
--     :: (Num a, 'R' '[\"x\" ':::' a, \"y\" ':::' a, \"z\" ':::' a] r)
--     => r -> r
-- incV3 r = r
--     & \#x . simple +~ 1
--     & \#y . simple +~ 1
--     & \#z . simple +~ 1
-- @
