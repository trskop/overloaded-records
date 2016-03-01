{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

#ifndef HAVE_OVERLOADED_LABELS
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
#endif

#if 0
#if HAVE_MONAD_FAIL && MIN_VERSION_template_haskell(2,11,0)
#define _FAIL_IN_MONAD
#else
#define _FAIL_IN_MONAD , fail
#endif
#endif

-- Package template-haskell 2.11 is missing MonadFail instance for Q.
#define _FAIL_IN_MONAD , fail

-- |
-- Module:       $HEADER$
-- Description:  Derive magic instances for OverloadedRecordFields.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, DataKinds, DeriveDataTypeable, DeriveGeneric,
--               FlexibleContexts (GHC <8), FlexibleInstances, LambdaCase,
--               MagicHash (GHC <8), MultiParamTypeClasses, NoImplicitPrelude,
--               RecordWildCards, TemplateHaskell, TupleSections, TypeFamilies,
--               TypeSynonymInstances
--
-- Derive magic instances for OverloadedRecordFields.
module Data.OverloadedRecords.TH
    (
    -- * Derive OverloadedRecordFields instances
      overloadedRecord
    , overloadedRecords
    , overloadedRecordFor
    , overloadedRecordsFor

    -- ** Customize Derivation Process
    , DeriveOverloadedRecordsParams
#ifndef HAVE_OVERLOADED_LABELS
    , fieldDerivation
#endif
    , FieldDerivation
    , OverloadedField(..)
    , defaultFieldDerivation
    , defaultMakeFieldName

    -- * Low-level Deriving Mechanism
    , field
    , simpleField
    , fieldGetter
    , fieldSetter
    , simpleFieldSetter
    )
  where

import Prelude (Num((-)), fromIntegral)

import Control.Applicative (Applicative((<*>)))
import Control.Monad (Monad((>>=) _FAIL_IN_MONAD, return), replicateM)
#if 0
#if HAVE_MONAD_FAIL && MIN_VERSION_template_haskell(2,11,0)
import Control.Monad.Fail (MonadFail(fail))
#endif
#endif
import Data.Bool (Bool(False), otherwise)
import qualified Data.Char as Char (toLower)
import Data.Foldable (concat, foldl)
import Data.Function ((.), ($))
import Data.Functor (Functor(fmap), (<$>))
import qualified Data.List as List
    ( drop
    , isPrefixOf
    , length
    , map
    , replicate
    , zip
    )
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Traversable (forM, mapM)
import Data.Typeable (Typeable)
import Data.Word (Word)
import GHC.Generics (Generic)
#ifndef HAVE_OVERLOADED_LABELS
import GHC.Exts (Proxy#, proxy#)
#endif
import Text.Show (Show(show))

import Language.Haskell.TH
    ( Con(ForallC, InfixC, NormalC, RecC)
    , Dec(DataD, NewtypeD)
    , DecsQ
    , ExpQ
    , Info(TyConI)
    , Name
    , Pat(ConP, VarP, WildP)
    , PatQ
    , Q
    , Strict
    , Type
    , TypeQ
    , TyVarBndr(KindedTV, PlainTV)
    , appE
    , appT
    , conE
    , conP
    , conT
    , lamE
    , litT
    , nameBase
    , newName
    , recUpdE
    , reify
    , strTyLit
    , varE
    , varP
    , varT
    , wildP
    )

import Data.Default.Class (Default(def))

#ifndef HAVE_OVERLOADED_LABELS
import Data.OverloadedLabels (IsLabel(fromLabel))
#endif
import Data.OverloadedRecords
    ( FieldType
    , HasField(getField)
    , SetField(setField)
    , UpdateType
    )


#ifndef HAVE_OVERLOADED_LABELS
-- | Overloaded label that can be used for accessing function of type
-- 'FieldDerivation' from 'DeriveOverloadedRecordsParams'.
fieldDerivation :: IsLabel "fieldDerivation" a => a
fieldDerivation = fromLabel (proxy# :: Proxy# "fieldDerivation")
#endif

-- | Parameters for customization of deriving process. Use 'def' to get
-- default behaviour.
data DeriveOverloadedRecordsParams = DeriveOverloadedRecordsParams
    { _strictFields :: Bool
    -- ^ Make setter and getter strict. **Currently unused.**
    , _fieldDerivation :: FieldDerivation
    -- ^ See 'FieldDerivation' for description.
    }
  deriving (Generic, Typeable)

type instance FieldType "fieldDerivation" DeriveOverloadedRecordsParams =
    FieldDerivation

instance
    HasField "fieldDerivation" DeriveOverloadedRecordsParams FieldDerivation
  where
    getField _proxy = _fieldDerivation

type instance
    UpdateType "fieldDerivation" DeriveOverloadedRecordsParams FieldDerivation =
        DeriveOverloadedRecordsParams

instance
    SetField "fieldDerivation" DeriveOverloadedRecordsParams FieldDerivation
  where
    setField _proxy s b = s{_fieldDerivation = b}

-- | Describes what should be the name of overloaded record field, and can also
-- provide custom implementation of getter and setter.
data OverloadedField
    = GetterOnlyField String (Maybe ExpQ)
    -- ^ Derive only getter instances. If second argument is 'Just', then it
    -- contains custom definition of getter function.
    | GetterAndSetterField String (Maybe (ExpQ, ExpQ))
    -- ^ Derive only getter instances. If second argument is 'Just', then it
    -- contains custom definitions of getter and setter functions,
    -- respectively.
  deriving (Generic, Typeable)

-- | Type signature of a function that can customize the derivation of each
-- individual overloaded record field.
--
-- If field has an selector then the function will get its name or 'Nothing'
-- otherwise.  Function has to return 'Nothing' in case when generating
-- overloaded record field instances is not desired.
type FieldDerivation
    =  String
    -- ^ Name of the type, of which this field is part of.
    -> String
    -- ^ Name of the constructor, of which this field is part of.
    -> Word
    -- ^ Field position as an argument of the constructor it is part of.
    -- Indexing starts from zero.
    -> Maybe String
    -- ^ Name of the field (record) accessor; 'Nothing' means that there is no
    -- record accessor defined for it.
    -> Maybe OverloadedField
    -- ^ Describes how overloaded record field should be generated for this
    -- specific constructor field. 'Nothing' means that no overloaded record
    -- field should be derived. See also 'OverloadedField' for details.

-- | Suppose we have a weird type definition as this:
--
-- @
-- data SomeType a b c = SomeConstructor
--     { _fieldX :: a
--     , someTypeFieldY :: b
--     , someConstructorFieldZ :: c
--     , anythingElse :: (a, b, c)
--     }
-- @
--
-- Then for each of those fields, 'defaultMakeFieldName' will produce
-- expected OverloadedLabel name:
--
-- * @_fieldX --> fieldX@
--
-- * @someTypeFieldY --> fieldY@
--
-- * @someConstructorFieldZ --> fieldZ@
--
-- * @anythingElse@ is ignored
defaultMakeFieldName
    :: String
    -- ^ Name of the type, of which this field is part of.
    -> String
    -- ^ Name of the constructor, of which this field is part of.
    -> Word
    -- ^ Field position as an argument of the constructor it is part of.
    -- Indexing starts from zero.
    -> Maybe String
    -- ^ Name of the field (record) accessor; 'Nothing' means that there is no
    -- record accessor defined for it.
    -> Maybe String
    -- ^ Overloaded record field name to be used for this specific constructor
    -- field; 'Nothing' means that there shouldn't be a label associated with
    -- it.
defaultMakeFieldName typeName constructorName _fieldPosition = \case
    Nothing -> Nothing
    Just fieldName
      | startsWith "_"               -> Just $ dropPrefix "_"        fieldName
      | startsWith typePrefix        -> Just $ dropPrefix typePrefix fieldName
      | startsWith constructorPrefix -> Just $ dropPrefix typePrefix fieldName
      | otherwise                    -> Nothing
      where
        startsWith :: String -> Bool
        startsWith = (`List.isPrefixOf` fieldName)

        dropPrefix :: String -> String -> String
        dropPrefix s = List.drop (List.length s)

        headToLower "" = ""
        headToLower (x : xs) = Char.toLower x : xs

        typePrefix = headToLower typeName
        constructorPrefix = headToLower constructorName

-- | Function used by default value of 'DeriveOverloadedRecordsParams'.
defaultFieldDerivation :: FieldDerivation
defaultFieldDerivation =
    (((fmap (`GetterAndSetterField` Nothing) .) .) .) . defaultMakeFieldName

-- |
-- @
-- 'def' = 'DeriveOverloadedRecordsParams'
--     { strictFields = 'False'
--     , 'fieldDerivation' = 'defaultFieldDerivation'
--     }
-- @
instance Default DeriveOverloadedRecordsParams where
    def = DeriveOverloadedRecordsParams
        { _strictFields = False
        , _fieldDerivation = defaultFieldDerivation
        }

-- | Derive magic OverloadedRecordFields instances for specified type.
overloadedRecord
    :: DeriveOverloadedRecordsParams
    -- ^ Parameters for customization of deriving process. Use 'def' to get
    -- default behaviour.
    -> Name
    -- ^ Name of the type for which magic instances should be derived.
    -> DecsQ
overloadedRecord params = withReified $ \name -> \case
    TyConI dec -> case dec of
        -- Not supporting DatatypeContexts, hence the [] required as the first
        -- argument to NewtypeD and DataD.
#if MIN_VERSION_template_haskell(2,11,0)
        NewtypeD [] typeName typeVars _kindSignature constructor _deriving ->
#else
        NewtypeD [] typeName typeVars constructor _deriving ->
#endif
            deriveForConstructor params typeName typeVars constructor
#if MIN_VERSION_template_haskell(2,11,0)
        DataD [] typeName typeVars _kindSignature constructors _deriving ->
#else
        DataD [] typeName typeVars constructors _deriving ->
#endif
            fmap concat . forM constructors
                $ deriveForConstructor params typeName typeVars
        x -> canNotDeriveError name x

    x -> canNotDeriveError name x
  where
    withReified :: (Name -> Info -> Q a) -> Name -> Q a
    withReified f t = (reify t >>= f t)

    canNotDeriveError :: Show a => Name -> a -> Q b
    canNotDeriveError = (fail .) . errMessage

    errMessage :: Show a => Name -> a -> String
    errMessage n x =
        "`" <> show n <> "' is neither newtype nor data type: " <> show x

-- | Derive magic OverloadedRecordFields instances for specified types.
overloadedRecords
    :: DeriveOverloadedRecordsParams
    -- ^ Parameters for customization of deriving process. Use 'def' to get
    -- default behaviour.
    -> [Name]
    -- ^ Names of the types for which magic instances should be derived.
    -> DecsQ
overloadedRecords params = fmap concat . mapM (overloadedRecord params)

-- | Derive magic OverloadedRecordFields instances for specified type.
--
-- Similar to 'overloadedRecords', but instead of
-- 'DeriveOverloadedRecordsParams' value it takes function which can modify its
-- default value.
--
-- @
-- data Coordinates2D a
--     { coordinateX :: a
--     , coordinateY :: a
--     }
--
-- 'overloadedRecordsFor' ''Coordinates2D
--     $ \#fieldDerivation .~ \\_ _ _ -> \\case
--         Nothing -> Nothing
--         Just field -> lookup field
--            [ (\"coordinateX\", 'GetterOnlyField' \"x\" Nothing)
--            , (\"coordinateY\", 'GetterOnlyField' \"y\" Nothing)
--            ]
-- @
overloadedRecordFor
    :: Name
    -- ^ Name of the type for which magic instances should be derived.
    -> (DeriveOverloadedRecordsParams -> DeriveOverloadedRecordsParams)
    -- ^ Function that modifies parameters for customization of deriving
    -- process.
    -> DecsQ
overloadedRecordFor typeName f = overloadedRecord (f def) typeName

-- | Derive magic OverloadedRecordFields instances for specified types.
overloadedRecordsFor
    :: [Name]
    -- ^ Names of the types for which magic instances should be derived.
    -> (DeriveOverloadedRecordsParams -> DeriveOverloadedRecordsParams)
    -- ^ Function that modifies parameters for customization of deriving
    -- process.
    -> DecsQ
overloadedRecordsFor typeNames f = overloadedRecords (f def) typeNames

-- | Derive magic instances for all fields of a specific data constructor of a
-- specific type.
deriveForConstructor
    :: DeriveOverloadedRecordsParams
    -- ^ Parameters for customization of deriving process. Use 'def' to get
    -- default behaviour.
    -> Name
    -> [TyVarBndr]
    -> Con
    -> DecsQ
deriveForConstructor params name typeVars = \case
    NormalC constructorName args ->
        deriveFor constructorName args $ \(strict, argType) f ->
            f Nothing strict argType

    RecC constructorName args ->
        deriveFor constructorName args $ \(accessor, strict, argType) f ->
            f (Just accessor) strict argType

    InfixC arg0 constructorName arg1 ->
        deriveFor constructorName [arg0, arg1] $ \(strict, argType) f ->
            f Nothing strict argType

#if 0
#if MIN_VERSION_template_haskell(2,11,0)
    GadtC _ _ _ ->
    RecGadtC _ _ _ ->
#endif
#endif

    -- Existentials aren't supported.
    ForallC _typeVariables _context _constructor -> return []
  where
    deriveFor
        :: Name
        -> [a]
        -> (a -> (Maybe Name -> Strict -> Type -> DecsQ) -> DecsQ)
        -> DecsQ
    deriveFor constrName args f =
        fmap concat . forM (withIndexes args) $ \(idx, arg) ->
            f arg $ \accessor strict fieldType' ->
                deriveForField params DeriveFieldParams
                    { typeName = name
                    , typeVariables = List.map getTypeName typeVars
                    , constructorName = constrName
                    , numberOfArgs = fromIntegral $ List.length args
                    , currentIndex = idx
                    , accessorName = accessor
                    , strictness = strict
                    , fieldType = fieldType'
                    }
      where
        getTypeName :: TyVarBndr -> Name
        getTypeName = \case
            PlainTV n -> n
            KindedTV n _kind -> n

    withIndexes = List.zip [(0 :: Word) ..]

-- | Parameters for 'deriveForField' function.
data DeriveFieldParams = DeriveFieldParams
    { typeName :: Name
    -- ^ Record name, i.e. type constructor name.
    , typeVariables :: [Name]
    -- ^ Free type variables of a type constructor.
    , constructorName :: Name
    -- ^ Data constructor name.
    , numberOfArgs :: Word
    -- ^ Number of arguments that data constructor takes.
    , currentIndex :: Word
    -- ^ Index of the current argument of a data constructor for which we are
    -- deriving overloaded record field instances. Indexing starts from zero.
    -- In other words 'currentIndex' is between zero (including) and
    -- 'numberOfArgs' (excluding).
    , accessorName :: Maybe Name
    -- ^ Record field accessor, if available, otherwise 'Nothing'.
    , strictness :: Strict
    -- ^ Strictness annotation of the current data constructor argument.
    , fieldType :: Type
    -- ^ Type of the current data constructor argument.
    }

-- | Derive magic instances for a specific field of a specific type.
deriveForField
    :: DeriveOverloadedRecordsParams
    -- ^ Parameters for customization of deriving process. Use 'def' to get
    -- default behaviour.
    -> DeriveFieldParams
    -- ^ All the necessary information for derivation procedure.
    -> DecsQ
deriveForField params DeriveFieldParams{..} =
    case possiblyLabel of
        Nothing -> return []
        Just (GetterOnlyField label customGetterExpr) ->
            deriveGetter' (strTyLitT label)
                $ fromMaybe derivedGetterExpr customGetterExpr
        Just (GetterAndSetterField label customGetterAndSetterExpr) -> (<>)
            <$> deriveGetter' labelType getterExpr
            <*> deriveSetter' labelType setterExpr
          where
            labelType = strTyLitT label

            (getterExpr, setterExpr) =
                fromMaybe (derivedGetterExpr, derivedSetterExpr)
                    customGetterAndSetterExpr
  where
    possiblyLabel = _fieldDerivation params (nameBase typeName)
        (nameBase constructorName) currentIndex (fmap nameBase accessorName)

    deriveGetter' labelType =
        deriveGetter labelType recordType (return fieldType)

    deriveSetter' labelType =
        deriveSetter labelType recordType (return fieldType) newRecordType
            newFieldType

    recordType = foldl appT (conT typeName) $ List.map varT typeVariables

    -- TODO: When field type is polymorphic, then we should allow to change it.
    newFieldType = return fieldType
    newRecordType = recordType

    -- Number of variables, i.e. arguments of a constructor, to the right of
    -- the currently processed field.
    numVarsOnRight = numberOfArgs - currentIndex - 1

    inbetween :: (a -> [b]) -> a -> a -> b -> [b]
    inbetween f a1 a2 b = f a1 <> (b : f a2)

    derivedGetterExpr = case accessorName of
        Just name -> varE name
        Nothing -> do
            a <- newName "a"
            -- \(C _ _ ... _ a _ _ ... _) -> a
            lamE [return . ConP constructorName $ nthArg (VarP a)] (varE a)
      where
        nthArg :: Pat -> [Pat]
        nthArg = inbetween wildPs currentIndex numVarsOnRight

    derivedSetterExpr = case accessorName of
        Just name -> do
            s <- newName "s"
            b <- newName "b"
            lamE [varP s, varP b] $ recUpdE (varE s) [(name, ) <$> varE b]
        Nothing -> do
            varsBefore <- newNames currentIndex "a"
            b <- newName "b"
            varsAfter <- newNames numVarsOnRight "a"

            -- \(C a_0 a_1 ... a_(i - 1) _ a_(i + 1) a_(i + 2) ... a_(n)) b ->
            --     C a_0 a_1 ... a_(i - 1) b a_(i + 1) a_(i + 2) ... a_(n)
            lamE [constrPattern varsBefore varsAfter, varP b]
                $ constrExpression varsBefore (varE b) varsAfter
          where
            constrPattern before after =
                conP constructorName $ inbetween varPs before after wildP

            constrExpression before b after = foldl appE (conE constructorName)
                $ varEs before <> (b : varEs after)

-- | Derive instances for overloaded record field, both getter and setter.
field
    :: String
    -- ^ Overloaded label name.
    -> TypeQ
    -- ^ Record type.
    -> TypeQ
    -- ^ Field type.
    -> TypeQ
    -- ^ Record type after update.
    -> TypeQ
    -- ^ Setter will set field to a value of this type.
    -> ExpQ
    -- ^ Getter function.
    -> ExpQ
    -- ^ Setter function.
    -> DecsQ
field label recType fldType newRecType newFldType getterExpr setterExpr = (<>)
    <$> deriveGetter labelType recType fldType getterExpr
    <*> deriveSetter labelType recType fldType newRecType newFldType setterExpr
  where
    labelType = strTyLitT label

-- | Derive instances for overloaded record field, both getter and setter. Same
-- as 'field', but record type is the same before and after update and so is
-- the field type.
simpleField
    :: String
    -- ^ Overloaded label name.
    -> TypeQ
    -- ^ Record type.
    -> TypeQ
    -- ^ Field type.
    -> ExpQ
    -- ^ Getter function.
    -> ExpQ
    -- ^ Setter function.
    -> DecsQ
simpleField label recType fldType = field label recType fldType recType fldType

-- | Derive instances for overloaded record field getter.
fieldGetter
    :: String
    -- ^ Overloaded label name.
    -> TypeQ
    -- ^ Record type.
    -> TypeQ
    -- ^ Field type
    -> ExpQ
    -- ^ Getter function.
    -> DecsQ
fieldGetter = deriveGetter . strTyLitT

-- | Derive only getter related instances.
deriveGetter :: TypeQ -> TypeQ -> TypeQ -> ExpQ -> DecsQ
deriveGetter labelType recordType fieldType getter =
    [d| type instance FieldType $(labelType) $(recordType) = $(fieldType)

        instance HasField $(labelType) $(recordType) $(fieldType) where
            getField _proxy = $(getter)
    |]

-- | Derive instances for overloaded record field setter. Same as
-- 'fieldSetter', but record type is the same before and after update and so is
-- the field type.
simpleFieldSetter
    :: String
    -- ^ Overloaded label name.
    -> TypeQ
    -- ^ Record type.
    -> TypeQ
    -- ^ Field type.
    -> ExpQ
    -- ^ Setter function.
    -> DecsQ
simpleFieldSetter label recordType fieldType =
    fieldSetter label recordType fieldType recordType fieldType

-- | Derive instances for overloaded record field setter.
fieldSetter
    :: String
    -- ^ Overloaded label name.
    -> TypeQ
    -- ^ Record type.
    -> TypeQ
    -- ^ Field type.
    -> TypeQ
    -- ^ Record type after update.
    -> TypeQ
    -- ^ Setter will set field to a value of this type.
    -> ExpQ
    -- ^ Setter function.
    -> DecsQ
fieldSetter = deriveSetter . strTyLitT

-- | Derive only setter related instances.
deriveSetter :: TypeQ -> TypeQ -> TypeQ -> TypeQ -> TypeQ -> ExpQ -> DecsQ
deriveSetter labelType recordType fieldType newRecordType newFieldType setter =
    [d| type instance UpdateType $(labelType) $(recordType) $(newFieldType) =
            $(newRecordType)

        instance SetField $(labelType) $(recordType) $(fieldType) where
            setField _proxy = $(setter)
    |]

-- | Construct list of wildcard patterns ('WildP').
wildPs :: Word -> [Pat]
wildPs n = List.replicate (fromIntegral n) WildP

-- | Construct list of new names usin 'newName'.
newNames :: Word -> String -> Q [Name]
newNames n s = fromIntegral n `replicateM` newName s

varPs :: [Name] -> [PatQ]
varPs = List.map varP

varEs :: [Name] -> [ExpQ]
varEs = List.map varE

strTyLitT :: String -> TypeQ
strTyLitT = litT . strTyLit
