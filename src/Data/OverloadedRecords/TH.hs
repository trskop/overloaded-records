{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

#if HAVE_MONAD_FAIL && MIN_VERSION_template_haskell(2,11,0)
#define _FAIL_IN_MONAD
#else
#define _FAIL_IN_MONAD , fail
#endif

-- |
-- Module:       $HEADER$
-- Description:  Derive magic instances for OverloadedRecordFields.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Derive magic instances for OverloadedRecordFields.
module Data.OverloadedRecords.TH
  where

import Prelude (Num((-)), fromIntegral)

import Control.Applicative (Applicative((<*>)))
import Control.Monad (Monad((>>=) _FAIL_IN_MONAD, return), replicateM)
#if HAVE_MONAD_FAIL && MIN_VERSION_template_haskell(2,11,0)
import Control.Monad.Fail (MonadFail(fail))
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
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Traversable (forM)
import Data.Typeable (Typeable)
import Data.Word (Word)
import GHC.Generics (Generic)
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

import Data.OverloadedRecords
    ( FieldType
    , HasField(getField)
    , SetField(setField)
    , UpdateType
    )


data DeriveOverloadedRecordsParams = DeriveOverloadedRecordsParams
    { _strictAccessors :: Bool
    -- ^ Make setter and getter strict. **Currently unused.**
    , _makeAccessorName
        :: String
        -> String
        -> Word
        -> Maybe String
        -> Maybe String
    -- ^ Pseudo type signature of this function is:
    --
    -- @
    -- :: TypeName
    -- -> ConstructorName
    -- -> FieldIndex
    -- -> Maybe AccessorName
    -- -> Maybe OverloadedLabelName
    -- @
    --
    -- If field has eccessor then the function will get its name or 'Nothing'
    -- otherwise. Function has to return 'Nothing' in case when generating
    -- overloaded label and overloaded record field instances is not desired.
    }
  deriving (Generic, Typeable)

-- | Supose we have a weird type definition as this:
--
-- @
-- data SomeType = SomeConstructor
--     { _fieldX :: a
--     , someTypeFieldY :: b
--     , someConstructorFieldZ :: c
--     }
-- @
--
-- Then for each of those fields, 'defaultMakeAccessorName' will produce
-- expected OverloadedLabel name:
--
-- * @_fieldX --> fieldX@
--
-- * @someTypeFieldY --> fieldY@
--
-- * @someConstructorFieldZ --> fieldZ@
--
-- Any other field name is kept unchanged.
defaultMakeAccessorName
    :: String
    -- ^ Name of the type, of which this field is part of.
    -> String
    -- ^ Name of the constructor, of which this field is part of.
    -> Word
    -- ^ Field position as an argument of the constructor it is part of.
    -> Maybe String
    -- ^ Name of the field (record) accessor; 'Nothing' means that there is no
    -- record accessor defined for it.
    -> Maybe String
    -- ^ Overloaded label name for the field; 'Nothing' means that there
    -- shouldn't be a label associated with it.
defaultMakeAccessorName typeName constructorName _fieldPosition = \case
    Nothing -> Nothing
    Just fieldName
      | startsWith "_"               -> Just $ dropPrefix "_"        fieldName
      | startsWith typePrefix        -> Just $ dropPrefix typePrefix fieldName
      | startsWith constructorPrefix -> Just $ dropPrefix typePrefix fieldName
      | otherwise                    -> Just fieldName
      where
        startsWith :: String -> Bool
        startsWith = (`List.isPrefixOf` fieldName)

        dropPrefix :: String -> String -> String
        dropPrefix s = List.drop (List.length s)

        headToLower "" = ""
        headToLower (x : xs) = Char.toLower x : xs

        typePrefix = headToLower typeName
        constructorPrefix = headToLower constructorName

-- |
-- @
-- 'def' = 'DeriveOverloadedRecordsParams'
--     { '_strictAccessors' = 'False'
--     , '_makeAccessorName' = 'defaultMakeAccessorName'
--     }
-- @
instance Default DeriveOverloadedRecordsParams where
    def = DeriveOverloadedRecordsParams
        { _strictAccessors = False
        , _makeAccessorName = defaultMakeAccessorName
        }

-- | Derive magic OverloadedRecordFields instances for specified type name.
deriveOverloadedRecords
    :: DeriveOverloadedRecordsParams
    -- ^ Parameters for customization of deriving process. Use 'def' to get
    -- default behaviour.
    -> Name
    -- ^ Name of the type for which magic instances should be derived.
    -> DecsQ
deriveOverloadedRecords params = withReified $ \name -> \case
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

deriveForConstructor
    :: DeriveOverloadedRecordsParams
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

-- TODO: Create a data type for all the arguments.
deriveForField
    :: DeriveOverloadedRecordsParams
    -> DeriveFieldParams
    -> DecsQ
deriveForField params DeriveFieldParams{..} =
    case possiblyLabel of
        Nothing -> return []
        Just label -> (<>)
            <$> deriveGetter labelType recordType (return fieldType) getterExpr
            <*> deriveSetter labelType recordType (return fieldType) newRecordType
                    newFieldType setterExpr
          where
            labelType = strTyLitT label
  where
    possiblyLabel = _makeAccessorName params (nameBase typeName)
        (nameBase constructorName) currentIndex (fmap nameBase accessorName)

    recordType = foldl appT (conT typeName) $ List.map varT typeVariables

    -- TODO: When field type is polymorphic, then we should allow to change it.
    newFieldType = return fieldType
    newRecordType = recordType

    -- Number of variables, i.e. arguments of a constructor, to the right of
    -- the currently processed field.
    numVarsOnRight = numberOfArgs - currentIndex - 1

    inbetween :: (a -> [b]) -> a -> a -> b -> [b]
    inbetween f a1 a2 b = f a1 <> (b : f a2)

    getterExpr = case accessorName of
        Just name -> varE name
        Nothing -> do
            a <- newName "a"
            -- \(C _ _ ... _ a _ _ ... _) -> a
            lamE [return . ConP constructorName $ nthArg (VarP a)] (varE a)
      where
        nthArg :: Pat -> [Pat]
        nthArg = inbetween wildPs currentIndex numVarsOnRight

    setterExpr = case accessorName of
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

-- | Derive only getter related instances.
deriveGetter :: TypeQ -> TypeQ -> TypeQ -> ExpQ -> DecsQ
deriveGetter labelType recordType fieldType getter =
    [d| type instance FieldType $(labelType) $(recordType) = $(fieldType)

        instance HasField $(labelType) $(recordType) $(fieldType) where
            getField _proxy = $(getter)
    |]

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
