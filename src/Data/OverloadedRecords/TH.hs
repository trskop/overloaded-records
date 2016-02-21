{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

#if HAVE_MONAD_FAIL && MIN_VERSION_template_haskell(2,11,0)
#define _FAIL_IN_MONAD
#else
#define _FAIL_IN_MONAD , fail
#endif

module Data.OverloadedRecords.TH
  where

import Prelude (Num((-)), error, fromIntegral)

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
import Data.Traversable (forM, mapM)
import Data.Typeable (Typeable)
import Data.Word (Word)
--import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import Text.Show (Show(show))

import Language.Haskell.TH
    ( Con(ForallC, InfixC, NormalC, RecC)
    , Dec(DataD, NewtypeD)
--  , DecQ
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
--  , mkName
    , nameBase
    , newName
    , reify
--  , sigD
    , strTyLit
    , varE
    , varP
    , varT
    , wildP
    )

import Data.Default.Class (Default(def))

import Data.OverloadedRecords


data DeriveOverloadedRecordsParams = DeriveOverloadedRecordsParams
    { _strictAccessors :: Bool
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

instance Default DeriveOverloadedRecordsParams where
    def = DeriveOverloadedRecordsParams
        { _strictAccessors = False
        , _makeAccessorName = defaultMakeAccessorName
        }

deriveOverloadedRecords :: DeriveOverloadedRecordsParams -> Name -> DecsQ
deriveOverloadedRecords params = withReified $ \name -> \case
    TyConI dec -> case dec of
        -- Not supporting DatatypeContexts, hence the [] required as the first
        -- argument to NewtypeD and DataD.
        NewtypeD [] typeName typeVars constructor _deriving ->
            deriveForConstructor params typeName typeVars constructor
        DataD [] typeName typeVars constructors _deriving ->
            concat <$> mapM (deriveForConstructor params typeName typeVars) constructors
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
        deriveFor constructorName args $ \(accessorName, strict, argType) f ->
            f (Just accessorName) strict argType

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
            f arg (deriveForField params name typeVars' constrName numArgs idx)
      where
        numArgs = fromIntegral $ List.length args
        typeVars' = List.map typeName typeVars

        typeName :: TyVarBndr -> Name
        typeName = \case
            PlainTV n -> n
            KindedTV n _kind -> n

    withIndexes = List.zip [(0 :: Word) ..]

-- TODO: Create a data type for all the arguments.
deriveForField
    :: DeriveOverloadedRecordsParams
    -> Name
    -- ^ Record type name.
    -> [Name]
    -- ^ Record type arguments.
    -> Name
    -- ^ Constructor name.
    -> Word
    -- ^ Number of constructor arguments.
    -> Word
    -- ^ Index of a constructor argument, starting from zero.
    -> Maybe Name
    -- ^ Accessor name of a constructor argument. 'Nothing' means that there is
    -- no accessor available for it.
    -> Strict
    -- ^ Strictness of constructor argument.
    -> Type
    -- ^ Type of constructor argument.
    -> DecsQ
deriveForField params typeName typeVars constrName numArgs idx accessor
  _strict fieldType =
    case possiblyLabel of
        Nothing -> return []
        Just label -> (<>)
            <$> deriveGetter labelType recordType (return fieldType) getterExpr
            <*> deriveSetter labelType recordType (return fieldType) newRecordType
                    newFieldType setterExpr
          where
            labelType = strTyLitT label
  where
    possiblyLabel = _makeAccessorName params (nameBase typeName) (nameBase constrName)
        idx (fmap nameBase accessor)

    recordType = foldl appT (conT typeName) $ List.map varT typeVars

    -- TODO: When field type is polymorphic, then we should allow to change it.
    newFieldType = return fieldType
    newRecordType = recordType

    -- \(C _ _ ... _ a _ _ ... _) -> a
    getterExpr = do
        a <- newName "a"
        lamE [return . ConP constrName $ nthArg (VarP a)] (varE a)
      where
        nthArg :: Pat -> [Pat]
        nthArg var = wildPs idx <> (var : wildPs (numArgs - idx - 1))

    -- \(C a_0 a_1 ... a_(i - 1) _ a_(i + 1) a_(i + 2) ... a_(n)) b ->
    --     C a_0 a_1 ... a_(i - 1) b a_(i + 1) a_(i + 2) ... a_(n)
    setterExpr = do
        varsBefore <- newNames idx "a"
        b <- newName "b"
        varsAfter <- newNames (numArgs - idx - 1) "a"

        lamE [constrPattern varsBefore varsAfter, varP b]
            $ constrExpression varsBefore (varE b) varsAfter
      where
        constrPattern before after =
            conP constrName $ varPs before <> (wildP : varPs after)

        constrExpression before b after =
            foldl appE (conE constrName) $ varEs before <> (b : varEs after)

deriveGetter :: TypeQ -> TypeQ -> TypeQ -> ExpQ -> DecsQ
deriveGetter labelType recordType fieldType getter =
    [d| type instance FieldType $(labelType) $(recordType) = $(fieldType)

        instance HasField $(labelType) $(recordType) $(fieldType) where
            getField _proxy = $(getter)
    |]

deriveSetter :: TypeQ -> TypeQ -> TypeQ -> TypeQ -> TypeQ -> ExpQ -> DecsQ
deriveSetter labelType recordType fieldType newRecordType newFieldType setter =
    [d| type instance UpdateType $(labelType) $(recordType) $(newFieldType) =
            $(newRecordType)

        instance SetField $(labelType) $(recordType) $(fieldType) where
            setField _proxy = $(setter)
    |]

wildPs :: Word -> [Pat]
wildPs n = List.replicate (fromIntegral n) WildP

newNames :: Word -> String -> Q [Name]
newNames n s = fromIntegral n `replicateM` newName s

varPs :: [Name] -> [PatQ]
varPs = List.map varP

varEs :: [Name] -> [ExpQ]
varEs = List.map varE

strTyLitT :: String -> TypeQ
strTyLitT = litT . strTyLit
