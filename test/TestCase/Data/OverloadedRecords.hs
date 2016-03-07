{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Unit tests for OverloadedRecords and related TH functions.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.OverloadedRecords (tests)
  where

import Data.Bool (Bool(False, True))
import Data.Eq (Eq)
import Data.Function (($), (.), const, flip, id)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Text.Show (Show(show))

import Data.Default.Class (Default(def))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Data.OverloadedLabels.TH (label, labels)
import Data.OverloadedRecords (HasField(getField), ModifyField(setField))
import Data.OverloadedRecords.TH (defaultMakeFieldName, overloadedRecord)


data Foo a = Foo
    { _x :: Int
    , _y :: a
    }
  deriving (Eq, Show)

overloadedRecord def ''Foo
labels ["x", "y"]

newtype Bar a = Bar {_bar :: a}
  deriving (Eq, Show)

overloadedRecord def ''Bar
label "bar"

-- Type isomorphic to (a, b), the reason for not using (a, b) directly is to
-- prevent any possible instance declaration clashes in the future.
data Pair a b = Pair a b
  deriving (Eq, Show)

{-
type instance FieldType "fst" (Pair a b) = a
type instance FieldType "snd" (Pair a b) = b

type instance UpdateType "fst" (Pair a b) a' = Pair a' b
type instance UpdateType "snd" (Pair a b) b' = Pair a  b'
-}

instance HasField "fst" (Pair a b) a where
    getField _proxy (Pair a _) = a

instance HasField "snd" (Pair a b) b where
    getField _proxy (Pair _ b) = b

instance ModifyField "fst" (Pair a b) (Pair a' b) a a' where
    setField _proxy (Pair _ b) a' = Pair a' b

instance ModifyField "snd" (Pair a b) (Pair a b') b b' where
    setField _proxy (Pair a _) b' = Pair a b'

labels ["fst", "snd"]

tests :: [Test]
tests =
    [ testGroup "defaultMakeFieldName" $ map test_defaultMakeField
        [ (Just "_foo", Just "foo")
        , (Just "_Foo", Just "foo")
        , (Just "typeNameFieldName", Just "fieldName")
        , (Just "constructorNameFieldName", Just "fieldName")
        , (Just "somethingElseEntirely", Nothing)
        , (Nothing, Nothing)
        ]
    , testGroup "overloadedRecord"
        [ testCase "x (Foo 1 False) = 1"
            $ x (Foo 1 False) @?= 1
        , testCase "y (Foo 1 False) = False"
            $ y (Foo 1 False) @?= False
        , testCase "bar (Bar (Just True)) = Just True"
            $ bar (Bar (Just True)) @?= Just True
        , testCase "Foo 1 False & x .~ 2 = Foo 2 False"
            $ (Foo 1 False & x .~ 2) @?= Foo 2 False
        , testCase "Foo 1 False & y .~ True = Foo 1 True"
            $ (Foo 1 False & y .~ True) @?= Foo 1 True
        , testCase "Bar (Just True) & bar .~ Nothing =\
            \ Bar (Nothing :: Maybe Bool)"
            $ (Bar (Just True) & bar .~ Nothing)
                @?= Bar (Nothing :: Maybe Bool)
        , testCase "Bar (Just True) & bar .~ Nothing =\
            \ Bar (Nothing :: Maybe Bool)"
            $ (Bar (Just True) & bar .~ Nothing)
                @?= Bar (Nothing :: Maybe Bool)
        , testCase "Bar (Just True) & simple . bar .~ Nothing = Bar Nothing"
            $ (Bar (Just True) & simple . bar .~ Nothing) @?= Bar Nothing
        ]
    , testGroup "Type changing assignment"
        [ testCase "fst (Pair (1 :: Int) False) = 1"
            $ fst (Pair (1 :: Int) False) @?= 1
        , testCase "snd (Pair (1 :: Int) False) = False"
            $ snd (Pair (1 :: Int) False) @?= False
        , testCase "Pair (1 :: Int) False & fst .~ True = Pair True False"
            $ (Pair (1 :: Int) False & fst .~ True) @?= Pair True False
        , testCase "Pair (1 :: Int) False & fst .~ Just True =\
            \ Pair 1 (Just True)"
            $ (Pair (1 :: Int) False & snd .~ Just True) @?= Pair 1 (Just True)
        ]
    ]
  where
    test_defaultMakeField (input, output) =
        testCase (show input <> " -> " <> show output)
            $ defaultMakeFieldName tn cn 0 input @?= output
      where
        tn = "TypeName"
        cn = "ConstructorName"

    (.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
    (.~) l b = runIdentity . l (const $ Identity b)
    infixr 4 .~

    -- Data.Function constains (&) since base 4.8.0.0, which was bundled with
    -- GHC 7.10, but we are supporting also GHC 7.8.
    (&) :: a -> (a -> b) -> b
    (&) = flip ($)
    infixl 1 &

    simple :: p a (f a) -> p a (f a)
    simple = id
