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
import Data.Function (($), (&), (.), const)
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
        , testCase "Bar (Just True) & bar .~ Nothing = Bar Nothing"
            $ (Bar (Just True) & bar .~ Nothing) @?= Bar Nothing
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
