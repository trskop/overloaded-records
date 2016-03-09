{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Derive magic instances for OverloadedRecordFields.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, NoImplicitPrelude
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

import Data.OverloadedRecords.TH.Internal
    ( DeriveOverloadedRecordsParams
    , FieldDerivation
    , OverloadedField(..)
    , defaultFieldDerivation
    , defaultMakeFieldName
    , field
#ifndef HAVE_OVERLOADED_LABELS
    , fieldDerivation
#endif
    , fieldGetter
    , fieldSetter
    , overloadedRecord
    , overloadedRecordFor
    , overloadedRecords
    , overloadedRecordsFor
    , simpleField
    , simpleFieldSetter
    )
