# Overloaded Records

[![Hackage](http://img.shields.io/hackage/v/overloaded-records.svg)][Hackage: overloaded-records]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/overloaded-records.svg)](http://packdeps.haskellers.com/reverse/overloaded-records)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/overloaded-records.svg)](https://travis-ci.org/trskop/overloaded-records)


## Description

Implementation of *Overloaded Record Fields* based on current GHC proposal. It
is built on top of functionality that is included in GHC 8.01, but it works on
older GHC versions as well. Most importantly, this library provides Template
Haskell functions for automatic deriving of instancess for `HasField` and
`SetField` type classes. With these instances overloaded fields can be used
directly as getters and lenses.

```Haskell
import Data.OverloadedRecords.TH (overloadedRecord)

newtype Bar a = Bar {_bar :: a}

overloadedRecord def ''Bar
```

On GHC 8.0.1 it is possible to just write:

```Haskell
{-# LANGUAGE OverloadedLabels #-}

import Control.Lens ((+~))

add :: Int -> Bar Int -> Bar Int
add n = #bar +~ n
```

For older GHC versions there is a family of Template Haskell functions that
will derive overloaded labels in form of standard haskell definitions:

```Haskell
import Control.Lens ((+~))
import Data.OverloadedLabels.TH (label)

label "bar"

add :: Int -> Bar Int -> Bar Int
add n = bar +~ n
```

This implementation is highly experimental and may change rapidly.

More about the current status of OverloadedRecordFields language extension can
be found on [GHC Wiki: OverloadedRecordFields][].


## Usage Example

```Haskell
{-# LANGUAGE DataKinds #-}              -- overloadedRecord, labels
{-# LANGUAGE FlexibleContexts #-}       -- labels
{-# LANGUAGE FlexibleInstances #-}      -- overloadedRecord
{-# LANGUAGE MultiParamTypeClasses #-}  -- overloadedRecord
{-# LANGUAGE TemplateHaskell #-}        -- overloadedRecord, labels
{-# LANGUAGE TypeFamilies #-}           -- overloadedRecord
module FooBar
  where

import Data.Default.Class (Default(def))

import Data.OverloadedRecords.TH (overloadedRecord)
import Data.OverloadedLabels.TH (label, labels)


data Foo a = Foo
    { _x :: Int
    , _y :: a
    }

overloadedRecord def ''Foo
labels ["x", "y"]

newtype Bar a = Bar {_bar :: a}

overloadedRecord def ''Bar
label "bar"
```


## License

The BSD 3-Clause License, see [LICENSE][] file for details. This implementation
is based on original prototype, which is under MIT License.


## Contributions

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail.


[GHC Wiki: OverloadedRecordFields]:
  https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields
  "OverloadedRecordFields language extension on GHC Wiki"
[Hackage: overloaded-records]:
  http://hackage.haskell.org/package/overloaded-records
  "overloaded-records package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[LICENSE]:
  https://github.com/trskop/overloaded-records/blob/master/LICENSE
  "License of overloaded-records package."
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
