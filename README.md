# Overloaded Records

[![Hackage](http://img.shields.io/hackage/v/overloaded-records.svg)][Hackage: overloaded-records]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/overloaded-records.svg)](http://packdeps.haskellers.com/reverse/overloaded-records)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/overloaded-records.svg)](https://travis-ci.org/trskop/overloaded-records)


## Description

Implementation of *Overloaded Record Fields* based on current GHC proposal. It
is built on top of functionality that is included in GHC 8.0.1, but it works on
older GHC versions as well. Most importantly, this library provides Template
Haskell functions for automatic deriving of instancess for `HasField` and
`ModifyField` type classes. With these instances overloaded fields can be used
directly as getters and lenses.

```Haskell
import Data.Default (Default(def))
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

Following is a more complex usage example that demonstrates some of the
possibilities of *Overloaded Labels* provided by this library.

```Haskell
-- Basic set of language extensions required when defining instances for
-- classes and type families from "Data.OverloadedRecords".
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Following language extensions are required by code like this:

{-# LANGUAGE ConstraintKinds #-}
    -- Codomain of type family 'R' is a 'Constraint' kind.

{-# LANGUAGE FlexibleContexts #-}
    -- Required in example when field type (second argument of ':::') is a
    -- specific type instead of a polymorphic type.

{-# LANGUAGE TypeOperators #-}
    -- Required due to usage of ':::' type alias.

-- Following language extensions are available only in GHC >=8:

{-# LANGUAGE OverloadedLabels #-}
    -- Enables #label syntactic sugar.

module Example
  where

import Data.Default (Default(def))
    -- Provided by one of these packages:
    --
    -- * data-default
    -- * data-default-extra

import Data.OverloadedRecords
import Data.OverloadedRecords.TH (overloadedRecord)


data V3 a = V3
    { v3x :: !a
    , v3y :: !a
    , v3z :: !a
    }
  deriving Show

-- Following line derives instances for various type classes and type
-- families that are provided by the overloaded-records library.
--
-- However with def (default settings) this is done only for fields that
-- start with type name, data constructor name, or underscore. Prefix is
-- stripped. In example "v3x" is transformed in to "x" and so would be
-- "_x".
overloadedRecord def ''V3

data V4 a = V4
    { v4x :: !a
    , v4y :: !a
    , v4z :: !a
    , v4t :: !a
    }
  deriving Show

overloadedRecord def ''V4

zeroV3
    :: (Num a, R '["x" ::: a, "y" ::: a, "z" ::: a] r)
    => r -> r
zeroV3 = set' #x 0 . set' #y 0 . set' #z 0
```

The following type signatures for `zeroV3` are equivalent:

```Haskell
zeroV3
    :: (Num a, R '["x" ::: a, "y" ::: a, "z" ::: a] r)
    => r -> r
```

```Haskell
zeroV3
    ::  ( Num a
        , ModifyField' "x" r a
        , ModifyField' "y" r a
        , ModifyField' "z" r a
        )
    => r -> r
```

One of the biggest features of *Overloaded Records* is the possibility to
define functions that do not depend on concrete data types, but on the "fields"
they provide. In example function `zeroV3` can be applied to anything that has
fields `"x"`, `"y"`, and `"z"` that reference values of some `Num` type:

```Haskell
λ> zeroV3 (V3 1 1 1 :: V3 Int)
V3 {_x = 0, _y = 0, _z = 0}
```

```Haskell
λ> zeroV3 (V4 1 1 1 1 :: V4 Int)
V4 {_x = 0, _y = 0, _z = 0, _t = 1}
```

Function `zeroV3` can be also defined using operators from
[lens][Hackage: lens] library:

```Haskell
import Control.Lens ((.~), simple)

zeroV3
    :: (Num a, R '["x" ::: a, "y" ::: a, "z" ::: a] r)
    => r -> r
zeroV3 r = r
    & #x . simple .~ 0
    & #y . simple .~ 0
    & #z . simple .~ 0
```


## License

The BSD 3-Clause License, see [LICENSE][] file for details. This implementation
is based on original prototype, which is under MIT License.


## Contributions

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail.


## Related Work

* [ruin][Hackage: ruin] is a DSL for working with record types that also
  leverages OverloadedLabels language extension.
* [vinyl][Hackage: vinyl] provides extensible records for Haskell with lenses
  using modern GHC features.



[GHC Wiki: OverloadedRecordFields]:
  https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields
  "OverloadedRecordFields language extension on GHC Wiki"
[Hackage: lens]:
  https://hackage.haskell.org/package/lens
  "lens package on Hackage"
[Hackage: overloaded-records]:
  http://hackage.haskell.org/package/overloaded-records
  "overloaded-records package on Hackage"
[Hackage: ruin]:
  https://hackage.haskell.org/package/ruin
  "ruin package on Hackage"
[Hackage: vinyl]:
  https://hackage.haskell.org/package/vinyl
  "vinyl package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[LICENSE]:
  https://github.com/trskop/overloaded-records/blob/master/LICENSE
  "License of overloaded-records package."
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
