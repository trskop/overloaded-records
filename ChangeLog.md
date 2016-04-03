# ChangeLog / ReleaseNotes


## Version 0.4.1.0

* Introducing `Getter` newtype along with `get` function. (**new**)
    * `get :: Getter s a -> s -> a`
* Introducing `Rec` data type that allows passing polymorphic record along with
  its instances as a normal value. (**new**)
* Corrections and updates in documentation (**change**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/overloaded-records-0.4.1.0>


## Version 0.4.0.0

* Renamed `SetField` type class to `ModifyField`, it now contains following
  methods (**breaking change**):
    * `modifyField :: Proxy# l -> (a -> b) -> s -> t`
    * `setField :: Proxy# l -> s -> b -> t`
    * `fieldLens :: Functor f => Proxy# l -> (a -> f b) -> s -> f t`
* Instances for tuples (i.e. `(a, b)`, `(a, b, c)`, ...) and lists (i.e.
  `[a]`). (**new**)
* Definitions from `Data.OverloadedRecords.TH` were moved to
  `Data.OverloadedRecords.TH.Internal`, so that API can be split in to stable
  and unstable (internal) API. (**change**)
* Aliases `HasField'` and `ModifyField'` that enforce `s = t` and `a = b`. This
  is similar to definitions like `Lens'`. Simplified versions of methods and
  functions are included using the same naming convention. (**new**)
* `Setter` and `Setter'` changed to type aliases for `Modifier` type.
  (**breaking change**)
* Introducing `Setting` type alias for `Modifier` along with `setting`, which
  is an alternative to `set` operation. (**new**)
    * `setting :: Setting a s t b -> Proxy a -> b -> s -> t`
* Changed order of arguments of functions `set` and `set'` (**breaking
  change**):
    * `set :: Setter s t b -> b -> s -> t`
    * `set' :: Setter' s a -> a -> s -> s`
* Introduced type family `R` that can be used to define more compact type
  signatures when mentioning multiple record fields in it. (**new**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/overloaded-records-0.4.0.0>


## Version 0.3.0.0

* Fixed defaultMakeFieldName, which actually behaved correctly only in very few
  cases. (**bug fix**)
* Added missing `HAVE_OVERLOADED_LABELS` macro, that actually allows us to use
  GHC's `IsLabel` on GHC >8. (**bug fix**)
* Exposing previously hidden `FromArrow` and `IsFieldAccessor`. (**change**)
* Few simple unit tests. (**new**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/overloaded-records-0.3.0.0>


## Version 0.2.0.0

* Function `overloadedRecords` renamed to `overloadedRecord`. There is also new
  `overloadedRecords` function, that behaves as `overloadedRecord`, but for
  multiple types at once. (**breaking change**)
* It is now possible to customize overloadedRecord\* family of functions with
  custom getter and setter implementation. (**new**)
* Types and functions follow, hopefully, better naming conventions. (**change**)
* More low-level template haskell functions for those cases when it is
  necessary to build your own higher-level ones, or when you need much more
  control over the result. (**new**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/overloaded-records-0.2.0.0>


## Version 0.1.0.0

* First public release.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/overloaded-records-0.1.0.0>



[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
