# ChangeLog / ReleaseNotes


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
