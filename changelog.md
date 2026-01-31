# Changelog

## 0.6.3.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jan 2026__
* Add `Control.Monad.Trans.Except.onE`
* Add `strictToLazyState` and `lazyToStrictStateT`

## 0.6.2.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Apr 2025__
* Redefine `runAccumT`, `runExceptT` and `runSelectT` as fields
* Document strictness of some transformers

## 0.6.1.2
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Sep 2024__
* Portability fixes for MicroHs
* Include image files in the bundle
* Expand `ExceptT` documentation

## 0.6.1.1
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Aug 2023__
* Additions to documentation, especially of `AccumT`.

## 0.6.1.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2023__
* Add instances of `Foldable1` (class added to base-4.18)
* Add `modifyM` to `StateT` transformers

## 0.6.0.6
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jan 2023__
* Fix for GHC 8.6

## 0.6.0.5
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jan 2023__
* Revert to allowing `MonadTrans` constraint with GHC >= 8.6

## 0.6.0.4
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2022__
* Restrict `deriving (Generic)` to GHC >= 7.4

## 0.6.0.3
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2022__
* Restrict `MonadTrans` constraint to GHC >= 8.8

## 0.6.0.2
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jul 2021__
* Further backward compatability fix

## 0.6.0.1
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jul 2021__
* Backward compatability fixes

## 0.6.0.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jul 2021__
* Added quantified constraint to `MonadTrans` (for GHC >= 8.6)
* Added `Generic` and `Data` instances
* Added `handleE`, `tryE` and `finallyE` to `Control.Monad.Trans.Except`
* Added `hoistMaybe` to `Control.Monad.Trans.Maybe`
* Added `Generic` and `Data` instances
* Added pass-throughs to instances for `Backwards`
* Made `Lift`'s `<*>` lazier
* Remove long-deprecated `selectToCont`
* Remove long-deprecated `Control.Monad.Trans.Error`
* Remove long-deprecated `Control.Monad.Trans.List`

## 0.5.6.2
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2019__
* Further backward compatability fix

## 0.5.6.1
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2019__
* Backward compatability fix for `MonadFix ListT` instance

## 0.5.6.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2019__
* Generalized type of `except`
* Added `Control.Monad.Trans.Writer.CPS` and `Control.Monad.Trans.RWS.CPS`
* Added `Contravariant` instances
* Added `MonadFix` instance for `ListT`

## 0.5.5.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Oct 2017__
* Added `mapSelect` and `mapSelectT`
* Renamed `selectToCont` to `selectToContT` for consistency
* Defined explicit method definitions to fix space leaks
* Added missing `Semigroup` instance to `Constant` functor

## 0.5.4.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2017__
* Migrate `Bifoldable` and `Bitraversable` instances for `Constant`

## 0.5.3.1
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2017__
* Fixed for pre-AMP environments

## 0.5.3.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2017__
* Added `AccumT` and `SelectT` monad transformers
* Deprecated `ListT`
* Added `Monad` (and related) instances for `Reverse`
* Added `elimLift` and `eitherToErrors`
* Added specialized definitions of several methods for efficiency
* Removed specialized definition of `sequenceA` for `Reverse`
* Backported `Eq1`/`Ord1`/`Read1`/`Show1` instances for `Proxy`

## 0.5.2.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Feb 2016__
* Re-added orphan instances for `Either` to deprecated module
* Added lots of `INLINE` pragmas

## 0.5.1.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jan 2016__
* Bump minor version number, required by added instances

## 0.5.0.2
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jan 2016__
* Backported extra instances for `Identity`

## 0.5.0.1
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Jan 2016__
* Tightened GHC bounds for `PolyKinds` and `DeriveDataTypeable`

## 0.5.0.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Dec 2015__
* `Control.Monad.IO.Class` in base for GHC >= 8.0
* `Data.Functor.`{`Classes`,`Compose`,`Product`,`Sum`} in base for GHC >= 8.0
* Added `PolyKinds` for GHC >= 7.4
* Added instances of base classes `MonadZip` and `MonadFail`
* Changed liftings of `Prelude` classes to use explicit dictionaries

## 0.4.3.0
__Ross Paterson__ &lt;R.Paterson@city.ac.uk&gt; __Mar 2015__
* Added `Eq1`, `Ord1`, `Show1` and `Read1` instances for `Const`

## 0.4.2.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Nov 2014__
* Dropped compatibility with base-1.x
* `Data.Functor.Identity` in base for GHC >= 7.10
* Added `mapLift` and `runErrors` to `Control.Applicative.Lift`
* Added `AutoDeriveTypeable` for GHC >= 7.10
* Expanded messages from `mfix` on `ExceptT` and `MaybeT`

## 0.4.1.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __May 2014__
* Reverted to record syntax for newtypes until next major release

## 0.4.0.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __May 2014__
* Added `Sum` type
* Added `modify'`, a strict version of `modify`, to the state monads
* Added `ExceptT` and deprecated `ErrorT`
* Added `infixr 9 Compose` to match `(.)`
* Added `Eq`, `Ord`, `Read` and `Show` instances where possible
* Replaced record syntax for newtypes with separate inverse functions
* Added delimited continuation functions to `ContT`
* Added instance `Alternative IO` to `ErrorT`
* Handled disappearance of `Control.Monad.Instances`

## 0.3.0.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Mar 2012__
* Added type synonyms for signatures of complex operations
* Generalized state, reader and writer constructor functions
* Added `Lift`, `Backwards`/`Reverse`
* Added `MonadFix` instances for `IdentityT` and `MaybeT`
* Added Foldable and Traversable instances
* Added `Monad` instances for `Product`

## 0.2.2.1
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Oct 2013__
* Backport of fix for disappearance of `Control.Monad.Instances`

## 0.2.2.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Sep 2010__
* Handled move of `Either` instances to base package

## 0.2.1.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Apr 2010__
* Added `Alternative` instance for `Compose`
* Added `Data.Functor.Product`

## 0.2.0.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Mar 2010__
* Added `Constant` and `Compose`
* Renamed modules to avoid clash with mtl
* Removed `Monad` constraint from `Monad` instance for `ContT`

## 0.1.4.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Mar 2009__
* Adjusted lifting of `Identity` and `Maybe` transformers

## 0.1.3.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Mar 2009__
* Added `IdentityT` transformer
* Added `Applicative` and `Alternative` instances for `Either e`

## 0.1.1.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Jan 2009__
* Made all `Functor` instances assume `Functor`

## 0.1.0.1
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Jan 2009__
* Adjusted dependencies

## 0.1.0.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Jan 2009__
* Two versions of lifting of `callcc` through `StateT`
* Added `Applicative` instances

## 0.0.1.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Jan 2009__
* Added constructors state, etc for simple monads

## 0.0.0.0
__Ross Paterson__ &lt;ross@soi.city.ac.uk&gt; __Jan 2009__
* Split Haskell 98 transformers from the mtl
