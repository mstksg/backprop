Changelog
=========

Version 0.2.3.0
---------------

*Unreleased*

<https://github.com/mstksg/backprop/releases/tag/v0.2.3.0>

*   Explicit `collectVar` no longer needs a `ZeroFunc` for the container, and
    so all versions of `collectVar` and functions that use it (`fmap`,
    `liftA2`, `liftA3`, `traverse`, `mapAccumL`, `mapAccumR`) no longer require
    `Backprop` or `Num` instances for the final returned container type.  This
    enables a lot more flexibility in container types.  **Breaking change**
    only for those using the *Explicit* interfaces.
*   `BV` pattern synonym added to *Numeric.Backprop*, abstracting over
    application of `splitBV` and `joinBV`.
*   `foldr` and `foldl'` added to Prelude modules, for convenience.

Version 0.2.2.0
---------------

*May 12, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.2.0>

*   `evalBP0` added, for convenience for no-argument values that need to be
    evaluated without backpropagation.
*   `splitBV` and `joinBV` for "higher-kinded data" style `BVar` manipulation,
    via the `BVGroup` helper typeclass.
*   `toList`, `mapAccumL`, and `mapAccumR` for *Prelude.Backprop* modules
*   `Backprop` instance for `BVar`
*   *COMPLETE* pragmas for `T2` and `T3`
*   Un-exported `gzero`, `gadd`, and `gone` from *Numeric.Backprop.Class*
*   Many, many more instances of `Backprop`
*   `Backprop` instance for `Proxy` made non-strict for `add`
*   Swapped type variable order for a few library functions, which might
    potentially be breaking changes.

*Internal*

*   Fixed documentation for Num and Explicit Prelude modules, and rewrote
    normal and Num Prelude modules in terms of canonical Prelude definitions
*   Switched to `errorWithoutStackTrace` wherever appropriate (in *Internal*
    module)

Version 0.2.1.0
---------------

*May 8, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.1.0>

*   Added `ABP` newtype wrapper to *Numeric.Backprop.Class* (re-exported from
    *Numeric.Backprop* and *Numeric.Backprop.Explicit*) to give free `Backprop`
    instances for Applicative actions.
*   Added `NumBP` newtype wrapper to *Numeric.Backprop.Class* (re-exported in
    the same places as `ABP`) to give free `Backprop` instances for `Num`
    instances.
*   Added `^^?!` (unsafe access) to *Numeric.Backprop* and
    *Numeric.Backprop.Num*.
*   `Backprop` instance for `Natural` from *Numeric.Natural*.  Should actually
    be safe, unlike its `Num` instance!
*   `zfFunctor` and `ofFunctor` for instances of `Functor` for
    *Numeric.Backprop.Explicit*.
*   `realToFrac` and `fromIntegral` to *Prelude* modules
*   `T2` and `T3` patterns for *Numeric.Backprop*, for conveniently
    constructing and deconstructing tuples.

Version 0.2.0.0
---------------

*May 1, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.0.0>

*   Added `Backprop` class in *Numeric.Backprop.Class*, which is a typeclass
    specifically for "backpropagatable" values.  This will replace `Num`.
*   API of *Numeric.Backprop* completely re-written to require values be
    instances of `Backprop` instead of `Num`.  This closes some outstanding
    issues with the reliance of `Num`, and allows backpropagation to work with
    non-Num instances like variable-length vectors, matrices, lists, tuples,
    etc. (including types from *accelerate*)
*   *Numeric.Backprop.Num* and *Prelude.Backprop.Num* modules added, providing
    the old interface that uses `Num` instances instead of `Backprop`
    instances, for those who wish to avoid writing orphan instances when
    working with external types.
*   *Numeric.Backprop.Explicit* and *Prelude.Backprop.Explicit* modules added,
    providing an interface that allows users to manually specify how zeroing,
    addition, and one-ing works on a per-value basis.  Useful for those who
    wish to avoid writing orphan instances of `Backprop` for types with no
    `Num` instances, or if you are mixing and matching styles.
*   `backpropWith` variants added, allowing you to specify a "final gradient",
    instead of assuming it to be 1.
*   Added `auto`, a shorter alias for `constVar` inspired by the *ad* library.
*   *Numeric.Backprop.Tuple* module removed.  I couldn't find a significant
    reason to keep it now that `Num` is no longer required for backpropagation.


Version 0.1.5.2
---------------

*Apr 26, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.5.2>

*   Added `coerceVar` to *Numeric.Backprop*
*   Added `Random` instaces for all tuple types.  Same as for `Binary`, this
    does incur a *random* and *time* dependency only from the tuple types.
    Again, because these packages are a part of GHC's boot libraries, this
    is hopefully not too bad.

Version 0.1.5.1
---------------

*Apr 9, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.5.1>

*   Fixed `NFData` instance for `T`; before, was shallow.
*   Added `Typeable` instances for all tuple types, and for `BVar`.
*   Added `Eq`, `Ord`, `Show`, etc. instances for `T`.
*   Added `Binary` instances for all tuple types.  Note that this does incur a
    *binary* dependency only because of the tuple types; however, this will
    hopefully be not too much of an issue because *binary* is a GHC library
    anyway.

Version 0.1.5.0
---------------

*Mar 30, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.5.0>

*   `T` added to *Numeric.Backprop.Tuple*: basically an `HList` with a `Num`
    instance.
*   `Eq` and `Ord` instances for `BVar`.  Is this sound?

*Internal*

*   Refactored `Monoid` instances in *Numeric.Backprop.Tuple*

Version 0.1.4.0
---------------

*Mar 25, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.4.0>

*   `isoVar`, `isoVar2`, `isoVar3`, and `isoVarN`: convenient aliases for
    applying isomorphisms to `BVar`s.  Helpful for use with constructors and
    deconstructors.
*   `opIso2` and `opIso3` added to *Numeric.Backprop.Op*, for convenience.
*   `T0` (Unit with numeric instances) added to *Numeric.Backprop.Tuple*.

*Internal*

*   Completely decoupled the internal implementation from `Num`, which showed
    some performance benefits.  Mostly just to make the code slightly cleaner,
    and to prepare for some day potentially decoupling the external API from
    `Num` as well.

Version 0.1.3.0
---------------

*Feb 12, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.3.0>

*   *Preulude.Backprop* module added with lifted versions of several *Prelude*
    and base functions.
*   `liftOpX` family of operators now have a more logical ordering for type
    variables.  This change breaks backwards-compatibility.
*   `opIsoN` added to export list of *Numeric.Backprop*
*   `noGrad` and `noGrad1` added to *Numeric.Backprop.Op*, for functions with
    no defined gradient.

*Internal*

*   Completely decoupled the internal implementation from `Num`, which showed
    some performance benefits.

Version 0.1.2.0
---------------

*Feb 7, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.2.0>

*   Added currying and uncurrying functions for tuples in
    *Numeric.Backprop.Tuple*.
*   `opIsoN`, for isomorphisms between a tuple of values and a value.
*   (Internal) AD engine now using `Any` from *ghc-prim* instead of `Some I`
    from *type-combinators*

Version 0.1.1.0
---------------

*Feb 6, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.1.0>

*   Added canonical strict tuple types with `Num` instances, in the module
    *Numeric.Backprop.Tuple*.  This is meant to be a band-aid for the problem
    of orphan instances and potential mismatched tuple types.
*   Fixed bug in `collectVar` that occurs if container sizes change

*Internal*

*   Internal tweaks to the underlying automatic differentiation types that
    decouple backpropagation from `Num`, internally.  `Num` is now just used
    externally as a part of the API, which might someday be made optional.

Version 0.1.0.0
---------------

*Feb 5, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.1.0.0>

*   First non-alpha release.
*   More or less complete redesign of library.  The entire API is completely
    changed, and there is no backwards compatibility!
    *   Everything is now "implicit" style, and there is no more `BP` monad.
    *   Accessing items in `BVar`s is now lens-, prism-, and traversal- based,
        instead of iso- and generics-based.
    *   `Op` is no longer monadic
    *   *Mono* modules are removed.
    *   *Implicit* modules are removed, since they are the default
    *   *Iso* module is removed, since `Iso`s no longer play major role in the
        implementation of the library.
*   Removed dependency on *ad* and *ad*-based ops, which had been pulling in
    the vast majority of dependencies.
*   Moved from *.cabal* file to *hpack* system.

Version 0.0.3.0
---------------

*Alpha*

<https://github.com/mstksg/backprop/releases/tag/v0.0.3.0>

*   Removed samples as registered executables in the cabal file, to reduce
    dependences to a bare minimum.  For convenience, build script now also
    compiles the samples into the local directory if *stack* is installed.

*   Added experimental (unsafe) combinators for working with GADTs with
    existential types, `withGADT`, to *Numeric.Backprop* module.

*   Fixed broken links in changelog.

Version 0.0.2.0
---------------

*Alpha*

<https://github.com/mstksg/backprop/releases/tag/v0.0.2.0>

*   Added optimized numeric `Op`s, and re-write `Num`/`Fractional`/`Floating`
    instances in terms of them.

*   Removed all traces of `Summer`/`Unity` from the library, eliminating a
    whole swath of "explicit-Summer"/"explicit-Unity" versions of functions.
    As a consequence, the library now only works with `Num` instances.  The
    API, however, is now much more simple.

*   Benchmark suite added for MNIST example.

Version 0.0.1.0
---------------

*Alpha*

<https://github.com/mstksg/backprop/releases/tag/v0.0.1.0>

*   Initial pre-release, as a request for comments.  API is in a usable form
    and everything is fully documented, but there are definitely some things
    left to be done. (See [README.md][readme-0.0.1.0])

    [readme-0.0.1.0]: https://github.com/mstksg/backprop/tree/v0.0.1.0#readme

