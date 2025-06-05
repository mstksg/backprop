Changelog
=========

Version 0.2.7.1
---------------

*June 1, 2025*

<https://github.com/mstksg/backprop/releases/tag/v0.2.7.1>

*   Actually export internal constructors and utilities in
    `Numeric.Backprop.Internal`.

Version 0.2.7.0
---------------

*June 3, 2025*

<https://github.com/mstksg/backprop/releases/tag/v0.2.7.0>

*   Export `Numeric.Backprop.Internal`.

Version 0.2.6.5
---------------

*July 23, 2023*

<https://github.com/mstksg/backprop/releases/tag/v0.2.6.5>

*   vinyl-0.14 compatibility (@msakai), which can be disabled via cabal flag
*   GHC 9.0+ compatibility (@msakai)

Thanks to all generous contributors and commenters!

Version 0.2.6.4
---------------

*June 30, 2020*

<https://github.com/mstksg/backprop/releases/tag/v0.2.6.4>

*   Compatibility with ghc-8.10.1 (@tonyday567)

Version 0.2.6.3
---------------

*August 13, 2019*

<https://github.com/mstksg/backprop/releases/tag/v0.2.6.3>

*   Add `Backprop` instances for the various *vinyl* types.
*   Rewrite many `Backprop` instances over newtypes to coerce instead of go
    through Generics

Version 0.2.6.2
---------------

*April 7, 2019*

<https://github.com/mstksg/backprop/releases/tag/v0.2.6.2>

*   Fix a numerical bug that would occur when an input is used directly as the
    result of a computation. (For example, `gradBP id` or `gradBP2 const`).
*   Some internal changes to strictness which offers some performance boosts in
    computation of gradients.

Version 0.2.6.1
---------------

*August 6, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.6.1>

*   Removed redundant constraints from `Floating` instance of `Op`.
*   Fixed lower bound for *vinyl* dependency.

Version 0.2.6.0
---------------

*August 6, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.6.0>

*   Dropped `Expr` instance of `Backprop`. I don't think anyone was actually
    using this.  If you need this, please use `Numeric.Backprop.Num` instead!
*   Removed *Rec* re-exports.
*   Compatibility with *vinyl-0.9*, using the *Data.Vinyl.Recursive* interface.
    This requires some minor reshuffling of constraints but should not affect
    any monomorphic usage.

Version 0.2.5.0
---------------

*June 19, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.5.0>

*   Since *type-combinators* has been unmaintained for over two years, and is
    no longer compatible with modern GHC, the library internals was rewritten
    to be built on the type-level combinators in the *vinyl* library instead.
    The main external API change is basically `Every` is replaced with
    `AllConstrained`, and `Known Length` is replaced with `RecApplicative`.

    To most users, this should make no difference API-wise.  The only users
    affected should be those using the "-N" family of functions (`backpropN`),
    who have to pass in heterogeneous lists.  Heterogeneous lists now must be
    passed in using *vinyl* syntax and operators instead of the previous
    *type-combinators* interface.
*   `bpOp` added, to allow for non-rank-N storage of backpropagatable
    functions in containers without impredicative types.
*   Benchmarks use *microlens* and *microlens-th* instead of *lens*.

Version 0.2.4.0
---------------

*May 28, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.4.0>

**NOTE** Major breaking changes to *Explicit* modules, and some re-shuffling of
typeclass constraints on various non-explicit functions that should only affect
polymorphic usage.

*   *Huge improvements in performance!*  Around 20-40% reduction in
    runtimes/overheads, with savings higher for large matrix situations or
    situations with expensive `add`.
*   However, this restructuring required *major* reshuffling of constraints on
    `Backprop`/`Num` for most functions.  These are potentially **breaking
    changes** for polymorphic code, but monomorphic code should remain
    unchanged.  However, code using the *Explicit* interfaces is most likely
    broken unfortunately.  Fixes just include adding or dropping `zeroFunc`s to
    the appropriate functions.
*   Added warnings to *Explicit* modules that the API is "semi-stable".
*   `overVar` and `%~~`, for modifying fields.  Essentially a wrapper over a
    `viewVar` and `setVar`.
*   Argument order in the `backpropWith` family of functions changed again;
    **breaking change** for those using any `backpropWith` function.  However,
    the new order is much more usable.
*   Changes to the argument order in the `backprop` family of functions in the
    *Explicit* interfaces now reverted back to previous order, from v0.2.0 and
    before.  Should be an "un-breaking" change, but will break code written in
    v0.2.3 style.
*   Bechmarks now include HKD access and a "hybrid" approach.  Documentation
    updated to reflect results.
*   Documentation updated to include a new "unital" law for `one`, namely `one
    = gradBP id`.
*   Fixity declarations for `^^?`, `^^?!`, and `<$>`.
*   Added `fmap . const` and `<$` to *Prelude* modules.
*   `Backprop` instances for `Expr` from *simple-reflect*
*   Added `zeroVecNum` and `oneVecNum` to *Numeric.Backprop.Class*, which is
    potentially more efficient than `zeroVec` and `oneVec` if the items are
    instances of `Num` and the vectors are larger.  Also added `NumVec` newtype
    wrapper giving `Backprop` instances to vectors using `zeroVecNum` and
    `oneVecNum` instead of `zeroVec` and `oneVec`.
*   `Build.hs` build script now also builds profiling results

Version 0.2.3.0
---------------

*May 25, 2018*

<https://github.com/mstksg/backprop/releases/tag/v0.2.3.0>

*   Argument order in `backpropWith` family of functions switched around to
    allow for final gradient to be given after-the-fact.  **Breaking change**
    for anyone using any `backpropWith` function.
*   As a consequence of the previous change, `backprop` family of functions in
    *Explicit* interfaces also all changed argument order.  **Breaking change**
    only for those using the *Explicit* interfaces.
*   Explicit `collectVar` no longer needs a `ZeroFunc` for the container, and
    so all versions of `collectVar` and functions that use it (`fmap`,
    `liftA2`, `liftA3`, `traverse`, `mapAccumL`, `mapAccumR`) no longer require
    `Backprop` or `Num` instances for the final returned container type.  This
    enables a lot more flexibility in container types.  **Breaking change**
    only for those using the *Explicit* interfaces.
*   `BV` pattern synonym added to *Numeric.Backprop*, abstracting over
    application of `splitBV` and `joinBV`.
*   `foldr` and `foldl'` added to Prelude modules, for convenience.
*   `round` and `fromIntegral'` ("unround") added to Prelude modules.

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

