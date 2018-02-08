Changelog
=========

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

