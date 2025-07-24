**You want the new `System.OsPath` library, which does the right thing (using pinned `ShortByteString` values under the hood).**

# empathy

[![Build Status](https://action-badges.now.sh/fused-effects/fused-effects)](https://github.com/fused-effects/fused-effects/actions) [![Hackage](https://img.shields.io/hackage/v/empathy.svg)](https://hackage.haskell.org/package/empathy)

`empathy` is a modern library for typed path manipulation. It takes advantage of newer GHC features like `DataKinds`, Backpack, and custom type errors so as to minimize the library footprint while yielding the maximum amount of type safety.

## What's cool about it?

* Platform details are abstracted away with Backpack. Every such detail is accounted for in a signature file.
* Path literals are constructed using type-level `Symbol`s, not value-level `String`s. (You will get a custom type error if you attempt to use a `String` literal for a path). This will let us do compile-time parsing to enforce well-formedness of paths; that is, asking the string `"/usr/bin/"` to represent anything but an absolute directory will be a compile-time error.
* Information about the relative/absolute or file/directory nature of a given path is stored on the type-level rather than the value level.
* It comes with batteries included (`optparse-applicative` parsers, `Hashable` and `NFData` instances, etc.).

## Why not [one of the alternative typed-path libraries]?

I evaluated all of the libraries and wasn't happy with any of them. `pathtype` came closest to my needs, but bears historical cruft and doesn't take full advantage of newer GHC features. `path` requires Template Haskell, which is avoidable in an era of type-level strings. `data-filepath` is unmaintained.

## This uses Backpack; does that mean it doesn't work with `stack`?

Alas, yes. Until https://github.com/commercialhaskell/stack/issues/2540 is fixed, you won't be able to use this library from a Stack project. (I should point out, however, that switching from `stack` to `cabal-install` v3.0 is generally pretty easy.)

## BUGS

* Currently there is no way to access the last element of a type-level string in anything other than linear time. As such, the ~absRel~-style constructors don't check for trailing slashes.

# License

`empathy` is open-source software provided to you under the terms of the [Hippocratic License](https://firstdonoharm.dev). Due to the fact that the Hippocratic license does not have an SPDX identifier yet, the Cabal file states that it is MIT licensed; this will be addressed when the SPDX allocates an identifier for the HL.
