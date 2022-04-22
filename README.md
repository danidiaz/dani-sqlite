dep-t-sqlite
============

A fork of Irene Knapp's [direct-sqlite](https://github.com/IreneKnapp/direct-sqlite).

Example of `cabal.project.local` pointing to local libs:

    package dep-t-sqlite
        extra-include-dirs: C:/Users/somefolder/sqlite-amalgamation-3350300
        extra-lib-dirs: C:/Users/somefolder/sqlite-dll-win64-x64-3350300

Links
=====

- [FFI in the Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html)

- [Foreign function interface (FFI) in the GHC user guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/ffi.html)

- [Using the latest version of SQLite with Haskell on Windows](https://danidiaz.medium.com/using-the-latest-version-of-sqlite-with-haskell-on-windows-1d6d4df2e683)

- [Foreign Function Interface at the Haskellwiki](https://wiki.haskell.org/Foreign_Function_Interface)

> I see very little reason to not use CApiFFI if your project doesn't need
to work on old GHCs.

- [Writing Haskell interfaces to C code: hsc2hs](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/utils.html)

- [Using_the_FFI](https://wiki.haskell.org/GHC/Using_the_FFI). 

- [Haskell/FFI](https://en.wikibooks.org/wiki/Haskell/FFI)

- [hsctohs](https://www.reddit.com/r/haskell/comments/tthrq0/comment/i5dpir1/)

- [Chapter 17. Interfacing with C: the FFI](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html)

> the #const keyword hsc2hs provides [...] We can bind to the constants manually, by listing the CPP symbols for them using the #const keyword


Low-level SQLite3 bindings for Haskell
======================================

[![Build Status](https://travis-ci.org/IreneKnapp/direct-sqlite.png?branch=master)](https://travis-ci.org/IreneKnapp/direct-sqlite) ![Hackage](https://img.shields.io/hackage/v/direct-sqlite.svg?style=flat-square)

This package is not very different from the other SQLite3 bindings out there, but it fixes a few deficiencies I was finding. As compared to bindings-sqlite3, it is slightly higher-level, in that it supports marshalling of data values to and from the database. In particular, it supports strings encoded as UTF8, and BLOBs represented as ByteStrings.

For contribtions, please read [contributing guide](CONTRIBUTING.md) before sending PRs.

# Contributors

- [Irene Knapp](https://github.com/nurpax) author
- [Janne Hellsten](https://github.com/nurpax) long-term maintainer
- [Sergey Bushnyak](https://github.com/sigrlami) current maintainer
