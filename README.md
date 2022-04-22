dep-t-sqlite
============

A fork of Irene Knapp's [direct-sqlite](https://github.com/IreneKnapp/direct-sqlite).

Example of `cabal.project.local` pointing to local libs:

    package dep-t-sqlite
        extra-include-dirs: C:/Users/somefolder/sqlite-amalgamation-3350300
        extra-lib-dirs: C:/Users/somefolder/sqlite-dll-win64-x64-3350300

Links
=====

- [Using the latest version of SQLite with Haskell on Windows](https://danidiaz.medium.com/using-the-latest-version-of-sqlite-with-haskell-on-windows-1d6d4df2e683)

Low-level SQLite3 bindings for Haskell
======================================

[![Build Status](https://travis-ci.org/IreneKnapp/direct-sqlite.png?branch=master)](https://travis-ci.org/IreneKnapp/direct-sqlite) ![Hackage](https://img.shields.io/hackage/v/direct-sqlite.svg?style=flat-square)

This package is not very different from the other SQLite3 bindings out there, but it fixes a few deficiencies I was finding. As compared to bindings-sqlite3, it is slightly higher-level, in that it supports marshalling of data values to and from the database. In particular, it supports strings encoded as UTF8, and BLOBs represented as ByteStrings.

For contribtions, please read [contributing guide](CONTRIBUTING.md) before sending PRs.

# Contributors

- [Irene Knapp](https://github.com/nurpax) author
- [Janne Hellsten](https://github.com/nurpax) long-term maintainer
- [Sergey Bushnyak](https://github.com/sigrlami) current maintainer
