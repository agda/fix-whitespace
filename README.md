fix-whitespace: Fixes whitespace issues
=======================================

[![Hackage version](https://img.shields.io/hackage/v/fix-whitespace.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/fix-whitespace)
[![fix-whitespace on Stackage Nightly](https://stackage.org/package/fix-whitespace/badge/nightly)](https://stackage.org/nightly/package/fix-whitespace)
[![Stackage LTS version](https://www.stackage.org/package/fix-whitespace/badge/lts?label=Stackage)](https://www.stackage.org/package/fix-whitespace)
[![Build status](https://github.com/agda/fix-whitespace/workflows/Build%20by%20Stack/badge.svg)](https://github.com/agda/fix-whitespace/actions)
[![Haskell-CI](https://github.com/agda/fix-whitespace/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/agda/fix-whitespace/actions/workflows/haskell-ci.yml)

This tool can keep your project and repository clean of trailing
whitespace and missing terminal newline.

Usage: `fix-whitespace [-h|--help] [-v|--verbose] [--version] [--check] [--config CONFIG] [FILES]`

The program does the following to files specified in `FILES` or in the
configuration file `fix-whitespace.yaml` under the current directory
(and its subdirectories):

  * Remove trailing whitespace.
  * Remove trailing lines containing nothing but whitespace.
  * Ensure that the file ends in a newline character.
  * Expand tabs to spaces (optionally).

Available options:

*  `-h  --help`

   Show this help information.

*  `-v  --verbose`

   Show files as they are being checked.

*  `--version`

   Show program's version.

*  `--config=CONFIG`

   Override the project configuration `fix-whitespace.yaml`.

*  `--tab=TABSIZE`

   Expand tab characters to TABSIZE (default: 8) many spaces.
   Keep tabs if 0 is given as TABSIZE.  _(Option available since 0.0.9.)_

*  `--check`

   With `--check` the program does not change any files,
   it just checks if any files would have been changed.
   In the latter case, it returns with a non-zero exit code.

For an example configuration file see [the one of Agda](https://github.com/agda/agda/blob/f9a181685397517b5d14943ca88a1c0acacc2075/fix-whitespace.yaml).

Continuous integration
----------------------

`fix-whitespace` comfortably integrates into your GitHub CI via the [`fix-whitespace-action`](https://github.com/andreasabel/fix-whitespace-action).
