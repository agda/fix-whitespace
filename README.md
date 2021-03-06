fix-whitespace: Fixes whitespace issues
=======================================

[![Hackage version](https://img.shields.io/hackage/v/fix-whitespace.svg?label=Hackage)](http://hackage.haskell.org/package/fix-whitespace)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/fix-whitespace/badge)](https://matrix.hackage.haskell.org/package/fix-whitespace)
[![fix-whitespace on Stackage Nightly](https://stackage.org/package/fix-whitespace/badge/nightly)](https://stackage.org/nightly/package/fix-whitespace)
[![Stackage LTS version](https://www.stackage.org/package/fix-whitespace/badge/lts?label=Stackage)](https://www.stackage.org/package/fix-whitespace)
[![Build status](https://github.com/agda/fix-whitespace/workflows/Build%20by%20Stack/badge.svg)](https://github.com/agda/fix-whitespace/actions)


This tool can keep your project and repository clean of trailing
whitespace and missing terminal newline.

Usage: `fix-whitespace [-h|--help] [-v|--verbose] [--check] [--config CONFIG] [FILES]`

The program does the following to files specified in `FILES` or in the
configuration file `fix-whitespace.yaml` under the current directory
(and its subdirectories):

  * Remove trailing whitespace.
  * Remove trailing lines containing nothing but whitespace.
  * Ensure that the file ends in a newline character.

Available options:

*  `-h  --help`

   Show this help information.

*  `-v  --verbose`

   Show files as they are being checked.

*  `--config=CONFIG`

   Override the project configuration `fix-whitespace.yaml`.

*  `--check`

   With `--check` the program does not change any files,
   it just checks if any files would have been changed.
   In this case it returns with a non-zero exit code.

For an example configuration file see [the one of Agda](https://github.com/agda/agda/blob/f9a181685397517b5d14943ca88a1c0acacc2075/fix-whitespace.yaml).
