fix-agda-whitespace: Fixes whitespace issues. [![Build Status](https://travis-ci.org/agda/fix-whitespace.svg?branch=master)](https://travis-ci.org/agda/fix-whitespace)
---------------------------------------------

Usage: `fix-agda-whitespace [--check]`

This program should be run in the base directory.

The program does the following for every file listed in

`fix-agda-whitespace.yaml`

under the current directory:

* Removes trailing whitespace.
* Removes trailing lines containing nothing but whitespace.
* Ensures that the file ends in a newline character.

With the `--check` flag the program does not change any files,
it just checks if any files would have been changed. In this
case it returns with a non-zero exit code.

Background: Agda was reported to fail to compile on Windows
because a file did not end with a newline character (Agda
uses `-Werror`).
