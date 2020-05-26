fix-whitespace: Fixes whitespace issues. [![Build Status](https://travis-ci.org/agda/fix-whitespace.svg?branch=master)](https://travis-ci.org/agda/fix-whitespace)
---------------------------------------------

Usage: `fix-whitespace [-h|--help] [-v|--verbose] [--check] [--config CONFIG] [FILES]`

The program does the following

* Removes trailing whitespace.
* Removes trailing lines containing nothing but whitespace.
* Ensures that the file ends in a newline character.

for files specified in `[FILES]` or

	fix-whitespace.yaml

under the current directory.

Background: Agda was reported to fail to compile on Windows
because a file did not end with a newline character (Agda
uses `-Werror`).

Available options:

  `-h  --help`           Show this help information.
  `-v  --verbose`        Show files as they are being checked.
      `--config=CONFIG`  Override the project configuration `fix-whitespace.yaml`.
      `--check`          With `--check` the program does not change any files,
                       it just checks if any files would have been changed.
                       In this case it returns with a non-zero exit code.
