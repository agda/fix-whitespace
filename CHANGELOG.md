# fix-whitespace

Version history.

# 0.2

- [BREAKING:] Flag `--verbose` now takes an optional argument
  that determines the maximum number of whitespace violations printed per file.
  This argument is an integer, defaulting to 10, but can be `all`
  to not limit the number of violations.

# 0.1 Rainy Summer edition released 2023-08-07

- Flag `--verbose` now also displays locations of whitespace violations
  ([#7](https://github.com/agda/fix-whitespace/issues/7), contributed by Artem Pelenitsyn).
- Tested with GHC 8.0.2 - 9.8.1-alpha1.

## 0.0.11 Santa Clause edition released 2022-12-06

- Delete trailing tabs even when `--tab=0`
  [#42](https://github.com/agda/fix-whitespace/issues/42)
- Tested with GHC 8.0.2 - 9.4.3.

## 0.0.10 released 2022-08-21

- Symlink problem
  [#9](https://github.com/agda/fix-whitespace/issues/9)
  fixed in dependency `filepattern-0.1.3`.
- Tested with GHC 8.0.2 - 9.4.1.

## 0.0.9 released 2022-08-10

- New option `--tab` to set tab-size or keep tabs
  [#31](https://github.com/agda/fix-whitespace/issues/31).
- Tested with GHC 8.0.2 - 9.4.1.

## 0.0.8 released 2022-05-29

- New option `--version` displaying program version
  [#33](https://github.com/agda/fix-whitespace/pull/33).
- Skip files that are not UTF8 encoded, rather than crashing
  [#29](https://github.com/agda/fix-whitespace/issues/29).
- Tested with GHC 8.0.2 - 9.2.2 and 9.4.1 alpha.

## 0.0.7 released 2021-09-07

- Support GHC 8.10.7.

## 0.0.6 released 2021-07-29

- Fix the release version: The tag `0.0.5` on the GitHub repo was released on 14 Oct 2019 while `0.0.5` on Hackage is the commit `1394ec6`.
- Include `-Wall` and `-Wcompact` during compilation.
- Update `stack-x.y.z.yaml` and add `stack-8.10.5.yaml`.

## 0.0.5 released 2021-03-11

- Initial release.
- Tested with GHC 8.0.2 - 9.0.1.
