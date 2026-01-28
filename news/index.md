# Changelog

## date4ts 0.1.0.9010

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

### [Unreleased](https://github.com/TractorTom/date4ts/compare/v0.1.0...HEAD)

#### Added

- New logo
- export
  [`ts2df()`](https://tractortom.github.io/date4ts/reference/ts2df.md)

#### Changed

- The argument `date_ts` of
  [`extend_ts()`](https://tractortom.github.io/date4ts/reference/extend_ts.md)
  is renamed `date_ts_to`
- [`extend_ts()`](https://tractortom.github.io/date4ts/reference/extend_ts.md)
  leaves the trailing NAs if `na_replace = TRUE` and `replacement`
  doesn’t cover the full range
- [`extend_ts()`](https://tractortom.github.io/date4ts/reference/extend_ts.md)
  works fully with raw.
- new arguments for
  [`extend_ts()`](https://tractortom.github.io/date4ts/reference/extend_ts.md)
  : `times` and `each` to repeat the replacement pattern several times

### [0.1.0](https://github.com/TractorTom/date4ts/releases/tag/v0.1.0) - 2024-09-12

#### Added

- add function to convert `ts` or `mts` to `data.frame`
- add to the user the possibility to allow mts in `assert_ts` and
  `check_ts`
- add function to manage ts object and date_ts object
