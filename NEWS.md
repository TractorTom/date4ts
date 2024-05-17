# TractorTsbox (development version)

# TractorTsbox actual

* `mutate_each()` and `summarise_each()` now throw correct deprecation messages
  (#6869).

* `setequal()` now requires the input data frames to be compatible, similar to
  the other set methods like `setdiff()` or `intersect()` (#6786).

# First release : TractorTsbox 0.1.0

* `count()` better documents that it has a `.drop` argument (#6820).

* Fixed tests to maintain compatibility with the next version of waldo (#6823).

* Joins better handle key columns will all `NA`s (#6804).
