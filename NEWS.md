<!--- Newest updates go at the top --->

<!--- Always increment the Version when pushing changes to the main branch  --->
<!--- Use a level 1 section head "Changes in version X.X.X" each update     --->
<!--- For many changes, use level 2 sections headers:                       --->
<!---         e.g. breaking changes, new features, bug fixes, etc           --->

# Changes in version 0.0.2

* New function: `check_for_missing()` to force stop or print missing values when present.
* New feature: `clean_excel_date()` now has a `verbose` argument set to `FALSE`.
* Many function additions from legacy tools package

# Changes in version 0.0.1

* Functions for converting non-finite values
  - `na_to()`, `inf_to()`, `nan_to`, `nonfinite_to()`
* Functions for string formatting (padding to given length)
  - `format_mrn()`, `safe_pad()`
* `perc_change()` for calculating % change
* `perc.table()` for pretty version of `base::prop.table()`
* `clean_excel_date()` for converting excel dates to `Date`/`POSIXct` classes

# Changes in version 0.0.0.9000 

* Initial creation (development version)
* contains shorthand functions `g()`, `gg()`, `hh()` for `dplyr::glimpse()`, `glue::glue()`, and `here::here()`
* `generate_template()` for reading in lines of text (e.g. SQL script) with `glue::glue()` syntax support
