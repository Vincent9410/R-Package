
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ausbushfire

`ausbushfire` is an R package specifically designed for analyzing the
relationship between Australian bushfires and climate data. It provides
a set of functions and an interactive Shiny application to help users
easily explore global temperature change trends over the past century.

## Installation

You can install the development version of ausbushfire like so:

``` r
# Install remotes if needed
# install.packages("remotes")

remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-rabbit292929")
```

## Quick start

Launch the Shiny app bundled in the package:

``` r
library(ausbushfire)
#run_ausbushfire_app()
```

Explore helper functions:

``` r
# Descriptive statistics for 2000–2020
calculate_statistics("climate", 2000, 2020)
#> # A tibble: 1 × 10
#>   Dataset Period        N N_non_missing  Mean Median    SD   Min   Max   IQR
#>   <chr>   <chr>     <int>         <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 climate 2000-2020   252           252  0.71    0.7 0.201  0.24  1.36  0.25
calculate_statistics("fwi",     2000, 2020)
#> # A tibble: 1 × 10
#>   Dataset Period        N N_non_missing  Mean Median    SD   Min   Max   IQR
#>   <chr>   <chr>     <int>         <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 fwi     2000-2020   252           252  7.42   7.28  3.36     0  15.6  4.86

# Annual trends (yearly mean / min / max)
annual_trends("climate", 2000, 2020)
#> # A tibble: 21 × 4
#>     Year Annual_Mean Annual_Max Annual_Min
#>    <int>       <dbl>      <dbl>      <dbl>
#>  1  2000       0.392       0.56       0.24
#>  2  2001       0.532       0.72       0.43
#>  3  2002       0.628       0.88       0.44
#>  4  2003       0.614       0.74       0.47
#>  5  2004       0.532       0.73       0.26
#>  6  2005       0.676       0.76       0.6 
#>  7  2006       0.635       0.79       0.46
#>  8  2007       0.659       1.01       0.49
#>  9  2008       0.54        0.74       0.29
#> 10  2009       0.654       0.79       0.53
#> # ℹ 11 more rows

# Extreme events per year (e.g., FWI ≥ 10)
extreme_events("fwi", threshold = 10, comparator = ">=", start_year = 2000, end_year = 2020)
#> # A tibble: 21 × 3
#>     Year Extreme_Count Extreme_Frequency
#>    <int>         <int>             <dbl>
#>  1  2000             4             0.333
#>  2  2001             4             0.333
#>  3  2002             3             0.25 
#>  4  2003             4             0.333
#>  5  2004             3             0.25 
#>  6  2005             2             0.167
#>  7  2006             4             0.333
#>  8  2007             2             0.167
#>  9  2008             2             0.167
#> 10  2009             3             0.25 
#> # ℹ 11 more rows
```

## What’s included

### Shiny app

- Fully interactive exploration of monthly / quarterly / yearly
  structure

- Uses **package datasets only** (no `read.csv` inside the app)

- Clear field descriptions and interpretation notes

- Custom styling (Bootswatch + optional CSS)

- Start with `run_ausbushfire_app()`

### Datasets

- `climate_data` — monthly global **temperature anomalies** (°C),
  1880–2025  

  Columns: `Year` (int), `Month` (abbrev `Jan`–`Dec`),
  `Temperature_Anomaly` (numeric), `Date` (mid-month `Date`), `Quarter`
  (`"Q1"`–`"Q4"`).

- `fwi_data` — monthly **Fire Weather Index (FWI)** (synthetic/demo),
  1900–2025  

  Columns: `Year`, `Month` (abbrev), `FWI`, `Date` (mid-month),
  `Quarter`.

<!-- -->

- Columns: `Year`, `Month` (abbrev), `FWI`, `Date` (mid-month),
  `Quarter`.

> Note: `fwi_synthetic` is **synthetic demo data** for teaching and
> interactive examples, not directly computed from ERA5/CFFDRS.

### Helper functions

- `calculate_statistics(data_type, start_year, end_year)` — descriptive
  stats within a year range

- `annual_trends(data_type, start_year, end_year)` — yearly mean/min/max

- `extreme_events(data_type, threshold, comparator, start_year, end_year)`
  — per-year extreme counts & frequency

## Documentation website (pkgdown)

**Website:**
<https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-rabbit292929>

- **Reference**: function documentation and dataset topics

- **Articles**: “Getting started with ausbushfire” vignette

- **Home**: brief description and quick links

## Reproducible data preparation

Raw data are cleaned under `data-raw/` and saved with
`usethis::use_data()` into the package:

- `data-raw/data-cleaning.R` reads the original CSV (e.g.,
  `GLB.Ts+dSST.csv`), tidies monthly anomalies, derives SH seasonal
  summaries if needed, and saves the final objects.

- All objects and functions are documented with **roxygen2**.

## License

This package is released under the **MIT License**. See
[LICENSE](LICENSE).

## Acknowledgements

- Temperature anomalies derived from NASA GISTEMP (e.g.,
  `GLB.Ts+dSST.csv`), cleaned for teaching/demo.

- FWI data in this package are **synthetic** and intended solely for
  instructional purposes.

## Maintainer

**ZHUO ZHANG** — <zzha0623@student.monash.edu>  

Issues & feature requests:
\<<https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-rabbit292929/ausbushfire/issues>\>
