
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Disclaimer

You may use this software for any purpose granted by the terms of the
MIT license (see license file). I make no guarantees that any of the
code will work for your tax preparation purposes. I am not a tax
professional nor an investment advisor, and any errors that occur in
your tax preparation are your own. Use at your own risk.

# capgains

<!-- badges: start -->
<!-- badges: end -->

The goal of capgains is to help calculate capital gains. The user simply
provides a log file of all of their transactions, and capgains will
calculate the capital gains incurred from their sales.

capgains uses the first-in, first-out accounting method. Others may be
supported in the future. capgains does not calculate your expected
taxes, only your actual gains for each asset. It does require that you
keep a record of every sale and buy for each asset (see the ‘log-format’
vignette.)

## Installation

You can install the development version of capgains from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("c0webster/capgains")
```
