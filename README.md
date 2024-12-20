
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 🗳 Plausi Package

**Predict votes and detect anomalies using R.**

<!-- badges: start -->

[![tic](https://github.com/machinelearningZH/plausi/workflows/tic/badge.svg)](https://github.com/machinelearningZH/plausi/actions)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

<p align="left">

<img src="https://raw.githubusercontent.com/machinelearningZH/plausi/refs/heads/main/plausi_hex.png" alt="" width="200"/>

The Plausi package is designed for R-supported election forensics. It
provides functions that enable the identification of statistical
irregularities and anomalies in vote results.

Key features include:

- Robust outlier detection for small sample sizes and skewed
  distributions
- Calculation of differences between all possible combinations of
  turnout-levels (e.g., for systematic comparison of voter turnout
  across all voting districts)
- Prediction of expected results using machine learning algorithms
  (e.g., yes-vote proportions, voter turnout, etc.)

It serves as a basis for the ***PlausiApp***, which is used for vote
result quality control in different cantons (TG / SG / ZH).

For the moment, the ***PlausiApp*** is made available upon request via a
private Repo (<a href="mailto:wahlen@statistik.zh.ch"
class="uri">mailto:wahlen@statistik.zh.ch</a>).

## Installation

You can install the plausi package from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("machinelearningZH/plausi")
```

## Usage

Attach the package and you are good to go.

``` r
library(plausi)
```

All you need now is data. The easiest way to access data on popular
votes in Switzerland is via the [swissdd
package](https://github.com/politanch/swissdd), with which you can
easily get a wide range of vote results, for example the results of the
national votes from 2024-11-24:

``` r
devtools::install_github("politanch/swissdd")

vote_data <- swissdd::get_nationalvotes(geolevel = "municipality", votedates = "2024-11-24")
```

## Licensing

The package is licensed under the MIT license.

## Project team

This is a joint project of the [Vote &
Election-Team](https://www.zh.ch/de/direktion-der-justiz-und-des-innern/statistisches-amt/wahlen-und-abstimmungen.html)
and [Team
Data](https://www.zh.ch/de/direktion-der-justiz-und-des-innern/statistisches-amt/data.html)
of the Statistical Office of the Canton of Zurich. Responsible: Simon
Graf, Thomas Lo Russo and Thomas Knecht.

## Feedback and contributing

We would love to hear from you. Please share your feedback and let us
know how you use the code. You can [write an
email](mailto:wahlen@statistik.zh.ch) or share your ideas by opening an
issue or a pull requests.
