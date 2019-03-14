
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shipstationr

Get Ship Done.

## Overview

Access the ShipStation API using R.

## Installation

``` r
# Install development version from GitHub
devtools::install_github("anthonypileggi/shipstationr")
```

## Setup

To use the `shipstationr` package, you need to first setup some
environment variables.

``` r
SHIPSTATION_API_KEY = "your_api_key"
SHIPSTATION_API_SECRET = "your_api_secret"
SHIPSTATION_STORE_ID = "your_store_id"
```

If setup correctly, you should be able to use package functions.

``` r
library(shipstationr)

# How many user accounts are setup?
nrow(ss_get_users())
#> [1] 11
```
