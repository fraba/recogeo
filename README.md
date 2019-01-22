recogeo
================

## Overview

recogeo helps reconciling geographies (such as administrative or
electoral unit) that might have changed overtime.

recogeocompares geographies from time 1 (for example \(A_1\)) with
geographies from time 2 (for example \(B_2\)) and determine if:

1.  Geographies \(A_1\) and \(B_2\) are the same (if \(A_1\) contains
    \(=B_2\) and \(B_2\) contains \(A_1\)).
2.  Geographies \(A_1\) has been merged into geography $B\_2 (if \(B_2\)
    contains \(A_1\) but \(A_1\) does not contain \(=B_2\)).
3.  Geography \(A_1\) has been split into two or more geographies
    \(B_2\), \(C_2\), \(D_2\), … (\(A_1\) intersects \(B_2\), \(C_2\),
    \(D_2\), …).

## Installation

To install:

``` r
# Install devtools if not already installed
# install.packages("devtools") 
devtools::install_github("fraba/recogeo")
```

## Usage

``` r
library(recogeo)
data(polygons, package = "recogeo")
res <- reconcileGeographies(poly_a, poly_b)
```

`reconcileGeographies()` takes two spatial objects (either a
`SpatialPolygons` object from sp or a Simple Feature from sf and
returns:

| unigeokey\_A | unigeokey\_B | type         |
| :----------- | :----------- | :----------- |
| 382          | 364          | same         |
| 135          | 153          | same         |
| 173          | 346          | AcontainsB   |
| 249          | 244          | same         |
| 79           | 107          | same         |
| 450          | 452          | same         |
| 8            | 10           | same         |
| 125          | 145          | same         |
| 292          | 272          | same         |
| 74           | 99           | AintersectsB |
