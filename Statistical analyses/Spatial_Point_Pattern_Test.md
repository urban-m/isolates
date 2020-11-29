Spatial Point Pattern Test
================
Matthias Urban
29 November, 2020

# Overview

This document describes the application of the Spatial Point Pattern
Test as implemented in the R package sppt to the data analyzed in the
article.

# Required packages

``` r
library(tidyverse)
require(sppt)
require(maptools)
require(spatstat)
require(sf)
```

# Data preparation

Load in datasets

``` r
languagesonlandmasswithmountains <- read.csv("../Data/languagesonlandmasswithmountains.csv", header = T, encoding = "UTF-8")
languagesonlandmasswithhighmountains <- read.csv("../Data/languagesonlandmasswithhighmountains.csv", header = T, encoding = "UTF-8")
```

For this test we first need to identify those landmasses in the
OpenStreetMap polygon dataset on which one or more language is spoken.
These will be used as the areal units for the test.

Load Workspace with geographical objects

``` r
load(file = "../Data/workspace.Rdata")
```

Compute which landmasses host languages, first for the full dataset

``` r
polygonswithlanguages <- st_intersects(languagessf, worldpolygon, sparse = FALSE)
polygonswithlanguagessummary <- as.vector(c())
for (i in 1:ncol(polygonswithlanguages)) {
  polygonswithlanguagessummary <- append(polygonswithlanguagessummary, any(polygonswithlanguages[, i]))
}
```

Add results to the landmasses, so that they are classified as either
hosting mountain areas or not hosting mountain areas

``` r
worldpolygonwithlanguages <- worldpolygon[which(polygonswithlanguagessummary == TRUE), ]
```

Convert the resulting dataset and the language dataset to
SpatialPolygonsDataFrame objects

``` r
worldpolygonswithlanguagesboundary <- as_Spatial(worldpolygonwithlanguages)
languagessp <- as_Spatial(languagessf)
```

Now the same procedure for landmasses with mountain areas

``` r
polygonswithlanguageswithmountains <- st_intersects(languagesonlandmasswithmountainssf, mountains, sparse = FALSE)
polygonswithlanguageswithmountainssummary <- as.vector(c())
for (i in 1:ncol(polygonswithlanguageswithmountains)) {
  polygonswithlanguageswithmountainssummary <- append(polygonswithlanguageswithmountainssummary, any(polygonswithlanguageswithmountains[, i]))
}
```

Add results to the landmasses, so that they are classified as either
hosting mountain areas or not hosting mountain areas

``` r
worldpolygonwithlanguageswithmountains <- mountains[which(polygonswithlanguageswithmountainssummary == TRUE), ]
```

Convert the resulting dataset and the language dataset to
SpatialPolygonsDataFrame objects

``` r
worldpolygonswithlanguageswithmountainsboundary <- as_Spatial(worldpolygonwithlanguageswithmountains)
languagesonlandmasswithmountainssp <- as_Spatial(languagesonlandmasswithmountainssf)
```

And finally the same procedure for landmasses with mountain areas

``` r
polygonswithlanguageswithhighmountains <- st_intersects(languagesonlandmasswithmountainssf, highmountains, sparse = FALSE)
polygonswithlanguageswithhighmountainssummary <- as.vector(c())
for (i in 1:ncol(polygonswithlanguageswithhighmountains)) {
  polygonswithlanguageswithhighmountainssummary <- append(polygonswithlanguageswithhighmountainssummary, any(polygonswithlanguageswithhighmountains[, i]))
}
```

Add results to the landmasses, so that they are classified as either
hosting mountain areas or not hosting mountain areas

``` r
worldpolygonwithlanguageswithhighmountains <- highmountains[which(polygonswithlanguageswithhighmountainssummary == TRUE), ]
```

Convert the resulting dataset and the language dataset to
SpatialPolygonsDataFrame objects

``` r
worldpolygonswithlanguageswithhighmountainsboundary <- as_Spatial(worldpolygonwithlanguageswithhighmountains)
languagesonlandmasswithhighmountainssp <- as_Spatial(languagesonlandmasswithhighmountainssf)
```

# Main analysis

``` r
isolatesnarrowonlandmasswithhighmountainssp <- languagesonlandmasswithhighmountainssp[which(languagesonlandmasswithhighmountainssp$IsolateNarrow == T), ]
nonisolatesnarrownlandmasswithhighmountainsssp <- languagesonlandmasswithhighmountainssp[which(languagesonlandmasswithhighmountainssp$IsolateNarrow == F), ]
proportiondifferencehighmountainsisolatenarrow <- sppt_diff(isolatesnarrowonlandmasswithhighmountainssp, nonisolatesnarrownlandmasswithhighmountainsssp, worldpolygonswithlanguageswithhighmountainsboundary, test = "Fisher")
summary_sppt(proportiondifferencehighmountainsisolatenarrow)
```

    ## $globalS.standard
    ## [1] 0.75
    ## 
    ## $globalS.robust
    ## [1] 0.75

# Ancillary analyses

## Full dataset, isolates in a narrow sense

``` r
isolatesnarrowsp <- languagessp[which(languagessp$IsolateNarrow == T), ]
nonisolatesnarrowsp <- languagessp[which(languagessp$IsolateNarrow == F), ]
proportiondifferencefulldataisolatesnarrow <- sppt_diff(isolatesnarrowsp, nonisolatesnarrowsp, worldpolygonswithlanguagesboundary, test = "Fisher")
summary_sppt(proportiondifferencefulldataisolatesnarrow)
```

    ## $globalS.standard
    ## [1] 0.9904535
    ## 
    ## $globalS.robust
    ## [1] 0.9904535

## Full dataset, isolates in a wide sense

``` r
isolateswidesp <- languagessp[which(languagessp$IsolateWide == T), ]
nonisolateswidesp <- languagessp[which(languagessp$IsolateWide == F), ]
proportiondifferencefulldataisolateswide <- sppt_diff(isolateswidesp, nonisolateswidesp, worldpolygonswithlanguagesboundary, test = "Fisher")
summary_sppt(proportiondifferencefulldataisolateswide)
```

    ## $globalS.standard
    ## [1] 0.9928401
    ## 
    ## $globalS.robust
    ## [1] 0.9928401

## Landmasses with mountains, isolates in a narrow sense

``` r
isolatesnarrowonlandmasswithmountainssp <- languagesonlandmasswithmountainssp[which(languagesonlandmasswithmountainssp$IsolateNarrow == T), ]
nonisolatesnarrownlandmasswithmountainsssp <- languagesonlandmasswithmountainssp[which(languagesonlandmasswithmountainssp$IsolateNarrow == F), ]
proportiondifferencemountainsisolatenarrow <- sppt_diff(isolatesnarrowonlandmasswithmountainssp, nonisolatesnarrownlandmasswithmountainsssp, worldpolygonswithlanguageswithmountainsboundary, test = "Fisher")
summary_sppt(proportiondifferencemountainsisolatenarrow)
```

    ## $globalS.standard
    ## [1] 0.92
    ## 
    ## $globalS.robust
    ## [1] 0.92

## Landmasses with mountains, isolates in a wide sense

``` r
isolateswideonlandmasswithmountainssp <- languagesonlandmasswithmountainssp[which(languagesonlandmasswithmountainssp$IsolateWide == T), ]
nonisolateswideonlandmasswithmountainsssp <- languagesonlandmasswithmountainssp[which(languagesonlandmasswithmountainssp$IsolateWide == F), ]
proportiondifferencemountainsisolatewide <- sppt_diff(isolateswideonlandmasswithmountainssp, nonisolateswideonlandmasswithmountainsssp, worldpolygonswithlanguageswithmountainsboundary, test = "Fisher")
summary_sppt(proportiondifferencemountainsisolatewide)
```

    ## $globalS.standard
    ## [1] 0.94
    ## 
    ## $globalS.robust
    ## [1] 0.94

## Landmasses with alpine mountains, isolates in a wide sense

``` r
isolateswideonlandmasswithhighmountainssp <- languagesonlandmasswithhighmountainssp[which(languagesonlandmasswithhighmountainssp$IsolateWide == T), ]
nonisolateswidenlandmasswithhighmountainsssp <- languagesonlandmasswithhighmountainssp[which(languagesonlandmasswithhighmountainssp$IsolateWide == F), ]
proportiondifferencehighmountainsisolatewide <- sppt_diff(isolateswideonlandmasswithhighmountainssp, nonisolateswidenlandmasswithhighmountainsssp, worldpolygonswithlanguageswithhighmountainsboundary, test = "Fisher")
summary_sppt(proportiondifferencehighmountainsisolatewide)
```

    ## $globalS.standard
    ## [1] 0.75
    ## 
    ## $globalS.robust
    ## [1] 0.75
