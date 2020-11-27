Wilcoxon-Mann-Whitney Test
================
Matthias Urban
27 November, 2020

# Overview

This document describes the procedurs for carrying out the
Wilcoxon-Mann-Whitney Test reported in the article.

# Required packages

``` r
library(tidyverse)
```

# Data preparation

Load in datasets

``` r
languagesonlandmasswithmountains <- read.csv("../Data/languagesonlandmasswithmountains.csv", header = T, encoding = "UTF-8")
languagesonlandmasswithhighmountains <- read.csv("../Data/languagesonlandmasswithhighmountains.csv", header = T, encoding = "UTF-8")
```

Treat Macroareas as factors

``` r
languagesonlandmasswithmountains$Macroarea <- as.factor(languagesonlandmasswithmountains$Macroarea)
languagesonlandmasswithhighmountains$Macroarea <- as.factor(languagesonlandmasswithhighmountains$Macroarea)
```

# Main test

``` r
isolatesnarrowonlandmasswithhighmountains <- languagesonlandmasswithhighmountains %>% filter(IsolateNarrow == T)
nonisolatesnarrowonlandmasswithhighmountains <- languagesonlandmasswithhighmountains %>% filter(IsolateNarrow == F)
wilcox.test(isolatesnarrowonlandmasswithhighmountains$Distancetosea, nonisolatesnarrowonlandmasswithhighmountains$Distancetosea, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolatesnarrowonlandmasswithhighmountains$Distancetosea and nonisolatesnarrowonlandmasswithhighmountains$Distancetosea
    ## W = 295520, p-value = 0.1839
    ## alternative hypothesis: true location shift is less than 0

``` r
wilcox.test(isolatesnarrowonlandmasswithhighmountains$Distancetohighmountain, nonisolatesnarrowonlandmasswithhighmountains$Distancetohighmountain, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolatesnarrowonlandmasswithhighmountains$Distancetohighmountain and nonisolatesnarrowonlandmasswithhighmountains$Distancetohighmountain
    ## W = 211644, p-value = 1.034e-09
    ## alternative hypothesis: true location shift is less than 0

# Ancillary test: High Mountain areas, Isolates Wide

``` r
isolateswideonlandmasswithhighmountains <- languagesonlandmasswithhighmountains %>% filter(IsolateWide == T)
nonisolateswideonlandmasswithhighmountains <- languagesonlandmasswithhighmountains %>% filter(IsolateWide == F)
wilcox.test(isolateswideonlandmasswithhighmountains$Distancetosea, nonisolateswideonlandmasswithhighmountains$Distancetosea, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolateswideonlandmasswithhighmountains$Distancetosea and nonisolateswideonlandmasswithhighmountains$Distancetosea
    ## W = 354771, p-value = 0.08249
    ## alternative hypothesis: true location shift is less than 0

``` r
wilcox.test(isolateswideonlandmasswithhighmountains$Distancetohighmountain, nonisolateswideonlandmasswithhighmountains$Distancetohighmountain, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolateswideonlandmasswithhighmountains$Distancetohighmountain and nonisolateswideonlandmasswithhighmountains$Distancetohighmountain
    ## W = 284870, p-value = 8.789e-08
    ## alternative hypothesis: true location shift is less than 0

# Ancillary test: All mountainareas, Isolates Narrow

``` r
isolatesnarrowonlandmasswithmountains <- languagesonlandmasswithmountains %>% filter(IsolateNarrow == T)
nonisolatesnarrowonlandmasswithmountains <- languagesonlandmasswithmountains %>% filter(IsolateNarrow == F)
wilcox.test(isolatesnarrowonlandmasswithmountains$Distancetosea, nonisolatesnarrowonlandmasswithmountains$Distancetosea, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolatesnarrowonlandmasswithmountains$Distancetosea and nonisolatesnarrowonlandmasswithmountains$Distancetosea
    ## W = 584796, p-value = 0.113
    ## alternative hypothesis: true location shift is less than 0

``` r
wilcox.test(isolatesnarrowonlandmasswithmountains$Distancetomountain, nonisolatesnarrowonlandmasswithmountains$Distancetomountain, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolatesnarrowonlandmasswithmountains$Distancetomountain and nonisolatesnarrowonlandmasswithmountains$Distancetomountain
    ## W = 654881, p-value = 0.9214
    ## alternative hypothesis: true location shift is less than 0

# Ancillary test: All Mountain areas, Isolates Wide

``` r
isolateswideonlandmasswithmountains <- languagesonlandmasswithmountains %>% filter(IsolateWide == T)
nonisolateswideonlandmasswithmountains <- languagesonlandmasswithmountains %>% filter(IsolateWide == F)
wilcox.test(isolateswideonlandmasswithmountains$Distancetosea, nonisolateswideonlandmasswithmountains$Distancetosea, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolateswideonlandmasswithmountains$Distancetosea and nonisolateswideonlandmasswithmountains$Distancetosea
    ## W = 690666, p-value = 0.08071
    ## alternative hypothesis: true location shift is less than 0

``` r
wilcox.test(isolateswideonlandmasswithmountains$Distancetomountain, nonisolateswideonlandmasswithmountains$Distancetomountain, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  isolateswideonlandmasswithmountains$Distancetomountain and nonisolateswideonlandmasswithmountains$Distancetomountain
    ## W = 802699, p-value = 0.9931
    ## alternative hypothesis: true location shift is less than 0
