Data preparation and selection
================
Matthias Urban
19 März, 2021

# Overview

This document describes the data preparation procedures. It prepares the
Glottolog 4.0. Dataset for the languages of the world for the analyses
reported in this paper, and it shows how the distance to the sea and the
distance to mountain areas on the same landmass as the individual
languages are spoken is calculated.

# Required datasets

The document assumes that the following files are stored in a directory
named “Data” within the project directory:

Glottolog 4.2 data (Hammarström et al. 2020) available at Zenodo:
<https://zenodo.org/record/3753877#.X7IoRFAxnOg>

OpenStreetMap land polygon data available at Zenodo:
<https://doi.org/10.5281/zenodo.4621263> These data were originally
downloaded from
<https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip>,
but have been superseded in the meantime by more recent versions. The
site does not retain older versions of the dataset.

GMBA mountain inventory (Körner et al. 2017) available here:
<https://ilias.unibe.ch/goto_ilias3_unibe_file_1047348_download.html>
<https://ilias.unibe.ch/goto_ilias3_unibe_file_1346114_download.html>

Workspace available here at Zenodo:
<https://doi.org/10.5281/zenodo.4621263>

Because of a large number of time-consuming calculations, I did not
rerun all analyses for creating this documentation. Caching was not an
option because it does not support objects of the size involved in these
analyses. All code that created the objects in the workspace is provided
in this documentation, though.

# Required packages

``` r
library(tidyverse)
require(OpenStreetMap)
require(rnaturalearth)
require(rgdal)
require(sf)
require(units)
require(lwgeom)
```

# Data preparation

## General settings

Switch off scientific notation

``` r
options(scipen = 999)
```

We need working memory. Lots of.

``` r
memory.limit(size = 100000)
```

    ## [1] 100000

Set WGS 84 projection

``` r
rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
```

## Load in workspace

``` r
load(file = "../Data/workspace.Rdata")
```

## Read in and prepare Glottolog 4.2 language data

Read in data

``` r
languages <- read.csv("../Data/languages.csv", header = T, encoding = "UTF-8")
```

Remove rows with empty cells for Latitude and Longitude

``` r
languages <- filter(languages, Latitude != "", Longitude != "")
```

Manually fix macroarea for Guiyang to Eurasia - apparently a glitch in
the Glottolog 4.2 dataset

``` r
languages[which(languages$ID == "guiy1235"), "Macroarea"] <- "Eurasia"
```

Treat Macroareas as factors

``` r
languages$Macroarea <- as.factor(languages$Macroarea)
```

Remove sign languages, artificial languages and data kept only for
bookkeeping purposes from dataset

``` r
languages <- filter(languages, (Family_ID != "sign1238" & Family_ID != "book1242" & Family_ID != "arti1236"))
```

Change separator from comma to dot for coordinates and treat as numeric

``` r
languages$Latitude <- as.numeric(gsub(",", ".", languages$Latitude))
languages$Longitude <- as.numeric(gsub(",", ".", languages$Longitude))
```

Add a column that indicates whether a language is an isolate in a narrow
sense (excluding “unclassifiable” languages)

``` r
languages <- mutate(languages, IsolateNarrow = Family_ID == "")
```

Add a column that indicates whether a language is an isolates in a broad
sense (including “unclassifiable” languages)

``` r
languages <- mutate(languages, IsolateWide = (Family_ID == "" | Family_ID == "uncl1493"))
```

Arrange languages by latidude in ascending order

``` r
languages <- languages %>% arrange(Latitude)
```

Export the resulting dataset

``` r
write.csv(languages, "../Data/languages.csv")
```

Tranform language coordinates to sf object with WGS84 projection

``` r
languagessf <- st_as_sf(languages, coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(rob)
languagessf2 <- st_transform(languagessf, coords = c("Longitude", "Latitude"), 4326)
```

## Compute distance from coast and add to dataset

Read in coastline

``` r
coastline <- ne_coastline(scale = 10, returnclass = "sf") %>%
  st_transform(rob) %>%
  st_cast("MULTILINESTRING")
```

Compute distance to coastline

``` r
distancetosea <- st_distance(coastline, languagessf) %>%
  set_units(km) %>%
  apply(2, min)
```

Add result to dataset

``` r
languages <- mutate(languages, Distancetosea = distancetosea)
```

## Compute distance to mountain areas on the same continent and add to dataset

### Preparation

Read GMBA Mountain Inventory dataset V 1.2

``` r
mountainareas <- st_read("../Data/GMBA Mountain Inventory_v1.2-World.shp")
```

    ## Reading layer `GMBA Mountain Inventory_v1.2-World' from data source `C:\Users\subur\Dropbox\Work\Current articles\Data\GMBA Mountain Inventory_v1.2-World.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 1048 features and 2 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -180 ymin: -55.71303 xmax: 180 ymax: 79.91116
    ## geographic CRS: WGS 84

Transform to multipolygon with WGS84 projection

``` r
mountainareaswgs84 <- mountainareas %>%
  st_transform(rob) %>%
  st_cast("MULTIPOLYGON")
```

Create a subset with only those mountainareas with alpine belts as
recognized in V 1.1. As the datasets in V 1.1 and V 1.2. differ
slightly, automatic joining is not an option, so relevant data are
selected manually.

``` r
mountainareas <- mountainareas %>%
  add_column(belt1 = as.logical(FALSE)) %>%
  mutate(belt1 = replace(belt1, c(54, 131, 182, 184, 185, 186, 187, 191, 192, 193, 195, 196, 198, 199, 200, 201, 203, 204, 206, 207, 208, 210, 212, 215, 216, 217, 218, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 240, 241, 244, 245, 247, 335, 336, 348, 356, 357, 358, 359, 382, 388, 389, 390, 392, 393, 394, 395, 396, 397, 398, 401, 402, 403, 404, 406, 407, 436, 508, 514, 515, 518, 519, 521, 538, 547, 548, 549, 553, 554, 565, 566, 568, 584, 593, 594, 595, 603, 615, 616, 617, 618, 623, 637, 678, 680, 681, 711, 714, 715, 722, 723, 724, 725, 728, 736, 776, 777, 778, 779, 780, 781, 782, 784, 785, 810, 812, 844, 907, 908, 909, 928, 932, 936, 939, 947, 952, 959, 961, 965, 966, 969, 973, 977, 978, 979, 980, 981, 984, 989, 990, 999, 1001, 1002, 1003, 1004, 1005, 1007), as.logical(TRUE)))
highmountainareas <- filter(mountainareas, belt1 == TRUE)
```

Also transform to multipolygon with WGS84 projection

``` r
highmountainareaswgs84 <- highmountainareas %>%
  st_transform(rob) %>%
  st_cast("MULTIPOLYGON")
```

Read OpenStreetMap polygon dataset

``` r
worldpolygon <- st_read("../Data/land_polygons.shp")
```

    ## Reading layer `land_polygons' from data source `C:\Users\subur\Dropbox\Work\Current articles\Data\land_polygons.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 678536 features and 1 field
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -16817520 ymin: -8625155 xmax: 16923550 ymax: 8344261
    ## CRS:            unknown

Transform to WGS84 projection

``` r
worldpolygon <- worldpolygon %>% st_transform(rob)
```

### First we identify all landmasses that do not feature any mountain area as defined by the GMBA dataset and remove them from the dataset

``` r
disjoint <- st_disjoint(mountainareaswgs84, worldpolygon, sparse = FALSE)
```

``` r
disjointsummary <- as.vector(c())
for (i in 1:ncol(disjoint)) {
  disjointsummary <- append(disjointsummary, all(disjoint[, i]))
}
```

Add results to the OpenStreetMap polygon dataset, so that polygons are
classified as either hosting mountain areas or not hosting mountain
areas

``` r
disjoint <- rbind(disjoint, disjointsummary)
worldpolygon <- mutate(worldpolygon, disjointsummary)
```

For confirmation plot results

``` r
plot(worldpolygon[disjointsummary == F, 2, fill = "grey"])
```

![](Data_preparation_and_selection_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
Subset the OpenStreetMap polygon dataset

``` r
mountains <- filter(worldpolygon, disjointsummary == FALSE)
```

### Then, we identify languages that are spoken on landmasses with mountain areas

``` r
languageswithmountainareas <- st_intersects(languagessf, mountains, sparse = FALSE)
languageswithmountainareassummary <- as.vector(c())
for (i in 1:nrow(languageswithmountainareas)) {
  languageswithmountainareassummary <- append(languageswithmountainareassummary, any(languageswithmountainareas[i, ]))
}
```

Add result to dataset

``` r
languages <- mutate(languages, OnLandmasswithMountains = as.logical(languageswithmountainareassummary))
languagessf <- mutate(languagessf, OnLandmasswithMountains = as.logical(languageswithmountainareassummary))
languagessf2 <- mutate(languagessf2, OnLandmasswithMountains = as.logical(languageswithmountainareassummary))
```

Create a subset of the data that only includes languages that are spoken
on landmasses with mountainareas

``` r
languagesonlandmasswithmountains <- filter(languages, OnLandmasswithMountains == TRUE)
languagesonlandmasswithmountainssf <- filter(languagessf, OnLandmasswithMountains == TRUE)
languagesonlandmasswithmountainssf2 <- filter(languagessf2, OnLandmasswithMountains == TRUE)
```

### Then we can proceed further to calculate the distance to mountain areas on the same landmass.

The following code serves to make sure that the closest mountain area on
the same landmass is used for calculating distances This is necessary
because they sometimes may be located on another landmass (e.g. for
Northern Australian languages, the closest relevant mountain area is
often the New Guinea highlands)

Identify the landmass the subsetted languages are located on

``` r
languagesonlandmasswithmountainsbinary <- st_intersects(languagesonlandmasswithmountainssf, mountains, sparse = FALSE)
```

And the landmass(es) the mountain areas are located on

``` r
mountainsonlandmassesbinary <- st_intersects(mountainareaswgs84, mountains, sparse = FALSE)
```

Then calculate for each language whether the landmass it is located on
is identical to that on which a given mountain area is located on. Even
on a fast computer, this will take a significant amount of time.

``` r
intersects <- data.frame(Language_ID = integer(), Mountainarea = integer(), Whichmountainarea = integer())
for (i in 1:nrow(languagesonlandmasswithmountainsbinary)) {
  for (j in 1:nrow(mountainsonlandmassesbinary)) {
    intersects <- rbind(intersects, c(i, j, which(languagesonlandmasswithmountainsbinary[i, ] == TRUE) %in% which(mountainsonlandmassesbinary[j, ] == TRUE)))
  }
}
```

Ensure that the output is recognized as a binary variable

``` r
intersects[, 4] <- as.logical(intersects[, 4])
```

Filter the results to only retain those cases where the same landmass
actually hosted a given language and a given mountain area

``` r
intersectsfilter <- intersects[which(intersects$X1L.2 == TRUE), ]
```

Compute the distance for each language to those mountain areas located
on the same landmass. There may be more than one.

``` r
intersectsfilter$distance <- NA
for (i in 1:nrow(intersectsfilter)) {
  intersectsfilter[i, "distance"] <- st_distance(languagesonlandmasswithmountainssf2[intersectsfilter[i, 2], ], mountainareas[intersectsfilter[i, 3], ]) %>% set_units(km)
}
```

Identify the closest mountain area among those on the same landmass

``` r
distance_unfiltered <- aggregate(distance ~ X1L, intersectsfilter, function(x) min(x))
```

And finally add the result to the dataset

``` r
languagesonlandmasswithmountains <- mutate(languagesonlandmasswithmountains, Distancetomountain = distance_unfiltered$distance)
```

``` r
write.csv(languagesonlandmasswithmountains, "../Data/languagesonlandmasswithmountains.csv")
```

### Then we proceed analogously for mountain areas with alpine belts.

First we identify all landmasses that do not feature any alpine mountain
area as defined by the GMBA dataset and remove them from the dataset

``` r
disjointhigh <- st_disjoint(highmountainareaswgs84, worldpolygon, sparse = FALSE)
disjointsummaryhigh <- as.vector(c())
for (i in 1:ncol(disjointhigh)) {
  disjointsummaryhigh <- append(disjointsummaryhigh, all(disjointhigh[, i]))
}
```

Add results to the landmasses, so that they are classified as either
hosting mountain areas with alpine conditions or not hosting them

``` r
disjointhigh <- rbind(disjointhigh, disjointsummaryhigh)
worldpolygon <- mutate(worldpolygon, disjointsummaryhigh = disjointsummaryhigh)
```

For confirmation plot results

``` r
plot(worldpolygon[disjointsummaryhigh == F, 2, fill = "grey"])
```

![](Data_preparation_and_selection_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

Subset the OpenStreetMap polygon dataset

``` r
highmountains <- filter(worldpolygon, disjointsummaryhigh == FALSE)
```

Identify languages that are spoken on landmasses with alpine mountain
areas

``` r
languageswithhighmountainareas <- st_intersects(languagessf, highmountains, sparse = FALSE)
languageswithhighmountainareassummary <- as.vector(c())
for (i in 1:nrow(languageswithmountainareas)) {
  languageswithhighmountainareassummary <- append(languageswithhighmountainareassummary, any(languageswithhighmountainareas[i, ]))
}
```

Add result to dataset

``` r
languages <- mutate(languages, OnLandmasswithHighMountains = as.logical(languageswithhighmountainareassummary))
languagessf <- mutate(languagessf, OnLandmasswithHighMountains = as.logical(languageswithhighmountainareassummary))
languagessf2 <- mutate(languagessf2, OnLandmasswithHighMountains = as.logical(languageswithhighmountainareassummary))
```

Create a subset of the data that only includes languages that are spoken
on landmasses with mountainareas

``` r
languagesonlandmasswithhighmountains <- filter(languages, OnLandmasswithHighMountains == TRUE)
languagesonlandmasswithhighmountainssf <- filter(languagessf, OnLandmasswithHighMountains == TRUE)
languagesonlandmasswithhighmountainssf2 <- filter(languagessf2, OnLandmasswithHighMountains == TRUE)
```

### Then we can again proceed further to calculate the distance to alpine mountain areas on the same landmass.

The following code serves to make sure that the closest alpine mountain
area on the same landmass is used for calculating distances. This is
necessary because they sometimes may be located on another landmass
(e.g. for Northern Australian languages, the closest relevant mountain
area is often the New Guinea highlands)

Identify the landmass the subsetted languages are located on

``` r
languagesonlandmasswithhighmountainsbinary <- st_intersects(languagesonlandmasswithhighmountainssf, highmountains, sparse = FALSE)
```

Identify the landmass(es) the alpine mountain areas are located on

``` r
highmountainsonlandmassesbinary <- st_intersects(highmountainareaswgs84, highmountains, sparse = FALSE)
```

Then calculate for each language whether the landmass it is located on
is identical to that on which a given alpine mountain area is located on

``` r
intersectshigh <- data.frame(Language_ID = integer(), Mountainarea = integer(), Whichmountainarea = integer())
for (i in 1:nrow(languagesonlandmasswithhighmountainsbinary)) {
  for (j in 1:nrow(highmountainsonlandmassesbinary)) {
    intersectshigh <- rbind(intersectshigh, c(i, j, which(languagesonlandmasswithhighmountainsbinary[i, ] == TRUE) %in% which(highmountainsonlandmassesbinary[j, ] == TRUE)))
  }
}
```

Ensure that the output is recognized as a binary variable

``` r
intersectshigh[, 4] <- as.logical(intersectshigh[, 4])
```

Filter the results to only retain those cases where the same landmass
actually hosted a given language and a given mountain area

``` r
intersectshighfilter <- intersectshigh[which(intersectshigh$X0L == TRUE), ]
```

Compute the distance for each language to those mountain areas located
on the same landmass. There may be more than one.

``` r
intersectshighfilter$distance <- NA
for (i in 1:nrow(intersectshighfilter)) {
  intersectshighfilter[i, "distance"] <- st_distance(languagesonlandmasswithhighmountainssf2[intersectshighfilter[i, 2], ], highmountainareas[intersectshighfilter[i, 3], ]) %>% set_units(km)
}
```

Then we select that mountainarea in the result with the smallest
distance, and add it to the dataset
distancehigh\_unfiltered\<-aggregate(distance\~X1L,
intersectshighfilter, function(x) min(x)) And finally add the result to
the dataset and export

``` r
languagesonlandmasswithhighmountains <- mutate(languagesonlandmasswithhighmountains, Distancetohighmountain = distancehigh_unfiltered$distance)
```

``` r
write.csv(languagesonlandmasswithhighmountains, "../Data/languagesonlandmasswithhighmountains.csv")
```
