Creating maps and plots for isolate distribution and analysis
================
Matthias Urban
15 März, 2021

# Overview

This document describes the creation of maps showing the distribution of
language isolates and associated descriptive statistics. It also
describes the creation of a panel plot that shows the successive removal
of landmasses (and languages spoken on them) depending on whether these
do or do not host (alpine) mountain areas as defined in the GMBA
dataset.

# Required packages

``` r
require(tidyverse)
require(rgdal)
require(maps)
require(OpenStreetMap)
require(sf)
require(ggpubr)
require(ggplot2)
require(cowplot)
require(ggtext)
```

We need working memory. Lots of.

``` r
memory.limit(size=100000)
```

    ## [1] 1e+05

Load in workspace

``` r
load(file = '../Data/workspace.Rdata')
```

## Creating a map showing the distribution of language isolates

Subset data creating datasets with isolates in a narrow sense, insolates
in a wide sense, and non-isolates and prepare for plotting

``` r
plotdataisolatesnarrow <- languages %>%
  filter(IsolateNarrow == T) %>%
  select(Latitude, Longitude)
coordinates(plotdataisolatesnarrow) <- ~ Longitude + Latitude
proj4string(plotdataisolatesnarrow) <- CRS("+init=epsg:4326")

plotdataisolateswide <- languages %>%
  filter(IsolateWide == T & IsolateNarrow == F) %>%
  select(Latitude, Longitude)
coordinates(plotdataisolateswide) <- ~ Longitude + Latitude
proj4string(plotdataisolateswide) <- CRS("+init=epsg:4326")

plotdataother <- languages %>%
  filter(IsolateWide == F) %>%
  select(Latitude, Longitude)
coordinates(plotdataother) <- ~ Longitude + Latitude
proj4string(plotdataother) <- CRS("+init=epsg:4326")
```

### Create the map

``` r
tiff(file = '../map.tiff', width=2684, height=2208)
map <- openmap(c(80, -180), c(-80, 180), type = "nps", minNumTiles = 100)
plot(map)
points(spTransform(plotdataother, osm()), pch = 21, col = "black", bg = "white", cex = 2.4)
points(spTransform(plotdataisolatesnarrow, osm()), pch = 21, col = "white", bg = "black", cex = 3.8)
points(spTransform(plotdataisolateswide, osm()), pch = 23, col = "white", bg = "black", cex = 3.4)
dev.off()
```

    ## png 
    ##   2

## Plots for descriptive statistics

### Density plots

Create individual plots

``` r
densityplotsea <- ggplot(languagesonlandmasswithhighmountains, aes(x = Distancetosea, color = IsolateNarrow)) +
  scale_color_manual(values = c("#736c6c", "#000000"), labels = c("Non-Isolates", "Isolates (narrow definition)")) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 40) +
  geom_density(alpha = .7, size = 1.2) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(axis.title = element_text(size = 14)) +
  xlim(0, 6000) +
  xlab("") +
  ylab("Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distance to sea")
plot(densityplotsea)
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
densityplotmountain <- ggplot(languagesonlandmasswithhighmountains, aes(x = Distancetohighmountain, color = IsolateNarrow)) +
  scale_color_manual(values = c("#736c6c", "#000000"), labels = c("Non-Isolates", "Isolates (narrow definition)")) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 52) +
  geom_density(alpha = .7, size = 1.2) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(axis.title = element_text(size = 14)) +
  xlab("") +
  ylab("Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distance to alpine mountain area")
plot(densityplotmountain)
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

Create and save panel plot

``` r
densityplotpanel <- ggarrange(densityplotmountain, densityplotsea, ncol = 1, common.legend = TRUE, legend = "bottom", labels=c("a", "b"), font.label = list(size = 10))
densityplotpanel
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("densityplotpanel.pdf", densityplotpanel, dpi = 600)
```

    ## Saving 7 x 5 in image

### Boxplots, including by-area plots

First, define functions for plotting the number of observations by group

``` r
nobservationshighmountains <- function(x) {
  return(c(y = 7000, label = length(x)))
}
nobservationssea <- function(x) {
  return(c(y = 3000, label = length(x)))
}
```

Create boxplots for distance of isolates and non-isolates spoken on
landmasses with alpine mountain areas to such alpine mountain areas and
the sea globally

``` r
boxplothighmountain <- ggplot(languagesonlandmasswithhighmountains, aes(x = reorder(IsolateNarrow, Distancetohighmountain), y = Distancetohighmountain)) +
  theme_bw() +
  geom_boxplot(fill = c("darkgray", "lightgray")) +
  geom_jitter(width = 0.1, alpha = 0.1, cex = 0.1) +
  stat_summary(fun.data = nobservationshighmountains, geom = "text", fun = median) +
  scale_x_discrete(labels = c("Isolates", "Non-Isolates")) +
  theme(axis.title = element_text(size = 14)) +
  ylab("Full dataset") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distance to alpine mountain area")
boxplothighmountain
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
boxplotsea <- ggplot(languagesonlandmasswithhighmountains, aes(x = reorder(IsolateNarrow, Distancetosea), y = Distancetosea)) +
  theme_bw() +
  geom_boxplot(fill = c("darkgray", "lightgray")) +
  geom_jitter(width = 0.1, alpha = 0.1, cex = 0.1) +
  stat_summary(fun.data = nobservationssea, geom = "text", fun = median) +
  scale_x_discrete(labels = c("Isolates", "Non-Isolates")) +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distance to sea")
boxplotsea
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

Create boxplots for distance of isolates and non-isolates spoken on
landmasses with alpine mountain areas to such alpine mountain areas and
the sea depending on macro-area

``` r
boxplotareasea <- ggplot(languagesonlandmasswithhighmountains, aes(x = reorder(IsolateNarrow, Distancetosea), y = Distancetosea)) +
  theme_bw() +
  geom_boxplot(fill = c("darkgray", "lightgray", "darkgray", "lightgray", "darkgray", "lightgray", "lightgray", "darkgray", "lightgray")) +
  facet_wrap(~Macroarea, nrow = 1) +
  theme(strip.text.x = element_text(size = 5.5)) +
  stat_summary(fun.data = nobservationssea, geom = "text", size = 2, fun = median) +
  scale_x_discrete(labels = c("Isolates", "Non-Isolates")) +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_markdown(color = "black", size = 7, angle = 90))
boxplotareasea
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
boxplotareamountain <- ggplot(languagesonlandmasswithhighmountains, aes(x = reorder(IsolateNarrow, Distancetohighmountain), y = Distancetohighmountain)) +
  theme_bw() +
  geom_boxplot(fill = c("darkgray", "lightgray", "darkgray", "lightgray", "darkgray", "lightgray", "lightgray", "darkgray", "lightgray")) +
  facet_wrap(~Macroarea, nrow = 1) +
  theme(strip.text.x = element_text(size = 5.5)) +
  stat_summary(fun.data = nobservationshighmountains, geom = "text", size = 2, fun = median) +
  theme(axis.text.x = element_markdown(color = "black", size = 7, angle = 90)) +
  scale_x_discrete(labels = c("Isolates", "Non-Isolates")) +
  theme(axis.title = element_text(size = 14)) +
  ylab("By area") +
  xlab("")
boxplotareamountain
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

Create and save panel plot

``` r
boxplotpanel <- ggarrange(boxplothighmountain, boxplotsea, boxplotareamountain, boxplotareasea, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom", labels=c("a", "b", "c", "d"),font.label = list(size = 10))
boxplotpanel
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggsave("boxplotpanel.pdf", boxplotpanel, dpi = 600)
```

    ## Saving 7 x 5 in image

## Plot showing the successive removal of landmasses depending on whether or not they feature mountain areas and then on whether or not they feature mountain areas with alpine conditions

First convert necessary polygon datasets to sf objects with WGS 84
projection for plotting

``` r
mountainsplot <- st_as_sf(mountains, coords = c("long", "lat"), crs = 4326)
highmountainsplot <- st_as_sf(highmountains, coords = c("long", "lat"), crs = 4326)
worldpolygonplot <- st_as_sf(worldpolygon, coords = c("long", "lat"), crs = 4326)
mountainareaswgs84plot <- st_as_sf(mountainareaswgs84, coords = c("long", "lat"), crs = 4326)
highmountainareaswgs84plot <- st_as_sf(highmountainareaswgs84, coords = c("long", "lat"), crs = 4326)
```

Plot all landmasses and mountain areas in the GMBA dataset (Körner et
al. 2017), distinguishing between those with alpine conditions and those
without

``` r
fulllandmassesplot <- ggplot() +
  theme(legend.title = element_blank()) +
  geom_sf(data = worldpolygonplot, color = "black", fill = "white") +
  geom_sf(data = mountainareaswgs84plot, aes(fill = "All mountain areas")) +
  geom_sf(data = highmountainareaswgs84plot, aes(fill = "Mountain areas with alpine conditions")) +
  coord_sf(xlim = c(-180, 180), ylim = c(-80, 80), crs = st_crs(4326)) +
  scale_fill_manual(values = c("Mountain areas with alpine conditions" = "gray0", "All mountain areas" = "dark grey"))
fulllandmassesplot
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Now remove all landmasses without any mountain area as defined in the
GMBA dataset

``` r
landmasseswithmountainsplot <- ggplot() +
  theme(legend.title = element_blank()) +
  geom_sf(data = mountainsplot, color = "black", fill = "white") +
  geom_sf(data = mountainareaswgs84plot, aes(fill = "All mountain areas")) +
  coord_sf(xlim = c(-180, 180), ylim = c(-80, 80), crs = st_crs(4326)) +
  scale_fill_manual(values = c("Mountain areas with alpine conditions" = "gray0", "All mountain areas" = "dark grey"))
landmasseswithmountainsplot
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

And now remove all landmasses without any mountain area with alpine
conditions as defined in the GMBA dataset

``` r
landmasseswithhighmountainsplot <- ggplot() +
  theme(legend.title = element_blank()) +
  geom_sf(data = highmountainsplot, color = "black", fill = "white") +
  geom_sf(data = highmountainareaswgs84plot, aes(fill = "Mountain areas with alpine conditions")) +
  coord_sf(xlim = c(-180, 180), ylim = c(-80, 80), crs = st_crs(4326)) +
  scale_fill_manual(values = c("Mountain areas with alpine conditions" = "gray0", "All mountain areas" = "dark grey"))
landmasseswithhighmountainsplot
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Finally, create and save panel plot

``` r
landmassespanel <- ggarrange(fulllandmassesplot, landmasseswithmountainsplot, landmasseswithhighmountainsplot, ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom", labels=c("a", "b", "c"), font.label = list(size = 10))
landmassespanel
```

![](Creating_maps_and_plots_for_isolate_distribution_and_-analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggsave("landmassespanel.pdf", landmassespanel, dpi = 600)
```

    ## Saving 7 x 5 in image
