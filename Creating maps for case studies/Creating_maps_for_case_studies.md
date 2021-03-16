Creating maps for case studies of isolate genesis and range reduction
================
Matthias Urban
16 März, 2021

# Overview

On the basis of shapefiles for language ranges created in house on the
basis of extant specialized literature as cited in the article, this
document describes the creation of the maps for case studies of the
genesis of isolates and the reduction of their ranges.

# Required packages

``` r
require(rgdal)
require(ggplot2)
require(cowplot)
require(maps)
```

# Create world map for coastlines and administrative boundaries

``` r
map.world <- map_data(map = "world")
```

# Basque

Read in data

``` r
basque1stmillenium <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 1st millenium BC.shp"))
basque7thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 7th century.shp"))
basque9thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 9th century.shp"))
basque11thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 11th century.shp"))
basque13thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 13th century.shp"))
basque16thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 18th century.shp"))
basque18thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 18th century.shp"))
basque19thcentury <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque 19th century.shp"))
basquetoday <- fortify(readOGR("../Data/Shapefiles/Fig. 2 Basque/Basque today.shp"))
```

Create main map

``` r
ggbasque <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black", size = 0.15) +
  coord_quickmap(ylim = c(40, 47), xlim = c(-5, 5)) +
  geom_polygon(data = basque1stmillenium, aes(x = long, y = lat, fill = "1st Millenium BC"), color = "black", size = 0.15) +
  geom_polygon(data = basque7thcentury, aes(x = long, y = lat, fill = "7th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basque9thcentury, aes(x = long, y = lat, fill = "9th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basque11thcentury, aes(x = long, y = lat, fill = "11th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basque13thcentury, aes(x = long, y = lat, fill = "13th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basque16thcentury, aes(x = long, y = lat, fill = "16th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basque18thcentury, aes(x = long, y = lat, fill = "18th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basque19thcentury, aes(x = long, y = lat, fill = "19th century AD"), color = "black", size = 0.15) +
  geom_polygon(data = basquetoday, aes(x = long, y = lat, fill = "Today"), color = "black", size = 0.15) +
  scale_fill_manual(name = "Basque speaking areas", values = c("1st Millenium BC" = "gray90", "7th century AD" = "gray80", "9th      century AD" = "gray70", "11th century AD" = "gray60", "13th century AD" = "gray50", "16th century AD" = "gray40", "18th       
      century AD" = "gray30", "19th century AD" = "gray20", "Today" = "gray10"), breaks = c(
    "1st Millenium BC", "7th century AD",
    "9th century AD", "11th century AD", "13th century AD", "16th century AD", "18th century AD", "19th century AD",
    "Today"
  )) +
  labs(x = "Longitude", y = "Latitude") +
  annotate("text", y = 45, x = 2.5, label = "France") +
  annotate("text", y = 41, x = -2.5, label = "Spain")
plot(ggbasque)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Create inset map

``` r
ggbasqueinset <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black", size = 0.15) +
  coord_quickmap(ylim = c(36, 59), xlim = c(-20, 30)) +
  geom_rect(aes(ymin = 40, ymax = 47, xmin = -5, xmax = 5), colour = "black", fill = NA, size = 0.7)
plot(ggbasqueinset)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Create and save final map

``` r
ggbasquefull <- ggdraw(ggbasque) +
  draw_plot(ggbasqueinset, x = 0.41, y = 0.69, width = 0.3, height = 0.3)
plot(ggbasquefull)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggsave("Basque map.pdf", plot = ggbasquefull, dpi = 600)
```

    ## Saving 7 x 5 in image

\#Burushaski

Read in data

``` r
wakhi <- fortify(readOGR("../Data/Shapefiles/Fig. 3 Burushaski/wakhi.shp"))
burushaski <- fortify(readOGR("../Data/Shapefiles/Fig. 3 Burushaski/burushaski.shp"))
shina <- fortify(readOGR("../Data/Shapefiles/Fig. 3 Burushaski/shina.shp"))
```

    ## Warning in OGRSpatialRef(dsn, layer, morphFromESRI = morphFromESRI, dumpSRS
    ## = dumpSRS, : Discarded ellps Unknown in CRS definition: +proj=merc +lat_ts=0
    ## +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs

    ## Warning in OGRSpatialRef(dsn, layer, morphFromESRI = morphFromESRI, dumpSRS =
    ## dumpSRS, : Discarded datum D_unknown in CRS definition: +proj=merc +lat_ts=0
    ## +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs

    ## Warning in showSRID(wkt2, "PROJ"): Discarded ellps Unknown in CRS definition:
    ## +proj=merc +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs
    ## +type=crs

    ## Warning in showSRID(wkt2, "PROJ"): Discarded datum D_unknown in CRS definition

``` r
khowar <- fortify(readOGR("../Data/Shapefiles/Fig. 3 Burushaski/khowar.shp"))
```

    ## Warning in OGRSpatialRef(dsn, layer, morphFromESRI = morphFromESRI, dumpSRS
    ## = dumpSRS, : Discarded ellps Unknown in CRS definition: +proj=merc +lat_ts=0
    ## +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs

    ## Warning in OGRSpatialRef(dsn, layer, morphFromESRI = morphFromESRI, dumpSRS =
    ## dumpSRS, : Discarded datum D_unknown in CRS definition: +proj=merc +lat_ts=0
    ## +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs

    ## Warning in showSRID(wkt2, "PROJ"): Discarded ellps Unknown in CRS definition:
    ## +proj=merc +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs
    ## +type=crs

    ## Warning in showSRID(wkt2, "PROJ"): Discarded datum D_unknown in CRS definition

Create main map

``` r
ggburushaski <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black",  size = 0.15) +
  coord_quickmap(ylim = c(34, 37), xlim = c(70, 76)) +
  geom_polygon(data = wakhi, aes(x = long, y = lat, group = id, fill = "Wakhi"), color = "black", size = 0.15) +
  geom_polygon(data = burushaski, aes(x = long, y = lat, fill = "Burushaski"), color = "black", size = 0.15) +
  geom_polygon(data = shina, aes(x = long, y = lat, group = id, fill = "Shina"), color = "black", size = 0.15) +
  geom_polygon(data = khowar, aes(x = long, y = lat, group = id, fill = "Khowar"), color = "black", size = 0.15) +
  scale_fill_manual(name = "Burushaski and neighboring languages", values = c("Burushaski" = "black", "Wakhi" = "gray60", "Khowar" = "darkgray", "Shina" = "bisque4")) +
  labs(x = "Longitude", y = "Latitude")
plot(ggburushaski)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Create inset map

``` r
ggburushaskiinset <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black", size = 0.15) +
  coord_quickmap(ylim = c(76, 5), xlim = c(-16, 180)) +
  geom_rect(aes(ymin = 34, ymax = 37, xmin = 70, xmax = 76), colour = "black", fill = NA, size = 1.1)
plot(ggburushaskiinset)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Create and save final map

``` r
ggburushaskifull <- ggdraw(ggburushaski) +
  draw_plot(ggburushaskiinset, x = 0.055, y = 0.2, width = 0.27, height = 0.27)
plot(ggburushaskifull)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("Burushaski map.pdf", plot = ggburushaskifull, dpi = 600)
```

    ## Saving 7 x 5 in image

As can be seen, the output map features an artifact that is generated
when plotting the shapefile for the Shina language. I have not been able
to fix this issue, and have therefore postprocessed the map manually.

# Pomoan

Read in data

``` r
kashaya <- fortify(readOGR("../Data/Shapefiles/Fig. 4 Pomoan/Kashaya.shp"))
otherpomo <- fortify(readOGR("../Data/Shapefiles/Fig. 4 Pomoan/Other Pomo.shp"))
```

Create main map

``` r
ggpomoan <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black", size = 0.15) +
  coord_quickmap(ylim = c(37.5, 39.75), xlim = c(-124.5, -122)) +
  geom_polygon(data = kashaya, aes(x = long, y = lat, fill = "Kashaya"), color = "black", size = 0.15) +
  geom_polygon(data = otherpomo, aes(x = long, y = lat, fill = "Other Pomoan languages", group = id), color = "black", size = 0.15) +
  scale_fill_manual(name = "Pomoan languages", values = c("Kashaya" = "black", "Other Pomoan languages" = "gray80")) +
  labs(x = "Longitude", y = "Latitude")
plot(ggpomoan)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Create inset map

``` r
ggpomoaninset <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black", size = 0.15) +
  coord_quickmap(ylim = c(22, 50), xlim = c(-129, -64)) +
  geom_rect(aes(ymin = 37.5, ymax = 40, xmin = -124.5, xmax = -122), colour = "black", fill = NA, size = 1.2)
plot(ggpomoaninset)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Create and save final map

``` r
ggpomoanfull <- ggdraw(ggpomoan) +
  draw_plot(ggpomoaninset, x = 0.081, y = 0.071, width = 0.24, height = 0.24)
plot(ggpomoanfull)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
ggsave("Pomo map.pdf", plot = ggpomoanfull, dpi = 600)
```

    ## Saving 7 x 5 in image

# Celtic

Read in data

``` r
celticmaximal <- fortify(readOGR("../Data/Shapefiles/Fig. 5 Celtic/Celtic maximal.shp"))
gaelic <- fortify(readOGR("../Data/Shapefiles/Fig. 5 Celtic/Gaelic.shp"))
breton <- fortify(readOGR("../Data/Shapefiles/Fig. 5 Celtic/Breton.shp"))
irish <- fortify(readOGR("../Data/Shapefiles/Fig. 5 Celtic/Irish.shp"))
welsh <- fortify(readOGR("../Data/Shapefiles/Fig. 5 Celtic/Welsh.shp"))
```

Create and save map

``` r
ggceltic <- ggplot() +
  theme() +
  geom_map(data = map.world, map = map.world, aes(map_id = region), fill = "white", colour = "black", size = 0.15) +
  coord_quickmap(ylim = c(35, 62), xlim = c(-15, 35)) +
  geom_polygon(data = celticmaximal, aes(x = long, y = lat, group = id, fill = "Maximal extension"), color = "black", size = 0.15) +
  geom_polygon(data = gaelic, aes(x = long, y = lat, group = id, fill = "Present-day distribution"), color = "black", size = 0.15) +
  geom_polygon(data = breton, aes(x = long, y = lat, fill = "Present-day distribution"), color = "black", size = 0.15) +
  geom_polygon(data = irish, aes(x = long, y = lat, group = id, fill = "Present-day distribution"), color = "black", size = 0.15) +
  geom_polygon(data = welsh, aes(x = long, y = lat, group = id, fill = "Present-day distribution"), color = "black", size = 0.15) +
  scale_fill_manual(name = "Extension of Celtic languages", values = c("Maximal extension" = "gray", "Present-day distribution" = "black")) +
  labs(x = "Longitude", y = "Latitude")
plot(ggceltic)
```

![](Creating_maps_for_case_studies_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggsave("Celtic map.pdf", plot = ggceltic, dpi = 600)
```

    ## Saving 7 x 5 in image
