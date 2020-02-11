# make map

# Libraries ---------------------------------------------------------------

# tidy stuff
library(tidyverse)

# mapping packages
library(sf)
library(mapview)
library(mapedit)
library(leafpm)
#library(tmap)
#library(tmaptools)

# data packages
#library(FedData)
#library(tidycensus)
#library(tigris)
#library(rnaturalearth)
#library(osmdata) 
#library(rnoaa) 
#library(getlandsat)
#library(hddtools) # hydrologic data

# see here: https://www.r-spatial.org/r/2018/03/23/stars3.html
# library(RGISTools)
# http://zevross.com/blog/2016/03/15/using-the-new-r-package-feddata-to-access-federal-open-datasets-including-interactive-graphics/

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList) # set defaults


# GET DATA --------------------------------------------------------------------

# amphib obs
obs <- read_csv("data/loney_survey_observations.csv")
obs <- st_as_sf(obs, coords = c("utm_e_m", "utm_n_m"), remove = FALSE, crs=32610) # 32610 (z10)

# site obs
sites_csv <- read_csv("data/loney_survey_utms.csv")
sites_start <- st_as_sf(sites_csv, coords = c("start_utm_e", "start_utm_n"), remove=FALSE, crs=32610)
sites_end <- st_as_sf(sites_csv, coords = c("end_utm_e", "end_utm_n"), remove=FALSE, crs=32610)

# all spp
spp_all <- obs %>% st_transform(4326) %>% filter(!is.na(Species_code))

# GET SHAPEFILES ----------------------------------------------------------

# get survey tracks
#surv15 <- read_sf("data/spatial/2015-loney-surveys.shp")
spp15 <- read_sf("data/spatial/2015-Loney_SPP.shp")
spp16 <- read_sf("data/spatial/2016-07-26-loney.shp") # all spp
mdw_bnd <- read_sf("data/spatial/LONEY_UCDSNM.shp") %>% st_transform(4326)

# CLEAN DATA: SPP Observations --------------------------------------------------

spp15_clean <- spp15 %>% select(SPP_ID, Stage, geometry) %>% 
  mutate(lon=sf::st_coordinates(.)[,1],
         lat=sf::st_coordinates(.)[,2],
         year=2015) %>% 
  st_drop_geometry()

spp16_clean <- spp16 %>% select(Species, Stage, geometry) %>% 
  rename(SPP_ID=Species) %>% 
  mutate(lon=sf::st_coordinates(.)[,1],
         lat=sf::st_coordinates(.)[,2],
         year=2016) %>% 
  st_drop_geometry()

spp_all_clean <- spp_all %>% select(Species_code, Stage, geometry) %>% 
  rename(SPP_ID=Species_code) %>% 
  mutate(lon=sf::st_coordinates(.)[,1],
         lat=sf::st_coordinates(.)[,2],
         year=2019) %>% 
  st_drop_geometry()

# bind and filter distinct
spp_all_df <- bind_rows(spp_all_clean, spp15_clean, spp16_clean) %>% 
  mutate(Stage = case_when(
    grepl("Larvae|Tad|L", Stage) ~ "Larvae",
    grepl("^A$", Stage) ~ "Adult",
    grepl("^Y", Stage) ~ "YOY",
    grepl("^Egg", Stage) ~ "Egg",
    TRUE ~ Stage
  )) %>% 
  distinct(.keep_all = TRUE) %>% 
  st_as_sf(., coords = c("lon", "lat"), remove=FALSE, crs=4326)

# mapview(spp_all_df,zcol="SPP_ID")

# MAPS: INTERACTIVE -------------------------------------------------

# * Mapview --------------------------------------------------------

# quick preview
m1 <- mapview(spp15, col.regions="orange")+
  mapview(spp16, col.regions="darkgreen", cex=3) +
  mapview(spp_all, col.regions="violet", cex=2) +
  mapview(mdw_bnd, col.regions="transparent")

m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# * MapDeck -----------------------------------------------------------------

library(mapdeck)
set_token(Sys.getenv("MAPBOX_TOKEN"))
 
# make a dark map
mapdeck( style=mapdeck_style("satellite")) %>%
  add_polygon(data = mdw_bnd, stroke_colour = "#9BCD9B",
              fill_opacity = 10, tooltip="ID_CON2MF", auto_highlight = FALSE) %>%
  add_sf(data = spp_all, fill_colour="Species_code",legend = TRUE,
         tooltip="Stage", layer_id="Species", radius=10, auto_highlight=TRUE)


# MAPS: STATIC PRINTABLE GGMAP -----------------------------------------------------

# ggmap
library(ggmap)
library(ggspatial)
# run once: register_google(key = "[your key]", write = TRUE

bbxy <- c(as.numeric(st_bbox(spp_all_df)))

# get imagery (try: maptype = "terrain" or source = "stamen", zoom=15,
#                     maptype = "terrain")
map_sat <- get_map(c( left = bbxy[1], bottom = bbxy[2],
                       right = bbxy[3], top = bbxy[4]), 
                    source = "google", zoom=15, 
                    maptype = "satellite")

# map
ggmap(map_sat) +
  geom_sf(data=mdw_bnd, inherit.aes = FALSE, 
          fill=NA, color="forestgreen",
          alpha=.3, size=1)+
  ggspatial::geom_spatial_text_repel(data=spp_all_df %>% filter(year==2019, SPP_ID!="PSRE"), aes(label=SPP_ID), force=2, 
                                     box.padding=unit(0.5, "cm"),
                                     family="Roboto Condensed", 
                                     color="white")+
  geom_sf(data=spp_all_df, inherit.aes = FALSE, 
          aes(fill=SPP_ID), 
          pch=21, alpha=.8, color="white", size=2.5)+
  labs(subtitle = "Loney Meadow Amphibians Monitoring", x="",y="") +
  theme_gray(base_family = "Roboto Condensed") +
  coord_sf(label_axes = c("---N")) +
  scale_y_continuous(breaks=c(39.418, 39.422, 39.426, 39.430), expand = expansion(0,0)) +
  scale_fill_brewer("Species", type="qual", palette = "Set1")+
  ggspatial::annotation_scale(location="br",text_col="gray70",
                              text_family = "Roboto Condensed") +
  ggspatial::annotation_north_arrow(location="bl",
                                    height = unit(1.2, "cm"), 
                                    width = unit(0.8, "cm"), 
                                    style = north_arrow_fancy_orienteering(
                                      line_width = 1,
                                      line_col = "gray40",
                                      fill = c("white", "black"),
                                      text_col = "gray70",
                                      text_family = "Roboto Condensed",
                                      text_size = 10)
                                    )

ggsave(filename = "figs/final_map_of_results_2019.png", width = 8, height = 7)
ggsave(filename = "figs/final_map_of_results_2019.pdf", device=cairo_pdf,width = 8, height = 7)



# MAPS: MAPEDIT ---------------------------------------------------------

# requires {mapedit}

# 1. first pick/make a map with layers/base to edit
# 2. Create or edit the map.
# 3. Make sure to click DONE!


#  * Make AMMASI Pond -----------------------------------------------------

# ADD POLYGON: the small unnamed pond
loneypond <- mapedit::editMap(m1)

# EXTRACT the feature
# whatever has been done lives in .$finished
names(loneypond)
loney_pond <- loneypond$finished

# CHECK: see if it worked
mapview(loney_pond) # yay!
 
# USING {leafpm} for snapping/advanced features
loney_pond_adj <- mapedit::editFeatures(loney_pond, editor="leafpm")
mapview(.Last.value) # should be edited version

# preview both layers
mapview(loney_pond_adj, color="red4", lwd=3,
        alpha.regions=0.3, col.regions="red2") + 
  mapview(loney_pond, color="darkblue", lwd=1.4, 
        alpha.regions=0.2, col.regions="blue2")

# save the sf object!
class(loney_pond_adj)
loney_ammasi_pond <- loney_pond_adj
write_rds(loney_ammasi_pond, "data/loney_ammasi_pond_sf.rds")


#  * Make Stock Pond ------------------------------------------------------


# make a polygon for the stock pond
loney_stock_pond <- mapedit::editMap(m1, editor="leafpm")
names(loney_stock_pond)
loney_stock_pond <- loney_stock_pond$finished

class(loney_stock_pond)
write_rds(loney_stock_pond, "data/loney_stock_pond_sf.rds")


#  * Make Upper Loney Pond ------------------------------------------------------

# make a polygon for the stock pond
loney_upper_pond <- mapedit::editMap(m1)
names(loney_upper_pond)
loney_upper_pond <- loney_upper_pond$finished

class(loney_upper_pond)
write_rds(loney_upper_pond, "data/loney_upper_pond_sf.rds")


#  * Make BullPen Lake ------------------------------------------------------

# make a polygon for the stock pond
loney_bullpen <- mapedit::editMap(m1, editor="leafpm")
names(loney_bullpen)
loney_bullpen <- loney_bullpen$finished

class(loney_bullpen)
write_rds(loney_bullpen, "data/loney_bullpen_sf.rds")


# Make Final Mapview --------------------------------------------------------

# quick preview
m2 <- mapview(spp15, col.regions="orange", cex=3.5, layer.name="2015")+
  mapview(spp16, col.regions="darkgreen", cex=3.5, layer.name="2016") +
  mapview(spp_all %>% filter(Year==2019), col.regions="violet", cex=4, layer.name="2019") +
  mapview(mdw_bnd, color="darkgreen", col.regions="transparent", alpha.regions=0.1, legend=FALSE) +
  mapview(loney_bullpen, color="darkblue", col.regions="transparent", alpha.regions=0.1, legend=FALSE) +
  mapview(loney_ammasi_pond, color="darkblue", alpha.regions=0.1,  legend=FALSE) +
  mapview(loney_stock_pond, color="darkblue", alpha.regions=0.1,  legend=FALSE) +
  mapview(loney_upper_pond, color="darkblue", alpha.regions=0.1,  legend=FALSE)

m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# Combine Polygons into One -----------------------------------------------

loney_ammasi_pond <- loney_ammasi_pond %>% 
  rename(featID = X_leaflet_id) %>%
  mutate(site_name = "Unnamed Pond - NE",
         feat_type = "pond") %>% 
  select(featID, feat_type, site_name, geometry)

loney_bullpen <- loney_bullpen %>% 
  rename(featID = X_leaflet_id) %>%
  mutate(site_name = "Bullpen Lake",
         feat_type = "lake") %>% 
  select(featID, feat_type, site_name, geometry)

loney_stock_pond <- loney_stock_pond %>% 
  rename(featID = X_leaflet_id) %>%
  mutate(site_name = "Stockpond",
         feat_type = "stockpond") %>% 
  select(featID, feat_type, site_name, geometry)

loney_upper_pond <- loney_upper_pond %>% 
  rename(featID = X_leaflet_id) %>%
  mutate(site_name = "Unnamed Pond - E",
         feat_type = "pond") %>% 
  select(featID, feat_type, site_name, geometry)

loney_mdw <- mdw_bnd[1,] %>% 
  rename(featID = ID) %>%
  mutate(site_name = "Loney Meadow",
         feat_type = "meadow") %>% 
  select(featID, feat_type, site_name, geometry)

# combine this list
loney_sf_list <- list(loney_mdw, loney_upper_pond, loney_stock_pond, loney_bullpen, loney_ammasi_pond)

# test two ways
# microbenchmark::microbenchmark(
#   loney_polys1 <- do.call(rbind, loney_sf_list),
#   loney_polys2 <- sf::st_as_sf(data.table::rbindlist(loney_sf_list))
# )

# clear winner
loney_polys <- sf::st_as_sf(data.table::rbindlist(loney_sf_list))

# Save {sf} Objects ---------------------------------------------------

# save all for mapping:
save(loney_ammasi_pond, loney_stock_pond, loney_upper_pond,
     loney_bullpen, loney_mdw, spp_all_df, 
     sites_start, sites_end, loney_polys,
     file = "data/loney_all_sf_objs.rda")


# Make a Site Map ---------------------------------------------------------

load("data/loney_all_sf_objs.rda")

# fix and add centroid for labeling
loney_polys <- loney_polys %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

# ggmap
library(ggmap)
library(ggspatial)
# run once: register_google(key = "[your key]", write = TRUE

bbxy <- c(as.numeric(st_bbox(spp_all_df)))

# get imagery (try: maptype = "terrain" or source = "stamen", zoom=15,
#                     maptype = "terrain")
map_sat <- get_map(c( left = bbxy[1], bottom = bbxy[2],
                       right = bbxy[3], top = bbxy[4]), 
                    source = "google", zoom=15, 
                    maptype = "satellite")

map_trrn <- get_map(c( left = bbxy[1], bottom = bbxy[2],
                      right = bbxy[3], top = bbxy[4]), 
                   source = "google", zoom=15, maptype = "terrain")

# tst
# ggmap(map_trrn) +
#   geom_sf(data=loney_polys, aes(color=feat_type),
#           inherit.aes = FALSE, fill=NA, alpha=.3, size=1) +
#   geom_sf(data=spp_all_df, inherit.aes = FALSE, 
#           aes(fill=SPP_ID), 
#           pch=21, alpha=.8, color="white", size=2.5) +
#   ylim(c(39.418,39.428))

#  * Without Spp ----------------------------------------------------------


# 2019 survey map
ggmap(map_trrn) +
  geom_sf(data=loney_polys, 
          aes(color=forcats::fct_relevel(feat_type, "meadow","lake","pond","stockpond"), 
              fill=forcats::fct_relevel(feat_type, "meadow","lake","pond","stockpond")),
          inherit.aes = FALSE, alpha=.5, size=1) +
  scale_color_viridis_d("Feature Type", labels=c("Lake", "Meadow", "Pond", "Stock Pond"))+
  scale_fill_viridis_d("Feature Type", labels=c("Lake", "Meadow", "Pond", "Stock Pond"))+
  #scale_color_manual("Feature Type", values = c("lake"="darkblue", "meadow"="springgreen3", "stockpond"="mediumpurple4", "pond"="steelblue4"), labels=c("Lake", "Meadow", "Pond", "Stock Pond"))+
  #scale_fill_manual("Feature Type", values = c("lake"="darkblue", "meadow"="springgreen3", "stockpond"="mediumpurple4", "pond"="steelblue4"), labels=c("Lake", "Meadow", "Pond", "Stock Pond"))+
  ylim(c(39.418,39.428))+
  ggspatial::geom_spatial_label_repel(data=loney_polys,
                                     aes(label=site_name), 
                                     #force=1.2,
                                     box.padding=unit(0.3, "cm"),
                                     family="Roboto Condensed",
                                     color="gray40")+
  labs(title = "Loney Meadow Amphibian Monitoring Sites: 2016-2019", x="",y="") +
  theme_gray(base_family = "Roboto Condensed") +
  coord_sf(label_axes = c("---N")) +
  theme(legend.position = c(.9,.8),
        legend.direction = "vertical",
        legend.background = element_rect(color="black"),
        #legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.spacing.x = unit(.07, 'cm'),
        legend.text = element_text(color="black"))+
  ggspatial::annotation_scale(location="br",text_col="black",
                              text_family = "Roboto Condensed") +
  ggspatial::annotation_north_arrow(location="bl",
                                    height = unit(1.2, "cm"), 
                                    width = unit(0.8, "cm"), 
                                    style = north_arrow_fancy_orienteering(
                                      line_width = 1,
                                      line_col = "black",
                                      fill = c("white", "black"),
                                      text_col = "black",
                                      text_family = "Roboto Condensed",
                                      text_size = 10)
  )

# site map
ggsave(filename = "figs/final_map_sites.png", width = 7, height = 5.5, units = "in", dpi=300, scale = 1.1)
ggsave(filename = "figs/final_map_sites.pdf", device=cairo_pdf,width = 7, height = 5.5, units = "in", scale=1.1)


# * With 2019 Spp ----------------------------------------------------------------

# make a slightly jittered version to see everything:
#spp_all_jitter_a <- st_transform(spp_all_df, 32610) %>% st_jitter(amount = 40)
spp_all_jitter_f <- st_jitter(spp_all_df, factor = 0.004)
#mapview(spp_all_jitter_a) + mapview(spp_all_jitter_f, col.regions="orange") + mapview(spp_all_df, col.regions="red", cex=2)

# 2019 survey map
ggmap(map_sat) +
  geom_sf(data=loney_polys, 
          aes(color=forcats::fct_relevel(feat_type, "meadow","lake","pond","stockpond")), 
          inherit.aes = FALSE, alpha=.5, size=1.2, show.legend = FALSE, fill=NA) +
  scale_color_viridis_d("Feature Type", labels=c("Lake", "Meadow", "Pond", "Stock Pond"))+

  # label 2019 data 
  ggspatial::geom_spatial_text_repel(data=spp_all_jitter_f %>%
                                       filter(year==2019, SPP_ID!="THCO", SPP_ID!="THEL"),
                                     aes(label=SPP_ID), force=1.5,
                                     box.padding=unit(0.3, "cm"),
                                     family="Roboto Condensed",
                                     color="white")+
  # 2019 data only
  geom_sf(data=spp_all_jitter_f %>% filter(year==2019, SPP_ID!="THCO", SPP_ID!="THEL"), inherit.aes = FALSE, 
          aes(fill=SPP_ID), 
          pch=21, alpha=.8, color="white", size=4) +
  # labels
  labs(title = "Loney Meadow Amphibian Monitoring: 2019", x="",y="")+
       #subtitle = "Southern Long-toed Salamander Observations") +
  theme_gray(base_family = "Roboto Condensed") +
  #coord_sf(label_axes = c("---N")) +
  theme(legend.position = c(.85,.75),
        legend.direction = "vertical",
        legend.key = element_blank(),
        legend.spacing.x = unit(.07, 'cm'),
        legend.background = element_rect(color="black"),
        #legend.background = element_blank(),
        legend.text = element_text(color="black"))+
  ylim(c(39.418,39.428))+
  scale_fill_brewer("Species", type="qual", palette = "Set1",
                    guide=guide_legend(
                      direction="vertical",
                      title.position="top",
                      title.theme = element_text(
                        color = "black",
                        family = "Roboto Condensed", vjust = -0.4,size = 11,
                        ))) +
  ggspatial::annotation_scale(location="br",text_col="white",
                              text_family = "Roboto Condensed") +
  ggspatial::annotation_north_arrow(location="bl",
                                    height = unit(1.2, "cm"), 
                                    width = unit(0.8, "cm"), 
                                    style = north_arrow_fancy_orienteering(
                                      line_width = 1,
                                      text_col = "white",
                                      line_col = "gray40",
                                      #fill = c("white", "black"),
                                      text_family = "Roboto Condensed",
                                      text_size = 10)
  )


# SAVE

ggsave(filename = "figs/final_map_2019_obs.png", width = 7, height = 5.5, units = "in", dpi=300, scale = 1.1)
ggsave(filename = "figs/final_map_2019_obs.pdf", device=cairo_pdf,width = 7, height = 5.5, units = "in", scale=1.1)


# Calculate Distances between Points --------------------------------------

# data
load("data/loney_all_sf_objs.rda")

st_crs(loney_polys)
library(units)

# fix and add centroid for labeling
loney_polys <- loney_polys %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

# make centroid points for measurement, otherwise measures from edge of polygon
poly_cnts <- loney_polys %>% st_drop_geometry() %>% select(featID, lon, lat) %>%
  sf::st_as_sf(., coords=c("lon","lat"), remove=FALSE, crs=4326) %>% 
  mutate(dist_to_loney=st_distance(geometry, .$geometry[1], by_element=TRUE),
         dist_to_loney=set_units(dist_to_loney, 'miles'))

# double check

m2dist <- mapview(loney_polys, col.regions="transparent") +
  mapview(poly_cnts, col.regions="yellow", cex=5)

m2dist@map %>% leaflet::addMeasure(primaryLengthUnit = "miles")
