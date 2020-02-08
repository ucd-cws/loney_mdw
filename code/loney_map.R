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



# DATA --------------------------------------------------------------------

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

# quick preview
mapview(spp15, col.regions="orange")+
  mapview(spp16, col.regions="darkgreen", cex=3) +
  mapview(spp_all, col.regions="violet", cex=2) +
  mapview(mdw_bnd, col.regions="transparent")


# Clean SPP Observations --------------------------------------------------

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

mapview(spp_all_df,zcol="SPP_ID")

# GGMAP -------------------------------------------------------------------

# ggmap
library(ggmap)
library(ggspatial)
# run once: register_google(key = "[your key]", write = TRUE

bbxy <- c(as.numeric(st_bbox(spp_all_df)))

# get imagery (try: maptype = "terrain" or source = "stamen", zoom=15,
#                     maptype = "terrain")
mad_map3 <- get_map(c( left = bbxy[1], bottom = bbxy[2],
                       right = bbxy[3], top = bbxy[4]), 
                    source = "google", zoom=15, 
                    maptype = "satellite")

# map
ggmap(mad_map3) +
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


# TMAP WITH OSM -----------------------------------------------------------
# library(tmap)
# library(tmaptools)
# st_crs(surv16)
# osm_surv <- read_osm(surv16, ext=1.1)

# yarg: see here:
# https://community.rstudio.com/t/errors-loading-rjava-onload-failed-in-loadnamespace-for-rjava-details/14896/23

# MAPVIEW -----------------------------------------------------------------

# map
# m1 <- mapview(obs, col.regions="orange") + 
#   mapview(sites_start, col.regions="green", alpha=0.5, cex=4)+
#   mapview(sites_end, col.regions="maroon", alpha=0.5, cex=4)

# MAPVIEW::MAPEDIT -----------------------------------------------------------------

# loneynorth <- mapedit::editMap(m1)
# # extract the feature
# loney_north <- loneynorth$finished
# # check: mapview(loney_north)
# 
# # now try with the leafpm
# mapedit::editFeatures(loney_north, editor="leafpm")
# mapview(.Last.value)
# mapview(loney_north)


