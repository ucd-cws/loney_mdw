# make map

library(sf)
library(tidyverse)
library(mapview)
library(mapedit)
library(leafpm)
library(FedData)
#library(tmap)
#library(tmaptools)

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

# PREVIEW -----------------------------------------------------------------


# map
m1 <- mapview(obs, col.regions="orange") + 
  mapview(sites_start, col.regions="green", alpha=0.5, cex=4)+
  mapview(sites_end, col.regions="maroon", alpha=0.5, cex=4)


# Mapedit -----------------------------------------------------------------

loneynorth <- mapedit::editMap(m1)
# extract the feature
loney_north <- loneynorth$finished
# check: mapview(loney_north)

# now try with the leafpm
mapedit::editFeatures(loney_north, editor="leafpm")
mapview(.Last.value)
mapview(loney_north)
