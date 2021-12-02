## ===========================
##
## Script name:
##
## Purpose of script: 
##
## Author: Tom Langbehn
## Email:  tom.langbehn@uib.no
##
## Date created:
## Date updated: 2021-10-25
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## !> load frequently used packages:

library(tidyverse)
library(patchwork)
library(here)
library(ggthemes)
library(scales)
library(janitor)
library(lubridate)
library(zoo)
library(viridis)

"%ni%" <- Negate("%in%")

## ===========================

here()

## !> find relevant files from the North Sea part
catch_files <- list.files(here("rawdata"), pattern = c("_NS_catch"), full.names = T)
ctd_files <- list.files(here("rawdata"), pattern = c("_NS_ctd"), full.names = T)
track_files <- list.files(here("rawdata"), pattern = c("_NS_cruise_track"), full.names = T)

## ==== READ DATA ====

## ---- CATCH MEASUREMENTS ----
## !> create a custom read_csv function, that unifies the column types across years
read_custom <- function(file) {
  read_csv(file) %>%
    mutate(
      catchcategory = as.factor(catchcategory),
      individualcomment = as.character(individualcomment),
      platform = as.factor(platform),
      across(c("sex", "maturationstage", "startyear"), as_factor)
    ) -> output_tmp
  
  return(output_tmp)
}
## !> read from .csv files
catch_raw <- catch_files %>% map_df(~ read_custom(.))

# catch_raw %>% names() %>%  sort() %>% .[str_detect(., "dttm")]

trawl_sta <- catch_raw %>%
  dplyr::select(startyear, stationstart_dttm, longitudestart, latitudestart, station, gear_name, bottomdepthstart, bottomdepthstop) %>%
  rename(year = startyear, datetime = stationstart_dttm, lon = longitudestart, lat = latitudestart) %>% 
  filter(gear_name == "Bottom trawl") %>%
  mutate(location = case_when(station %in% c("46", "47", "49", "344", "345", "346", "347", "351") ~ "norwegian_trench",
                              station %in% c("38", "40", "42", "44", "355", "359", "360") ~ "faroe_trench",
                              station %in% c("35", "36", "37","361") ~ "north_sea_plateu",
                              station %in% c("226", "225", "223", "222", "221", "224", "220", "219", "228", "229", "230") ~ "maloy_plateau",
                              station %in% c("232", "234", "235", "236") ~ "westfjord"),
         bottommean = (bottomdepthstart + bottomdepthstop)/2) %>% 
  distinct()

## ---- MAP ----
source(here("src", "northsea_basemap.R"))

## !> lets add things
basemap(add_raster = T, add_contours = T, add_eez = F, add_poi = F, survextent) +

## !> track
# geom_path(data =  track, aes(x=lon, y=lat, group=year), size = 0.2)+

# # !> add trawl stations
geom_point(data = trawl_sta, aes(x = lon, y = lat, fill = location), shape = 21, size = 5) +
  theme_bw() +
  facet_wrap(vars(year)) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 20, face = "bold")) +
  theme(legend.title = element_text(size = 20, face = "bold")) +
  theme(axis.text.x = element_text(size = 18)) + 
  theme(axis.title.x = element_text(size = 20, face = "bold")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 20, face = "bold")) +
  theme(strip.text = element_text(size = 20, face = "bold")) +
  theme(legend.key.width = unit(4,"cm")) +
  scale_fill_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  guides(fill = FALSE) +
  
# 
## !> add labels
geom_text_repel(data =  trawl_sta, aes(x=lon, y=lat, label = station),
                min.segment.length = 0, size = 5, col= "black",  max.overlaps = 100)+
  
theme()

## !> save last plot
ggsave(
  here("figs", "map_study_area.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

## !> this trick removea white space around plot
knitr::plot_crop(here("figs", "map_study_area.png"))
