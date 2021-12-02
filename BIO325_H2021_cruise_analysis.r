## ===========================
##
## Script name: BIO325 Life history group 2021
##
## Purpose of script: Life history measurements of Argentina silus and Argentina sphyrinea
##
## Author: Hannah 
## Email: hannah.rokke@uib.no 
##
## Date Created: 2021-10-18
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

#!> load key packages:
# install.packages("devtools")
# 
# devtools::install_github("trobinj/trtools")

library(tidyverse)
library(patchwork)
library(here)
library(ggthemes)
library(scales)
library(janitor)
library(RColorBrewer)
library(maps)
library(marmap)
library(GISTools)
library(cowplot)
library(raster)
library(FSA)
library(FSAdata)
library(nlstools)
library(ggpmisc)
library(colorspace)
library(ggpubr)
library(magrittr)
library(nlstools)


"%ni%" = Negate("%in%")

## ===========================

here()

##!> find relevant files
ns_data <- list.files(here("rawdata"), pattern = c("_NS_"), full.names = T)
##!> select just catch data files
catch_data <- ns_data[str_detect(ns_data, "_catch_")]
# eye_data <- ns_data[str_detect(ns_data, "_eyesize_")]
individual_data <- ns_data[str_detect(ns_data, "_ind_")]

##!> create a custom read_csv function, that mutates the column "catchcategory" to factor
custom_read <-  function(file){
  
  ##!> just debugging
  # file <- catch_data[2]
  
  read.csv(file) %>% 
    as_tibble() %>% 
    mutate(catchcategory = as.factor(catchcategory),
           individualcomment = as.character(individualcomment),
           platform = as.factor(platform)) -> output_tmp
  
  return(output_tmp)
  
}

##!> READING IN DATA ####

##!> read in catch data
catch_all_yrs <- catch_data %>% 
  map_df(~custom_read(.))

catch_all_yrs <- catch_all_yrs %>% 
  mutate(fish_id = serialnumber*1000+specimenid)

##!> read in individual measurements (age, sex, eye, gonad etc)

individual_catch <-  individual_data %>% 
  map_df(~read_delim(., delim =",")) 

all_data <- left_join(catch_all_yrs, individual_catch, by = c("scientificname", "fish_id", "startyear", "serialnumber", "station"))

sorted_data <- all_data %>% 
  dplyr::select("startyear", "stationstartdate", "stationstarttime", "station", "serialnumber", "latitudestart", "longitudestart", "latitudeend", "longitudeend", "bottomdepthstart", "bottomdepthstop", "scientificname", "fish_id", "length", "length_m", "individualweight", "weight_kg", "age_yrs", "gonadweight_kg", "maturationstage.x", "eye_dia_mm", "pup_dia_mm", "sex", "gear_name", "head_length_mm") %>% 
  mutate(length_cm = length * 100,
         weight_g = individualweight * 1000,
         depth = (bottomdepthstart + bottomdepthstop) / 2,
         gonadweight_g = gonadweight_kg * 1000,
         location = case_when(station %in% c("46", "47", "49", "344", "345", "346", "347", "349", "351") ~ "Norwegian Trench",
                              station %in% c("38", "40", "42", "44", "355", "359", "360") ~ "Faroe Trench",
                              station %in% c("35", "36", "37","361") ~ "North Sea Plateu",
                              station %in% c("226", "225", "223", "222", "221", "224", "220", "219", "228", "229", "230") ~ "Måløy Plateau",
                              station %in% c("232", "233", "234", "235", "236") ~ "Westfjord"),
         eye_length_ratio = eye_dia_mm / length_cm,
         depth_range = case_when(depth > 600 ~ "Over 600",
                                 depth >= 500 ~ "501 - 600",
                                 depth >= 400 ~ "401 - 500",
                                 depth >= 300 ~ "301 - 400",
                                 depth >= 200 ~ "201 - 300",
                                 depth >= 100 ~ "101 - 200",
                                 depth <= 100 ~ "0 - 100"),
         age_range = case_when(age_yrs > 10 ~ "over 10",
                               age_yrs >= 6 ~ "6 - 10",
                               age_yrs >= 3 ~ "3 - 5",
                               age_yrs >= 1 ~ "1 - 2",
                               age_yrs == 0 ~ "0"),
         length_range = case_when(length_cm > 50 ~ "Over 50",
                                  length_cm >= 40 ~ "40 - 50",
                                  length_cm >= 30 ~ "30 - 40",
                                  length_cm >= 20 ~ "20 - 30",
                                  length_cm >= 10 ~ "10 - 20",
                                  length_cm > 0 ~ "0 - 10"),
         gsi = (gonadweight_g/weight_g) * 100,
         gsi_range = cut(gsi, breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))

sorted_data$sex[sorted_data$sex == "1"] <- "Female"
sorted_data$sex[sorted_data$sex == "2"] <- "Male"
sorted_data$scientificname <- as.factor(sorted_data$scientificname)

# all_data %>% names() %>% sort()

## STATION CATCH ####

##!> Getting an overview of the catch at every station 
station_catch <- sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena"), 
         gear_name == "Bottom trawl") %>% 
  group_by(startyear, stationstartdate, station, stationstarttime, latitudestart, longitudestart, location, scientificname) %>% 
  summarise(count = n()) %>% 
  mutate(location = case_when(station %in% c("46", "47", "49", "344", "345", "346", "347", "349", "351") ~ "Norwegian Trench",
                              station %in% c("38", "40", "42", "44", "355", "359", "360") ~ "Faroe Trench",
                              station %in% c("35", "36", "37", "361") ~ "North Sea Plateu",
                              station %in% c("226", "225", "223", "222", "221", "224", "220", "219", "228", "229", "230") ~ "Måløy Plateau",
                              station %in% c("232", "233", "234", "235", "236") ~ "Westfjord"))

##!> Station catches of Argentina silus and Argentina sphyraena
sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena"),
         gear_name == "Bottom trawl") %>%
  group_by(location, scientificname) %>% 
  summarize(count = n())

##!> Station location (long/lat)
stations <- sorted_data %>% 
  filter(gear_name == "Bottom trawl") %>% 
  group_by(startyear, station, latitudestart, longitudestart, latitudeend, longitudeend) %>% 
  distinct(station)
  
  
### GENERAL MEASUREMENTS ####

##!> Length/weight figure

sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(log(length_cm), log(weight_g), col = location)) + 
  geom_point()+
  labs(y ="log Weight (g)", x =" log Length (cm)")+
  facet_grid(location ~ scientificname) +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164", "black")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

ggsave(
  here("figs", "legnth_weight.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

### POPULATION COMPOSITION DATA ####

##!> Length count by location

# sorted_data %>% 
#   filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
#   ggplot(aes(x = location, fill = as.factor(length_cm))) +
#   geom_bar(position = "fill") +
#   theme_bw() +
#   facet_wrap(vars(scientificname)) +
#   xlab("Location") +
#   ylab("Length (cm)") +
#   scale_fill_discrete(name = "Length (cm)") +
#   theme(axis.title.x = element_text(size = 20)) +
#   theme(axis.title.y = element_text(size = 20)) +
#   theme(axis.text.x = element_text(size = 17)) +
#   theme(axis.text.y = element_text(size = 17)) +
#   theme(strip.text.x = element_text(size = 20)) +
#   theme(strip.text.y = element_text(size = 20)) +
#   guides(x = guide_axis(angle = 90))

##!> Length count at depth

sorted_data %>%  
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(x = depth_range, fill = as.factor(length_range))) +
  geom_bar(position = "fill") +
  theme_bw() +
  facet_grid(location ~ scientificname) +
  scale_fill_discrete_sequential(name = "Length (cm)", palette = "TealGrn") +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  labs(x="Depth (m)", y="Length composition (%)")+
  guides(x = guide_axis(angle = 90))

ggsave(
  here("figs", "length_depth.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

##!> Age count by location
# 
# sorted_data %>% 
#   filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>%
#   drop_na(age_range) %>% 
#   ggplot(aes(x = location, fill = as.factor(age_range))) +
#   geom_bar(position = "fill") +
#   theme_bw() +
#   facet_wrap(vars(scientificname)) +
#   xlab("Location") +
#   ylab("Age (Years)") +
#   scale_x_discrete(labels=c("faroe_trench" = "Faroe Trench", "maloy_planteau" = "Måløy Plateau",
#                             "norwegian_trench" = "Norwegian Trench", "westfjorden" = "Westfjorden")) +
#   scale_fill_discrete_sequential(name = "Age (Years)", palette = "TealGrn") +
#   theme(axis.title.x = element_text(size = 20)) +
#   theme(axis.title.y = element_text(size = 20)) +
#   theme(axis.text.x = element_text(size = 17)) +
#   theme(axis.text.y = element_text(size = 17)) +
#   theme(strip.text.x = element_text(size = 20)) +
#   theme(strip.text.y = element_text(size = 20)) +
#   guides(x = guide_axis(angle = 90)) + 
#   theme(legend.position = "none") 
# 
# ## !> save last plot
# ggsave(
#   here("figs", "age_comp.png"),
#   width = 200,
#   height = 150,
#   unit = "mm",
#   dpi = 600,
#   scale = 2
# )


##!> Age count by depth

sorted_data %>%  
  drop_na(age_range) %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(x = depth_range, fill = as.factor(age_range))) +
  geom_bar(position = "fill") +
  theme_bw() +
  facet_grid(location ~ scientificname) +
  scale_fill_discrete_sequential(name = "Age (Years)", palette = "TealGrn") +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 20, face = "italic")) +
  theme(strip.text.y = element_text(size = 20)) +
  labs(x="Depth (m)", y="Age composition (%)") +
  guides(x = guide_axis(angle = 90)) 

## !> save last plot
ggsave(
  here("figs", "age_depth.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

##!> Composition of sex by location

# sorted_data %>% 
#   drop_na(sex) %>% 
#   filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
#   ggplot(aes(x = location, fill = as.factor(sex))) +
#   geom_bar(position = "fill") +
#   theme_bw() +
#   facet_wrap(vars(scientificname)) +
#   theme(axis.title.x = element_text(size = 20)) +
#   theme(axis.title.y = element_text(size = 20)) +
#   theme(axis.text.x = element_text(size = 17)) +
#   theme(axis.text.y = element_text(size = 17)) +
#   theme(strip.text.x = element_text(size = 20)) +
#   theme(strip.text.y = element_text(size = 20)) +
#   labs(x="Location", y="Sex composition (%)")+
#   scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
#   theme(legend.position = "none") 

##!> Composition of sex by depth at locations

sorted_data %>% 
  drop_na(sex) %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(x = depth_range, fill = as.factor(sex))) +
  geom_bar(position = "fill") +
  theme_bw() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 20)) +
  labs(x="Depth (m)", y="Number of individuals (%)")+
  theme(legend.position = "none") +
  facet_grid(scientificname ~ location)

##!> Save last plot

ggsave(
  here("figs", "sex_depth.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

### EYE MEASUREMENTS ####


##!> logarithmic Eye size by logarithmic length

sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena"), startyear %in% c("2019", "2021")) %>% 
  ggplot(aes(log(length), log(eye_dia_mm), col = location)) + 
  geom_point()+
  labs(y="log Length (cm)",x=" log Eye diameter (mm)")+
  facet_grid(location ~ scientificname) +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

##!> Save last plot

ggsave(
  here("figs", "eye_length.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)


p1 <- sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena"), startyear %in% c("2019", "2021")) %>% 
  ggplot(aes(log(length), log(eye_dia_mm), col = location)) + 
  geom_point()+
  labs(y="log Length (cm)",x=" log Eye diameter (mm)")+
  facet_grid(location ~ scientificname) +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

##!> Eye size by depth

p2 <- sorted_data %>% 
  filter(scientificname == "Argentina silus") %>% 
  ggplot(aes(eye_dia_mm, depth, col = location)) + 
  geom_point()+
  labs(x="Eye Diameter (mm)",y="Bottom Depth (m)")+
  theme_bw() +
  scale_y_reverse() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

##!> Length by depth

p3 <- sorted_data %>% 
  filter(scientificname == "Argentina silus") %>% 
  ggplot(aes(length, depth, col = location)) + 
  geom_point() +
  labs(x="Length (m)",y="Bottom Depth (m)") +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  scale_y_reverse()+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

p1+p2+p3
ggsave(
  here("figs", "eye_silus.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)


p4 <- sorted_data %>% 
  filter(scientificname == "Argentina sphyraena") %>% 
  ggplot(aes(log(length), log(eye_dia_mm), col = location)) + 
  geom_point()+
  labs(y="log Length (cm)",x=" log Eye diameter (mm)") +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

##!> Eye size by depth

p5 <- sorted_data %>% 
  filter(scientificname == "Argentina sphyraena") %>% 
  ggplot(aes(eye_dia_mm, depth, col = location)) + 
  geom_point()+
  labs(x="Eye Diameter (mm)",y="Bottom Depth (m)") +
  theme_bw() +
  scale_y_reverse()+
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none")

##!> Length by depth

p6 <- sorted_data %>% 
  filter(scientificname == "Argentina sphyraena") %>% 
  ggplot(aes(length, depth, col = location)) + 
  geom_point()+
  labs(x="Length (m)",y="Bottom Depth (m)") +
  theme_bw() +
  scale_y_reverse() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

p4 + p5 + p6
ggsave(
  here("figs", "eye_sphyraena.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)


##!> Eye width and pupil width relationship

sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(log(eye_dia_mm), log(pup_dia_mm), col = location)) + 
  geom_point()+
  labs(y="log Pupil Diameter (mm)",x="log Eye Diameter (mm)")+
  facet_grid(~ scientificname) +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 20)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

##!> Save last plot

ggsave(
  here("figs", "eye_pupil.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

sorted_data <- sorted_data %>% 
  mutate(preorb = head_length_mm - eye_dia_mm,
         preorb_eye_ratio = eye_dia_mm/preorb)

sorted_data %>% 
  drop_na(preorb_eye_ratio) %>% 
  group_by(scientificname) %>% 
  summarize(preorb_eye = mean(preorb_eye_ratio))


##!> Pupil width to pre

individual_catch %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(log(eye_dia_mm), log(head_length_mm))) + 
  geom_point()+
  labs(y="log Pupil Diameter (mm)",x="log Eye Diameter (mm)")+
  facet_grid(~ scientificname) +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 20)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

### LENGTH AT AGE RELATIONSHIP ~ FRANCESCO ####

sorted_silus <- sorted_data %>% 
  filter(scientificname == "Argentina silus")

sorted_sphyraena <- sorted_data %>% 
  filter(scientificname == "Argentina sphyraena")

## define VB growth model
vbmod <- length_cm~Linf*(1-exp(-K*(age_yrs-t0)))

gr1 <- sorted_silus %>% 
  filter(sex %in% "Male") %>% 
  dplyr::select("startyear", "station", "serialnumber", "scientificname", "fish_id", "length_cm", "weight_g", "age_yrs", "location")

gr1 <- na.omit(gr1)

start1 <- vbStarts(length_cm ~ age_yrs, gr1) # use subset per group
unlist(start1) 
mod1 <- nls(vbmod,data=gr1,start=start1)
VBpar <- data.frame(Linf=NA, k=NA, t0=NA)
VBpar[,1:3] <- coef(mod1);VBpar
mod1

## confidence intervals for estimated parameters for group1

boot1 <- nlsBoot(mod1,niter=200)
confint(boot1,plot=TRUE)

## calculate length predictions + 95% CI for group1
range(gr1$age)
new<-seq(0,30,1) # Danner 0 - 30 med intervall 1 for å se på alle årsklassene. 

pred1 <- trtools::nlsint(mod1, data.frame(age_yrs = new, interval="prediction", level=0.95)) #modellen
pred1 <- data.frame(age=new, pred1) # fit = forventet lengde ved alder, lwr = minimumnslengde, ups = maximum

plot1<-gr1 %>% 
  ggplot(aes(age_yrs, length_cm)) + geom_point(size = 2)+
  labs(y="Length (cm)",x="Age (years)",title="Male Walleye A. silus")+
  theme_classic()+ 
  geom_line(data=pred1, aes(age,fit), lwd=1) + #setter inn linje fra predicted model+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) 
plot1


plot1 + geom_smooth(method = "lm",colour="black", linetype="dashed")

signif(AIC(mod1))
signif(AIC(lm(gr1$length~gr1$age_yrs)))


### GROUP 2 ##

gr2<- sorted_silus %>% 
  filter(sex %in% "Female") %>% 
  dplyr::select("startyear", "station", "serialnumber", "scientificname", "fish_id", "length", "individualweight", "age_yrs", "length_cm") 

gr2 <- na.omit(gr2)

start2 <- vbStarts(length_cm ~ age_yrs,gr2)
unlist(start2)
mod2 <- nls(vbmod,data=gr2,start=start2)
VBpar2 <- data.frame(Linf=NA, k=NA, t0=NA)
VBpar2[,1:3] <- coef(mod2);VBpar2
mod2

## confidence intervals for estimated parameters for group2
boot2 <- nlsBoot(mod2,niter=200) # niter should be nearer 1000
confint(boot2,plot=TRUE)

## calculate 95% CI around length predictions for group2
pred2 <- trtools::nlsint(mod2, data.frame(age_yrs=new, interval="prediction", level=0.95))
pred2 <- data.frame(age_yrs=new, pred2)

## plot growth curve for group2
plot2<-gr2 %>% 
  ggplot(aes(age_yrs, length_cm)) + geom_point(size = 2)+
  labs(y="Length (cm)",x="Age (years)",title="Female Walleye A. silus")+
  theme_classic()+
  geom_line(data=pred2, aes(age_yrs,fit), lwd=1) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) 
plot2

plot2 + geom_smooth(method = "lm",colour="black", linetype="dashed")

signif(AIC(mod2))
signif(AIC(lm(gr2$length~gr2$age_yrs)))

##!> for Argentina sphyraena
gr3 <- sorted_sphyraena %>%
  filter(sex %in% "Male") %>%
  dplyr::select("startyear", "station", "serialnumber", "scientificname", "fish_id", "length_cm", "weight_g", "age_yrs")

gr3 <- na.omit(gr3)

start3 <- vbStarts(length_cm ~ age_yrs, gr3) # use subset per group
unlist(start3)
mod3 <- nls(vbmod,data=gr3,start=start3)
VBpar3 <- data.frame(Linf=NA, k=NA, t0=NA)
VBpar3[,1:3] <- coef(mod3);VBpar3
mod3

## confidence intervals for estimated parameters for group1

boot3 <- nlsBoot(mod3,niter=200)
confint(boot3,plot=TRUE)

new2 <-seq(0,15,1) # Danner 0 - 15 med intervall 1 for å se på alle årsklassene. 

pred3 <- trtools::nlsint(mod3, data.frame(age_yrs = new2, interval="prediction", level=0.95)) #modellen
pred3 <- data.frame(age=new2, pred3) # fit = forventet lengde ved alder, lwr = minimumnslengde, ups = maximum

plot3<-gr3 %>%
  ggplot(aes(age_yrs, length_cm)) + geom_point(size = 2)+
  labs(y="Length (cm)",x="Age (years)",title="Male A. silus")+
  theme_classic()+
  geom_line(data=pred3, aes(age,fit), lwd=1) + #setter inn linje fra predicted model+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17))
plot3


plot3 + geom_smooth(method = "lm",colour="black", linetype="dashed")

signif(AIC(mod3))
signif(AIC(lm(gr3$length_cm~gr3$age_yrs)))

### GROUP 4 ##

gr4<- sorted_sphyraena %>%
  filter(sex %in% "Female") %>%
  dplyr::select("startyear", "station", "serialnumber", "scientificname", "fish_id", "length", "individualweight", "age_yrs", "length_cm")

gr4 <- na.omit(gr4)

start4 <- vbStarts(length_cm ~ age_yrs,gr4)
unlist(start4)

mod4 <- nls(vbmod,data=gr4,start=start4)
VBpar4 <- data.frame(Linf=NA, k=NA, t0=NA)
VBpar4[,1:3] <- coef(mod4);VBpar4
mod4

## confidence intervals for estimated parameters for group2
boot4 <- nlsBoot(mod4,niter=200) # niter should be nearer 1000
confint(boot4,plot=TRUE)

## calculate 95% CI around length predictions for group2
pred4 <- trtools::nlsint(mod4, data.frame(age_yrs=new2, interval="prediction", level=0.95))
pred4 <- data.frame(age_yrs=new2, pred4)

## plot growth curve for group2
plot4<-gr4 %>%
  ggplot(aes(age_yrs, length_cm)) + geom_point(size = 2)+
  labs(y="Length (cm)",x="Age (years)",title="Female A. silus")+
  theme_classic()+
  geom_line(data=pred4, aes(age_yrs,fit), lwd=1) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17))
plot4

plot4 + geom_smooth(method = "lm",colour="black", linetype="dashed")

signif(AIC(mod2))
signif(AIC(lm(gr4$length~gr4$age_yrs)))


## let`s now plot growth curves+conf.intervals for both groups

sorted_silus %>%
  ggplot(aes(age_yrs, length_cm)) + geom_point(aes(colour = sex), size = 2) + 
  scale_x_continuous(breaks=new) + 
  labs(y="Length (cm)", x="Age (years)", colour="Sex") +
  geom_line(data=pred1, aes(age,fit), lwd=1.5, colour = "#00B9E3") +
  geom_line(data=pred1, aes(age,lwr), lty=2) +
  geom_line(data=pred1, aes(age,upr), lty=2) +
  geom_line(data=pred2, aes(age_yrs,fit), lwd=1.5, colour="#F8766D") +
  geom_line(data=pred2, aes(age_yrs,lwr), lty=2) +
  geom_line(data=pred2, aes(age_yrs,upr), lty=2) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position = "none")

##!> Save plot 

ggsave(
  here("figs", "length_at_silus.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

plot6 <- sorted_sphyraena %>%
  ggplot(aes(age_yrs, length_cm)) + geom_point(aes(colour = sex), size = 2) +
  scale_x_continuous(breaks=new2) +
  theme_bw() +
  labs(y="Length (cm)", x="Age (years)", colour="Sex")+
  geom_line(data=pred3, aes(age,fit), lwd=1.5, colour = "#00B9E3") +
  geom_line(data=pred3, aes(age,lwr), lty=2) +
  geom_line(data=pred3, aes(age,upr), lty=2) +
  geom_line(data=pred4, aes(age_yrs,fit), lwd=1.5, colour="#F8766D") +
  geom_line(data=pred4, aes(age_yrs,lwr), lty=2) +
  geom_line(data=pred4, aes(age_yrs,upr), lty=2) +
  ylim(0,40) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position = "none")

plot6

ggsave(
  here("figs", "length_at_age_sphyraena.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

gr5<- sorted_data %>%
  filter(scientificname == "Argentina sphyraena") %>%
  dplyr::select("startyear", "station", "serialnumber", "scientificname", "fish_id", "length", "individualweight", "age_yrs", "length_cm")

gr5 <- na.omit(gr5)

start5 <- vbStarts(length_cm ~ age_yrs,gr5)
unlist(start5)

mod5 <- nls(vbmod,data=gr5,start=start5)
VBpar5 <- data.frame(Linf=NA, k=NA, t0=NA)
VBpar5[,1:3] <- coef(mod5);VBpar5
mod5

## confidence intervals for estimated parameters for group2
boot5 <- nlsBoot(mod5,niter=200) # niter should be nearer 1000
confint(boot5,plot=TRUE)

## calculate 95% CI around length predictions for group2
pred5 <- trtools::nlsint(mod5, data.frame(age_yrs=new2, interval="prediction", level=0.95))
pred5 <- data.frame(age_yrs=new2, pred5)

## plot growth curve for group2
plot9<-gr5 %>%
  ggplot(aes(age_yrs, length_cm)) + geom_point(size = 2)+
  labs(y="Length (cm)",x="Age (years)",title="A. sphyraena")+
  theme_classic()+
  geom_line(data=pred5, aes(age_yrs,fit), lwd=1) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17))
plot9

plot9 + geom_smooth(method = "lm",colour="black", linetype="dashed")

signif(AIC(mod5))
signif(AIC(lm(gr5$length~gr5$age_yrs)))

plot7 <- sorted_data %>%
  filter(scientificname == "Argentina sphyraena") %>% 
  ggplot(aes(age_yrs, length_cm)) + geom_point(size = 2) +
  scale_x_continuous(breaks=new2) +
  theme_bw() +
  labs(y="Length (cm)", x="Age (years)")+
  geom_line(data=pred5, aes(age_yrs,fit), lwd=1.5) +
  geom_line(data=pred5, aes(age_yrs,lwr), lty=2) +
  geom_line(data=pred5, aes(age_yrs,upr), lty=2) +
  ylim(0,40) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position = "none")

plot7

ggsave(
  here("figs", "length_at_age_sphyraena_all.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

##!> GONAD ####

sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena"), startyear %in% c("2019", "2021")) %>% 
  ggplot(aes(weight_g, gonadweight_g, col = location)) + 
  geom_point()+
  labs(y="Gonad Weight (g)",x="Total Weigth (g)")+
  facet_grid(location~ scientificname) +
  theme_bw() +
  scale_color_manual(values = c("#DB4325" , "#EDA247", "#EBD850", "#57C4AD", "#006164")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  guides(x = guide_axis(angle = 90)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 20)) +
  geom_smooth(method = "lm", col = "black") +
  theme(legend.position = "none") +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..rr.label.., sep = "~~~")),
               parse = TRUE, col = "black", size = 7)

##!> Save last plot

ggsave(
  here("figs", "weight_gonad.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

##!> GSI by depth plot? 
sorted_data %>%  
  drop_na(gsi_range) %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  ggplot(aes(x = depth_range, fill = as.factor(gsi_range))) +
  geom_bar(position = "fill") +
  theme_bw() +
  facet_grid(location ~ scientificname) +
  scale_fill_discrete_sequential(name = "GSI", palette = "TealGrn") +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  labs(x="Depth (m)", y="GSI (%)")+
  guides(x = guide_axis(angle = 90))

ggsave(
  here("figs", "gsi_depth.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

##!> GSI versus total length 

sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>% 
  drop_na(maturationstage.x) %>% 
  ggplot(aes(x = length_cm, y = gsi, shape = as.factor(maturationstage.x), col = as.factor(maturationstage.x))) +
  geom_jitter(size = 4, alpha = 0.75) +
  theme_bw() +
  facet_wrap(vars(scientificname)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  labs(x="Length (cm)", y="Gonadosomatic Index (GSI)") +
  theme(legend.position = "none") 
  
ggsave(
  here("figs", "gsi_length.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)

##!> Maturity plot

sorted_data %>% 
  filter(scientificname %in% c("Argentina silus", "Argentina sphyraena")) %>%
  drop_na(maturationstage.x) %>% 
  ggplot(aes(x = depth_range, fill = as.factor(maturationstage.x))) + 
  geom_bar(position = "fill") +
  theme_bw() +
  facet_grid(location ~ scientificname) +
  scale_fill_discrete_sequential(name = "Maturity stage", palette = "TealGrn") +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 17)) +
  theme(axis.text.y = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 18)) +
  labs(x="Depth (m)", y="Maturation composition (%)")+
  guides(x = guide_axis(angle = 90))
  
ggsave(
  here("figs", "maturity_depth.png"),
  width = 200,
  height = 150,
  unit = "mm",
  dpi = 600,
  scale = 2
)
