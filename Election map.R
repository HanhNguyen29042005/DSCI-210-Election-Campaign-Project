library(tidyverse)
library(sf)
library(readxl)
library(RColorBrewer)
library(raster)
library(dplyr)
library(ggplot2)
####Loading the datasets####
map2006 <- st_read("../data/maps/Precincts_2006_region.shp")
results2006 <- read_excel("~/GitHub/DSCI210-f24/data/election results/General_2006_11_OFFCANVASS.xls",skip=2,sheet=1)
####draw the map with precinct####
map2006 %>%
  ggplot(aes())+
  geom_sf()
#### combining the result and map of 2006####
mapANDresults2006 <-
  left_join(map2006, results2006, by = c("PLAN_NAME" = "PRECINCT"))
####draw the map of hamilton county with the voting result
#sum of all voter for democratic candidates
dem_order <- c(12,13,15,17,21,23,27,31,33,35,36,39,41,44,46,48,49)
dem <- mapANDresults2006[,dem_order]
dem_non_geom <- dem %>% st_set_geometry(NULL)
#sum of all voter for republican candidates
rep_order <-  c(8,14,16,19,22,24,26,30,32,34,37,38,40,42,43,45,47,50)
rep <- mapANDresults2006[,rep_order]
rep_non_geom <- rep %>% st_set_geometry(NULL)
# Compute the sum of the values in the selected columns for each precinct
dem_sum <- rowSums(dem_non_geom, na.rm = TRUE)  
rep_sum <- rowSums(rep_non_geom, na.rm = TRUE)
# Calculate proportion for each precinct
# Add proportion to the original sf object
mapANDresults2006 <- mapANDresults2006 %>%
  mutate(dem_prop = dem_sum/(dem_sum+rep_sum)) %>% 
  mutate(base = case_when(dem_prop>.6~ "Base",
                           dem_prop<.40~"Residual",
                           TRUE ~"Swing"))
# Plot the map
mapANDresults2006 %>%
  ggplot(aes(fill = base)) +
  geom_sf() +
  scale_fill_manual(
    values = c("Base" = "red", "Residual" = "blue", "Swing" = "gray"),
    na.value = "transparent") +
  labs(
    title = "2006 Voting Result Map in Hamilton County",
    fill = "Vote for \n Democrat")

mapANDresults2006 %>% 
  mutate(dem_prop = dem_sum/(dem_sum+rep_sum)) %>% 
  mutate(dem_baseswing = cut(dem_prop, breaks=c(-0.001,.4,.6,1), label=c("Residual", "Swing", "Base"))) %>% 
  ggplot(aes(fill=dem_baseswing)) +
  geom_sf()+
  scale_fill_manual(
    values = c("Base" = "red", "Residual" = "blue", "Swing" = "gray"),
    na.value = "transparent") +
  labs(
    title = "2006 Voting Result Map in Hamilton County",
    fill = "Vote for \n Democrat")
#_______________________________________________________________________________
map2020 <- st_read("C:/Users/Owner/OneDrive - Xavier University/Documents/GitHub/DSCI210-f24/data/maps/PRECINCT2020_052219.shp")
results2020 <- read_excel("G20_Official_Canvass.xlsx",sheet = "Candidates",skip=1)
map2020 %>%
  ggplot(aes())+
  geom_sf()
mapANDresults2020 <-
  left_join(map2020, results2020, by = c("PRECINCT" = "PRECINCT"))
dem_order2020 <- c(6,17,19,20,24,25,27,34,33,35,39,40,43,45,47,48,50)
dem2020 <- mapANDresults2020_valid[,dem_order2020]
dem_non_geom2020 <- dem2020 %>% st_set_geometry(NULL)
#sum of all voter for republican candidates
rep_order2020 <-  c(9,16,22,23,26,28,29,32,36,37,41,42,44,46,49)
rep2020 <- mapANDresults2020_valid[,rep_order2020]
rep_non_geom2020 <- rep2020 %>% st_set_geometry(NULL)
# Compute the sum of the values in the selected columns for each precinct
dem_sum2020 <- rowSums(dem_non_geom2020, na.rm = TRUE)  
rep_sum2020 <- rowSums(rep_non_geom2020, na.rm = TRUE)
# Calculate proportion for each precinct
dem_prop2020 <- dem_sum2020 / (dem_sum2020 + rep_sum2020)
#st_make_valid(mapANDresults2020) --> the geometries are unvalid --> attempt to fix the geometries
mapANDresults2020_valid <- mapANDresults2020_valid %>% mutate(dem.prop.2020=dem_prop2020)
mapANDresults2020_valid %>% 
  #mutate(dem.drop=dem_drop2020) %>% 
  ggplot(aes(fill=dem_prop2020)) +
  geom_sf()+
  labs(title = "2020 general election", 
       subtitle = "Democrats vs Republicans",
       fill = "Vote for \nDemocratic candidates (%)", 
       caption = "")+
  scale_fill_gradientn(colours=brewer.pal(n=6,name="RdBu"),na.value = "transparent",                        
                       breaks=c(0,.25,0.5,.75,1),labels=c("0%","25%","50%","75%","100%"),
                       limits=c(0,1))




