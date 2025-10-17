# Quantitative Methods in Ecology and Evolution, Homework 6
# Nathan Lin, Victoria Salerno, and Keeley Kuru
# October 16, 2025
# Coding in R 5: mapping

##### Setup #####
# setwd("./HW 7")

library(dplyr)
library(ggplot2)
library(stringr)
library(tigris)
library(sf)

##### Data cleaning #####
ticks_raw <- read.csv("nathan_spring_2025_tick_collecting.csv")
ticks_raw

# I am going to just plot and color by if there are Dermacentor or Ixodes or both
# I don't need to do any filtering here because my plot window will just cut the extra points out
  # Raw strings are r"()"
ticks_cleaned <- ticks_raw %>% 
  mutate(lat=as.numeric(str_extract(Coords, pattern=r"([0-9]+\.[0-9]+)")), 
         long=as.numeric(str_extract(Coords, pattern=r"(-[0-9]+\.[0-9]+)")),
         species=factor(Specimen.s., levels=c("Ixodes", "Dermacentor", "Ixodes + Dermacentor"))) %>%  
  select(long, lat, species) 

# Set the bounds for the area I want to plot
bay_area_bounds <- st_as_sfc(st_bbox(c(xmin=-122.75, ymin=37.5, xmax=-121.95, ymax=38.2))) 
# Make sure the CRS is 4326 for everything
st_crs(bay_area_bounds) <- 4326

# Tigris gives me a high resolution plot of the US
ca_tigris <- tigris::divisions(resolution="500k", class="sf") %>% st_transform(crs=4326) %>% st_intersection(bay_area_bounds)

# I downloaded this shapefile from the East Bay Regional Park District GIS page
  # https://www.arcgis.com/home/item.html?id=ca1a3caff08142759e40ee467d52b0ff
  # https://geodatadownloader.com/
east_bay_parks <- st_read("./east_bay_parks/Ebrparkp m Status.shp")

# Plot with arrows and text annotation for the regional parks I found ticks at
ggplot() + 
  geom_sf(data=ca_tigris) + 
  geom_sf(data=east_bay_parks) +
  geom_point(data=ticks_cleaned, aes(x=long, y=lat, colour=species)) + 
  coord_sf(default_crs=st_crs(4326), xlim=c(-122.24, -122.14), ylim=c(37.795, 37.865)) +
  geom_segment(aes(x=-122.23, y=37.83, xend=-122.23, yend=37.84),
           arrow=arrow(length=unit(0.2, "cm"), type="closed"), color="red", size=1) +
  geom_segment(aes(x=-122.184, y=37.80, xend=-122.163, yend=37.806),
           arrow=arrow(length=unit(0.2, "cm"), type="closed"), color="red", size=1) +
  geom_segment(aes(x=-122.175, y=37.859, xend=-122.186, yend=37.8535),
           arrow=arrow(length=unit(0.2, "cm"), type="closed"), color="red", size=1) + 
  annotate("text", x=-122.22, y=37.824, label="Temescal Regional\nRecreation Area") +
  annotate("text", x=-122.204, y=37.802, label="Reinhardt Redwood\n Regional Park") +
  annotate("text", x=-122.157, y=37.859, label="Sibley Volcanic\n   Regional Preserve") +
  labs(x="Longitude", y="Latitude", color="Species found") + 
  scale_color_manual(values=c("red", "blue", "purple")) 