# Quantitative Methods in Ecology and Evolution, Homework 8
# Nathan Lin and Frank
# October 23, 2025
# Coding in R 6: connecting to databases

library(here)
library(EDIutils)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# We're not using setwd anymore
here::i_am("HW 8.R")

###### Objective 1 - downloading the dataset ######

### Download tick NEON data from EDI portal ###
# https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1118&revision=1
# Save the NEON 2022 data package ID 
package_id <- "edi.1118.1"
# Get the entity IDs in this package and their corresponding names
entity_names <- read_data_entity_names(package_id)
# Pull out the entity ID we want (access the value of the 1st column for rows with selected entity name)
entity_id <- entity_names[entity_names$entityName == "Ticks sampled using drag cloths", 1]

# Download raw data in bytes first then parse with read_csv
raw_data_bytes <- read_data_entity(package_id, entity_id)
neon_tick_data <- read_csv(raw_data_bytes)

# A function to pull out the month and year from a datetime object
month_year_from_datetime <- function(datetime_object) {
  # Built in R constant
  month_names <- month.name
  # Pull out month number
  our_month <- lubridate::month(datetime_object)
  # Pull out year
  our_year <- lubridate::year(datetime_object)
  # Return the month name and the year
  return(paste(month.name[our_month], our_year))
}

# View() shows that they tracked abundance inconsistently so let's not use those
# I will only keep these values: siteID, plotID, date, latitude, longitude, elevation, land cover class,
    # taxon name, taxon rank, life stage, sampling method, total sampled area, and field remarks
# Let's also only keep things identified down to genus or species
# Finally I am going to pull the month and year out of the datetime because I'm not too interested in the finer scale times
cleaned_ticks <- neon_tick_data %>%
  select(siteID, plotID, observation_datetime, taxon_name, taxon_rank, LifeStage, 
         samplingMethod, totalSampledArea, latitude, longitude, elevation, nlcdClass) %>% 
  filter(taxon_rank %in% c("genus", "species"), !is.na(nlcdClass)) %>% 
  mutate(sampling_time = month_year_from_datetime(observation_datetime),
         month = month.name[month(observation_datetime)],
         month_number = month(observation_datetime),
         year = factor(year(observation_datetime), levels=c(2014, 2015, 2016, 2017, 2018, 2019, 2020)),
         taxon_life_stage = paste(taxon_name, LifeStage)) %>% 
  select(-observation_datetime, -taxon_name, -LifeStage)


###### Objective 2 - EDA ######
### FIGURE 1: I'd like to look at the sampling sites and how many sampling events were at each
  # There are different plots at each sampling site, but I will assume they are close together
  # Close enough such that picking the first set of coords from the site will be good enough
# I will start by grouping to get sampling events (plot + sampling time combo) for each site
sampling_events <- cleaned_ticks %>%
  group_by(siteID, plotID, sampling_time) %>% 
  summarise(one_lat = first(latitude),
            one_long = first(longitude),
            avg_elevation = mean(elevation),
            what_ticks = paste(unique(taxon_life_stage), collapse=", "))
# Then group by site to get the total number of sampling events per site
sampling_events_per_site <- sampling_events %>%
  group_by(siteID) %>% 
  summarise(how_many_sampling_events=n(),
            one_lat = first(one_lat),
            one_long = first(one_long),
            avg_elevation = mean(avg_elevation))
## Map sites onto the US with size varying based on how many sampling events
# Start with getting a shapefile of the US with state boundaries
us_states <- tigris::states()

# Then make the map - I am going to bound it to the continental US even though there is Puerto Rico and Alaska
sampling_events_map <- ggplot() + 
  geom_sf(data=us_states) + 
  geom_point(data=sampling_events_per_site, mapping=aes(x=one_long, y=one_lat, size=how_many_sampling_events)) +
  coord_sf(default_crs=sf::st_crs(4326), xlim=c(-125, -68), ylim=c(24, 50)) 
# Preview the map
sampling_events_map
# Save the map
ggsave(plot=sampling_events_map, filename=here("figures", "sampling_events.png"), width=12, height=7, dpi=300)

## Comments for figure 1
# Here I count one PLOT and sampling time combination as a sampling event, then tally up the number of sampling events per site.
# This shows us that the sampling effort at every site is roughly even (which is good),
  # BUT there are areas that are greatly underrepresented (such as the western and northwestern US, southern Texas, 
    # most of the Midwest, and some of the east-most states).
# I should mention that Alaska and Puerto Rico are represented but not plotted. Hawaii is not though (perhaps they don't have ticks there?)
  # A quick google search showed that Hawaii has ticks, but not the kind you'd regularly find through dragging and flagging.

### FIGURE 2: I'd also like to look at the dates of sampling and if we have consistent date representation at each site
# Group by site ID, month, and year, then get counts for each to see what ticks are found and when
months_and_years_per_site <- cleaned_ticks %>% 
  group_by(siteID, month_number, year) %>%
  summarise(how_many=n())
# Make a plot facet wrapping by site ID and plotting the number of ticks collected at different months and years
sample_times_plot <- ggplot(months_and_years_per_site, aes(x=month_number, y=how_many, col=year)) + 
  geom_point() + scale_x_continuous(breaks=seq(1, 12, by=1)) +
  facet_wrap(~siteID, ncol=6) + labs(x="Month", y="Ticks collected")
# Preview the plot
sample_times_plot
# Save the plot
ggsave(plot=sample_times_plot, filename=here("figures", "sample_times.png"), width=12, height=7, dpi=300)

## Comments for figure 2
# In some years ticks were not collected at some sites, and sites ranged from consistent collections throughout a span of many years (HARV)
  # to only a couple collections made over a couple months in a single year (TOOL)
# The fact that there is so much variation in which sites were sampled at which months and which years
  # indicates that this data should not be used to make claims about seasonality of these ticks (most of the sampling is generally done in the summer)
  # beyond "{this species} was collected at {this time} in {this place}"
# It should be noted though that I filtered out ticks not identified to genus or species, and this doesn't reflect
  # when ticks are NOT found (because records of collecting events that failed to produce ticks are also not included)

### FIGURE 3: Finally I'd like to see if there are any differences in total sampled area and sampling method in each site
# Group by site ID and sample method and vegetation class and get counts and average total sampled area for each, see what ticks are found and how
sample_methods_vegetation_and_site_counts <- cleaned_ticks %>%
  group_by(siteID, nlcdClass, samplingMethod) %>%
  summarise(how_many = n(), 
            avg_sampled_area = mean(totalSampledArea),
            what_ticks = paste(unique(taxon_life_stage), collapse=", "))
# Make a plot facet wrapping by site ID and plotting the number of habitat types each sampling method was applied for
sample_methods_plot <- sample_methods_vegetation_and_site_counts %>%
  ggplot(aes(x=samplingMethod, fill=samplingMethod)) +
  geom_bar() + facet_wrap(~siteID, ncol=6)
# Preview the plot
sample_methods_plot  
# Save the plot
ggsave(plot=sample_methods_plot, filename=here("figures", "sample_methods.png"), width=12, height=7, dpi=300)

## Comments for figure 3
# This graph provides some extra insight into the differences in sampling methods used per site.
  # The data is grouped by site, sampling method, and habitat type, so the counts per site represent 
  # how many collecting method-habitat type combinations there are per site.
# Some sites have more habitat types, with both dragging and flagging used in the majority of sites and habitat types.
  # This is good, but in a great deal of habitat types only dragging was used.
# Dragging vs flagging sampling method is important because dragging may miss ticks questing higher on vegetation or on larger shrubs,
  # while flagging may miss ticks hunting on leaf litter.
# For our dataset, this just plot shows that our sampling methodology is pretty sound and consistent across sites,
  # but we would just need to acknowledge that we may miss ticks on vegetation it is hard or impossible to drag over.