# Quantitative Methods in Ecology and Evolution, Homework 5
# Nathan Lin and Kristine Schoenecker
# October 2, 2025
# Coding in R 3: data import

# Loading in packages
library(readxl)

# Question 1 (reading different data types)
setwd("C:/Users/Natha/Documents/Zoo800_f25/HW 5")
# csv
fish_csv <- read.csv("./Data/fish.csv")
head(fish_csv, n=5)
# Excel file
fish_xlsx <- read_xlsx("./Data/fish.xlsx")
head(fish_xlsx, n=5)
# RDS
fish_rds <- readRDS("./Data/fish.rds")
head(fish_rds, n=5)

# Try with lapply
fish_list = list(fish_csv, fish_xlsx, fish_rds)
# lapply takes additional arguuments as the named arguments of FUN
lapply(X=fish_list, FUN=head, n=5)


# Question 2 (saving different types)
library(writexl)

# We will use the fish_csv dataset
# Save as csv, xlsx, and rds -- make the Outputs folder first
# dir.create("./Outputs")
write.csv(fish_csv, "Outputs/second_fish.csv", row.names=FALSE)
write_xlsx(fish_csv, "Outputs/second_fish.xlsx")
saveRDS(fish_csv, "Outputs/second_fish.rds")

# Grab the file names
second_fish_filenames_list <- list.files("Outputs", full.names=TRUE)

# Check the file info to compare file sizes
lapply(X=second_fish_filenames_list, FUN=file.info)
# The csv was 12128 bytes, the xlsx was 13870 bytes, and the rds was 3033 bytes.

# It looks like csv is the best for sharing because it's not a huge file size and viewable in R or with most other platforms. For compact storage RDS is the smallest.


# Question 3 (wrangling with dplyr)
library(dplyr)

# Transforming the data
fish_output <- fish_csv %>% 
  filter(Species %in% c("Walleye", "Yellow Perch", "Smallmouth Bass")) %>% 
  filter(Lake %in% c("Erie", "Michigan")) %>%
  select(-Age_years) %>% 
  mutate(Length_mm = Length_cm * 10) %>% 
  mutate(Length_group = cut(Length_mm, breaks=c(0, 200, 400, 600, Inf)))

# Counts of each species-length group combination
species_length_group_counts <- fish_output %>% count(Species, Length_group)

# Mean weight, median weight, and count of each species-year combination
species_year_summaries <- fish_output %>% 
  group_by(Species, Year) %>%
  summarise("Mean_weight"=mean(Weight_g), "Median_weight"=median(Weight_g), "Sample_size"=n())

# Quick plot of mean weight for each species over time
library(ggplot2)
fish_weights_plot <- species_year_summaries %>% 
  ggplot(mapping=aes(x=Year, y=Mean_weight, col=Species)) + 
  geom_line()
ggsave("Outputs/fish_plot.png")

# Saving the summary data
write.csv(species_length_group_counts, "Outputs/species_length_group_counts.csv", row.names=FALSE)
write.csv(species_year_summaries, "Outputs/species_year_summaries.csv", row.names=FALSE)


# Question 4 (reading multiple files in at once)
# Get names of multiple files
fish_survey_files <- list.files("Multiple_files", full.names=TRUE)
# Open them all at once
fish_survey_list_of_csv_data <- lapply(fish_survey_files, read.csv)
# Put all the rows together
fish_survey_all_data <- bind_rows(fish_survey_list_of_csv_data)


# Question 5 (parallel computing)
# Done, it took 1.42 seconds serial mode versus 0.31 seconds parallel mode so 4.58x speedier that is really impressive.
# My (Nathan's) computer used 15 cores.