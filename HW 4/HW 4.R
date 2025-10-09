# Quantitative Methods in Ecology and Evolution, Homework 4
# Nathan Lin, Kristine Schoenecker, and Elizabeth Braatz
# September 25, 2025
# Coding in R 2: functions

# This is a super cool one where I am making a function that takes in a dataframe 
  # and some relevant column names and I am automatically assigning groups based 
  # on the value of a continuous variable and where it lies in the overall distribution
# If I want to vectorize perhaps I can use between?? 
  # can do between(negative infinity, first breakpoint) and same with last and positive infinity


library(palmerpenguins)

# Question 1a - making the continuous to binary function
continuous_to_binary <- function(values, breakpoint, first_label, second_label) {
  # Takes a value or vector, a break point, and labels for each group
  # Values less than or equal to the break point will be part of the first group
  # Values higher than the break point will be part of the second group.
  which_group <- values <= breakpoint
  # The function ifelse works for binary data since I can just make it true/false
  # If I had more than 2 cases I would use if statements
  labels_subbed <- ifelse(which_group, first_label, second_label)
  return(labels_subbed)
}

# Notes: you can nest in ifelse?? with that false case
  # You can also use casewhen for more breakpoints??


# Question 1b - small and large penguins
# Just take a look at penguins dataset first, width=Inf prevents truncation
print(penguins, n=3, width=Inf)
# Let's see what the min and max and average are for body mass
print(c(range(penguins$body_mass_g, na.rm=TRUE), 
        mean(penguins$body_mass_g, na.rm=TRUE)))
# Add a new column to penguins based on body mass
penguins$body_mass_binary <- continuous_to_binary(penguins$body_mass_g, 4200, "small", "large")
# Did it work? looks like it
print(penguins$body_mass_binary)

# Question 2a - any number of break points
continuous_to_categorical <- function(values, breakpoints, category_labels) {
  # This assumes the breakpoints and label_categories vectors are ordered,
    # and that breakpoints is 1 element smaller than label_categories.
  # This is our storage vector
  converted_values <- character(length(values))
  # Go through each value (it would be nice if this was vectorizable)
  # Seq in R makes the range, range gives you the min and max
  for (value_number in seq(length(values))) {
    # NA will crash the function unless I do this
    if (is.na(values[value_number])) {
      converted_values[value_number] <- NA
      # Continues with the next thing in the loop
      next
    }
    # Otherwise, evaluate if it's less than or equal to each breakpoint at a time
    for (breakpoint_number in seq(length(breakpoints))) {
      # If value is less than breakpoint we assign label and stop looping
      if (values[value_number] <= breakpoints[breakpoint_number]) {
        # Assign the label, which is the corresponding index in label_categories EXCEPT for the last one
        converted_values[value_number] <- category_labels[breakpoint_number]
        # Stop the loop
        break
      }
    }
    # If the label wasn't assigned yet after we hit the last breakpoint it's the last label by default
    if (converted_values[value_number] == "") {
      # NEGATIVE INDEXING DROPS THE ELEMENT IN R IT DOES NOT ACCESS THE LAST
      converted_values[value_number] <- tail(category_labels, n=1)
    }
  }
  return(converted_values)
}

# Test omg it works!!!!
continuous_to_categorical(c(1:10), breakpoints=c(2, 4, 6, 8), category_labels=c("below 2", "below 4", "below 6", "below 8", "above 8"))
# Second test with NA things
continuous_to_categorical(c(2, NA, 3), breakpoints=c(2.5), category_labels=c("small", "large"))


##### Something else we can do #####

convert_to_discrete <- function(data, breakpoints, labels) {
  # This one takes the actual dataframe first
  require(dplyr)
  categorized_data <- data %>% 
    mutate(
      # Creates multiple if-else statements and evaluates them sequentially, if no cases match there's a final default value
      discrete_variable = case_when(
        body_mass_g <= breaks[1] ~ labels[1],
        body_mass_g <= breaks[2] ~ labels[2],
        .default = labels[3]
      )
    )
  return(categorized_data <- discrete_variable)
}


# Question 2b - small, medium, and large penguins
# Look at a range again
range(penguins$body_mass_g, na.rm=TRUE)
# I'll make the cutoffs 3900 and 5100
penguins$body_mass_sml <- continuous_to_categorical(penguins$body_mass_g, c(3900, 5100), c("small", "medium", "large"))

# Question 3a - I'll do quantiles for 2 breakpoints aka small medium large
# First let's do Adelie
adelie_penguins <- subset(penguins, species == "Adelie")
quantile(adelie_penguins$body_mass_g, probs=seq(0, 1, 1/3), na.rm=TRUE)
  # Small (<= 3450), Medium (<= 3900), Large (above that)
# Then Gentoo 
gentoo_penguins <- subset(penguins, species == "Gentoo")
quantile(gentoo_penguins$body_mass_g, probs=seq(0, 1, 1/3), na.rm=TRUE)
  # Small (<= 4800), Medium (<= 5350), Large (above that)
# Finally Chinstrap
chinstrap_penguins <- subset(penguins, species == "Chinstrap")
quantile(chinstrap_penguins$body_mass_g, probs=seq(0, 1, 1/3), na.rm=TRUE)
  # Small (<= 3583), Medium (<= 3833), Large (above that)
# We could say for each species something like small penguins being less than or equal to the 50% quantile

# Question 3b - update our function to account for species differences
continuous_to_categorical_by_species <- function(dataframe, species_vector, species_specific_breakpoints, category_labels) {
  # This assumes the species and breakpoint species order match, 
    # and that the breakpoints and label_categories vectors are ordered,
    # and that each breakpoints vector is 1 element smaller than label_categories.
  # This also assumes our dataframe will have values and species in the "body_mass_g" and "species" columns
  # We'll store the results in a new column called body_mass_cat and return the updated dataframe
  
  # We will be subsetting the dataframe and will need to join them together after proccessing
  dataframes_storage <- list()
  # Iterate through the species first and foremost
  for (species_number in seq(length(species_vector))) {
    # First isolate the rows with the species in question ([rows, cols])
    relevant_entries <- dataframe[dataframe$species == species_vector[species_number],]
    # Get the breakpoints - R does not support vectors in vectors so species_specific_breakpoints must be a list
    species_breakpoints <- species_specific_breakpoints[[species_number]]
    # Make storage vector for our current species
    converted_values <- character(nrow(relevant_entries))
    # Iterate through each of the values for the current species
    relevant_values <- relevant_entries$body_mass_g
    for (value_number in seq(length(relevant_values))) {
      # Deal with NA again
      if (is.na(relevant_values[value_number])) {
        converted_values[value_number] <- NA
        # Continue with the loop
        next
      }
      # Otherwise evaluate if it's less than or equal to each breakpoint at a time
      for (breakpoint_number in seq(length(species_breakpoints))) {
        # If value is less than breakpoint we assign label and stop looping
        if (relevant_values[value_number] <= species_breakpoints[breakpoint_number]) {
          # Assign label 
          converted_values[value_number] <- category_labels[breakpoint_number]
          # Get out of the for loop
          break
        }
      }
      # If label wasn't assigned yet after we hit the last breakpoint it's the last label
      if (converted_values[value_number] == "") {
        # Assign as the last label
        converted_values[value_number] <- tail(category_labels, n=1)
      }
    }
    # Add the converted values to the relevant entries
    relevant_entries$body_mass_sml_by_species <- converted_values
    # Store the finished subset for later joining
    dataframes_storage[[species_number]] <- relevant_entries
  }
  # Join the subsetted dataframes back together
  updated_dataframe <- do.call(rbind, dataframes_storage)
  # End by returning the new dataframe
  return(updated_dataframe)
}

# Question 3c - apply this to penguins holy moly it actually works after like 20 tries
size_categories_by_species_penguins <- continuous_to_categorical_by_species(dataframe=penguins,
                                                                            species_vector=c("Adelie", "Gentoo", "Chinstrap"),
                                                                            species_specific_breakpoints=list(c(3450, 3900), c(4800, 5350), c(3583, 3833)),
                                                                            category_labels=c("small", "medium", "large"))


# Question 4 - make a box plot
continuous_to_categorical_by_species_boxplot <- function(dataframe, species_vector, species_specific_breakpoints, category_labels) {
  # This assumes the species and breakpoint species order match, 
  # and that the breakpoints and label_categories vectors are ordered,
  # and that each breakpoints vector is 1 element smaller than label_categories.
  # This also assumes our dataframe will have values and species in the "body_mass_g" and "species" columns
  # We'll store the results in a new column called body_mass_cat and return the updated dataframe
  
  # We will be subsetting the dataframe and will need to join them together after proccessing
  dataframes_storage <- list()
  # Iterate through the species first and foremost
  for (species_number in seq(length(species_vector))) {
    # First isolate the rows with the species in question ([rows, cols])
    relevant_entries <- dataframe[dataframe$species == species_vector[species_number],]
    # Get the breakpoints - R does not support vectors in vectors so species_specific_breakpoints must be a list
    species_breakpoints <- species_specific_breakpoints[[species_number]]
    # Make storage vector for our current species
    converted_values <- character(nrow(relevant_entries))
    # Iterate through each of the values for the current species
    relevant_values <- relevant_entries$body_mass_g
    for (value_number in seq(length(relevant_values))) {
      # Deal with NA again
      if (is.na(relevant_values[value_number])) {
        converted_values[value_number] <- NA
        # Continue with the loop
        next
      }
      # Otherwise evaluate if it's less than or equal to each breakpoint at a time
      for (breakpoint_number in seq(length(species_breakpoints))) {
        # If value is less than breakpoint we assign label and stop looping
        if (relevant_values[value_number] <= species_breakpoints[breakpoint_number]) {
          # Assign label 
          converted_values[value_number] <- category_labels[breakpoint_number]
          # Get out of the for loop
          break
        }
      }
      # If label wasn't assigned yet after we hit the last breakpoint it's the last label
      if (converted_values[value_number] == "") {
        # Assign as the last label
        converted_values[value_number] <- tail(category_labels, n=1)
      }
    }
    # Add the converted values to the relevant entries
    relevant_entries$body_mass_sml_by_species <- converted_values
    # Store the finished subset for later joining
    dataframes_storage[[species_number]] <- relevant_entries
  }
  # Join the subsetted dataframes back together
  updated_dataframe <- do.call(rbind, dataframes_storage)
  # End with making a boxplot
  boxplot(body_mass_g ~ species + body_mass_sml_by_species, 
          data=updated_dataframe, names=c("", "Large", "", "", "Medium", "", "", "Small", ""),
          col=c("blue", "green", "yellow"), main="Penguin body masses by size categories and species",
          cex.lab=1, cex.axis=1, xlab="Body size categories", ylab="Body mass (g)")
  legend("topright", fill=c("blue", "green", "yellow"), 
          legend=c("Adelie", "Gentoo", "Chinstrap"))
}

# Run it with penguins
size_categories_by_species_penguins_boxplot <- continuous_to_categorical_by_species_boxplot(dataframe=penguins,
                                                                            species_vector=c("Adelie", "Gentoo", "Chinstrap"),
                                                                            species_specific_breakpoints=list(c(3450, 3900), c(4800, 5350), c(3583, 3833)),
                                                                            category_labels=c("small", "medium", "large"))

# Another way to do it with ggplot thanks to Elizabeth: (requires ggplot2)
alt_boxplot <- ggplot(size_categories_by_species_penguins, aes(x = body_mass_sml_by_species, y = body_mass_g, fill = species)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Penguin Sizes by Species", x = "Body Size Category", y = "Body Mass (g)") +
  theme_classic() +
  scale_x_discrete(labels = c("large" = "Large", "medium" = "Medium", "small" = "Small"))


# Question 5 - the most hands-off version possible with no breakpoint specifications

# No breakpoints needed because we are automatically determining them inside
# Breakpoints determiner is where we have already-established groups with unique breakpoints 
  # Like penguin species (each species gets a set of breakpoints)
# Values to assign group by is the continuous variable
# We'll store the results in a new column called assigned_category and return the updated dataframe
ok_final_function <- function(dataframe, breakpoints_determiner_colname,
                              values_to_assign_group_by_colname, category_labels) {
  # First make the storage column
  dataframe$assigned_category <- ""
  # We will be subsetting the dataframe and will need to join them together after proccessing
  dataframes_storage <- list()
  # First we need to see how many breakpoints we will have
  how_many_breakpoints <- length(category_labels) - 1
  # We will start by establishing the breakpoints for each group
  # Turns out R takes the variable name after $ literally so we need to pull the column with [[]]  
  for (breakpoint_determiner in unique(dataframe[[breakpoints_determiner_colname]])) {
    # Take out the observations in the group to see the distribution
    relevant_values <- dataframe[(dataframe[[breakpoints_determiner_colname]] == breakpoint_determiner), values_to_assign_group_by_colname][[values_to_assign_group_by_colname]]
      # this works too: relevant_values <- subset(dataframe, breakpoints_determiner_colname == breakpoint_determiner)
    # Use quantile to determine our breakpoints (it looks messy but gets rid of the 0th and 100th quantile)
    # This is a double apparently
    breakpoints_for_this_group <- quantile(relevant_values, na.rm=TRUE,
                                           probs=seq(from=(1/(how_many_breakpoints + 1)), 
                                                      to=(how_many_breakpoints/(how_many_breakpoints + 1)), 
                                                      by=(1/(how_many_breakpoints + 1))))
    # Make storage vector for our current species
    converted_values <- character(length(relevant_values))
    # Iterate through each of the values for the current species
    for (value_number in seq(length(relevant_values))) {
      # Deal with NA values
      if (is.na(relevant_values[value_number])) {
        converted_values[value_number] <- NA
        # Continue with the loop
        next
      }
      # Otherwise evaluate if it's less than or equal to each breakpoint at a time
      for (breakpoint_number in seq(length(breakpoints_for_this_group))) {
        # If value is less than breakpoint we assign label and stop looping
        if (as.numeric(relevant_values[value_number]) <= breakpoints_for_this_group[breakpoint_number]) {
          # Assign label 
          converted_values[value_number] <- category_labels[breakpoint_number]
          # Get out of the for loop
          break
        }
      }
      # If label wasn't assigned yet after we hit the last breakpoint it's the last label
      if (converted_values[value_number] == "") {
        # Assign as the last label
        converted_values[value_number] <- tail(category_labels, n=1)
      }
    }
    # Add the converted values to the relevant entries
    dataframe[(dataframe[[breakpoints_determiner_colname]] == breakpoint_determiner), ]$assigned_category <- converted_values
  }
  return(dataframe)
}

# Testing with 5 categories
with_5_categories <- ok_final_function(penguins, "species", "body_mass_g", c("Extra Small", "Small", "Medium", "Large", "Extra Large"))
# Box plot for fun
boxplot(body_mass_g ~ species + assigned_category, 
        data=with_5_categories, names=c("", "XL", "", "", "L", "", "", "M", "", "", "S", "", "", "XS", ""),
        col=c("blue", "green", "yellow"), main="Penguin body masses by size categories and species",
        cex.lab=1, cex.axis=1, xlab="Body size categories", ylab="Body mass (g)")
legend("topright", fill=c("blue", "green", "yellow"), 
       legend=c("Adelie", "Gentoo", "Chinstrap"))

# ggplot way thanks to Elizabeth
second_boxplot_ggplot <- ggplot(with_5_categories, aes(x = assigned_category, y = body_mass_g, fill = species)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Penguin Sizes by Species and Size Categories", x = "Body Size Category", y = "Body Mass (g)") +
  theme_classic()
