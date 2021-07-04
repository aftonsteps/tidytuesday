# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
tuesdata <- tidytuesdayR::tt_load('2021-06-29')
tuesdata <- tidytuesdayR::tt_load(2021, week = 27)

animal_rescues <- tuesdata$animal_rescues

# Or read in the data manually

animal_rescues <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv') %>%
  dplyr::mutate(animal_group_parent = tolower(animal_group_parent))

