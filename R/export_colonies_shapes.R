# Export colony locations as shp files

# loading libraries to analyze the data
library(pacman) # easier to load other packages
p_load(tidyverse, # tools for working with data
       lubridate, # work with time
       data.table, # faster loading of large datasets
       ggplot2, # plotting data
       rnaturalearth, # global maps
       sf) # making maps

# load the data
old_counts <- read.csv("../../../Dropbox/MPI/Eidolon/Data/Count/clean_colonies.csv")
new_counts <- read.csv("../../../Dropbox/MPI/Eidolon/Data/Count/2024-12-09_counts_summaries.csv")

new_counts %>% View()
new_counts$City.Place[which(new_counts$City.Place == "")] <- new_counts$Locality[which(new_counts$City.Place == "")]
table(new_counts$City.Place)
table(new_counts$Country)

new_counts$Date <- ymd(new_counts$Month) # make it a time variable
new_counts$Month <- {}
