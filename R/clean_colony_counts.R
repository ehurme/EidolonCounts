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
new_counts <- read.csv("../../../Dropbox/MPI/Eidolon/Data/Count/new_counts2024.csv")

new_counts$Country %>% table()

summary(old_counts)
summary(new_counts)

new_counts$Date <- ymd(new_counts$Month) # make it a time variable
new_counts$Month <- {}

old_counts$Date <- dmy(old_counts$Date)

counts <- full_join(old_counts, new_counts)

## rename locations
table(counts$Location)
# Yaounde
counts$Location[grep("Yaounde", counts$Location)] <- "Yaounde"
# Freetown
counts$Location[grep("Freetown", counts$Location)] <- "Freetown"
counts$Lat[grep("Freetown", counts$Location)] <- counts$Lat[grep("Freetown", counts$Location)][1]
counts$Long[grep("Freetown", counts$Location)] <- counts$Long[grep("Freetown", counts$Location)][1]

write.csv(counts, file = "full_counts.csv")

# clean up multiple locations for a single site
unique(paste(counts$Long, counts$Lat, counts$Location))
unique_locations <- unique(counts[,c("Lat", "Long", "Location", "Country")])

"29.7818 -2.228383333 Ruhango, Southern Province"
ru <- unique_locations[unique_locations$Location == "Ruhango, Southern Province",]
which.min(abs(ru$Lat - median(ru$Lat))) # 5
which.min(abs(ru$Long - median(ru$Long)))

with(ru, plot(Long, Lat, asp = 1))
points(ru$Long[5], ru$Lat[5], col = 2)
points(ru$Long[7], ru$Lat[7], col = 2)

ru$Lat[5]
ru$Long[5]

unique_locations$Lat[unique_locations$Location == "Ruhango, Southern Province"] <- ru$Lat[5]
unique_locations$Long[unique_locations$Location == "Ruhango, Southern Province"] <- ru$Long[5]

"29.2598 -1.704383333 Serena Hotel Gisenyi, Lake Kivu"
kivu <- unique_locations[unique_locations$Location == "Serena Hotel Gisenyi, Lake Kivu",]
unique_locations$Lat[unique_locations$Location == "Serena Hotel Gisenyi, Lake Kivu"] <- kivu$Lat[1]
unique_locations$Long[unique_locations$Location == "Serena Hotel Gisenyi, Lake Kivu"] <- kivu$Long[1]

"29.667296 -2.791267 Akanyaru, Southern Province"
ak <- unique_locations[unique_locations$Location == "Akanyaru, Southern Province",]
unique_locations$Lat[unique_locations$Location == "Akanyaru, Southern Province"] <- ak$Lat[1]
unique_locations$Long[unique_locations$Location == "Akanyaru, Southern Province"] <- ak$Long[1]

"Ngamba Island, Lake Victoria, Mukono District"
ng <- unique_locations[unique_locations$Location == "Ngamba Island, Lake Victoria, Mukono District",]
unique_locations$Lat[unique_locations$Location == "Ngamba Island, Lake Victoria, Mukono District"] <- ng$Lat[1]
unique_locations$Long[unique_locations$Location == "Ngamba Island, Lake Victoria, Mukono District"] <- ng$Long[1]

df <- unique(unique_locations)
df$geeIndex <- 1:nrow(df)

plot(df$Long, df$Lat, col = as.factor(df$Location), pch = 16, asp = 1)

df_coord <- st_as_sf(df, coords = c(2,1))

ggplot(data = countries110)+
  geom_sf(fill = "white")+
  geom_sf(data = df_coord %>% st_set_crs(st_crs(countries110)),
          aes(col = Location), size = 3)+
  xlim(c(-15,35))+ylim(-20, 20)+
  theme(legend.position = "bottom")

plot(df_coord)
st_write(df_coord %>% st_set_crs(st_crs(countries110)), "unique_eidolon_colonies24.shp", append = TRUE)

write.csv(df, file = "unique_eidolon_colonies24.csv")
