library(move2)
library(units)
source("../../../Desktop/movebank_login.R")

study = 1504383381
e <- move2::movebank_download_study(study_id = study)
e$tag_voltage
coords <- st_coordinates(e)
e$longitude <- coords[,1]
e$latitude <- coords[,2]

ggplot(e, aes(longitude, latitude, col = tag_voltage %>% as.numeric(),
              group = individual_local_identifier))+
  geom_point()+geom_path()

e <- e %>% group_by(individual_local_identifier) %>%
  mutate(time_deployed = difftime(timestamp, min(timestamp), units = "days") %>% round(1))
e$time_deployed

ggplot(e, aes(time_deployed, tag_voltage %>% as.numeric()/1000,
              col = individual_local_identifier))+
  geom_point()+
  geom_smooth()+
  ylab("Tag Voltage (V)")+
  xlab("Time deployed (days)")+
  theme_minimal()

data <- mt_track_data(e)
data$deploy_on_timestamp
