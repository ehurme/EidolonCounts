# summarize the count data
library(pacman)
p_load(tidyverse, lubridate, mgcv, lme4, sjPlot)

# load data
counts <- read.csv("../../../Dropbox/MPI/Eidolon/Data/Count/2024-12-11_counts_summaries.csv")

# round dates to monthly
counts$Month <- ymd(counts$Month)
counts$Month[which(is.na(counts$Month))] <- dmy(counts$Date[which(is.na(counts$Month))]) %>%
  round_date(unit = "month")

# make sure counts are numeric
counts$Count <- counts$Count %>% as.numeric()

# replace empty city.place
counts$City.Place[which(counts$City.Plac == "")] <- counts$Locality[which(counts$City.Plac == "")]
table(counts$City.Place)

colonies <- counts %>% group_by(City.Place) %>%
  reframe(lat = first(lat), long = first(long))
write.csv(colonies, file = "../../../Dropbox/GreenWave/geemap/colonies.csv")

counts %>% reframe(
  start_year = year(min(Month, na.rm = TRUE)),
  end_year = year(max(Month, na.rm = TRUE)),
  duration_in_years = round(difftime(max(Month, na.rm = TRUE),
                                     min(Month, na.rm = TRUE),
                                     units = "days")/365, 1) %>% as.numeric(),
  missing_counts = length(which(is.na(Count))),
  max_size = max(Count, na.rm = TRUE),
  size_0 = length(which(Count == 0)),
  total_counts = n(),
    .by = c(City.Place, Country))


# are missing counts distributed evenly?

# tally counts by year?


# which months have no bats?
counts <- counts[order(counts$Month),]
ggplot(counts,#[counts$City.Place != "Kasanka",],
       aes(x = month(Month), y = Count, #/max(Count, na.rm = TRUE),
                   group = City.Place,
                   col = City.Place))+
  geom_point()+#geom_path()+
  geom_smooth(aes(group = City.Place), se = FALSE)+
      # , method = "gam", formula = y ~ s(x,  bs="cc"))+
  ylab("# of bats")+
  xlab("Month")+
  xlim(0,12)+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~Country, scales = "free_y")

# polar plot?

ggsave(filename = "../../../Dropbox/MPI/Eidolon/Plots/eidolon_colony_counts.svg", width = 14, height = 10)
# add to a map or compare distances between colonies and
# how they may be correlated or anti-correlated in time

counts$year <- year(counts$Month)
counts$loc <- factor(counts$City.Place)

# How does max count change over time?
yearly_max_count <- counts %>% reframe(
  max_size = max(Count, na.rm = TRUE),
  size_0 = length(which(Count == 0)),
  total_counts = n(),
  total_months = length(unique(Month)),
  .by = c(City.Place, Country, year))

yearly_max_count %>% filter(City.Place == "Kasanka")

ggplot(yearly_max_count %>% filter(total_counts > 3),
       aes(year, max_size, color = City.Place))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Country, scales = "free_y")+
  theme_bw()+
  guides(col=guide_legend(ncol=3))+
  theme(legend.position = "none")

max_model <- glm(max_size ~ year + (1/City.Place),
  data = yearly_max_count %>%
    filter(max_size > 0, City.Place != "Kasanka"))
summary(max_model)
tab_model(max_model)
