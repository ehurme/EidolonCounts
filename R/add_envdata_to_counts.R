# Update env data thru 2025
library(data.table)
library(tidyverse)
library(scales)

# load colony data
counts <- fread("../../../Dropbox/MPI/Eidolon/Data/Count/2024-12-11_counts_summaries.csv")
idx_month <- which(is.na(counts$Month))
counts$Month[idx_month] <- dmy(counts$Date[idx_month])
summary(counts$Month)

# load env data
evi <- fread("../../../Dropbox/GreenWave/geemap/africa_evi_timeseries.csv")
prp <- fread("../../../Dropbox/GreenWave/geemap/chirp_precipitation.csv")

plot(evi$date, evi$Accra,
     type = "l",
     ylim = c(0,1),
     col = "darkgreen")
lines(prp$date, prp$Accra/100, col = 4)

colonies <- colnames(evi)[-1]
# colonies[which(colonies == "Ngamba Island, Lake Victoria, Mukono District")] <- "Ngamba"
colonies
counts$`City/Place` %>% unique()

# environmental
counts$evi <- NA
counts$irg <- NA
counts$precip <- NA
counts$precip_p <- NA

# anthropogenic change
counts$human_footprint_index <- NA
counts$forest_change <- NA
counts$night_light <- NA

# static variables
## forest proximate people https://developers.google.com/earth-engine/datasets/catalog/FAO_SOFO_1_FPP#description

counts$Comments

i = 1
for(i in 1:length(colonies)){

  e <- evi %>% select(date, colonies[i])
  p <- prp %>% select(date, colonies[i])

  # par(mar = c(1,4,1,1), oma = c(1,1,1,1), xpd=FALSE)
  # layout(rbind(1,2))

  time <- e[,1] %>% unlist()
  EVI <- e[,2] %>% unlist()
  spl <- smooth.spline(x = time, y = EVI)
  # plot(spl, type = "l")
  pred <- predict(spl)

  timep <- p[,1] %>% unlist()
  PRP <- p[,2] %>% unlist()
  spl_p <- smooth.spline(x = timep, y = PRP)
  # plot(spl_p, type = "l")
  predp <- predict(spl_p)

  e$pred <- pred$y
  p$pred <- predp$y

  ## get derivative of EVI -> IRG
  pred.prime <- predict(spl, deriv=1)
  # plot(spl, type = "l", ylim = c(-1, 1))
  pred.prime$y_scale <- scales::rescale(pred.prime$y, to= c(-1,1))
  # lines(pred.prime$x, pred.prime$y_scale, col = 2)
  e$IRG <- pred.prime$y_scale

  ## get derivative of precipitation
  predp.prime <- predict(spl_p, deriv=1)
  # plot(spl_p, type = "l")
  predp.prime$y_scale <- scales::rescale(predp.prime$y, to= c(-1,1))
  # lines(predp.prime$x, predp.prime$y_scale, col = 2)
  p$IRP <- predp.prime$y_scale

  # plot first derivatives
  # plot(e$date, e$IRG, type = "l",
  #      ylab = "scaled IRG", col = 2,
  #      ylim = c(-1, 1), xlab = "Date")
  # lines(p$date, p$pred.prime, col = 4)

  ## add EVI, IRG, and Precip to reg colonies
  c_idx <- which(counts$`City/Place` == colonies[i])
  if(length(c_idx) == 0){
    c_idx <- which(substr(colonies[i], 1, 5) == substr(counts$`City/Place`, 1, 5))
  }
  j = 1
  for(j in 1:length(c_idx)){
    idxe <- which.min(abs(as.Date(counts$Month[c_idx[j]]) -
                    as.Date(e$date)))
    # counts$Month[c_idx[j]]
    # e$date[idx]

    counts$evi[c_idx[j]] <- e$pred[idxe]
    counts$irg[c_idx[j]] <- e$IRG[idxe]

    idxp <- which.min(abs(as.Date(counts$Month[c_idx[j]]) -
                            as.Date(p$date)))
    # counts$Month[c_idx[j]]
    # p$date[idxp]
    counts$precip[c_idx[j]] <- p$pred[idxp]
    counts$precip_p[c_idx[j]] <- p$IRP[idxp]
  }
}
summary(counts)

hist(counts$precip, breaks = 100)
counts$count <- counts$Count %>% as.numeric()
# counts$count[is.na(counts$count)] <- 0

library(mgcv)
counts$year <- counts$Month %>% year()
counts$loc <- counts$`City/Place` %>% factor()
counts$precip[counts$precip < 0] <- 0
counts$mon <- month(counts$Month)

m <- gam(count ~
           s(evi)+
           s(irg)+
           s(precip)+
           s(precip_p)+
           s(mon)+
           s(year)+
           s(loc, bs = "re"),
         method = "REML",
         family = nb,
         data = counts %>% filter(loc != "Kasanka"))
summary(m)
plot(m, ask = FALSE, pages = 1)

mgcv::gam.check(m)

# how does peak colony size per year change?
summary_data <- counts %>%
  group_by(year, loc) %>%
  summarize(
    max_count = suppressWarnings(max(count, na.rm = TRUE)),  # Suppress -Inf warnings
    EVI = evi[which.max(count)[1]],  # [1] handles empty which.max results
    IRG = irg[which.max(count)[1]],
    PRP = precip[which.max(count)[1]],
    IRP = precip_p[which.max(count)[1]],
    .groups = 'drop'
  ) %>%
  mutate(max_count = na_if(max_count, -Inf))  # Convert -Inf to NA

m_max <- gam(max_count ~
           # s(EVI)+
           # s(IRG)+
           # s(PRP)+
           # s(IRP)+
           s(year)+
           s(loc, bs = "re"),
         method = "REML",
         # family = nb,
         data = summary_data %>% filter(loc != "Kasanka"))
summary(m_max)
plot(m_max, ask = FALSE, pages = 1)


## Update to include environmental factors in the model
counts %>% reframe(max_count = max(count, na.rm = TRUE),
                   EVI = evi[which.max(count, na.rm = TRUE)],
                   IRG = irg[which.max(count, na.rm = TRUE)],
                   PRP = precip[which.max(count, na.rm = TRUE)],
                   IRP = precip_p[which.max(count, na.rm = TRUE)],
                   .by = c(year, loc))



# years <- unique(year(e$time))
  # e$year <- year(e$time)
  #
  # # to account for double peaks in a season, we will  pull out half year estimates
  # e$halfyear <- year(e$time)+round(month(e$time)/12, 0)/2
  # e_sum <- e %>% group_by(halfyear, year, geeID) %>%
  #   dplyr::summarise(EVI_spline = time[which.max(pred)],
  #                    IRG_spline = time[which.max(IRG)],
  #                    EOS_spline = time[which.min(IRG)],
  #                    max_IRG = IRG[which.max(IRG)],
  #                    min_IRG = IRG[which.min(IRG)],
  #                    max_EVI = pred[which.max(pred)],
  #                    min_EVI = pred[which.min(pred)],
  #                    entropy = mean(entropy, na.rm = TRUE),
  #                    seasonality = mean(seasonality, na.rm = TRUE))
  #
  # summary(p)
  # p$year <- year(p$time)
  # p$geeID <- ID
  # p_sum <- p %>% group_by(year, geeID) %>%
  #   dplyr::summarise(precip_spline = time[which.max(precip)],
  #                    precip_pspline = time[which.max(pred.prime)])
  #
  # MEVI$year <- year(MEVI$date_EVI)
  # m_sum1 <- MEVI[MEVI$geeID == ID,] %>% group_by(geeID, year) %>%
  #   dplyr::summarise(EVI1_model = date_EVI[which(peak == 1)],
  #                    EVI_model = date_EVI[which.max(amp)],
  #                    IRG1_model = date_IRG[which(peak == 1)],
  #                    IRG_model = date_IRG[which.max(amp)],
  #                    amp = amp[which.max(amp)])
  #
  # m_sum2 <- MEVI[MEVI$geeID == ID,] %>% group_by(geeID, year) %>%
  #   dplyr::summarise(EVI2_model = date_EVI[which(peak == 2)],
  #                    IRG2_model = date_IRG[which(peak == 2)])
  # m_sum <- full_join(m_sum1, m_sum2)
  #
  # C <- full_join(e_sum, m_sum)
  # C <- full_join(C, p_sum)
  # rs <- rbind(rs, C)
# }


save(counts, file = "./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/colony_counts_EVI_PRP_20250428.Rdata")
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/colony_counts_EVI_PRP_20250428.Rdata")
# load("../../../../Dropbox/MPI/Eidolon/Greenwave/rdata/regular_colonies_TRMM.Rdata")
write.csv(counts, file = "./../../../Dropbox/MPI/Eidolon/Greenwave/colony_counts_EVI_PRP_20250428.csv")

