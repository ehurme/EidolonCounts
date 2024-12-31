# explore gam model
library(pacman)
p_load(tidyverse,lubridate,
       ggplot2,
       glmmTMB, # generalize linear mixed models
        mgcv, gamm4 # Generalized additive model packages
        )

load("../../../Dropbox/MPI/Eidolon/GreenWave/rdata/colonies_EVI_PRP.Rdata")

colonies$evi

colonies$ratio %>% plot()

# check distribution
colonies$Count %>% hist()
colonies$Count %>% log %>% hist()
# https://fromthebottomoftheheap.net/2017/05/04/compare-mgcv-with-glmmtmb/

ggplot(colonies)+
  geom_path(aes(date, ratio, group = Location))+
  geom_path(aes(date, evi), col = "green", lwd = 1)+
  geom_path(aes(date, irg), col = "orange", lwd = 1)+
  geom_path(aes(date, precip), col = "cyan", lwd = 1)+
  geom_path(aes(date, precip_p), col = "purple", lwd = 1)+

  facet_wrap(~Location, scales = "free_y")+theme_classic()

ggplot(colonies, aes(precip, evi))+
  geom_point()+geom_smooth(method = "lm")
ggplot(colonies, aes(precip_p, irg))+
  geom_point()+geom_smooth(method = "lm")
psych::cor.plot(colonies[,c("evi", "irg", "precip", "precip_p")])

colonies$Loc <- factor(colonies$Location)
colonies$year
m1 <- gam(#log(Count+1) ~
          Count ~
            s(evi)+s(irg)+
            s(precip)+s(precip_p)+
            s(year)+
            # human impact
          # forest loss
          # s(Long, Lat)+ # search for spatial gams and add get this working
            s(Loc, bs = "re"),
          family = nb,
          method = "REML",
  data = colonies, #  %>% filter(Location != "Kasanka")
)
summary(m1)

plot(m1, ask = FALSE, pages = 1)

k.check(m1)
par(mfrow = c(2,2))
gam.check(m1)

## References for info on GAMs
# https://r.qcbs.ca/workshop08/book-en/gam-model-checking.html