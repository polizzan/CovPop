####################################################
# OBSERVED TFR VS. DIFFERENT LINEAR EXTRAPOLATIONS #
####################################################

# ---

#############
# LOAD DATA #
#############

## load unwpp data
tfr.wpp <-
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(Location == "United States of America") %>% 
  filter(Variant == "Medium") %>% 
  filter(Time %in% 2010:2021) %>% 
  select(Year = Time, TFR) 

## load hfd data
tfr.hfd <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "USA.zip"), 
                          filename=paste0("Files/USA/20230328/", "USAasfrRR.txt"))) %>% 
  mutate(Code="USA") %>% 
  relocate(Code) %>% 
  summarize(TFR = sum(ASFR), .by = Year)

## load nvss data
tfr.nvss <-
  data.frame(
    rbind(
      c(2010, 1.9310),
      c(2011, 1.8945),
      c(2012, 1.8805),
      c(2013, 1.8575),
      c(2014, 1.8625),
      c(2015, 1.8435),
      c(2016, 1.8205),
      c(2017, 1.7655),
      c(2018, 1.7295),
      c(2019, 1.7060),
      c(2020, 1.6415),
      c(2021, 1.6640)
    )
  )

names(tfr.nvss) <- c("Year", "TFR")

###################################
# CALCULATE REGRESSION PARAMETERS #
###################################

## based on 2010-2019 data
beta1.wpp.2010 <- 
  cov(tfr.wpp$Year[tfr.wpp$Year %in% 2010:2019], tfr.wpp$TFR[tfr.wpp$Year %in% 2010:2019])/var(tfr.wpp$Year[tfr.wpp$Year %in% 2010:2019])

beta0.wpp.2010 <-
  mean(tfr.wpp$TFR[tfr.wpp$Year %in% 2010:2019]) - beta1.wpp.2010 * mean(tfr.wpp$Year[tfr.wpp$Year %in% 2010:2019])

beta1.hfd.2010 <- 
  cov(tfr.hfd$Year[tfr.hfd$Year %in% 2010:2019], tfr.hfd$TFR[tfr.hfd$Year %in% 2010:2019])/var(tfr.hfd$Year[tfr.hfd$Year %in% 2010:2019])

beta0.hfd.2010 <-
  mean(tfr.hfd$TFR[tfr.hfd$Year %in% 2010:2019]) - beta1.hfd.2010 * mean(tfr.hfd$Year[tfr.hfd$Year %in% 2010:2019])

beta1.nvss.2010 <- 
  cov(tfr.nvss$Year[tfr.nvss$Year %in% 2010:2019], tfr.nvss$TFR[tfr.nvss$Year %in% 2010:2019])/var(tfr.nvss$Year[tfr.nvss$Year %in% 2010:2019])

beta0.nvss.2010 <-
  mean(tfr.nvss$TFR[tfr.nvss$Year %in% 2010:2019]) - beta1.nvss.2010 * mean(tfr.nvss$Year[tfr.nvss$Year %in% 2010:2019])

## compare with lm() command
lm(tfr.wpp$TFR[tfr.wpp$Year %in% 2010:2019] ~ tfr.wpp$Year[tfr.wpp$Year %in% 2010:2019])
lm(tfr.hfd$TFR[tfr.hfd$Year %in% 2010:2019] ~ tfr.hfd$Year[tfr.hfd$Year %in% 2010:2019])
lm(tfr.nvss$TFR[tfr.nvss$Year %in% 2010:2019] ~ tfr.nvss$Year[tfr.nvss$Year %in% 2010:2019])

## based on 2015-2019 data
beta1.wpp.2015 <- 
  cov(tfr.wpp$Year[tfr.wpp$Year %in% 2015:2019], tfr.wpp$TFR[tfr.wpp$Year %in% 2015:2019])/var(tfr.wpp$Year[tfr.wpp$Year %in% 2015:2019])

beta0.wpp.2015 <-
  mean(tfr.wpp$TFR[tfr.wpp$Year %in% 2015:2019]) - beta1.wpp.2015 * mean(tfr.wpp$Year[tfr.wpp$Year %in% 2015:2019])

beta1.hfd.2015 <- 
  cov(tfr.hfd$Year[tfr.hfd$Year %in% 2015:2019], tfr.hfd$TFR[tfr.hfd$Year %in% 2015:2019])/var(tfr.hfd$Year[tfr.hfd$Year %in% 2015:2019])

beta0.hfd.2015 <-
  mean(tfr.hfd$TFR[tfr.hfd$Year %in% 2015:2019]) - beta1.hfd.2015 * mean(tfr.hfd$Year[tfr.hfd$Year %in% 2015:2019])

beta1.nvss.2015 <- 
  cov(tfr.nvss$Year[tfr.nvss$Year %in% 2015:2019], tfr.nvss$TFR[tfr.nvss$Year %in% 2015:2019])/var(tfr.nvss$Year[tfr.nvss$Year %in% 2015:2019])

beta0.nvss.2015 <-
  mean(tfr.nvss$TFR[tfr.nvss$Year %in% 2015:2019]) - beta1.nvss.2015 * mean(tfr.nvss$Year[tfr.nvss$Year %in% 2015:2019])

## compare with lm() command
lm(tfr.wpp$TFR[tfr.wpp$Year %in% 2015:2019] ~ tfr.wpp$Year[tfr.wpp$Year %in% 2015:2019])
lm(tfr.hfd$TFR[tfr.hfd$Year %in% 2015:2019] ~ tfr.hfd$Year[tfr.hfd$Year %in% 2015:2019])
lm(tfr.nvss$TFR[tfr.nvss$Year %in% 2015:2019] ~ tfr.nvss$Year[tfr.nvss$Year %in% 2015:2019])

########
# PLOT #
########

## unwpp
svg(file = here::here("out", "other-output", "tfr-wpp-rr2.svg"), width = 7.5, height = 5)
 
plot(2010:2021, tfr.wpp$TFR[tfr.wpp$Year %in% 2010:2021], 
     type = "p", ylim = c(1.5, 2), cex.lab =  1.2, 
     xlab = "Year", ylab = "Total Fertility Rate",
     main = "UN World Population Prospects")
axis(1, at = 2010:2021)
axis(2, at = seq(1.5, 2, 0.1))
lines(2010:2021, (beta0.wpp.2010 + beta1.wpp.2010 * 2010:2021))
lines(2015:2021, (beta0.wpp.2015 + beta1.wpp.2015 * 2015:2021), col = "red")
legend("bottomleft", 
       inset = 0.05,
       legend = c("Observed", paste0("Trend 2010", "\U2013", "2019"), paste0("Trend 2015", "\U2013", "2019")),
       bty = "n",
       col = c("black", "black", "red"), 
       cex = c(1, 1, 1), pch = c(1, NA, NA), x.intersp = 0.5, pt.cex = c(1.5, 0, 0), 
       seg.len = 0.5, lty = c(NA, 1, 1)
)

dev.off()

## hfd
svg(file = here::here("out", "other-output", "tfr-hfd-rr2.svg"), width = 7.5, height = 5)

plot(2010:2021, tfr.hfd$TFR[tfr.hfd$Year %in% 2010:2021], 
     type = "p", ylim = c(1.5, 2), cex.lab =  1.2, 
     xlab = "Year", ylab = "Total Fertility Rate",
     main = "Human Fertility Database")
axis(1, at = 2010:2021)
axis(2, at = seq(1.5, 2, 0.1))
lines(2010:2021, (beta0.hfd.2010 + beta1.hfd.2010 * 2010:2021))
lines(2015:2021, (beta0.hfd.2015 + beta1.hfd.2015 * 2015:2021), col = "red")
legend("bottomleft", 
       inset = 0.05,
       legend = c("Observed", paste0("Trend 2010", "\U2013", "2019"), paste0("Trend 2015", "\U2013", "2019")),
       bty = "n",
       col = c("black", "black", "red"), 
       cex = c(1, 1, 1), pch = c(1, NA, NA), x.intersp = 0.5, pt.cex = c(1.5, 0, 0), 
       seg.len = 0.5, lty = c(NA, 1, 1)
)

dev.off()

## nvss
svg(file = here::here("out", "other-output", "tfr-nvss-rr2.svg"), width = 7.5, height = 5)

plot(2010:2021, tfr.nvss$TFR[tfr.nvss$Year %in% 2010:2021], 
     type = "p", ylim = c(1.5, 2), cex.lab =  1.2, 
     xlab = "Year", ylab = "Total Fertility Rate",
     main = "National Vital Statistics System")
axis(1, at = 2010:2021)
axis(2, at = seq(1.5, 2, 0.1))
lines(2010:2021, (beta0.nvss.2010 + beta1.nvss.2010 * 2010:2021))
lines(2015:2021, (beta0.nvss.2015 + beta1.nvss.2015 * 2015:2021), col = "red")
legend("bottomleft", 
       inset = 0.05,
       legend = c("Observed", paste0("Trend 2010", "\U2013", "2019"), paste0("Trend 2015", "\U2013", "2019")),
       bty = "n",
       col = c("black", "black", "red"), 
       cex = c(1, 1, 1), pch = c(1, NA, NA), x.intersp = 0.5, pt.cex = c(1.5, 0, 0), 
       seg.len = 0.5, lty = c(NA, 1, 1)
)

dev.off()

#####################
# CLEAR ENVIRONMENT #
#####################

rm(list=ls())