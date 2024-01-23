######################################################################
# COMPARE MORTALITY AND FERTILITY INFORMATION FROM DIFFERENT SOURCES #
######################################################################

# ---

#########################
# LOAD SINGLE-AGE ASFRs #
#########################

## auxiliary data set with zero fertility
df.1 <- data.frame(
  Year = rep(2019:2021, each = length(c(0:14, 50:100))),
  Age = rep(c(0:14, 50:100), length(2019:2021)),
  ASFR = rep(rep(0, length(c(0:14, 50:100))), length(2019:2021))
)

## load asfrs from WPP
WPP.asfr.1 <-
  read_csv(here::here("data", "WPP2022_Fertility_by_Age1.zip")) %>% 
  filter(Location == "United States of America") %>%
  filter(Variant == "Medium") %>% 
  mutate(ASFR = ASFR/1000) %>%
  select(Year=Time, Age=AgeGrp, ASFR) %>% 
  add_row(df.1) %>% ## add zero fertility data set
  arrange(Year, Age)

## load asfrs from HFD
HFD.asfr.1 <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "USA.zip"), 
                        filename=paste0("Files/USA/20230328/", "USAasfrRR.txt"))) %>% 
  mutate(Code="USA") %>% 
  relocate(Code)

########################
# LOAD FIVE-YEAR ASFRs #
########################

## auxiliary data set with zero fertility
df.5 <- data.frame(
  Year = rep(2019:2021, each = length(c(0:9, 55:100))),
  Age = rep(c(0:9, 55:100), length(2019:2021)),
  ASFR = rep(rep(0, length(c(0:9, 55:100))), length(2019:2021))
)

## load asfrs from WPP
WPP.asfr.5 <-
  read_csv(here::here("data", "WPP2022_Fertility_by_Age5.zip")) %>% 
  filter(Location == "United States of America") %>%
  filter(Variant == "Medium") %>% 
  mutate(ASFR = ASFR/1000) %>%
  select(Year=Time, Age=AgeGrp, ASFR) %>% 
  group_by(Year) %>% 
  mutate(Age=c(10, 15, 20, 25, 30, 35, 40, 45, 50)) %>% 
  ungroup() %>% 
  add_row(df.5) %>% ## add zero fertility data set
  arrange(Year, Age)

## load asfrs from NVSS
NVSS.asfr.5 <-
  data.frame(
    Year=rep(c(2019, 2020, 2021), each=8),
    Age=rep(c(10, 15, 20, 25, 30, 35, 40, 45), 3),
    Source="NVSS",
    ASFR=c(
      0.2, ## 2019
      16.7, 
      66.6,
      93.7,
      98.3,
      52.8,
      12.0,
      0.9,
      0.2, ## 2020
      15.0, 
      63.3, 
      90.9, 
      94.9, 
      51.3, 
      11.8, 
      0.9,
      0.2, ## 2021
      13.9,
      61.5,
      93.0,
      97.6,
      53.7, 
      12.0,
      0.9
    )
  ) %>% 
  mutate(ASFR=ASFR/1000)

############
# LOAD mxs #
############

## load life tables from WPP
WPP.LT.m <-
  read_csv(here::here("data", "WPP2022_Life_Table_Complete_Medium_Male_1950-2021.zip")) %>% 
  filter(Location=="United States of America") %>% 
  filter(Variant=="Medium") %>% 
  select(Year=Time, Sex, Age=AgeGrpStart, mx:ax) 

WPP.LT.f <-
  read_csv(here::here("data", "WPP2022_Life_Table_Complete_Medium_Female_1950-2021.zip")) %>% 
  filter(Location=="United States of America") %>% 
  filter(Variant=="Medium") %>% 
  select(Year=Time, Sex, Age=AgeGrpStart, mx:ax) 

## load life tables from HMD
HMD.LT.m <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_20230403.zip"), 
                          filename=paste0("lt_male/mltper_1x1/", "USA", ".mltper_1x1.txt"))) %>%
  mutate(Sex = "Male")

HMD.LT.f <-
  HMDHFDplus::readHMD(unz(description=here::here("data", "hmd_statistics_20230403.zip"), 
                          filename=paste0("lt_female/fltper_1x1/", "USA", ".fltper_1x1.txt"))) %>%
  mutate(Sex = "Female")

############
# LOAD qxs #
############

## load NVSS probabilities of death 
NVSS.qx <-
  data.frame(
    Year=rep(rep(c(2019, 2020), each=101), 2),
    Age=rep(0:100, 4),
    Sex=rep(c("Male", "Female"), each=202),
    Source="NVSS",
    qx=c(
      0.006080, ## male, 2019
      0.000415,
      0.000257,
      0.000193,
      0.000153,
      0.000149,
      0.000137,
      0.000126,
      0.000113,
      0.000101,
      0.000095,
      0.000106,
      0.000145,
      0.000221,
      0.000324,
      0.000438,
      0.000554,
      0.000679,
      0.000809,
      0.000940,
      0.001077,
      0.001211,
      0.001324,
      0.001407,
      0.001465,
      0.001514,
      0.001564,
      0.001614,
      0.001669,
      0.001730,
      0.001794,
      0.001858,
      0.001926,
      0.001998,
      0.002072,
      0.002154,
      0.002241,
      0.002323,
      0.002399,
      0.002476,
      0.002569,
      0.002685,
      0.002824,
      0.002983,
      0.003162,
      0.003365,
      0.003596,
      0.003856,
      0.004154,
      0.004497,
      0.004867,
      0.005284,
      0.005789,
      0.006385,
      0.007034,
      0.007687,
      0.008338,
      0.009024,
      0.009768,
      0.010575,
      0.011445,
      0.012342,
      0.013242,
      0.014130,
      0.015033,
      0.016003,
      0.017154,
      0.018348,
      0.019601,
      0.020915,
      0.022303,
      0.023930,
      0.025553,
      0.028257,
      0.030698,
      0.033891,
      0.037078,
      0.041154,
      0.045300,
      0.049951,
      0.054931,
      0.060709,
      0.067466,
      0.074945,
      0.083495,
      0.093585,
      0.103957,
      0.116315,
      0.129805,
      0.144448,
      0.160250,
      0.177194,
      0.195239,
      0.214316,
      0.234332,
      0.255162,
      0.276659,
      0.298653,
      0.320956,
      0.343371,
      1.000000,
      0.005849, ## male, 2020
      0.000403,
      0.000259,
      0.000208,
      0.000155,
      0.000144,
      0.000132,
      0.000121,
      0.000109,
      0.000096,
      0.000091,
      0.000107,
      0.000159,
      0.000255,
      0.000385,
      0.000529,
      0.000676,
      0.000831,
      0.000991,
      0.001152,
      0.001320,
      0.001483,
      0.001620,
      0.001717,
      0.001785,
      0.001840,
      0.001899,
      0.001966,
      0.002050,
      0.002148,
      0.002251,
      0.002351,
      0.002448,
      0.002539,
      0.002627,
      0.002722,
      0.002827,
      0.002931,
      0.003033,
      0.003140,
      0.003264,
      0.003411,
      0.003580,
      0.003769,
      0.003983,
      0.004231,
      0.004515,
      0.004831,
      0.005181,
      0.005570,
      0.005985,
      0.006450,
      0.007004,
      0.007657,
      0.008381,
      0.009115,
      0.009859,
      0.010668,
      0.011568,
      0.012548,
      0.013599,
      0.014668,
      0.015723,
      0.016751,
      0.017793,
      0.018910,
      0.020241,
      0.021617,
      0.023122,
      0.024700,
      0.026327,
      0.028145,
      0.030318,
      0.032487,
      0.036455,
      0.039507,
      0.043893,
      0.048013,
      0.053409,
      0.058234,
      0.064014,
      0.070301,
      0.077280,
      0.086551,
      0.095951,
      0.107089,
      0.116675,
      0.130906,
      0.146410,
      0.163192,
      0.181227,
      0.200462,
      0.220810,
      0.242150,
      0.264330,
      0.287167,
      0.310455,
      0.333969,
      0.357477,
      0.380747,
      1.000000,
      0.005045, ## female, 2019
      0.000341,
      0.000209,
      0.000166,
      0.000137,
      0.000125,
      0.000112,
      0.000102,
      0.000096,
      0.000093,
      0.000095,
      0.000102,
      0.000116,
      0.000139,
      0.000170,
      0.000204,
      0.000241,
      0.000280,
      0.000319,
      0.000360,
      0.000404,
      0.000449,
      0.000490,
      0.000524,
      0.000553,
      0.000579,
      0.000608,
      0.000644,
      0.000690,
      0.000746,
      0.000808,
      0.000870,
      0.000933,
      0.000992,
      0.001049,
      0.001112,
      0.001178,
      0.001240,
      0.001295,
      0.001351,
      0.001414,
      0.001493,
      0.001593,
      0.001714,
      0.001851,
      0.002005,
      0.002172,
      0.002347,
      0.002534,
      0.002741,
      0.002964,
      0.003217,
      0.003519,
      0.003869,
      0.004246,
      0.004623,
      0.005000,
      0.005405,
      0.005853,
      0.006346,
      0.006891,
      0.007455,
      0.008004,
      0.008520,
      0.009033,
      0.009573,
      0.010236,
      0.011030,
      0.012018,
      0.013187,
      0.014484,
      0.015956,
      0.017382,
      0.019431,
      0.021198,
      0.023549,
      0.025917,
      0.029002,
      0.032220,
      0.035944,
      0.040043,
      0.044578,
      0.050431,
      0.056908,
      0.063205,
      0.071076,
      0.080252,
      0.090433,
      0.101682,
      0.114054,
      0.127590,
      0.142319,
      0.158250,
      0.175367,
      0.193631,
      0.212972,
      0.233293,
      0.254465,
      0.276333,
      0.298719,
      1.000000, 
      0.004918, ## female, 2020
      0.000310,
      0.000199,
      0.000161,
      0.000123,
      0.000115,
      0.000103,
      0.000094,
      0.000089,
      0.000087,
      0.000089,
      0.000098,
      0.000117,
      0.000147,
      0.000185,
      0.000230,
      0.000276,
      0.000324,
      0.000371,
      0.000418,
      0.000468,
      0.000519,
      0.000570,
      0.000617,
      0.000662,
      0.000705,
      0.000750,
      0.000798,
      0.000853,
      0.000914,
      0.000978,
      0.001044,
      0.001111,
      0.001176,
      0.001243,
      0.001315,
      0.001393,
      0.001471,
      0.001548,
      0.001629,
      0.001720,
      0.001826,
      0.001946,
      0.002079,
      0.002226,
      0.002393,
      0.002579,
      0.002780,
      0.002996,
      0.003231,
      0.003484,
      0.003768,
      0.004097,
      0.004475,
      0.004885,
      0.005300,
      0.005725,
      0.006192,
      0.006717,
      0.007296,
      0.007928,
      0.008578,
      0.009217,
      0.009834,
      0.010462,
      0.011129,
      0.011932,
      0.012871,
      0.014000,
      0.015265,
      0.016693,
      0.018272,
      0.020046,
      0.021730,
      0.024519,
      0.026862,
      0.029942,
      0.033037,
      0.037086,
      0.041213,
      0.045945,
      0.051104,
      0.057111,
      0.064163,
      0.072353,
      0.081451,
      0.090029,
      0.101896,
      0.115015,
      0.129437,
      0.145190,
      0.162282,
      0.180688,
      0.200353,
      0.221184,
      0.243052,
      0.265792,
      0.289207,
      0.313074,
      0.337151,
      1.000000))

##################
# SAVE DATA SETS #
##################

save(
  WPP.asfr.1,
  WPP.asfr.5,
  WPP.LT.f,
  WPP.LT.m,
  HFD.asfr.1,
  HMD.LT.f,
  HMD.LT.m,
  NVSS.asfr.5,
  NVSS.qx,
  file = here::here("data", "compare-sources.RData")
)

rm(list=ls())

##################
# LOAD DATA SETS #
##################

load(file = here::here("data", "compare-sources.RData"))

## determine color scale
sources.pal <- RColorBrewer::brewer.pal(n=9, "RdBu")[c(3, 8, 1)]

############################
# COMPARE SINGLE-AGE ASFRs #
############################

## determine axis labels
asfr.1.label <-
  plyr::round_any(
      max(WPP.asfr.1 %>% filter(Year %in% 2019:2021) %>% pull(ASFR),
      HFD.asfr.1 %>% filter(Year %in% 2019:2021) %>% pull(ASFR)),
      f=ceiling,
      accuracy=0.02
  )

## plot asfrs from different sources alongside each other
asfr.1.comparison <-  
  WPP.asfr.1 %>% 
  select(Year, Age, ASFR) %>% 
  mutate(Source="WPP") %>% 
  add_row(HFD.asfr.1 %>% select(Year, Age, ASFR) %>% mutate(Source="HFD")) %>% 
  filter(Year %in% 2019:2021) %>% 
  ggplot(aes(x=Age, y=ASFR, color=Source)) +
  geom_area(data = . %>% filter(Source=="HFD"), 
            aes(x=Age, y=ASFR), 
            fill = sources.pal[1],
            show.legend = FALSE) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(10, 60, 5)) +
  scale_y_continuous(breaks = seq(0, asfr.1.label, 0.02)) +
  scale_color_manual("", values = c("HFD"=sources.pal[1], "WPP"=sources.pal[2])) +
  guides(color = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="white"),
        strip.background = element_rect(fill="white"),
        axis.text = element_text(size=12),
        axis.title.x = element_text(size=14, vjust=-1),
        axis.title.y = element_text(size=14, vjust=1),
        strip.text = element_text(size=14),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        aspect.ratio = 1) +
  facet_wrap(~ Year) +
  coord_cartesian(xlim=c(10, 60),
                  ylim=c(0, asfr.1.label))

###########################
# COMPARE FIVE-YEAR ASFRs #
###########################

## determine axis labels
asfr.5.label <-
  plyr::round_any(
    max(WPP.asfr.5 %>% filter(Year %in% 2019:2021) %>% pull(ASFR),
        NVSS.asfr.5 %>% filter(Year %in% 2019:2021) %>% pull(ASFR)),
    f=ceiling,
    accuracy=0.02
  )

## plot asfrs from different sources alongside each other
asfr.5.comparison <-  
  WPP.asfr.5 %>% 
  select(Year, Age, ASFR) %>% 
  mutate(Source="WPP") %>% 
  add_row(NVSS.asfr.5) %>% 
  filter(Year %in% 2019:2021) %>% 
  ggplot(aes(x=Age, y=ASFR, color=Source)) +
  geom_area(data = . %>% filter(Source=="NVSS"), 
            aes(x=Age, y=ASFR), 
            fill = sources.pal[1],
            show.legend = FALSE) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(10, 60, 5)) +
  scale_y_continuous(breaks = seq(0, asfr.5.label, 0.02)) +
  scale_color_manual("", values = c("NVSS"=sources.pal[1], "WPP"=sources.pal[2])) +
  guides(color = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="white"),
        strip.background = element_rect(fill="white"),
        axis.text = element_text(size=12),
        axis.title.x = element_text(size=14, vjust=-1),
        axis.title.y = element_text(size=14, vjust=1),
        strip.text = element_text(size=14),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        aspect.ratio = 1) +
  facet_wrap(~ Year) +
  coord_cartesian(xlim=c(10, 60),
                  ylim=c(0, asfr.5.label))

###############
# COMPARE mxs #
###############

## determine axis labels
mx.label <-
  plyr::round_any(
    min(WPP.LT.m %>% filter(Year %in% 2019:2021) %>% pull(mx) %>% log(),
        WPP.LT.f %>% filter(Year %in% 2019:2021) %>% pull(mx) %>% log(),
        HMD.LT.m %>% filter(Year %in% 2019:2021) %>% pull(mx) %>% log(),
        HMD.LT.f %>% filter(Year %in% 2019:2021) %>% pull(mx) %>% log()),
    f=floor,
    accuracy=1
  )

## plot mxs from different sources alongside each other
mx.comparison <-
  WPP.LT.m %>% 
  add_row(WPP.LT.f) %>% 
  select(Year, Age, Sex, mx) %>% 
  mutate(Source="WPP") %>% 
  add_row(HMD.LT.m %>% add_row(HMD.LT.f) %>% select(Year, Age, Sex, mx) %>% mutate(Source="HMD")) %>% 
  filter(Year %in% 2019:2021) %>% 
  ggplot(aes(x=Age, y=log(mx), color=Source)) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0, 110, 10)) +
  scale_y_continuous(breaks = seq(mx.label, 0, 1)) +
  scale_color_manual("", values = c("HMD"=sources.pal[1], "WPP"=sources.pal[2])) +
  guides(color = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="white"),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14, vjust=-1),
        axis.title.y = element_text(size=14, vjust=2),
        strip.text = element_text(size=14),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        aspect.ratio = 1) +
  facet_grid(Sex ~ Year) +
  coord_cartesian(xlim=c(0, 110),
                  ylim=c(mx.label, 0))

###############
# COMPARE qxs #
###############

## determine axis labels
qx.label <-
  plyr::round_any(max(HMD.LT.m %>% add_row(HMD.LT.f) %>% filter(Year %in% 2019:2021, Age<max(Age)) %>% pull(qx)),
                  f=ceiling,
                  accuracy=0.1)

## plot qxs from different sources alongside each other
qx.comparison <-  
  WPP.LT.m %>% 
  add_row(WPP.LT.f) %>% 
  select(Year, Age, Sex, qx) %>% 
  mutate(Source="WPP") %>% 
  add_row(HMD.LT.m %>% add_row(HMD.LT.f) %>% select(Year, Age, Sex, qx) %>% mutate(Source="HMD")) %>% 
  add_row(NVSS.qx) %>% 
  filter(Year %in% 2019:2021) %>%
  ggplot(aes(x=Age, y=qx, color=Source)) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0, 110, 10)) +
  scale_y_continuous(breaks = seq(0, qx.label, 0.1)) +
  scale_color_manual("", values = c("HMD"=sources.pal[1], "WPP"=sources.pal[2], "NVSS"=sources.pal[3])) +
  guides(color = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="white"),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14, vjust=-1),
        axis.title.y = element_text(size=14, vjust=2),
        strip.text = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = "bottom",
        aspect.ratio = 1) +
  facet_grid(Sex ~ Year) +
  coord_cartesian(xlim=c(0, 110),
                  ylim=c(0, qx.label))

##############
# SAVE PLOTS #
##############

png(here::here("out", "other-output", "asfr-1-comparison.png"),
    width=16, height=8, res=200, unit="in")

print(asfr.1.comparison)

dev.off()

png(here::here("out", "other-output", "asfr-5-comparison.png"),
    width=16, height=8, res=200, unit="in")

print(asfr.5.comparison)

dev.off()

png(here::here("out", "other-output", "mx-comparison.png"),
    width=16, height=8, res=200, unit="in")

print(mx.comparison)

dev.off()

png(here::here("out", "other-output", "qx-comparison.png"),
    width=16, height=8, res=200, unit="in")

print(qx.comparison)

dev.off()

#####################
# CLEAR ENVIRONMENT #
#####################

rm(list=ls())