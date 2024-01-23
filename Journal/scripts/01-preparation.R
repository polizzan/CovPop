##########################
# UNWPP DATA PREPARATION #
##########################

# ---

##############
# POPULATION #
##############

Population20222100 <-
  read_csv(here::here("data", "WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, AgeGrp, PopMale, PopFemale, PopTotal)

Population <- 
  read_csv(here::here("data", "WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, AgeGrp, PopMale, PopFemale, PopTotal) %>% 
  add_row(Population20222100)

###############
# LIFE TABLES #
###############

LT20222100_m <-
  read_csv(here::here("data", "WPP2022_Life_Table_Complete_Medium_Male_2022-2100.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, Sex, AgeGrp = AgeGrpStart, mx:ax)
  
LT_m <-
  read_csv(here::here("data", "WPP2022_Life_Table_Complete_Medium_Male_1950-2021.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, Sex, AgeGrp = AgeGrpStart, mx:ax) %>% 
  add_row(LT20222100_m) %>% 
  mutate(AgeGrp = case_when(AgeGrp == 100 ~ "100+",
                            TRUE ~ as.character(AgeGrp)))

LT20222100_f <-
  read_csv(here::here("data", "WPP2022_Life_Table_Complete_Medium_Female_2022-2100.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, Sex, AgeGrp = AgeGrpStart, mx:ax)

LT_f <-
  read_csv(here::here("data", "WPP2022_Life_Table_Complete_Medium_Female_1950-2021.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, Sex, AgeGrp = AgeGrpStart, mx:ax) %>% 
  add_row(LT20222100_f) %>% 
  mutate(AgeGrp = case_when(AgeGrp == 100 ~ "100+",
                            TRUE ~ as.character(AgeGrp)))

#######################
# SEX RATIOS AT BIRTH #
#######################

SexRB <- 
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, SRB) %>% 
  mutate(SRB = SRB/100)

#############
# FERTILITY #
#############

## age-specific fertility rates by maternal age
## add zeros for ages below 15 and above 49
df <- data.frame(
  Time = rep(1950:2100, each = length(c(0:14, 50:100))),
  AgeGrp = rep(c(0:14, 50:100), length(1950:2100)),
  ASFR = rep(rep(0, length(c(0:14, 50:100))), length(1950:2100)),
  PASFR = rep(rep(0, length(c(0:14, 50:100))), length(1950:2100)),
  Location = rep(rep("United States of America", length(c(0:14, 50:100))), length(1950:2100))
)

ASFR <-
  read_csv(here::here("data", "WPP2022_Fertility_by_Age1.zip")) %>% 
  filter(Location == "United States of America") %>%
  filter(Variant == "Medium") %>% 
  select(Location, Time, AgeGrp, ASFR, PASFR) %>% 
  add_row(df) %>% 
  arrange(Time, AgeGrp) %>% 
  mutate(Observed = ASFR/1000,
         PASFR = PASFR/100,
         AgeGrp = case_when(AgeGrp == 100 ~ "100+",
                            TRUE ~ as.character(AgeGrp))) %>% 
  select(-ASFR)

## interpolation of tfr for counterfactual scenario
TFR <-
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(Location == "United States of America") %>% 
  filter(Variant == "Medium") %>% 
  filter(Time %in% 2010:2100) %>% 
  select(Location, Time, Observed = TFR) %>% 
  mutate(Counterfactual = Observed)

beta1 <- 
  cov(TFR$Time[TFR$Time %in% 2010:2019], TFR$Observed[TFR$Time %in% 2010:2019])/var(TFR$Time[TFR$Time %in% 2010:2019])

beta0 <-
  mean(TFR$Observed[TFR$Time %in% 2010:2019]) - beta1 * mean(TFR$Time[TFR$Time %in% 2010:2019])

## TFR$Observed[TFR$Time %in% 2010:2019] ## observed TFR in 2010-2019
## beta0+beta1*2010:2019 ## predicted TFR in 2010-2019 based on linear regression

TFR_IP_Time <-
  which(TFR$Observed[TFR$Time %in% 2020:2029] > (beta0+beta1*2020:2029))[1]+2019 ## first year post-COVID = predicted < 'observed'
  
TFR$Counterfactual[TFR$Time>2019 & TFR$Time<TFR_IP_Time] <- NA ## set counterfactual TFR to missing in COVID years

TFR$Counterfactual <- zoo::na.approx(TFR$Counterfactual) ## linear interpolation of counterfactual TFR for COVID years

ASFR$Counterfactual <- ASFR$Observed

for(i in 2020:(TFR_IP_Time-1)){

## counterfactual ASFR = counterfactual TFR * fertility age structure in year of fertility rebound  
ASFR$Counterfactual[ASFR$Time==i] <- 
  TFR$Counterfactual[TFR$Time==i] * ASFR$PASFR[ASFR$Time==TFR_IP_Time] 

}

#############
# MIGRATION #
#############

## total net migration counts
Mig <-
read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(Location == "United States of America") %>% 
  select(Location, Time, NetMigrations) %>% 
  filter(Time %in% c(2019:2100)) %>% 
  mutate(Observed = NetMigrations * 1000,
         Counterfactual = Observed,
         Census = Observed) %>% 
  select(-NetMigrations)

Mig$Counterfactual[Mig$Time %in% c(2020, 2021)] <- NA ## set counterfactual net migration counts to missing for COVID years
Mig$Counterfactual <- zoo::na.approx(Mig$Counterfactual) ## linear interpolation of counterfactual net migration counts for COVID years

Mig$Census[Mig$Time %in% c(2020, 2021)] <- c(551000, 693500) ## add migration estimates from census bureau for 2020 and 2021

Mig <-
  Mig %>% 
    filter(Time > 2019) %>% 
    pivot_longer(cols = c("Observed", "Counterfactual", "Census"),
                 names_to = "Scenario",
                 values_to = "NetMigrations")

## distribute total net migration counts by age and sex
Migration <- ## create empty data frame
data.frame(Age = as.numeric(),
           Time = as.numeric(),
           MigFemale = as.numeric(),
           MigMale = as.numeric(),
           Scenario = as.character())

for(j in unique(Mig$Scenario)){ 
for(i in min(Mig$Time):max(Mig$Time)){

df_mig <-
DemoTools::mig_un_fam(NM = Mig %>% filter(Time == i) %>% filter(Scenario == j) %>% pull(NetMigrations),
                      family = "Family",
                      Single = TRUE,
                      OAnew = 100)[["net_migr"]] %>% ## for each year: determine age- and sex-specific migration counts
  as_tibble() %>% 
  select(-family) %>%  
  mutate(sex=case_when(sex=="Male" ~ "MigMale",
                       TRUE ~ "MigFemale")) %>% 
  pivot_wider(names_from = "sex", ## for each year: create wide data set
              values_from = "nm") %>% 
  rename(Age = age) %>% 
  mutate(Time = i) %>% 
  mutate(Scenario = j)

Migration <-
  Migration %>% 
  add_row(df_mig) ## add new data set to existing data set to create long data set

  }
}

#############
# MORTALITY #
#############

## interpolate age-specific mortality rates between 2019 and 2025
mx_f_IP <- ## female mortality rates
  LT_f %>% 
  filter(Time %in% c(1950:2100)) %>% 
  select(AgeGrp, Time, mx) %>% 
  pivot_wider(names_from = "AgeGrp", ## columns = age, rows = years
              values_from = "mx") %>%  
  as.matrix

mx_m_IP <- ## male mortality rates
  LT_m %>% 
  filter(Time %in% c(1950:2100)) %>% 
  select(AgeGrp, Time, mx) %>% 
  pivot_wider(names_from = "AgeGrp", ## columns = age, rows = years
              values_from = "mx") %>% 
  as.matrix

mx_f_IP[c(71:75), -1] <- NA ## set age-specific mortality rates to missing for years 2020 to 2024
mx_m_IP[c(71:75), -1] <- NA ## set age-specific mortality rates to missing for years 2020 to 2024
  
mx_f_IP[, -1] <- 
  exp(zoo::na.approx(log(mx_f_IP[, -1]))) ## interpolate log of age-specific mortality rates for missing years

mx_m_IP[, -1] <- 
  exp(zoo::na.approx(log(mx_m_IP[, -1]))) ## interpolate log of age-specific mortality rates for missing years

mx_f_IP <- ## reshape to long
  mx_f_IP %>% 
  as_tibble() %>% 
  pivot_longer(cols = c(-"Time"),
               names_to = "AgeGrp",
               values_to = "mx")        

mx_m_IP <- ## reshape to long
  mx_m_IP %>% 
  as_tibble() %>% 
  pivot_longer(cols = c(-"Time"),
               names_to = "AgeGrp",
               values_to = "mx")

#######################
# COMBINE INFORMATION #
#######################

## combine relevant information from above into one data frame & change format for convenience
projection_input <- 
  data.frame(
             Year = Population %>% filter(Time %in% c(2020:2060)) %>% pull(Time),
             Age = rep(0:100, length(2020:2060)),
             PopFemale = Population %>% filter(Time %in% c(2020:2060)) %>% pull(PopFemale) * 1000,
             PopMale = Population %>% filter(Time %in% c(2020:2060)) %>% pull(PopMale) * 1000,             
             Sx_f = LT_f %>% filter(Time %in% c(2020:2060)) %>% group_by(Time) %>% mutate(Sx=Sx(mx, female=TRUE)) %>% pull(Sx),
             Sx_m = LT_m %>% filter(Time %in% c(2020:2060)) %>% group_by(Time) %>% mutate(Sx=Sx(mx, female=FALSE)) %>% pull(Sx),
             Sx_f_IP = mx_f_IP %>% filter(Time %in% c(2020:2060)) %>% group_by(Time) %>% mutate(Sx=Sx(mx, female=TRUE)) %>% pull(Sx),
             Sx_m_IP = mx_m_IP %>% filter(Time %in% c(2020:2060)) %>% group_by(Time) %>% mutate(Sx=Sx(mx, female=FALSE)) %>% pull(Sx),
             Fx = ASFR %>% filter(Time %in% c(2020:2060)) %>% pull(Observed),
             Fx_IP = ASFR %>% filter(Time %in% c(2020:2060)) %>% pull(Counterfactual),
             SRB = rep(SexRB %>% filter(Time %in% c(2020:2060)) %>% pull(SRB), each = 101),
             MigFemale = Migration %>% filter(Time %in% c(2020:2060)) %>% filter(Scenario=="Observed") %>% pull(MigFemale),
             MigMale = Migration %>% filter(Time %in% c(2020:2060)) %>% filter(Scenario=="Observed") %>% pull(MigMale),
             MigFemale_IP = Migration %>% filter(Time %in% c(2020:2060)) %>% filter(Scenario=="Counterfactual") %>% pull(MigFemale),
             MigMale_IP = Migration %>% filter(Time %in% c(2020:2060)) %>% filter(Scenario=="Counterfactual") %>% pull(MigMale),
             MigFemale_Census = Migration %>% filter(Time %in% c(2020:2060)) %>% filter(Scenario=="Census") %>% pull(MigFemale),
             MigMale_Census = Migration %>% filter(Time %in% c(2020:2060)) %>% filter(Scenario=="Census") %>% pull(MigMale)
             ) %>% 
  pivot_longer(cols=-c("Year", "Age"),
               names_to = "Indicator",
               values_to = "Value") %>% 
  pivot_wider(names_from = "Year", ## put years in columns
              values_from = "Value")

########
# SAVE #
########

## save input data set
save(projection_input, file=here::here("data", "projection-input.RData"))

#####################
# CLEAR ENVIRONMENT #
#####################

## delete everything except for a few objects
rm(list=setdiff(ls(), c("CCPM", "pyramid", "nsim", "estimates_years", 
                        "pal_f", "pal_m", "scenarios", "grid")))
