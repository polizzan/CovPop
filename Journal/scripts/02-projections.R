##########################
# POPULATION PROJECTIONS #
##########################

# ---

###################
# LOAD INPUT DATA #
###################

load(here::here("data", "projection-input.RData"))

###############################
# PREPARE DATA FOR PROJECTION #
############################### 

Pop_f <- ## column vector of age-specific starting population (female)
  subset(projection_input, Indicator=="PopFemale", select=`2020`) %>% 
  as.matrix

Pop_m <- ## column vector of age-specific starting population (male)
  subset(projection_input, Indicator=="PopMale", select=`2020`) %>% 
  as.matrix

Sx_f <- ## matrix of age-specific survivorship ratios for baseline scenario (female) 
  subset(projection_input, Indicator=="Sx_f", select=-c(Age, Indicator)) %>% 
  as.matrix

Sx_m <- ## matrix of age-specific survivorship ratios for baseline scenario (male) 
  subset(projection_input, Indicator=="Sx_m", select=-c(Age, Indicator)) %>% 
  as.matrix

Sx_f_IP <- ## matrix of age-specific survivorship ratios for counterfactual scenario (female) 
  subset(projection_input, Indicator=="Sx_f_IP", select=-c(Age, Indicator)) %>% 
  as.matrix

Sx_m_IP <- ## matrix of age-specific survivorship ratios for counterfactual scenario (male) 
  subset(projection_input, Indicator=="Sx_m_IP", select=-c(Age, Indicator)) %>% 
  as.matrix

Fx <- ## matrix of age-specific fertility rates for baseline scenario (female)
  subset(projection_input, Indicator=="Fx", select=-c(Age, Indicator)) %>% 
  as.matrix

Fx_IP <- ## matrix of age-specific fertility rates for counterfactual scenario (female)
  subset(projection_input, Indicator=="Fx_IP", select=-c(Age, Indicator)) %>% 
  as.matrix

SRB <- ## vector of sex ratios at birth (male to female)
  subset(projection_input, Indicator=="SRB" & Age==0, select=-c(Age, Indicator)) %>% 
  as.matrix %>% 
  as.vector
  
Migra_f <- ## matrix of age-specific migration counts for baseline scenario (female)
  subset(projection_input, Indicator=="MigFemale", select=-c(Age, Indicator)) %>% 
  as.matrix

Migra_m <- ## matrix of age-specific migration counts for baseline scenario (male)
  subset(projection_input, Indicator=="MigMale", select=-c(Age, Indicator)) %>% 
  as.matrix

Migra_f_IP <- ## matrix of age-specific migration counts for counterfactual scenario (female)
  subset(projection_input, Indicator=="MigFemale_IP", select=-c(Age, Indicator)) %>% 
  as.matrix

Migra_m_IP <- ## matrix of age-specific migration counts for counterfactual scenario (male)
  subset(projection_input, Indicator=="MigMale_IP", select=-c(Age, Indicator)) %>% 
  as.matrix

Migra_f_Census <- ## matrix of age-specific migration counts from census bureau (female)
  subset(projection_input, Indicator=="MigFemale_Census", select=-c(Age, Indicator)) %>% 
  as.matrix

Migra_m_Census <- ## matrix of age-specific migration counts from census bureau (male)
  subset(projection_input, Indicator=="MigMale_Census", select=-c(Age, Indicator)) %>% 
  as.matrix

########################################
# COUNTERFACTUAL POPULATION PROJECTION #
########################################

## create empty list object for projection output
projection_output <- 
 vector(mode="list", length=nsim)
  
## run projections
for(z in 1:nsim){

print(z)  
    
Projection_... <- ## baseline
  cbind(
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f,
       Migra_m = Migra_m,
       stochastic = TRUE),
  99) ## number indicating scenario

Projection_CEN <- ## baseline w/ census migration estimates
  cbind(
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f_Census,
       Migra_m = Migra_m_Census,
       stochastic = TRUE), 
  98) 

Projection_MO.. <- ## counterfactual: mortality
  cbind(
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f_IP,
       Sx_m = Sx_m_IP,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f,
       Migra_m = Migra_m,
       stochastic = TRUE),
  1)

Projection_.FE. <- ## counterfactual: fertility
  cbind(
    CCPM(Pop_f = Pop_f,
         Pop_m = Pop_m,
         Sx_f = Sx_f,
         Sx_m = Sx_m,
         Fx = Fx_IP,
         SRB = SRB,
         Migra_f = Migra_f,
         Migra_m = Migra_m,
         stochastic = TRUE),
    2) 

Projection_..MI <- ## counterfactual: migration
  cbind(
    CCPM(Pop_f = Pop_f,
         Pop_m = Pop_m,
         Sx_f = Sx_f,
         Sx_m = Sx_m,
         Fx = Fx,
         SRB = SRB,
         Migra_f = Migra_f_IP,
         Migra_m = Migra_m_IP,
         stochastic = TRUE),
    3) 

Projection_MOFEMI <- ## counterfactual: mortality, fertility, migration
  cbind(
    CCPM(Pop_f = Pop_f,
         Pop_m = Pop_m,
         Sx_f = Sx_f_IP,
         Sx_m = Sx_m_IP,
         Fx = Fx_IP,
         SRB = SRB,
         Migra_f = Migra_f_IP,
         Migra_m = Migra_m_IP,
         stochastic = TRUE),
    4)

Projection_MOFE. <- ## counterfactual: mortality, fertility
  cbind(
    CCPM(Pop_f = Pop_f,
         Pop_m = Pop_m,
         Sx_f = Sx_f_IP,
         Sx_m = Sx_m_IP,
         Fx = Fx_IP,
         SRB = SRB,
         Migra_f = Migra_f,
         Migra_m = Migra_m,
         stochastic = TRUE),
    5) 

Projection_MO.MI <- ## counterfactual: mortality, migration
  cbind(
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f_IP,
       Sx_m = Sx_m_IP,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f_IP,
       Migra_m = Migra_m_IP,
       stochastic = TRUE),
  6)

Projection_.FEMI <- ## counterfactual: fertility, migration
  cbind(
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx_IP,
       SRB = SRB,
       Migra_f = Migra_f_IP,
       Migra_m = Migra_m_IP,
       stochastic = TRUE),
  7)

Projection_cf_all <- ## combine all counterfactual scenarios
    rbind(Projection_MO.., 
          Projection_.FE.,
          Projection_..MI, 
          Projection_MOFEMI,
          Projection_MOFE.,
          Projection_MO.MI,
          Projection_.FEMI) 

Pop_bl <- Projection_...[, 1] ## column 1 = population counts
Pop_bl_census <- Projection_CEN[, 1]
Pop_cf <- Projection_cf_all[, 1]
Pop_diff_absolute = Pop_bl - Pop_cf
Pop_diff_percent = Pop_diff_absolute/Pop_bl
Pop_diff_absolute_census = Pop_bl_census - Pop_cf
Pop_diff_percent_census = Pop_diff_absolute_census/Pop_bl_census
Pop_diff_absolute_census[!Projection_cf_all[, 5] %in% c(3, 4, 6, 7)] = NA ## column 5 = scenario
Pop_diff_percent_census[!Projection_cf_all[, 5] %in% c(3, 4, 6, 7)] = NA ## set to missing if not a migration scenario

projection_output[[z]] <-
  cbind(
    Scenario = Projection_cf_all[, 5], 
    Year = Projection_cf_all[, 4] + 2020, 
    Sex = Projection_cf_all[, 3],   
    Age = Projection_cf_all[, 2], 
    Pop_bl,
    Pop_bl_census,
    Pop_cf,
    Pop_diff_absolute,
    Pop_diff_percent,
    Pop_diff_absolute_census,
    Pop_diff_percent_census,
    nsim = z
        )

}

#####################
# CLEAR ENVIRONMENT #
#####################

## delete everything except for a few objects
rm(list=setdiff(ls(), c("pyramid", "projection_output", "nsim", "estimates_years", 
                        "pal_f", "pal_m", "scenarios", "grid")))

################################
# PROCESSING PROJECTION OUTPUT #
################################

## difference in age groups by sex
delta_agegrp_sex <-
  do.call(rbind, projection_output) %>% 
  as_tibble(.name_repair="unique") %>% ## convert list object into tibble
  mutate(AgeGrp = case_when(Age %in% c(0:14) ~ "0-14",
                            Age %in% c(15:49) ~ "15-49",
                            Age %in% c(50:64) ~ "50-64",                            
                            Age %in% c(65:84) ~ "65-84",
                            Age %in% c(85:100) ~ "85+")) %>% 
  group_by(nsim, Year, Sex, AgeGrp, Scenario) %>% 
  summarize(Pop_cf = sum(Pop_cf, na.rm = TRUE),
            Pop_bl = sum(Pop_bl, na.rm = TRUE),
            Pop_bl_census = sum(Pop_bl_census, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Pop_diff_absolute = Pop_bl - Pop_cf,
         Pop_diff_percent = Pop_diff_absolute/Pop_bl,
         Pop_diff_absolute_census = Pop_bl_census - Pop_cf,
         Pop_diff_percent_census = Pop_diff_absolute_census/Pop_bl_census) 

## difference in age groups, w/o sex dimension
delta_agegrp_total <-
  delta_agegrp_sex %>%  
  group_by(nsim, Year, AgeGrp, Scenario) %>% 
  summarize(Pop_cf = sum(Pop_cf, na.rm = TRUE),
            Pop_bl = sum(Pop_bl, na.rm = TRUE),
            Pop_bl_census = sum(Pop_bl_census, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Pop_diff_absolute = Pop_bl - Pop_cf,
         Pop_diff_percent = Pop_diff_absolute/Pop_bl,
         Pop_diff_absolute_census = Pop_bl_census - Pop_cf,
         Pop_diff_percent_census = Pop_diff_absolute_census/Pop_bl_census) %>%  
  group_by(nsim, Year, Scenario) %>% 
  mutate(PopShare_cf = 100 * Pop_cf/sum(Pop_cf, na.rm = TRUE),
         PopShare_bl = 100 * Pop_bl/sum(Pop_bl, na.rm = TRUE),
         PopShare_bl_census = 100 * Pop_bl_census/sum(Pop_bl_census, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(PopShare_diff_absolute = PopShare_bl - PopShare_cf,
         PopShare_diff_absolute_census = PopShare_bl_census - PopShare_cf)

## difference in total population, w/o sex dimension
delta_total <-
  delta_agegrp_total %>% 
  group_by(nsim, Year, Scenario) %>% 
  summarize(Pop_cf = sum(Pop_cf, na.rm = TRUE),
            Pop_bl = sum(Pop_bl, na.rm = TRUE),
            Pop_bl_census = sum(Pop_bl_census, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Pop_diff_absolute = Pop_bl - Pop_cf,
         Pop_diff_percent = Pop_diff_absolute/Pop_bl,
         Pop_diff_absolute_census = Pop_bl_census - Pop_cf,
         Pop_diff_percent_census = Pop_diff_absolute_census/Pop_bl_census)

## set to missing if not a migration scenario
delta_agegrp_sex$Pop_diff_absolute_census[!delta_agegrp_sex$Scenario %in% c(3, 4, 6, 7)] <-
delta_agegrp_sex$Pop_diff_percent_census[!delta_agegrp_sex$Scenario %in% c(3, 4, 6, 7)] <-
delta_agegrp_total$Pop_diff_absolute_census[!delta_agegrp_total$Scenario %in% c(3, 4, 6, 7)] <-
delta_agegrp_total$Pop_diff_percent_census[!delta_agegrp_total$Scenario %in% c(3, 4, 6, 7)] <-
delta_agegrp_total$PopShare_diff_absolute_census[!delta_agegrp_total$Scenario %in% c(3, 4, 6, 7)] <-
delta_total$Pop_diff_absolute_census[!delta_total$Scenario %in% c(3, 4, 6, 7)] <-
delta_total$Pop_diff_percent_census[!delta_total$Scenario %in% c(3, 4, 6, 7)] <- NA

## difference in dependency ratios
dependency_ratio <-
  delta_agegrp_total %>% 
  filter(Scenario %in% c(1, 2, 3, 4)) %>% 
  mutate(group=case_when(AgeGrp == "0-14" ~ "young",
                         AgeGrp %in% c("65-84", "85+") ~ "old",
                         TRUE ~ "working")) %>% 
  group_by(nsim, Year, Scenario, group) %>% 
  summarize(cf = sum(Pop_cf, na.rm=TRUE),
            bl = sum(Pop_bl, na.rm=TRUE),
            bl_census = sum(Pop_bl_census, na.rm=TRUE)) %>% 
  pivot_wider(names_from="group",
              values_from=c("cf", "bl", "bl_census")) %>% 
  ungroup() %>% 
  mutate(cf_young = 100 * cf_young/cf_working,
         cf_old = 100 * cf_old/cf_working,
         bl_young = 100 * bl_young/bl_working,         
         bl_old = 100 * bl_old/bl_working,
         cf_total = cf_young + cf_old,
         bl_total = bl_young + bl_old,
         bl_census_young = 100 * bl_census_young/bl_census_working,
         bl_census_old = 100 * bl_census_old/bl_census_working,
         bl_census_total = bl_census_young + bl_census_old) %>% 
  select(-c(cf_working, bl_working, bl_census_working)) %>% 
  mutate(young = bl_young - cf_young,
         old = bl_old - cf_old,
         total = bl_total - cf_total,
         `young (census)` = bl_census_young - cf_young,
         `old (census)` = bl_census_old - cf_old,
         `total (census)` = bl_census_total - cf_total) %>% 
  select(-c(bl_young, cf_young, bl_old, cf_old, bl_total, cf_total, 
            bl_census_young, bl_census_old, bl_census_total)) %>% 
  pivot_longer(cols=c("young", "old", "total",
                      "young (census)", "old (census)", "total (census)"),
               names_to = "DR",
               values_to = "delta") %>% 
  mutate(Scenario=case_when(Scenario==1 ~ "Mortality",
                            Scenario==2 ~ "Fertility", 
                            Scenario==3 ~ "Migration",
                            Scenario==4 ~ "All"),
         Scenario=case_when(grepl("census", DR) ~ paste(Scenario, "(Census)"),
                                  TRUE ~ Scenario),
         DR=case_when(grepl("young", DR) ~ "young",
                      grepl("old", DR) ~ "old",
                      grepl("total", DR) ~ "total"),
         DR=factor(DR, 
                   levels=c("young", "old", "total"),
                   labels=c("Panel A: Young-age", "Panel B: Old-age", "Panel C: Total"))) %>% 
  filter(Scenario %in% c("All", "Mortality", "Fertility", "Migration", "Migration (Census)")) %>% 
  mutate(Scenario=factor(Scenario, levels=c("All", "Mortality", "Fertility", "Migration", "Migration (Census)"))) %>% 
  arrange(nsim, DR, Scenario)

############################################################
# CREATE DATA SETS WITH LOWER, MIDDLE, AND UPPER ESTIMATES #
############################################################

## age- and sex-specific
age_lower <-
  do.call(rbind, projection_output) %>% 
  as_tibble(.name_repair="unique") %>% 
  group_by(Year, Sex, Age, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.025), na.rm=TRUE, names=FALSE))) %>% 
  ungroup()

age_mid <-
  do.call(rbind, projection_output) %>% 
  as_tibble(.name_repair="unique") %>%  
  group_by(Year, Sex, Age, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.5), na.rm=TRUE, names=FALSE))) %>% 
  ungroup()

age_upper <-
  do.call(rbind, projection_output) %>% 
  as_tibble(.name_repair="unique") %>% 
  group_by(Year, Sex, Age, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.975), na.rm=TRUE, names=FALSE))) %>% 
  ungroup()

df_age <-
  age_mid %>% 
  left_join(age_lower, by=c("Year", "Sex", "Age", "Scenario"), suffix=c("", "_lower")) %>% 
  left_join(age_upper, by=c("Year", "Sex", "Age", "Scenario"), suffix=c("_mid", "_upper"))

## age-group- and sex-specific
agegrp_sex_lower <-
  delta_agegrp_sex %>% 
  group_by(Year, Sex, AgeGrp, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.025), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

agegrp_sex_mid <-
  delta_agegrp_sex %>% 
  group_by(Year, Sex, AgeGrp, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.5), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

agegrp_sex_upper <-
  delta_agegrp_sex %>% 
  group_by(Year, Sex, AgeGrp, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.975), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

df_agegrp_sex <-
  agegrp_sex_mid %>% 
  left_join(agegrp_sex_lower, by=c("Year", "Sex", "AgeGrp", "Scenario"), suffix=c("", "_lower")) %>% 
  left_join(agegrp_sex_upper, by=c("Year", "Sex", "AgeGrp", "Scenario"), suffix=c("_mid", "_upper"))

## age-group-specific
agegrp_total_lower <-
  delta_agegrp_total %>% 
  group_by(Year, AgeGrp, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.025), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

agegrp_total_mid <-
  delta_agegrp_total %>% 
  group_by(Year, AgeGrp, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.5), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

agegrp_total_upper <-
  delta_agegrp_total %>% 
  group_by(Year, AgeGrp, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.975), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

df_agegrp_total <-
  agegrp_total_mid %>% 
  left_join(agegrp_total_lower, by=c("Year", "AgeGrp", "Scenario"), suffix=c("", "_lower")) %>% 
  left_join(agegrp_total_upper, by=c("Year", "AgeGrp", "Scenario"), suffix=c("_mid", "_upper"))

## total
total_lower <-
  delta_total %>% 
  group_by(Year, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.025), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

total_mid <-
  delta_total %>% 
  group_by(Year, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.5), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

total_upper <-
  delta_total %>% 
  group_by(Year, Scenario) %>% 
  summarize(across(everything(), ~ quantile(.x, probs=c(0.975), na.rm=TRUE, names=FALSE))) %>% 
  ungroup() 

df_total <-
  total_mid %>% 
  left_join(total_lower, by=c("Year", "Scenario"), suffix=c("", "_lower")) %>% 
  left_join(total_upper, by=c("Year", "Scenario"), suffix=c("_mid", "_upper"))

###############
# SAVE OUTPUT #
###############

save(
  dependency_ratio,
  df_age,
  df_agegrp_sex,
  df_agegrp_total,
  df_total, 
  file = here::here("data", "projection-output.RData")
)

#####################
# CLEAR ENVIRONMENT #
#####################

## delete everything except for a few objects
rm(list=setdiff(ls(), c("pyramid", "dependency_ratio", "df_age", "df_agegrp_sex",
                        "df_agegrp_total", "df_total","estimates_years", 
                        "pal_f", "pal_m", "grid", "scenarios")))
