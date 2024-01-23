####################################
#COHORT COMPONENT PROJECTION MODEL#
##################################

#load data set
load(here("data", "Proj.RData"))

#prepare input for CCPM
Pop_f <- ##vector of age-specific starting population (female)
  Proj %>% filter(Year==2020) %>% pull(PopFemale) %>% 
  as.matrix()

Pop_m <- ##vector of age-specific starting population (male)
  Proj %>% filter(Year==2020) %>% pull(PopMale) %>% 
  as.matrix()

Sx_f <- ##matrix of age-specific survivorship ratios for baseline scenario (female) 
  Proj %>% 
  select(Age, Year, Sx_f) %>% 
  pivot_wider(names_from = "Year",
              values_from = "Sx_f") %>% 
  select(-Age) %>% 
  as.matrix

Sx_m <- ##matrix of age-specific survivorship ratios for baseline scenario (male) 
  Proj %>% 
  select(Age, Year, Sx_m) %>% 
  pivot_wider(names_from = "Year",
              values_from = "Sx_m") %>% 
  select(-Age) %>% 
  as.matrix

Sx_f_IP <- ##matrix of age-specific survivorship ratios for counterfactual scenario (female) 
  Proj %>% 
  select(Age, Year, Sx_f_IP) %>% 
  pivot_wider(names_from = "Year",
              values_from = "Sx_f_IP") %>% 
  select(-Age) %>% 
  as.matrix

Sx_m_IP <- ##matrix of age-specific survivorship ratios for counterfactual scenario (male) 
  Proj %>% 
  select(Age, Year, Sx_m_IP) %>% 
  pivot_wider(names_from = "Year",
              values_from = "Sx_m_IP") %>% 
  select(-Age) %>% 
  as.matrix

Fx <- ##matrix of age-specific fertility rates for baseline scenario (female)
  Proj %>% 
  select(Age, Year, Fx) %>% 
  pivot_wider(names_from = "Year",
              values_from = "Fx") %>% 
  select(-Age) %>% 
  as.matrix()

Fx_IP <- ##matrix of age-specific fertility rates for counterfactual scenario (female)
  Proj %>% 
  select(Age, Year, Fx_IP) %>% 
  pivot_wider(names_from = "Year",
              values_from = "Fx_IP") %>% 
  select(-Age) %>% 
  as.matrix()

SRB <- Proj %>% filter(Age==0) %>% pull(SRB) ##vector of sex ratios at birth (male to female)

Migra_f <- ##matrix of age-specific migration counts for baseline scenario (female)
  Proj %>% 
  select(Age, Year, MigFemale) %>% 
  pivot_wider(names_from = "Year",
              values_from = "MigFemale") %>% 
  select(-Age) %>% 
  as.matrix()

Migra_m <- ##matrix of age-specific migration counts for baseline scenario (male)
  Proj %>% 
  select(Age, Year, MigMale) %>% 
  pivot_wider(names_from = "Year",
              values_from = "MigMale") %>% 
  select(-Age) %>% 
  as.matrix()

Migra_f_IP <- ##matrix of age-specific migration counts for counterfactual scenario (female)
  Proj %>% 
  select(Age, Year, MigFemale_IP) %>% 
  pivot_wider(names_from = "Year",
              values_from = "MigFemale_IP") %>% 
  select(-Age) %>% 
  as.matrix()

Migra_m_IP <- ##matrix of age-specific migration counts for counterfactual scenario (male)
  Proj %>% 
  select(Age, Year, MigMale_IP) %>% 
  pivot_wider(names_from = "Year",
              values_from = "MigMale_IP") %>% 
  select(-Age) %>% 
  as.matrix()

#apply CCPM function
Projection_... <- ##baseline
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f,
       Migra_m = Migra_m) %>% 
  mutate(Year=Year+2020,
         Scenario="---")

Projection_MO.. <- ##counterfactual: mortality
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f_IP,
       Sx_m = Sx_m_IP,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f,
       Migra_m = Migra_m) %>% 
  mutate(Year=Year+2020,
         Scenario="MO--")   

Projection_MOMI. <- ##counterfactual: mortality, migration
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f_IP,
       Sx_m = Sx_m_IP,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f_IP,
       Migra_m = Migra_m_IP) %>% 
  mutate(Year=Year+2020,
         Scenario="MOMI-")

Projection_MO.FE <- ##counterfactual: mortality, fertility
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f_IP,
       Sx_m = Sx_m_IP,
       Fx = Fx_IP,
       SRB = SRB,
       Migra_f = Migra_f,
       Migra_m = Migra_m) %>% 
  mutate(Year=Year+2020,
         Scenario="MO-FE")

Projection_MOMIFE <- ##counterfactual: mortality, migration, fertility
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f_IP,
       Sx_m = Sx_m_IP,
       Fx = Fx_IP,
       SRB = SRB,
       Migra_f = Migra_f_IP,
       Migra_m = Migra_m_IP) %>% 
  mutate(Year=Year+2020,
         Scenario="MOMIFE")

Projection_.MI. <- ##counterfactual: migration
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx,
       SRB = SRB,
       Migra_f = Migra_f_IP,
       Migra_m = Migra_m_IP) %>% 
  mutate(Year=Year+2020,
         Scenario="-MI-")

Projection_.MIFE <- ##counterfactual: migration, fertility
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx_IP,
       SRB = SRB,
       Migra_f = Migra_f_IP,
       Migra_m = Migra_m_IP) %>% 
  mutate(Year=Year+2020,
         Scenario="-MIFE")

Projection_..FE <- ##counterfactual: fertility
  CCPM(Pop_f = Pop_f,
       Pop_m = Pop_m,
       Sx_f = Sx_f,
       Sx_m = Sx_m,
       Fx = Fx_IP,
       SRB = SRB,
       Migra_f = Migra_f,
       Migra_m = Migra_m) %>% 
  mutate(Year=Year+2020,
         Scenario="--FE")

Projection_cf_all <-
    Projection_MO.. %>% 
    add_row(Projection_MOMI.) %>% 
    add_row(Projection_MO.FE) %>% 
    add_row(Projection_MOMIFE) %>% 
    add_row(Projection_.MI.) %>% 
    add_row(Projection_.MIFE) %>% 
    add_row(Projection_..FE)

#pull UN projections
##Projection_UN <-
##  Proj %>% 
##  select(Age, Year, PopFemale) %>%
##  mutate(Sex = "Female") %>% 
##  rename(Pop = PopFemale) %>% 
##  add_row(Proj %>% 
##            select(Age, Year, PopMale) %>% 
##            mutate(Sex = "Male") %>% 
##            rename(Pop = PopMale))

#plot population pyramids
for (j in unique(Projection_cf_all$Scenario)) {
for (i in c(2020:2060)) {

absolute <-
  pyramid(observed=Projection_... %>% filter(Year==i) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==i) %>% filter(Scenario==j) %>% pull(Pop),
          year=i,
          percent=FALSE)

percent <-
  pyramid(observed=Projection_... %>% filter(Year==i) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==i) %>% filter(Scenario==j) %>% pull(Pop),
          year=i,
          percent=TRUE)

ggsave(filename=here("plots", "pyramid", "absolute", "with year", paste0(j), paste0("pyramid", i, ".png")), 
       width = 7, height = 9, absolute, device=png)

ggsave(filename=here("plots", "pyramid", "percent", "with year", paste0(j), paste0("pyramid", i, ".png")), 
       width = 7, height = 9, percent, device=png)

absolute <-
  absolute +
  ggtitle("")
  
percent <-
  percent +
  ggtitle("")

ggsave(filename=here("plots", "pyramid", "absolute", "without year", paste0(j), paste0("pyramid", i, ".png")), 
       width = 7, height = 9, absolute, device=png)

ggsave(filename=here("plots", "pyramid", "percent", "without year", paste0(j), paste0("pyramid", i, ".png")), 
       width = 7, height = 9, percent, device=png)

gc()
}
}

##combined plot of changes over time
for (j in c("absolute", "relative")) {

if (j=="absolute") {  
  
pyramid_2025 <-
  pyramid(observed=Projection_... %>% filter(Year==2025) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2025) %>% filter(Scenario=="MOMIFE") %>% pull(Pop),
          year=2025,
          percent=FALSE) 

pyramid_2040 <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="MOMIFE") %>% pull(Pop),
          year=2040,
          percent=FALSE)

pyramid_2060 <-
  pyramid(observed=Projection_... %>% filter(Year==2060) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2060) %>% filter(Scenario=="MOMIFE") %>% pull(Pop),
          year=2060,
          percent=FALSE) 

}else{
  
pyramid_2025 <-
  pyramid(observed=Projection_... %>% filter(Year==2025) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2025) %>% filter(Scenario=="MOMIFE") %>% pull(Pop),
          year=2025,
          percent=TRUE)  

pyramid_2040 <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="MOMIFE") %>% pull(Pop),
          year=2040,
          percent=TRUE)

pyramid_2060 <-
  pyramid(observed=Projection_... %>% filter(Year==2060) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2060) %>% filter(Scenario=="MOMIFE") %>% pull(Pop),
          year=2060,
          percent=TRUE) 

}

for (i in 7:4) {
pyramid_2025$layers[[i]] <- NULL
}

for (i in 9:5) {
  pyramid_2040$layers[[i]] <- NULL
}

for (i in 9:5) {
  pyramid_2060$layers[[i]] <- NULL
}  

pyramid_2025 <-
  pyramid_2025 +
    ggtitle("2025") +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x=element_text(size=16),
          axis.title.x=element_text(color="transparent",
                                    size=20,
                                    vjust=-2),
          axis.text.y=element_text(size=16),
          axis.title.y=element_text(size=20,
                                    vjust=3),
          text=element_text(family="serif"))

pyramid_2040 <-
  pyramid_2040 + 
    ggtitle("2040") +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x=element_text(size=16),
          axis.title.x=element_text(size=20,
                                    vjust=-2),
          axis.text.y=element_text(color="transparent",
                                   size=16),
          axis.title.y=element_text(color="transparent",
                                    size=20,
                                    vjust=3),
          axis.ticks.y=element_line(color="transparent"),
          text=element_text(family="serif")) 

pyramid_2060 <-
  pyramid_2060 + 
    ggtitle("2060") +
    theme(plot.title=element_text(hjust=0.5), 
          axis.text.x=element_text(size=16),
          axis.title.x=element_text(color="transparent",
                                    size=20,
                                    vjust=-2),
          axis.text.y=element_text(color="transparent",
                                   size=16),
          axis.title.y=element_text(color="transparent",
                                    size=20,
                                    vjust=3),
          axis.ticks.y=element_line(color="transparent"),
          text=element_text(family="serif"),
          legend.position="none")

if (j=="absolute") {

pyramid_2025 <-
  pyramid_2025 +
  scale_y_continuous(limits = c(-45, 45),
                     breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                5, 10, 15, 20, 25, 30, 35, 40, 45),
                     labels=c("-45", " ", "-35", " ", "-25", " ", "-15", " ", "-5", " ",
                              "-5", " ", "-15", " ", "-25", " ", "-35", " ", "-45")) +
  annotate(geom="text", x=2025-2021+2, y=-45, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2025-2021-2, y=-45, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE)

pyramid_2040 <-
  pyramid_2040 +
  scale_y_continuous(limits = c(-45, 45),
                     breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                5, 10, 15, 20, 25, 30, 35, 40, 45),
                     labels=c("-45", " ", "-35", " ", "-25", " ", "-15", " ", "-5", " ",
                              "-5", " ", "-15", " ", "-25", " ", "-35", " ", "-45")) +
  annotate(geom="text", x=2040-2021+2, y=-45, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-45, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-45, 
           label="Birth Cohorts >2024", hjust=0, size=3.5)

pyramid_2060 <-
  pyramid_2060 +
  scale_y_continuous(limits = c(-45, 45),
                     breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                5, 10, 15, 20, 25, 30, 35, 40, 45),
                     labels=c("-45", " ", "-35", " ", "-25", " ", "-15", " ", "-5", " ",
                              "-5", " ", "-15", " ", "-25", " ", "-35", " ", "-45")) +
  annotate(geom="text", x=2060-2021+2, y=-45, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2060-2021-2, y=-45, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2060-2025-2, y=-45, 
           label="Birth Cohorts >2024", hjust=0, size=3.5) +
  geom_rect(aes(ymin=25, ymax=32.5, xmin=90, xmax=95), fill="#1B7837") +
  geom_rect(aes(ymin=25, ymax=32.5, xmin=85, xmax=90), fill="#762A83") +
  annotate(geom="text", y=35, x=92.5, label="Male", size=5, hjust=0, family="serif") + 
  annotate(geom="text", y=35, x=87.5, label="Female", size=5, hjust=0, family="serif")

}else{
  
pyramid_2025 <-
  pyramid_2025 +
  scale_y_continuous(limits = c(-6, 6),
                     breaks = c(seq(-6, 0, 1), seq(1, 6, 1)),
                     labels = c("-6", "", "-4", "", "-2", "", "0",
                                "", "-2", "", "-4", "", "-6")) +
  annotate(geom="text", x=2025-2021+2, y=-6, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2025-2021-2, y=-6, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE)

pyramid_2040 <-
  pyramid_2040 +
  scale_y_continuous(limits = c(-6, 6),
                     breaks = c(seq(-6, 0, 1), seq(1, 6, 1)),
                     labels = c("-6", "", "-4", "", "-2", "", "0",
                                "", "-2", "", "-4", "", "-6")) +
  annotate(geom="text", x=2040-2021+2, y=-6, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-6, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-6, 
           label="Birth Cohorts >2024", hjust=0, size=3.5)

pyramid_2060 <-
  pyramid_2060 +
  scale_y_continuous(limits = c(-6, 6),
                     breaks = c(seq(-6, 0, 1), seq(1, 6, 1)),
                     labels = c("-6", "", "-4", "", "-2", "", "0",
                                "", "-2", "", "-4", "", "-6")) +
  annotate(geom="text", x=2060-2021+2, y=-6, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2060-2021-2, y=-6, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2060-2025-2, y=-6, 
           label="Birth Cohorts >2024", hjust=0, size=3.5) +
  geom_rect(aes(ymin=3.5, ymax=4.5, xmin=90, xmax=95), fill="#1B7837") +
  geom_rect(aes(ymin=3.5, ymax=4.5, xmin=85, xmax=90), fill="#762A83") +
  annotate(geom="text", y=4.75, x=92.5, label="Male", size=5, hjust=0, family="serif") + 
  annotate(geom="text", y=4.75, x=87.5, label="Female", size=5, hjust=0, family="serif")

} 

ggarrange(pyramid_2025, pyramid_2040, pyramid_2060,
          ncol=3) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(filename=here("plots", paste0("pyramid_time_", j, ".pdf")), 
       width = 15/2.54*900, height = 7.5/2.54*900, units="px", device="pdf")

}

##combined plot of (single) counterfactuals
for (j in c("absolute", "relative")) {
  
if (j=="absolute") {  

pyramid_MO.. <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="MO--") %>% pull(Pop),
          year=2040,
          percent=FALSE) 

pyramid_..FE <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="--FE") %>% pull(Pop),
          year=2040,
          percent=FALSE) 

pyramid_.MI. <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="-MI-") %>% pull(Pop),
          year=2040,
          percent=FALSE)

}else{
  
pyramid_MO.. <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="MO--") %>% pull(Pop),
          year=2040,
          percent=TRUE)  

pyramid_..FE <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="--FE") %>% pull(Pop),
          year=2040,
          percent=TRUE) 

pyramid_.MI. <-
  pyramid(observed=Projection_... %>% filter(Year==2040) %>% pull(Pop),
          counterfactual=Projection_cf_all %>% filter(Year==2040) %>% filter(Scenario=="-MI-") %>% pull(Pop),
          year=2040,
          percent=TRUE)

}

for (i in 9:5) {
  pyramid_MO..$layers[[i]] <- NULL
}

for (i in 9:5) {
    pyramid_..FE$layers[[i]] <- NULL
}  
  
for (i in 9:5) {
  pyramid_.MI.$layers[[i]] <- NULL
}  
  
pyramid_MO.. <-
  pyramid_MO.. +
  ggtitle("Mortality") +
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(color="transparent",
                                  size=20,
                                  vjust=-2),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=20,
                                  vjust=3),
        text=element_text(family="serif")) 
  
pyramid_..FE <-
  pyramid_..FE + 
  ggtitle("Fertility") +
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(size=20,
                                  vjust=-2),
        axis.text.y=element_text(color="transparent",
                                 size=16),
        axis.title.y=element_text(color="transparent",
                                  size=20,
                                  vjust=3),
        axis.ticks.y=element_line(color="transparent"),
        text=element_text(family="serif")) 

pyramid_.MI. <-
  pyramid_.MI. + 
  ggtitle("Migration") +
  theme(plot.title=element_text(hjust=0.5), 
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(color="transparent",
                                  size=20,
                                  vjust=-2),
        axis.text.y=element_text(color="transparent",
                                 size=16),
        axis.title.y=element_text(color="transparent",
                                  size=20,
                                  vjust=3),
        axis.ticks.y=element_line(color="transparent"),
        text=element_text(family="serif"),
        legend.position="none") 
  
if (j=="absolute")  {

pyramid_MO.. <-
  pyramid_MO.. +  
  scale_y_continuous(limits = c(-45, 45),
                     breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                5, 10, 15, 20, 25, 30, 35, 40, 45),
                     labels=c("-45", " ", "-35", " ", "-25", " ", "-15", " ", "-5", " ",
                              "-5", " ", "-15", " ", "-25", " ", "-35", " ", "-45")) +
  annotate(geom="text", x=2040-2021+2, y=-45, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-45, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-45, 
           label="Birth Cohorts >2024", hjust=0, size=3.5)

pyramid_..FE <-
  pyramid_..FE +   
  scale_y_continuous(limits = c(-45, 45),
                     breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                5, 10, 15, 20, 25, 30, 35, 40, 45),
                     labels=c("-45", " ", "-35", " ", "-25", " ", "-15", " ", "-5", " ",
                              "-5", " ", "-15", " ", "-25", " ", "-35", " ", "-45")) +
  annotate(geom="text", x=2040-2021+2, y=-45, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-45, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-45, 
           label="Birth Cohorts >2024", hjust=0, size=3.5)

pyramid_.MI. <-
  pyramid_.MI. + 
  scale_y_continuous(limits = c(-45, 45),
                     breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                5, 10, 15, 20, 25, 30, 35, 40, 45),
                     labels=c("-45", " ", "-35", " ", "-25", " ", "-15", " ", "-5", " ",
                              "-5", " ", "-15", " ", "-25", " ", "-35", " ", "-45")) +
  annotate(geom="text", x=2040-2021+2, y=-45, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-45, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-45, 
           label="Birth Cohorts >2024", hjust=0, size=3.5) +
  geom_rect(aes(ymin=25, ymax=32.5, xmin=90, xmax=95), fill="#1B7837") +
  geom_rect(aes(ymin=25, ymax=32.5, xmin=85, xmax=90), fill="#762A83") +
  annotate(geom="text", y=35, x=92.5, label="Male", size=5, hjust=0, family="serif") + 
  annotate(geom="text", y=35, x=87.5, label="Female", size=5, hjust=0, family="serif")

}else{

pyramid_MO.. <-
  pyramid_MO.. +  
  scale_y_continuous(limits = c(-6, 6),
                     breaks = c(seq(-6, 0, 1), seq(1, 6, 1)),
                     labels = c("-6", "", "-4", "", "-2", "", "0",
                                "", "-2", "", "-4", "", "-6")) +
  annotate(geom="text", x=2040-2021+2, y=-6, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-6, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-6, 
           label="Birth Cohorts >2024", hjust=0, size=3.5)

pyramid_..FE <-
  pyramid_..FE +   
  scale_y_continuous(limits = c(-6, 6),
                     breaks = c(seq(-6, 0, 1), seq(1, 6, 1)),
                     labels = c("-6", "", "-4", "", "-2", "", "0",
                                "", "-2", "", "-4", "", "-6")) +
  annotate(geom="text", x=2040-2021+2, y=-6, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-6, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-6, 
           label="Birth Cohorts >2024", hjust=0, size=3.5)

pyramid_.MI. <-
  pyramid_.MI. + 
  scale_y_continuous(limits = c(-6, 6),
                     breaks = c(seq(-6, 0, 1), seq(1, 6, 1)),
                     labels = c("-6", "", "-4", "", "-2", "", "0",
                                "", "-2", "", "-4", "", "-6")) +
  annotate(geom="text", x=2040-2021+2, y=-6, 
           label="Birth Cohorts <2020", hjust=0, size=3.5) +
  annotate(geom="text", x=2040-2021-2, y=-6, 
           label="Birth~Cohorts~2020-2024", hjust=0, size=3.5, parse=TRUE) +
  annotate(geom="text", x=2040-2025-2, y=-6, 
           label="Birth Cohorts >2024", hjust=0, size=3.5) +
  geom_rect(aes(ymin=3.5, ymax=4.5, xmin=90, xmax=95), fill="#1B7837") +
  geom_rect(aes(ymin=3.5, ymax=4.5, xmin=85, xmax=90), fill="#762A83") +
  annotate(geom="text", y=4.75, x=92.5, label="Male", size=5, hjust=0, family="serif") + 
  annotate(geom="text", y=4.75, x=87.5, label="Female", size=5, hjust=0, family="serif")
  
}

ggarrange(pyramid_MO.., pyramid_..FE, pyramid_.MI., 
          ncol=3) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(filename=here("plots", paste0("pyramid_scenario_", j, ".pdf")), 
       width = 15/2.54*900, height = 7.5/2.54*900, units="px", device="pdf")

}

##generate gif of changes over time
for (j in unique(Projection_cf_all$Scenario)) {
for (i in c("absolute", "percent")) {

imgs <- 
  list.files(here("plots", "pyramid", paste0(i), "with year", paste(j)),
             full.names=TRUE)

img_list <-
  lapply(imgs, image_read)

img_joined <-
  image_join(img_list)

img_animated <-
  image_animate(img_joined, fps=2, optimize=TRUE)

image_write(image = img_animated,
            path = here("plots", "pyramid", paste0(i), "with year", paste(j), "pyramid.gif"))

rm(imgs, img_list,img_joined, img_animated)
gc()
}
}

#plot missing population by age group
pal_f <- brewer.pal(10, "PRGn")[1:5]
pal_m <- brewer.pal(10, "PRGn")[6:10]

delta <-
  Projection_... %>% 
  mutate(Diff = Pop - Projection_MOMIFE$Pop) %>% 
  mutate(AgeGrp = case_when(Age %in% c(0:14) ~ "0-14",
                            Age %in% c(15:49) ~ "15-49",
                            Age %in% c(50:64) ~ "50-64",                            
                            Age %in% c(65:84) ~ "65-84",
                            Age %in% c(85:100)~ "85+")) %>% 
  group_by(Year, Sex, AgeGrp) %>% 
  summarize(Diff_absolute = sum(Diff),
            Diff_percent = -100*abs(sum(Diff))/sum(Pop)) %>% 
  ungroup() 

delta$Sex <- factor(delta$Sex, levels = c("Male", "Female"))

delta_agegrp_absolute <-
  ggplot() +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="85+" & Sex=="Male"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_m[5], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="65-84" & Sex=="Male"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_m[4], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="50-64" & Sex=="Male"), 
              aes(x=Year, y=Diff_absolute/1000,fill="Male"), color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="15-49" & Sex=="Male"), 
            aes(x=Year, y=Diff_absolute/1000), fill=pal_m[2], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="0-14" & Sex=="Male"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_m[1], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="85+" & Sex=="Female"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_f[1], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="65-84" & Sex=="Female"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_f[2], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="50-64" & Sex=="Female"), 
              aes(x=Year, y=Diff_absolute/1000, fill="Female"), color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="15-49" & Sex=="Female"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_f[4], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="0-14" & Sex=="Female"), 
              aes(x=Year, y=Diff_absolute/1000), fill=pal_f[5], color="transparent") +
    ylab(Delta ~ "Population (in Thousands)") +
    xlab("") +
    scale_fill_manual(name=" ", values=c("Male"=pal_m[3],
                                         "Female"=pal_f[3]),
                      guide = guide_legend(reverse=TRUE)) +
    scale_x_continuous(limits=c(2020, 2060),
                       breaks=(seq(2020, 2060, 5)),
                       labels=c("2020", "", "2030",
                                "", "2040", "", 
                                "2050", "", "2060"),
                       minor_breaks=(seq(2020, 2060, 1)),
                       guide="prism_minor") +
    scale_y_continuous(limits=c(-500, 0),
                       breaks=(seq(-500, 0, 50))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=12),
          axis.title.y=element_text(size=16,
                                    vjust=3),
          strip.text=element_text(size=12),
          legend.position=c(.94, .1), 
          legend.title=element_text(size=12),
          legend.text=element_text(size=12),
          legend.key.height=unit(1, 'cm'),
          legend.key.width=unit(1, 'cm'),
          strip.background=element_blank(),
          strip.text.x=element_text(size=16),
          strip.text.y=element_blank(),
          panel.spacing=unit(0.75, "cm"),
          plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          text=element_text(family="serif")) +
    facet_grid(rows=vars(Sex), 
               cols=vars(AgeGrp)) 

delta_agegrp_percent <-
  ggplot() +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="85+" & Sex=="Male"), 
              aes(x=Year, y=Diff_percent), fill=pal_m[5], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="65-84" & Sex=="Male"), 
              aes(x=Year, y=Diff_percent), fill=pal_m[4], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="50-64" & Sex=="Male"), 
              aes(x=Year, y=Diff_percent,fill="Male"), color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="15-49" & Sex=="Male"), 
              aes(x=Year, y=Diff_percent), fill=pal_m[2], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="0-14" & Sex=="Male"), 
              aes(x=Year, y=Diff_percent), fill=pal_m[1], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="85+" & Sex=="Female"), 
              aes(x=Year, y=Diff_percent), fill=pal_f[1], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="65-84" & Sex=="Female"), 
              aes(x=Year, y=Diff_percent), fill=pal_f[2], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="50-64" & Sex=="Female"), 
              aes(x=Year, y=Diff_percent, fill="Female"), color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="15-49" & Sex=="Female"), 
              aes(x=Year, y=Diff_percent), fill=pal_f[4], color="transparent") +
    geom_area(data=delta %>% filter(Year<=2060 & AgeGrp=="0-14" & Sex=="Female"), 
              aes(x=Year, y=Diff_percent), fill=pal_f[5], color="transparent") +
    ylab(Delta ~ "Population (%)") +
    xlab("") +
    scale_fill_manual(name=" ", values=c("Male"=pal_m[3],
                                         "Female"=pal_f[3]),
                      guide = guide_legend(reverse=TRUE)) +
    scale_x_continuous(limits=c(2020, 2060),
                       breaks=(seq(2020, 2060, 5)),
                       labels=c("2020", "", "2030",
                                "", "2040", "", 
                                "2050", "", "2060"),
                       minor_breaks=(seq(2020, 2060, 1)),
                       guide="prism_minor") +
    scale_y_continuous(limits=c(-3.5, 0),
                       breaks=(seq(-3.5, 0, 0.5))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=12),
          axis.title.y=element_text(size=16,
                                    vjust=3),
          strip.text=element_text(size=12),
          legend.position=c(.94, .1), 
          legend.title=element_text(size=12),
          legend.text=element_text(size=12),
          legend.key.height=unit(1, 'cm'),
          legend.key.width=unit(1, 'cm'),
          strip.background=element_blank(),
          strip.text.x=element_text(size=16),
          strip.text.y=element_blank(),
          panel.spacing=unit(0.75, "cm"),
          plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          text=element_text(family="serif")) +
    facet_grid(rows=vars(Sex), 
               cols=vars(AgeGrp)) 

ggsave(filename=here("plots", paste0("delta_agegrp_absolute.pdf")), 
       width = 10/2.54*900, height = 7/2.54*900, units="px",
       delta_agegrp_absolute, device="pdf")

ggsave(filename=here("plots", paste0("delta_agegrp_percent.pdf")), 
       width = 10/2.54*900, height = 7/2.54*900, units="px",
       delta_agegrp_percent, device="pdf")

#differences in dependency ratio: table/plot
##see this blog for code: https://themockup.blog/posts/2020-09-04-10-table-rules-in-r
DR_table <-
  Projection_cf_all %>% 
    left_join(Projection_... %>% 
                select(-Scenario) %>% 
                rename(bl=Pop),
              by=c("Age", "Year", "Sex")) %>% 
    filter(Scenario %in% c("MO--", "-MI-", "--FE", "MOMIFE")) %>% 
    filter(Year<=2060) %>% 
    mutate(group=case_when(Age<15 ~ "young",
                           Age>64 ~ "old",
                           TRUE ~"working")) %>% 
    group_by(Scenario, Year, group) %>% 
    summarize(cf=sum(Pop),
              bl=sum(bl)) %>% 
    pivot_wider(names_from="group",
                values_from=c("cf", "bl")) %>% 
    ungroup() %>% 
    mutate(cf_young=100*cf_young/cf_working,
           cf_old=100*cf_old/cf_working,
           bl_young=100*bl_young/bl_working,         
           bl_old=100*bl_old/bl_working,
           cf_total=cf_young+cf_old,
           bl_total=bl_young+bl_old) %>% 
    select(-cf_working, -bl_working) %>% 
    mutate(`Panel A: Young-age`=bl_young-cf_young,
           `Panel B: Old-age`=bl_old-cf_old,
           `Panel C: Total`=bl_total-cf_total) %>% 
    select(-c(bl_young, cf_young, bl_old, cf_old, bl_total, cf_total)) %>% 
    pivot_longer(cols=c("Panel A: Young-age", 
                        "Panel B: Old-age", 
                        "Panel C: Total"),
                 names_to="DR",
                 values_to="delta") %>% 
    mutate(DR=factor(DR, levels=c("Panel A: Young-age", "Panel B: Old-age", "Panel C: Total")),
           Scenario=case_when(Scenario=="MO--" ~ "Mortality",
                              Scenario=="-MI-" ~ "Migration",
                              Scenario=="--FE" ~ "Fertility",
                              TRUE ~ "All"),
           Scenario=factor(Scenario, levels=c("All", "Mortality", "Fertility", "Migration"))) %>% 
    arrange(DR, Scenario)

DR_minmax <- range(DR_table$delta)
DR_limits <- DR_minmax
DR_limits[1] <- plyr::round_any(DR_limits[1], 0.2, f=floor)
DR_limits[2] <- abs(DR_limits[1])

DR_minyear <- DR_table %>% filter(delta==DR_minmax[1]) %>% pull(Year)
DR_maxyear <- DR_table %>% filter(delta==DR_minmax[2]) %>% pull(Year)

plot_spark <- function(data){

plot <-  
  data %>% 
    ggplot(aes(x=Year, y=delta)) +
    geom_hline(yintercept=0, linetype="dotted", linewidth=5) 

if(min(data$delta)==DR_minmax[1]){

plot <-
 plot +
    geom_segment(aes(x=DR_minyear,
                     y=DR_minmax[1],
                     xend=DR_minyear,
                     yend=0.075), linewidth=5, color="#6E6E6E") 

}else{}

if(max(data$delta)==DR_minmax[2]){
  
plot <-
   plot +   
    geom_segment(aes(x=DR_maxyear,
                     y=-0.075,
                     xend=DR_maxyear,
                     yend=DR_minmax[2]), linewidth=5, color="#6E6E6E") 
}else{}

plot <-
  plot +
    geom_line(linewidth=10, color=brewer.pal(5, "RdBu")[5]) +
    geom_point(data=data %>% filter(Year %in% c(2025, 2040, 2060)),
               aes(x=Year, y=delta), size=20, color=brewer.pal(5, "RdBu")[1]) +
    geom_text(data=data %>% filter(delta==DR_minmax[1]), aes(x=Year, y=0, label=round(delta,2)), size=40, hjust=0, vjust=-0.5, color="#6E6E6E") +
    geom_text(data=data %>% filter(delta==DR_minmax[2]), aes(x=Year, y=0, label=round(delta,2)), size=40, hjust=1, vjust=+1.5, color="#6E6E6E") +    
    scale_y_continuous(limits=DR_limits) +
    theme_void() +
    scale_color_identity() +
    theme(legend.position="none") 

}

plots <- 
  DR_table %>% 
    select(DR, Scenario, Year, delta) %>% 
    nest(trend=c(Year, delta)) %>% 
    mutate(plot=map(trend, plot_spark)) %>% 
    select(-trend)

DR_trends <-
  DR_table %>% 
  filter(Year %in% c(2025, 2040, 2060)) %>% 
  pivot_wider(names_from="Year",
              values_from="delta") %>% 
  mutate(ggplot=NA) %>% 
  gt(groupname_col="DR") %>% 
  text_transform(
    locations=cells_body(columns=ggplot),
    fn=function(x){
      map(plots$plot, ggplot_image, height=px(30), aspect_ratio=2)
    }
  )  %>% 
  cols_width(columns=ggplot ~ px(100)) %>% 
  cols_label(
    Scenario="",
    ggplot="Trend"
  ) %>% 
  fmt_number(3:5) %>% 
  tab_spanner(
    label = "\u0394 Dependency Ratio",
    columns = c(3,4,5,6)
  ) %>% 
  tab_style(
    style=cell_text(color="black", weight="bold"),
    locations=list(
      cells_row_groups(),
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )
  ) %>%  
  tab_options(
    table.border.top.width=px(3),
    table.border.top.color="transparent",
    table.border.bottom.width=px(2.2),
    table.border.bottom.color="black",
    table_body.hlines.color="white",
    column_labels.border.bottom.width=px(2),
    column_labels.border.bottom.color="black",
    row_group.border.top.width=px(2),
    row_group.border.top.color="black",
    row_group.border.bottom.width=px(2),
    row_group.border.bottom.color="black",
    source_notes.font.size=px(16)
  ) 

gtsave(DR_trends, here("plots", "dependency-ratio-trends.png"), expand=25) ##requires google chrome
gtsave(DR_trends, here("plots", "dependency-ratio-trends.pdf"), expand=25)

#relative difference in 2025, 2040, 2060: table 
delta_percent_table <-
  Projection_cf_all %>% 
    left_join(Projection_... %>% 
                select(-Scenario) %>% 
                rename(bl=Pop),
              by=c("Age", "Year", "Sex")) %>% 
    filter(Scenario %in% c("MO--", "-MI-", "--FE", "MOMIFE")) %>% 
    filter(Year %in% c(2025, 2040, 2060)) %>% 
    group_by(Scenario, Year) %>% 
    summarize(cf=sum(Pop),
              bl=sum(bl)) %>% 
    ungroup() %>% 
    mutate(Delta=paste0(round(100*(bl-cf)/bl, 2), "%")) %>%
    select(-c(cf, bl)) %>%
    mutate(Scenario=factor(Scenario,
                           levels=c("All"="MOMIFE", "Mortality"="MO--",
                                    "Fertility"="--FE", "Migration"="-MI-"), 
                           labels=c("All", "Mortality", "Fertility", "Migration"))) %>% 
    arrange(Scenario, Year) %>% 
    pivot_wider(names_from="Scenario",
                values_from=c("Delta"))

write.xlsx(data.frame(delta_percent_table), 
           here("plots", "delta_percent_table.xlsx"), 
           sheetName="delta_percent",
           row.names = FALSE, col.names = TRUE, append = FALSE)

#values reported in manuscript
manuscript <-
  Projection_... %>% 
  mutate(All = Pop - Projection_MOMIFE$Pop,  
         Mortality = Pop - Projection_MO..$Pop,  
         Migration = Pop - Projection_.MI.$Pop,  
         Fertility = Pop - Projection_..FE$Pop) %>% 
  select(-c(Scenario))

manuscript %>% ##total population
  group_by(Year) %>% 
  summarize(across(.cols=c(All, Mortality, Migration, Fertility, Pop), .fns=sum)) %>% 
  ungroup() %>% 
  mutate(All_r=round(100*All/Pop, 2),
         Mortality_r=round(100*Mortality/Pop, 2), 
         Migration_r=round(100*Migration/Pop, 2), 
         Fertility_r=round(100*Fertility/Pop, 2)) %>% 
  filter(Year %in% c(2025, 2040, 2060)) %>% 
  select(-Pop)

manuscript %>% ##reproductive-age population
  filter(Age %in% 15:49) %>% 
  group_by(Year) %>% 
  summarize(across(.cols=c(All, Mortality, Migration, Fertility, Pop), .fns=sum)) %>% 
  ungroup() %>% 
  mutate(All_r=round(100*All/Pop, 2),
         Mortality_r=round(100*Mortality/Pop, 2), 
         Migration_r=round(100*Migration/Pop, 2), 
         Fertility_r=round(100*Fertility/Pop, 2)) %>% 
  filter(Year %in% c(2025, 2040, 2060)) %>% 
  select(-Pop)

manuscript %>% ##working-age population
  filter(Age %in% 15:64) %>% 
  group_by(Year) %>% 
  summarize(across(.cols=c(All, Mortality, Migration, Fertility, Pop), .fns=sum)) %>% 
  ungroup() %>% 
  mutate(All_r=round(100*All/Pop, 2),
         Mortality_r=round(100*Mortality/Pop, 2), 
         Migration_r=round(100*Migration/Pop, 2), 
         Fertility_r=round(100*Fertility/Pop, 2)) %>% 
  filter(Year %in% c(2025, 2040, 2060)) %>% 
  select(-Pop)

manuscript %>% ##old population
  filter(Age>=85) %>% 
  group_by(Year) %>% 
  summarize(across(.cols=c(All, Mortality, Migration, Fertility, Pop), .fns=sum)) %>% 
  ungroup() %>% 
  mutate(All_r=round(100*All/Pop, 2),
         Mortality_r=round(100*Mortality/Pop, 2), 
         Migration_r=round(100*Migration/Pop, 2), 
         Fertility_r=round(100*Fertility/Pop, 2)) %>% 
  filter(Year %in% c(2025, 2040, 2060)) %>% 
  select(-Pop)

manuscript %>% ##cohort 2020
  filter(Year==2025 & Age==4) %>% 
  summarize(across(.cols=c(All, Mortality, Migration, Fertility, Pop), .fns=sum)) %>% 
  ungroup() %>% 
  mutate(All_r=round(100*All/Pop, 2),
         Mortality_r=round(100*Mortality/Pop, 2), 
         Migration_r=round(100*Migration/Pop, 2), 
         Fertility_r=round(100*Fertility/Pop, 2)) %>%
  select(-Pop)
  
manuscript %>% ##cohort 2020
  filter(Year==2040 & Age==19) %>% 
  summarize(across(.cols=c(All, Mortality, Migration, Fertility, Pop), .fns=sum)) %>% 
  ungroup() %>% 
  mutate(All_r=round(100*All/Pop, 2),
         Mortality_r=round(100*Mortality/Pop, 2), 
         Migration_r=round(100*Migration/Pop, 2), 
         Fertility_r=round(100*Fertility/Pop, 2)) %>%
  select(-Pop)
