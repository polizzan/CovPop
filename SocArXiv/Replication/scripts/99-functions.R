############
#FUNCTIONS#
##########

#derive survivorship ratio, Sx, from age-specific mortality rates
Sx <- function(nmx, radix=1, female=TRUE) {
  
nax <- rep(0.5, length(nmx)) ##average number of years lived in age interval
  
if (female==TRUE) { ##nax in age interval 0-1 dependent on nmx and sex (following HMD methodology)
  
  if (nmx[1] >= 0 & nmx[1] < 0.01724) {
  
  nax[1] <- 0.14903 - 2.05527 * nmx[1]
  
  } else {
    
    if (nmx[1] >= 0.01724 & nmx[1] < 0.06891) {

  nax[1] <- 0.04667 + 3.88089 * nmx[1]
      
    } else {
      
  nax[1] <- 0.31411
      
    }
  }
  
} else {
  
  if (nmx[1] >= 0 & nmx[1] < 0.02300) {
    
    nax[1] <- 0.14929 - 1.99545 * nmx[1]
    
  } else {
    
    if (nmx[1] >= 0.02300 & nmx[1] < 0.08307) {
      
      nax[1] <- 0.02832 + 3.26021 * nmx[1]
      
    } else {
      
      nax[1] <- 0.29915
      
    }
  }
}
  
nax[length(nmx)] <- 1/nmx[length(nmx)] ##nax in open-ended age interval = inverse of nmx

nqx <- nmx/(1+(1-nax)*nmx) ##probability of death (Preston et al. 2001: 47)
nqx[length(nmx)] <- 1 ##probability of death in open-ended age interval = 1

npx <- 1-nqx ##probability of survival in age interval

lx <- rep(radix, length(nmx))

for (i in 2:length(nmx)) {
  
  lx[i] <- lx[i-1] * npx[i-1] ##number of survivors at the beginning of age interval
  
}

ndx <- lx * nqx ##number of deaths in age interval
nLx <- lx * npx + nax * ndx ##years lived in age interval
nLx[length(nmx)] <- lx[length(nmx)]/nmx[length(nmx)] ##years lived in open-ended age interval (Preston et al. 2001: 49)

#survivorship ratios (see Preston et al. 2001: chapter 6.3)
Sx <- nLx/lag(nLx) ##survivorship ratio
Sx[1] <- nLx[1]/radix ##survivorship ratio in age interval 0-1
Sx[length(nmx)] <- nLx[length(nmx)]/(nLx[length(nmx)] + nLx[length(nmx)-1]) ##survivorship ratio in open-ended age interval

return(Sx)
  
}

#cohort component projection model
CCPM <- function(Pop_f, ##vector of age-specific starting population (female)
                 Pop_m, ##vector of age-specific starting population (male) 
                 Sx_f, ##matrix of age-specific survivorship ratios (female) 
                 Sx_m, ##matrix of age-specific  survivorship ratios (male)
                 Fx, ##matrix of age-specific fertility rates (female)
                 SRB, ##vector of sex ratios at birth (male to female)
                 Migra_f, ##matrix of age-specific migration counts (female)
                 Migra_m) { ##matrix of age-specific migration counts (male)
  
Projection_f <- Pop_f ##vectors of age-specific starting populations 
Projection_m <- Pop_m ##(will be expanded to matrices containing complete population projections below)

Leslie <- 
  Leslie_f <- ##array of female Leslie matrices (complete with female births + female mortality)
  Leslie_m <- ##array of male Leslie matrices (male mortality only)
  Births_m <- ##array of female Leslie matrices (male births only)
  array(0,
        dim=c(dim(Pop_f)[1], ##number of rows in Leslie matrix = number of age groups
              dim(Pop_f)[1], ##number of columns in Leslie matrix = number of age groups
              dim(Sx_f)[2])) ##number of Leslie matrices = number of complete sets of survivorship ratios

for (j in 1:dim(Sx_f)[2]) { ##stepwise construction of Leslie matrices + projection
  
  ##first row of Leslie matrix = fertility (for construction, see Preston et al. 2001: 130)
  k_f <-
    1/2 *
    (1/(1+SRB[j])) *
    Sx_f[1, j]
  
  k_m <-
    1/2 *
    (SRB[j]/(1+SRB[j])) *
    Sx_m[1, j]
  
  Leslie[1, 15, j] <- ##age 14
    Fx[16, j] *
    Sx_f[16, j]
  
  for (i in 16:49) { ##ages 15-48
    
    Leslie[1, i, j] <- 
      Fx[i, j] +
      Fx[i+1, j] *
      Sx_f[i+1, j]
    
  }
  
  Leslie[1, 50, j] <- ##age 49
    Fx[50, j]
  
  Leslie_f[1, 15:50, j] <-
    Leslie[1, 15:50, j] * k_f ##to obtain female births
  
  Births_m[1, 15:50, j] <-
    Leslie[1, 15:50, j] * k_m ##to obtain male births
  
  ##remaining rows of Leslie matrix = mortality
  for (i in 0:(dim(Sx_f)[1]-2)) {
    
    Leslie_f[i+2, i+1, j] <- ##survival to next age group (columns -> rows), female
      Sx_f[i+2, j]
    
    Leslie_m[i+2, i+1, j] <- ##survival to next age group (columns -> rows), male
      Sx_m[i+2, j]
    
  }
  
  Leslie_f[dim(Sx_f)[1], dim(Sx_f)[1], j] <- ##survival in open-ended age interval (female)
    Sx_f[dim(Sx_f)[1], j]
  
  Leslie_m[dim(Sx_f)[1], dim(Sx_f)[1], j] <- ##survival in open-ended age interval (male)
    Sx_m[dim(Sx_f)[1], j]
  
  ##population projection
  aux_f <- ##female projection
    Leslie_f[, , j] %*% Projection_f[, j] + ##multiply vector of population counts with appropriate Leslie matrix
    Migra_f[, j] ##add female migration
  
  Projection_f <-
    cbind(Projection_f, aux_f) ##add vector of projected population as new column to existing matrix
  
  aux_m <- ##male projection
    Leslie_m[, , j] %*% Projection_m[, j] + ##multiply vector of population counts with appropriate Leslie matrix = male survival only
    Births_m[, , j] %*% Projection_f[, j] + ##add male births
    Migra_m[, j] ##add male migration
  
  Projection_m <-
    cbind(Projection_m, aux_m) ##add vector of projected population as new column to existing matrix
  
}

colnames(Projection_f) <- 0:(dim(Sx_f)[2]) ##rename matrix columns = numbers starting from 0
colnames(Projection_m) <- 0:(dim(Sx_f)[2]) ##rename matrix columns = numbers starting from 0

##combine male and female projections
Projection <-
  Projection_f %>% ##female projections
  as_tibble() %>%
  mutate(Age = 0:(dim(Sx_f)[1]-1)) %>% ##add age column
  pivot_longer(col = c(-Age), ##reshape to long format
               names_to = "Year",
               values_to = "Pop") %>% 
  mutate(Year = as.numeric(Year)) %>% ##column names become Year variable: declare as numeric
  mutate(Sex = "Female") %>% ##add sex variable
  add_row(
    Projection_m  %>% ##male projections
      as_tibble() %>%
      mutate(Age = 0:(dim(Sx_f)[1]-1)) %>% ##add age column
      pivot_longer(col = c(-Age), ##reshape to long format
                   names_to = "Year",
                   values_to = "Pop") %>% 
      mutate(Year = as.numeric(Year)) %>% ##column names become Year variable: declare as numeric
      mutate(Sex = "Male") ##add sex variable
  ) %>%
  arrange(Year, Sex, Age) ##sort combined data set

return(Projection)
  
}

#plot population pyramids
pyramid <-
  function(observed, counterfactual, year, percent=FALSE){
    
    colF = brewer.pal(11, "PRGn")[2]  
    colM = brewer.pal(11, "PRGn")[10]  
    
    sex = rep(c("Female", "Male") , each=101)   
    age = rep(c(0:(length(observed)/2-1)), 2)
    
    df <- data.frame(sex, age, observed, counterfactual)
    
    
    df <- 
      df %>% 
      mutate(diff = counterfactual - observed,
             `Alive in No-Covid Scenario Only` = case_when(diff > 0 ~ diff,
                                                           TRUE ~ 0),
             `Alive in Covid Scenario Only` = case_when(diff < 0 ~ abs(diff),
                                                        TRUE ~ 0),
             `Alive in Both Scenarios` = case_when(diff > 0 ~ counterfactual - diff,
                                                   diff <= 0 ~ counterfactual)) %>% 
      select(-diff)
    
    
    if(percent==TRUE){
      
      df <-
        df %>% 
        mutate(across(.cols=5:7, ~ .x/observed*100)) %>% 
        select(-observed, -counterfactual) %>% 
        pivot_longer(cols = -c("sex", "age"),
                     names_to = "scenario",
                     values_to = "pop") %>% 
        mutate(Scenario = fct_relevel(scenario, 
                                      "Alive in Covid Scenario Only", 
                                      "Alive in No-Covid Scenario Only", 
                                      "Alive in Both Scenarios"))}
    else{
      
      df <-
        df %>%
        mutate(across(.cols=5:7, ~ .x/1000)) %>% 
        select(-observed, -counterfactual) %>% 
        pivot_longer(cols = -c("sex", "age"),
                     names_to = "scenario",
                     values_to = "pop") %>% 
        mutate(Scenario = fct_relevel(scenario, 
                                      "Alive in Covid Scenario Only", 
                                      "Alive in No-Covid Scenario Only", 
                                      "Alive in Both Scenarios"))
      
    }
    
    df$pop <- 
      ifelse(df$sex == "Male", 
             -1 * df$pop, 
             df$pop)
    
    
    plot <-
      ggplot() + 
      geom_bar(data = df %>% filter(sex=="Male" & scenario=="Alive in No-Covid Scenario Only"), 
               aes(x = age, y = pop), fill=colM , color="gray80", size=0.0001, width=1, position="stack", stat = "identity") +
      geom_bar(data = df %>% filter(sex=="Female" & Scenario=="Alive in No-Covid Scenario Only"), 
               aes(x = age, y = pop), fill=colF, color="gray80", size=0.0001, width=1, position="stack", stat = "identity") +
      ggtitle(paste(year)) +
      xlab("Age") +
      scale_x_continuous(limits = c(-5, 105),
                         breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +   
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(size = 20, face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=12),
            axis.title.y=element_text(size=14,
                                      vjust=0.75),
            axis.title.x=element_text(size=14,
                                      vjust=-0.75))
    
    if (year>=2021){
      
      plot <-
        plot +
        geom_vline(xintercept=year-2021+0.5, linetype="dashed")
      
    }else{}
    
    if (year<=2025){}else{
      plot <-
        plot +
        geom_vline(xintercept=year-2025-0.5, linetype="dashed")
      
    }
    
    if(percent==TRUE){
      
      plot <-
        plot +
        ylab(expression(Delta ~ "Population (%)")) +
        scale_y_continuous(limits = c(-10, 10),
                           breaks = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 
                                      1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                           labels = paste0(as.character(c(seq(-10, 0, 1), seq(-1, -10, -1))))) +   
        annotate(geom="text", x=105, y=10, label="Female",
                 color="black", size=6, hjust=1) +
        annotate(geom="text", x=105, y=-10, label="Male",
                 color="black", size=6, hjust=0) 
      
      if (year>=2021){
        
        plot <-
          plot +
          annotate(geom="text", x=year-2021-2, y=-10, label="Birth~Cohorts~2020-2024", hjust=0, parse=TRUE) +
          annotate(geom="text", x=year-2021+2, y=-10, label="Birth Cohorts <2020", hjust=0)
        
      }else{}
      
      
      if (year<=2025){}else{
        plot <-
          plot +
          annotate(geom="text", x=year-2025-2, y=-10, label="Birth Cohorts >2024", hjust=0)
        
      }
      
    }else{
      
      plot <-
        plot +
        ylab(expression(Delta ~ "Population (in Thousands)")) +
        scale_y_continuous(limits = c(-45, 45),
                           breaks = c(-45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 
                                      5, 10, 15, 20, 25, 30, 35, 40, 45),
                           labels = paste0(as.character(c(seq(-45, 0, 5), seq(-5, -45, -5))))) +   
        annotate(geom="text", x=105, y=45, label="Female",
                 color="black", size=6, hjust=1) +
        annotate(geom="text", x=105, y=-45, label="Male",
                 color="black", size=6, hjust=0) 
      
      
      if (year>=2021){
        
        plot <-
          plot +
          annotate(geom="text", x=year-2021-2, y=-45, label="Birth~Cohorts~2020-2024", hjust=0, parse=TRUE) +
          annotate(geom="text", x=year-2021+2, y=-45, label="Birth Cohorts <2020", hjust=0)
        
      }else{}
      
      if (year<=2025){}else{
        plot <-
          plot +
          annotate(geom="text", x=year-2025-2, y=-45, label="Birth Cohorts >2024", hjust=0)
        
      }
      
    }
    
    return(plot)
    
  }
