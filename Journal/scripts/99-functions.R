#############
# FUNCTIONS #
#############

# ---

######################
# SURVIVORSHIP RATIO #
######################

## derive survivorship ratios, Sx, from age-specific mortality rates
Sx <- function(nmx, radix=1, female=TRUE) {
  
## years lived by those who die in age interval, nax  
nax <- rep(0.5, length(nmx))  
  
if(female==TRUE){ ## nax in age interval 0-1 dependent on nmx and sex (Andreev & Kingkade)
  
  if(nmx[1] >= 0 & nmx[1] < 0.01724){
  
  nax[1] <- 0.14903 - 2.05527 * nmx[1]
  
  }else{
    
    if(nmx[1] >= 0.01724 & nmx[1] < 0.06891){

  nax[1] <- 0.04667 + 3.88089 * nmx[1]
      
    }else{
      
  nax[1] <- 0.31411
      
    }
  }
  
}else{
  
  if(nmx[1] >= 0 & nmx[1] < 0.02300){
    
    nax[1] <- 0.14929 - 1.99545 * nmx[1]
    
  }else{
    
    if(nmx[1] >= 0.02300 & nmx[1] < 0.08307){
      
      nax[1] <- 0.02832 + 3.26021 * nmx[1]
      
    }else{
      
      nax[1] <- 0.29915
      
    }
  }
}
  
nax[length(nmx)] <- 1/nmx[length(nmx)] ## nax in open-ended age interval = inverse of nmx

## probability of death, nqx (Preston et al. 2001: 49)
nqx <- nmx/(1+(1-nax)*nmx) 
nqx[length(nmx)] <- 1 ## nqx in open-ended age interval = 1

## probability of survival, npx
npx <- 1-nqx 

## number of survivors at beginning of age interval, lx
lx <- rep(radix, length(nmx))

for(i in 2:length(nmx)){
  
  lx[i] <- lx[i-1] * npx[i-1] 
  
}

## number of deaths in age interval, ndx
ndx <- lx * nqx 

## years lived in age interval, nLx (Preston et al. 2001: 49)
nLx <- lx * npx + nax * ndx 
nLx[length(nmx)] <- lx[length(nmx)]/nmx[length(nmx)] ## years lived in open-ended age interval 

## survivorship ratios, Sx (Preston et al. 2001: chapter 6.3)
Sx <- nLx/lag(nLx) 
Sx[1] <- nLx[1]/radix ## survivorship ratio in age interval 0-1
Sx[length(nmx)] <- nLx[length(nmx)]/(nLx[length(nmx)] + nLx[length(nmx)-1]) ## survivorship ratio in open-ended age interval

return(Sx)
  
}

##########################
# COHORT COMPONENT MODEL #
##########################

CCPM <- function(Pop_f, ## column vector of age-specific starting population (female)
                 Pop_m, ## column vector of age-specific starting population (male) 
                 Sx_f, ## matrix of age-specific survivorship ratios (female) 
                 Sx_m, ## matrix of age-specific  survivorship ratios (male)
                 Fx, ## matrix of age-specific fertility rates (female)
                 SRB, ## vector of sex ratios at birth (male to female)
                 Migra_f, ## matrix of age-specific migration counts (female)
                 Migra_m, ## matrix of age-specific migration counts (male)
                 stochastic = FALSE){ 
  
  Projection_f <- Pop_f ## column vector of female population, to be turned into a matrix
  Projection_m <- Pop_m ## column vector of male population, to be turned into a matrix
  
  for(j in 1:dim(Sx_f)[2]){ ## for each year of the projection period
  
    aux_f <- ## column vector of female/male population at start of next time interval
      aux_m <-
      matrix(0, 
             nrow = dim(Pop_f)[1],
             ncol = 1)
    
    ## STEP I: age-wise survival of population to next age group
    for(i in 1:(dim(Pop_f)[1]-1)){
      
      aux_f[i+1, 1] <- ## female
        
        if(stochastic){
          
          rbinom(n=1, size=trunc(Projection_f[i, j]), prob=Sx_f[i+1, j]) ## trunc(): binomial requires integers for 'size' argument
      
        }else{
          
          Projection_f[i, j] * Sx_f[i+1, j]
          
        }
          
      aux_m[i+1, 1] <- ## male
        
        if(stochastic){
          
          rbinom(n=1, size=trunc(Projection_m[i, j]), prob=Sx_m[i+1, j])
      
        }else{
          
          Projection_m[i, j] * Sx_m[i+1, j]
          
        }  
      } 
    
    ## survival in open-ended age interval
    aux_f[dim(Pop_f)[1], 1] <- ## female
      
      if(stochastic){
        
        aux_f[dim(Pop_f)[1], 1] + rbinom(n=1, size=trunc(Projection_f[dim(Pop_f)[1], j]), prob=Sx_f[dim(Sx_f)[1], j])
    
      }else{
        
        aux_f[dim(Pop_f)[1], 1] + Projection_f[dim(Pop_f)[1], j] * Sx_f[dim(Sx_f)[1], j]
        
      }  
        
    aux_m[dim(Pop_m)[1], 1] <- ## male
      
      if(stochastic){
        
        aux_m[dim(Pop_m)[1], 1] + rbinom(n=1, size=trunc(Projection_m[dim(Pop_m)[1], j]), prob=Sx_m[dim(Sx_m)[1], j])
    
      }else{
        
        aux_m[dim(Pop_m)[1], 1] + Projection_m[dim(Pop_m)[1], j] * Sx_m[dim(Sx_m)[1], j]
        
      }  

    ## STEP II: fertility
    ## calculate total number of births
    births <- 1/2 * (Fx[, j] %*% Projection_f[, j] + 
                       Fx[, j] %*% aux_f)
    
    if(stochastic){
      
    aux0 <- rpois(n=1, lambda=c(births))
    
    }else{
      
    aux0 <- c(births)  
      
    }
    
    ## split into female and male births
    aux0_f <- aux0 * (1/(1+SRB[j]))
    aux0_m <- aux0 * (SRB[j]/(1+SRB[j]))
    
    ## STEP III: survival of newborns
    if(stochastic){
    
    aux_f[1, 1] <- rbinom(n=1, size=trunc(aux0_f), prob=Sx_f[1, j])
    aux_m[1, 1] <- rbinom(n=1, size=trunc(aux0_m), prob=Sx_m[1, j])
    
    }else{
      
    aux_f[1, 1] <- aux0_f * Sx_f[1, j]
    aux_m[1, 1] <- aux0_m * Sx_m[1, j]      
      
    }

    ## STEP IV: add migration
    aux_f <- aux_f + Migra_f[, j]
    aux_m <- aux_m + Migra_m[, j]
    
    Projection_f <-
      cbind(Projection_f, aux_f) ## add vector of projected population as new column to existing matrix
    
    Projection_m <-
      cbind(Projection_m, aux_m) ## add vector of projected population as new column to existing matrix
    
  }
  
  ## STEP V: generate matrix with projection output
  Projection <- 
    cbind(
      
      c(rbind(Projection_f, Projection_m)), ## population
      
      rep(0:(dim(Projection_f)[1]-1), 2 * dim(Projection_f)[2]), ## age
      
      rep(rep(c(0, 1), each = dim(Projection_f)[1]), dim(Projection_f)[2]), ## sex
      
      rep(0:(dim(Projection_f)[2]-1), each = 2 * dim(Projection_f)[1]) ## year
    
    )

  return(Projection)
  
}

#################
# PYRAMID PLOTS #
#################

pyramid <- function(mid, 
                    lower, 
                    upper, 
                    year, 
                    title = NULL, 
                    limit.1, 
                    limit.2, 
                    percent = FALSE, 
                    show.x = TRUE,
                    show.y = TRUE,
                    font = "serif"){  
  
  pal_f <- RColorBrewer::brewer.pal(10, "PRGn")[c(3, 1)]
  pal_m <- RColorBrewer::brewer.pal(10, "PRGn")[c(8, 10)]
  
  if(show.x){
    
    color.x <- "black"
    
  }else{
    
    color.x <- "transparent"  
    
  }
  
  
  if(show.y){
    
    color.y <- "black"
    
  }else{
    
    color.y <- "transparent"  
    
  }
  
  title <- ifelse(is.null(title), year, title)
  
  step <- ifelse(percent==TRUE, 2, 10)  
  limit.1 <- plyr::round_any(limit.1, f=ceiling, accuracy=step)
  limit.2 <- plyr::round_any(limit.2, f=ceiling, accuracy=step)
  age.max <- length(mid)/2-1
  
  sex = rep(c("Female", "Male"), each=length(mid)/2)   
  age = rep(c(0:age.max), 2)
  
  df <- data.frame(sex, age, mid, lower, upper)
  
  if(percent==TRUE){
    
    df$mid <- 
      ifelse(df$sex == "Female", 
             -1 * df$mid * 100, 
             df$mid * 100)
    
    df$lower <- 
      ifelse(df$sex == "Female", 
             -1 * df$lower * 100, 
             df$lower * 100)
    
    df$upper <- 
      ifelse(df$sex == "Female", 
             -1 * df$upper * 100, 
             df$upper * 100)  
    
  }else{
    
    df$mid <- 
      ifelse(df$sex == "Female", 
             -1 * df$mid/1000, 
             df$mid/1000)
    
    df$lower <- 
      ifelse(df$sex == "Female", 
             -1 * df$lower/1000, 
             df$lower/1000)
    
    df$upper <- 
      ifelse(df$sex == "Female", 
             -1 * df$upper/1000, 
             df$upper/1000)
    
  }
  
  df2 <- 
    df %>% 
    add_row(df %>% filter(age==max(age)) %>% mutate(age=age+1)) 
  
  df$sex <-
    factor(df$sex, levels=c("Male", "Female"))
  
  df2$sex <-
    factor(df2$sex, levels=c("Male", "Female"))
  
  plot <-
    ggplot() +
    geom_hline(yintercept=0, color="grey90") +
    geom_bar(data=df, aes(x = age, y = mid, fill = sex), color = "transparent", width = 1, stat = "identity", show.legend = FALSE) +
    geom_step(data=df2, aes(x = age-0.5, y = lower, color = sex), linewidth = 0.4, linetype="solid", show.legend = FALSE) +
    geom_step(data=df2, aes(x = age-0.5, y = upper, color = sex), linewidth = 0.4, linetype="solid", show.legend = FALSE) +
    scale_fill_manual(values = c(pal_m[1], pal_f[1])) +
    scale_color_manual(values = c(pal_m[2], pal_f[2])) +
    ggtitle(title) +
    xlab("Age") +
    coord_flip() +
    facet_wrap(~ sex, scales = "free_x") +
    theme_bw() 
  
  if (year>=2021){
    
    plot <-
      plot +
      geom_vline(xintercept=year-2021+0.5, linetype="dashed") +
      geom_text(data=data.frame(x=year-2021-2, y=-limit.1, sex=factor("Male", levels=c("Male", "Female"))), aes(x=x, y=y), label="Born~2020-2024", hjust=0, parse=TRUE, family=font, size=4) +
      geom_text(data=data.frame(x=year-2021+2, y=-limit.1, sex=factor("Male", levels=c("Male", "Female"))), aes(x=x, y=y), label="Born <2020", hjust=0, family=font, size=4)
    
  }
  
  if(year>2025){
    
    plot <-
      plot +
      geom_vline(xintercept=year-2025-0.5, linetype="dashed") +
      geom_text(data=data.frame(x=year-2025-2, y=-limit.1, sex=factor("Male", levels=c("Male", "Female"))), aes(x=x, y=y), label="Born >2024", hjust=0, family=font, size=4)
    
  }
  
  if(percent==TRUE){
    
    plot <-
      plot +  
      ylab(expression("Change in Population Size (%)")) 
    
  }else{
    
    plot <-
      plot +
      ylab(expression("Change in Population Size (in Thousands)")) 
    
  }
  
  plot <- 
    plot +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white"),
          plot.title = element_text(size = 20, face = "bold"),
          strip.text = element_text(size = 20),
          text = element_text(family = font),
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(color = color.x,
                                      size = 20,
                                      vjust = -1),
          axis.text.y = element_text(color = color.y,
                                     size = 14),
          axis.title.y = element_text(color = color.y,
                                      size = 20,
                                      vjust = 2),
          axis.ticks.y = element_line(color = color.y))
  
  g=ggplot_build(plot)
  
  ## adjust x axis in male panel (panel 1)
  g$layout$panel_params[[1]]$x$limits <- c(-limit.1, limit.2) 
  g$layout$panel_params[[1]]$x$continuous_range <- c(-limit.1 * 1.1, limit.2 + limit.1 * 0.05)
  g$layout$panel_params[[1]]$x$breaks <- seq(-limit.1, limit.2, step/2)
  g$layout$panel_params[[1]]$x$minor_breaks <- seq(-limit.1, limit.2, step/2)
  g$layout$panel_params[[1]]$x$scale$labels <- rev(rev(c(rbind(seq(-limit.1, limit.2, step), "")))[-1]) 
  
  ## adjust x axis in female panel (panel 2)
  g$layout$panel_params[[2]]$x$limits <- c(-limit.2, limit.1)
  g$layout$panel_params[[2]]$x$continuous_range <- c(-limit.2 - limit.1 * 0.05, limit.1 * 1.1)
  g$layout$panel_params[[2]]$x$breaks <- seq(-limit.2, limit.1, step/2)
  g$layout$panel_params[[2]]$x$minor_breaks <- seq(-limit.2, limit.1, step/2)
  g$layout$panel_params[[2]]$x$scale$labels <- rev(rev(c(rbind(seq(limit.2, -limit.1, -step), "")))[-1])
  
  ## adjust y axis in male panel (panel 1)
  g$layout$panel_params[[1]]$y$limits <- c(0, age.max)
  g$layout$panel_params[[1]]$y$continuous_range <- c(-5, age.max + 5)
  g$layout$panel_params[[1]]$y$breaks <- seq(0, age.max, 10)
  g$layout$panel_params[[1]]$y$minor_breaks <- seq(0, age.max, 10)
  g$layout$panel_params[[1]]$y$scale$labels <- seq(0, age.max, 10)
  
  ## adjust y axis in female panel (panel 2)
  g$layout$panel_params[[2]]$y$limits <- c(0, age.max)
  g$layout$panel_params[[2]]$y$continuous_range <- c(-5, age.max + 5)
  g$layout$panel_params[[2]]$y$breaks <- seq(0, age.max, 10)
  g$layout$panel_params[[2]]$y$minor_breaks <- seq(0, age.max, 10)
  g$layout$panel_params[[2]]$y$scale$labels <- seq(0, age.max, 10)
  
  ## transform back into ggplot
  plot <- ggpubr::as_ggplot(ggplot_gtable(g))
  
}
