#############
# FUNCTIONS #
#############

# ---

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
    geom_hline(yintercept=0, color="grey90", linewidth = 0.25) +
    geom_bar(data=df, aes(x = age, y = mid, fill = sex), color = "transparent", width = 1, stat = "identity", show.legend = FALSE) +
    geom_step(data=df2, aes(x = age-0.5, y = lower, color = sex), linewidth = 0.1, linetype="solid", show.legend = FALSE) +
    geom_step(data=df2, aes(x = age-0.5, y = upper, color = sex), linewidth = 0.1, linetype="solid", show.legend = FALSE) +
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
      geom_vline(xintercept=year-2021+0.5, linetype="dashed", linewidth = 0.25) +
      geom_text(data=data.frame(x=year-2021-2, y=-limit.1, sex=factor("Male", levels=c("Male", "Female"))), aes(x=x, y=y), label=paste0("Born 2020\U2013", "2024"), hjust=0, family=font, size=5*0.36) +
      geom_text(data=data.frame(x=year-2021+2, y=-limit.1, sex=factor("Male", levels=c("Male", "Female"))), aes(x=x, y=y), label="Born <2020", hjust=0, family=font, size=5*0.36)
    
  }
  
  if(year>2025){
    
    plot <-
      plot +
      geom_vline(xintercept=year-2025-0.5, linetype="dashed", linewidth = 0.25) +
      geom_text(data=data.frame(x=year-2025-2, y=-limit.1, sex=factor("Male", levels=c("Male", "Female"))), aes(x=x, y=y), label="Born >2024", hjust=0, family=font, size=5*0.36)
    
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
          panel.border = element_rect(linewidth = 0.25),
          strip.background = element_rect(fill="white",
                                          linewidth = 0.25),
          strip.text = element_text(size = 7),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(color = color.y,
                                     size = 7),
          axis.title.x = element_text(color = color.x,
                                      size = 7),
          axis.title.y = element_text(color = color.y,
                                      size = 7),
          axis.ticks.x = element_line(linewidth = 0.25),
          axis.ticks.y = element_line(color = color.y,
                                      linewidth = 0.25),
          axis.ticks.length = unit(1, "pt"),
          text = element_text(family = font),
          plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"))
  
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
