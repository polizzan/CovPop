#######################################
# GENERATE PLOT FILES FOR PUBLICATION #
#######################################

# ---

#########
# SETUP #
#########

## packages to be installed from cran
from.cran <- c("cowplot", "geofacet", "ggpubr", "gridExtra", "here",
               "plyr", "RColorBrewer", "tidyverse")

## check if installed, else install
for(i in c(from.cran)){
  
    if(system.file(package=i)==""){install.packages(i)}
    
  }

## load packages
library(tidyverse)

## year combinations for which outputs should be created
estimates_years <-
  rbind(c(2025, 2040, 2060)) ## one combination per row

## color scales
pal_f <- RColorBrewer::brewer.pal(10, "PRGn")[c(3, 1)] ## lighter shade first
pal_m <- RColorBrewer::brewer.pal(10, "PRGn")[c(8, 10)]

## set up grid for figure 5
grid <-
  data.frame(code = c("All", "Mortality", "Fertility", "Migration"),
             name = c("All", "Mortality", "Fertility", "Migration"),
             row = c(1, 2, 2, 2),
             col = c(2, 1, 2, 3))

## set path
here::i_am("scripts/03-out-pub.R")

## create folders
if(!dir.exists(here::here("out", "journal-output"))){dir.create(here::here("out", "journal-output"))}

## load custom functions
source(here::here("scripts", "99-functions-pub.R"))

## load projection output
load(here::here("data", "projection-output.RData"))

############
# Figure 1 #
############

for(i in estimates_years[, 3]){ 
  
  ## determine axis labels
  agegrp_sex_label_min_abs <-
    plyr::round_any(min(df_agegrp_sex$Pop_diff_absolute_lower[df_agegrp_sex$Scenario==4 &
                                                                df_agegrp_sex$Year<=i]/1000),
                    f=floor, accuracy=50)
  
  agegrp_sex_label_max_abs <-
    plyr::round_any(max(0, df_agegrp_sex$Pop_diff_absolute_upper[df_agegrp_sex$Scenario==4 &
                                                                   df_agegrp_sex$Year<=i]/1000),
                    f=ceiling, accuracy=50)
  
  agegrp_sex_label_min_per <-
    plyr::round_any(min(100 * df_agegrp_sex$Pop_diff_percent_lower[df_agegrp_sex$Scenario==4 &
                                                                     df_agegrp_sex$Year<=i]),
                    f=floor, accuracy=0.5)
  
  agegrp_sex_label_max_per <-
    plyr::round_any(max(0, 100 * df_agegrp_sex$Pop_diff_percent_upper[df_agegrp_sex$Scenario==4 &
                                                                        df_agegrp_sex$Year<=i]),
                    f=ceiling, accuracy=0.5)
  
  ## auxiliary data set with last year added again for nicer rendering of CIs
  df_agegrp_sex_2 <-
    df_agegrp_sex %>% 
    filter(Year %in% 2020:i) %>% 
    add_row(df_agegrp_sex %>% filter(Year==i) %>% mutate(Year=Year+1)) 
  
  df_agegrp_sex$Sex <- factor(df_agegrp_sex$Sex, levels = c(1, 0)) 
  df_agegrp_sex_2$Sex <- factor(df_agegrp_sex_2$Sex, levels = c(1, 0))
  
  ## panel a, absolute
  delta_agegrp_sex_absolute <-
    ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.25, color = "black") + ## add horizontal line
    geom_bar(data = df_agegrp_sex %>% filter(Scenario == 4, Year %in% 2020:i), 
             aes(x = Year, y = Pop_diff_absolute_mid/1000, fill = Sex), 
             color = "transparent", width = 1, stat = "identity") +
    geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
              aes(x = Year-0.5, y = Pop_diff_absolute_lower/1000, color = Sex), 
              linetype = "solid", linewidth = 0.1, show.legend = FALSE) +
    geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
              aes(x = Year-0.5, y = Pop_diff_absolute_upper/1000, color = Sex), 
              linetype = "solid", linewidth = 0.1, show.legend = FALSE) +
    xlab("") +
    ylab("Change in Population Size (in Thousands)") +
    ggtitle("Panel A") +
    scale_fill_manual("", 
                      values = c("1" = pal_m[1], "0" = pal_f[1]),
                      labels = c("Male", "Female")) +
    scale_color_manual("", 
                       values = c("1" = pal_m[2], "0" = pal_f[2]),
                       labels = c("Male", "Female")) + 
    scale_x_continuous(breaks = seq(2020, i, 5),
                       labels = rev(rev(c(rbind(seq(2020, i, 10), "")))[-1])) + ## label only every other tick
    scale_y_continuous(limits = c(agegrp_sex_label_min_abs, agegrp_sex_label_max_abs),
                       breaks = seq(agegrp_sex_label_min_abs, agegrp_sex_label_max_abs, 50)) +
    theme_bw() +
    theme(plot.title = element_text(size = 7, hjust = 0, vjust = -4, face = "bold", margin = margin(0, 0, 0, 0)),
          panel.border = element_rect(linewidth = 0.25),
          panel.grid = element_blank(),
          panel.spacing = unit(0.25, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 7),
          strip.text.y = element_blank(),
          axis.text = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(1, "pt"),
          legend.position = c(.94, .15), 
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.5, 'cm'),
          legend.background = element_rect(fill = "transparent"),
          text = element_text(family = "serif"),
          plot.margin = margin(2, 2, 2, 2, unit = "pt")) +
    facet_grid(rows = vars(Sex), cols = vars(AgeGrp)) +
    coord_cartesian(ylim = c(agegrp_sex_label_min_abs, agegrp_sex_label_max_abs))
  
  ## panel b, relative
  delta_agegrp_sex_percent <-
    ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.25, color = "black") +
    geom_bar(data = df_agegrp_sex %>% filter(Scenario == 4, Year %in% 2020:i), 
             aes(x = Year, y =  100 * Pop_diff_percent_mid, fill = Sex), 
             color = "transparent", width = 1, stat = "identity") +
    geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
              aes(x = Year-0.5, y = 100 * Pop_diff_percent_lower, color = Sex), 
              linetype = "solid", linewidth = 0.1, show.legend = FALSE) +
    geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
              aes(x = Year-0.5, y = 100 * Pop_diff_percent_upper, color = Sex), 
              linetype = "solid", linewidth = 0.1, show.legend = FALSE) +
    xlab("") +
    ylab("Change in Population Size (%)") +
    ggtitle("Panel B") +
    scale_fill_manual("", 
                      values = c("1" = pal_m[1], "0" = pal_f[1]),
                      labels = c("Male", "Female")) +
    scale_color_manual("", 
                       values = c("1" = pal_m[2], "0" = pal_f[2]),
                       labels = c("Male", "Female")) + 
    scale_x_continuous(breaks = seq(2020, i, 5),
                       labels = rev(rev(c(rbind(seq(2020, i, 10), "")))[-1])) +
    scale_y_continuous(limits = c(agegrp_sex_label_min_per, agegrp_sex_label_max_per),
                       breaks = seq(agegrp_sex_label_min_per, agegrp_sex_label_max_per, 0.5)) +
    theme_bw() +
    theme(plot.title = element_text(size = 7, hjust = 0, vjust = -4, face = "bold", margin = margin(0, 0, 0, 0)),
          panel.border = element_rect(linewidth = 0.25),
          panel.grid = element_blank(),
          panel.spacing = unit(0.25, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 7),
          strip.text.y = element_blank(),
          axis.text = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(1, "pt"),
          legend.position = c(.94, .15), 
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.5, 'cm'),
          legend.background = element_rect(fill = "transparent"),
          text = element_text(family = "serif"),
          plot.margin = margin(2, 2, 2, 2, unit = "pt")) +
    facet_grid(rows = vars(Sex), cols = vars(AgeGrp)) +
    coord_cartesian(ylim=c(agegrp_sex_label_min_per, agegrp_sex_label_max_per))
  
  plots <- cowplot::align_plots(delta_agegrp_sex_absolute, delta_agegrp_sex_percent, align = "v")
  
  plot.1a <- cowplot::ggdraw(plots[[1]])
  plot.1b <- cowplot::ggdraw(plots[[2]])
  
  plot.1 <- gridExtra::grid.arrange(plot.1a, plot.1b, ncol = 1)

  ggsave(plot = plot.1,
         filename = here::here("out", "journal-output", paste0("Figure-1-", i, ".pdf")), 
         width = 180, height = 160, units = "mm", device = "pdf")
  
}

#################
# Figures 2 & 3 #
#################

for(i in 1:dim(estimates_years)[1]){
for(j in c("a", "b")){
  
  if (j=="a"){ ## panel a, absolute  
    
    a_panel_1_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 1], 
              title = paste(estimates_years[i, 1]), 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = TRUE,
              PANEL = "Panel A")

    a_panel_2_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = paste(estimates_years[i, 2]), 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = TRUE,
              show.y = FALSE,
              PANEL = "")

    a_panel_3_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 3], 
              title = paste(estimates_years[i, 3]),
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = FALSE,
              PANEL = "")

    a_pyramid_MO.. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = "Mortality", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = TRUE,
              PANEL = "Panel A")
    
    a_pyramid_.FE. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = "Fertility", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = TRUE,
              show.y = FALSE,
              PANEL = "")
    
    a_pyramid_..MI_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = "Migration", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = FALSE,
              PANEL = "")

  }else{ ## panel b, relative
    
    b_panel_1_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 1], 
              title = paste(estimates_years[i, 1]), 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = TRUE,
              PANEL = "Panel B")
    
    b_panel_2_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = paste(estimates_years[i, 2]), 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = TRUE,
              show.y = FALSE,
              PANEL = "")
    
    b_panel_3_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 3], 
              title = paste(estimates_years[i, 3]),
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = FALSE,
              PANEL = "")
    
    b_pyramid_MO.. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = "Mortality", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = TRUE,
              PANEL = "Panel B")
    
    b_pyramid_.FE. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = "Fertility", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = TRUE,
              show.y = FALSE,
              PANEL = "")
    
    b_pyramid_..MI_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = "Migration", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = FALSE,
              PANEL = "")
    
    }
  }
  
  plot.2a <- ggpubr::ggarrange(a_panel_1_wpp, a_panel_2_wpp, a_panel_3_wpp, ncol = 3)
  plot.2b <- ggpubr::ggarrange(b_panel_1_wpp, b_panel_2_wpp, b_panel_3_wpp, ncol = 3)
  
  plot.3a <- ggpubr::ggarrange(a_pyramid_MO.., a_pyramid_.FE., a_pyramid_..MI_wpp, ncol = 3)
  plot.3b <- ggpubr::ggarrange(b_pyramid_MO.., b_pyramid_.FE., b_pyramid_..MI_wpp, ncol = 3)
  
  plot.2 <- cowplot::align_plots(plot.2a, plot.2b, align = "v")
  plot.3 <- cowplot::align_plots(plot.3a, plot.3b, align = "v")
  
  plot.2a <- cowplot::ggdraw(plot.2[[1]])
  plot.2b <- cowplot::ggdraw(plot.2[[2]])
  
  plot.3a <- cowplot::ggdraw(plot.3[[1]])
  plot.3b <- cowplot::ggdraw(plot.3[[2]])
  
  plot.2 <- gridExtra::grid.arrange(plot.2a, plot.2b, ncol = 1) 
    
  plot.3 <- gridExtra::grid.arrange(plot.3a, plot.3b, ncol = 1) 
  
  ggsave(plot = plot.2,
         filename = here::here("out", "journal-output", paste0("Figure-2-", estimates_years[i, 3], ".pdf")), 
         width = 180, height = 160, units = "mm", device = "pdf")
  
  ggsave(plot = plot.3,
         filename = here::here("out", "journal-output", paste0("Figure-3-", estimates_years[i, 2], ".pdf")), 
         width = 180, height = 160, units = "mm", device = "pdf")
  
}

############################################
# CHANGES IN POPULATION SHARE BY AGE GROUP #
############################################

for(i in 1:dim(estimates_years)[1]){ 

## determine axis labels  
popshare_label_min <-
  plyr::round_any(min(df_agegrp_total$PopShare_diff_absolute_lower[df_agegrp_total$Scenario %in% c(1, 2, 3, 4) &
                                                                   df_agegrp_total$Year %in% estimates_years[i,]]),
                  f=floor, accuracy=0.05)

popshare_label_max <-
  plyr::round_any(max(df_agegrp_total$PopShare_diff_absolute_upper[df_agegrp_total$Scenario %in% c(1, 2, 3, 4) &
                                                                   df_agegrp_total$Year %in% estimates_years[i,]]),
                  f=ceiling, accuracy=0.05)

delta_popshare <-
  df_agegrp_total %>% 
  filter(Year %in% estimates_years[i,],
         Scenario %in% c(1, 2, 3, 4)) %>% 
  mutate(Scenario = factor(Scenario, 
                           levels = c(4, 1, 2, 3),
                           labels = c("All", "Mortality", "Fertility", "Migration"))) %>% 
  ggplot(aes(fill = AgeGrp, 
             x = as.factor(Year), 
             y = PopShare_diff_absolute_mid, 
             ymin = PopShare_diff_absolute_lower, 
             ymax = PopShare_diff_absolute_upper)) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "black") + ## add horizontal line
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75, linewidth = 0.1, color = "black") +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.3, linewidth = 0.1) +
  xlab("") +
  ylab(expression(atop("Change in Population Share", "(in Percentage Points)"))) +
  scale_fill_manual("Age Group", values = RColorBrewer::brewer.pal(n = 5, "Purples")) +
  scale_y_continuous(breaks = seq(popshare_label_min, popshare_label_max, 0.05)) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom", 
                             label.hjust = 0.5, title.position = "top", title.hjust = 0.5)) +
  geofacet::facet_geo(vars(Scenario), grid = grid) + ## arrange panels
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 0.25),
        strip.background = element_rect(fill="white",
                                        linewidth = 0.25),
        strip.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title.y = element_text(size = 7, hjust = 0.5),
        axis.ticks = element_line(linewidth = 0.25),
        axis.ticks.length = unit(1, "pt"),
        legend.position = c(0.85, 0.8),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.5, "cm"),
        text = element_text(family = "serif"),
        plot.margin = margin(2, 2, 2, 2, unit = "pt")) +
  coord_cartesian(ylim = c(popshare_label_min, popshare_label_max))

ggsave(plot = delta_popshare,
       filename = here::here("out", "journal-output", paste0("Figure-5-", estimates_years[i, 3], ".pdf")), 
       width = 180, height = 80, units = "mm", device = pdf)

}

