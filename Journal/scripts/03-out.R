###################
# GENERATE OUTPUT #
###################

# ---

##########################
# LOAD PROJECTION OUTPUT #
##########################

load(here::here("data", "projection-output.RData"))

#########################################################
# COMBINED PLOTS OF YEAR- AND SCENARIO-SPECIFIC RESULTS #
#########################################################

for(i in 1:dim(estimates_years)[1]){
for(j in c("absolute", "relative")){
  
  if(j=="absolute"){  
    
    ## by year
    panel_a_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 1], 
              title = paste("Panel a:", estimates_years[i, 1]), 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = TRUE)

    panel_a_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_absolute_census_upper), 
              year = estimates_years[i, 1], 
              title = paste("Panel a:", estimates_years[i, 1]), 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = TRUE)
    
    panel_b_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = paste("Panel b:", estimates_years[i, 2]), 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = TRUE,
              show.y = FALSE)
    
    panel_b_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_absolute_census_upper), 
              year = estimates_years[i, 2], 
              title = paste("Panel b:", estimates_years[i, 2]),
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = TRUE,
              show.y = FALSE)
    
    panel_c_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 3], 
              title = paste("Panel c:", estimates_years[i, 3]),
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = FALSE)

    panel_c_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_absolute_census_upper), 
              year = estimates_years[i, 3], 
              title = paste("Panel c:", estimates_years[i, 3]), 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = FALSE)

    ## by scenario
    pyramid_MO.. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = "Panel a: Mortality", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = TRUE)
    
    pyramid_.FE. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = "Panel b: Fertility", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = TRUE,
              show.y = FALSE)
    
    pyramid_..MI_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_upper), 
              year = estimates_years[i, 2], 
              title = "Panel c: Migration", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = FALSE)

    pyramid_..MI_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_absolute_census_upper), 
              year = estimates_years[i, 2], 
              title = "Panel c: Migration", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE,
              show.x = FALSE,
              show.y = FALSE)
    
  }else{
    
    ## by year
    panel_a_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 1], 
              title = paste("Panel a:", estimates_years[i, 1]), 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = TRUE)
    
    panel_a_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 1], Scenario==4) %>% pull(Pop_diff_percent_census_upper), 
              year = estimates_years[i, 1], 
              title = paste("Panel a:", estimates_years[i, 1]), 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = TRUE)
    
    panel_b_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = paste("Panel b:", estimates_years[i, 2]), 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = TRUE,
              show.y = FALSE)
    
    panel_b_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==4) %>% pull(Pop_diff_percent_census_upper), 
              year = estimates_years[i, 2], 
              title = paste("Panel b:", estimates_years[i, 2]),
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = TRUE,
              show.y = FALSE)
    
    panel_c_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 3], 
              title = paste("Panel c:", estimates_years[i, 3]),
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = FALSE)
    
    panel_c_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 3], Scenario==4) %>% pull(Pop_diff_percent_census_upper), 
              year = estimates_years[i, 3], 
              title = paste("Panel c:", estimates_years[i, 3]), 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = FALSE)
    
    ## by scenario
    pyramid_MO.. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==1) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = "Panel a: Mortality", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = TRUE)
    
    pyramid_.FE. <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==2) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = "Panel b: Fertility", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = TRUE,
              show.y = FALSE)
    
    pyramid_..MI_wpp <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_upper), 
              year = estimates_years[i, 2], 
              title = "Panel c: Migration", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = FALSE)
    
    pyramid_..MI_census <-
      pyramid(mid = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_census_mid), 
              lower = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_census_lower), 
              upper = df_age %>% filter(Year==estimates_years[i, 2], Scenario==3) %>% pull(Pop_diff_percent_census_upper), 
              year = estimates_years[i, 2], 
              title = "Panel c: Migration", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE,
              show.x = FALSE,
              show.y = FALSE)
    
  }
  
  ggpubr::ggarrange(panel_a_wpp, panel_b_wpp, panel_c_wpp, ncol = 3) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  
  ggsave(filename = here::here("out", "other-output", paste0("pyramid_time_", j, "_wpp_", estimates_years[i, 3], ".pdf")), 
         width = 15/2.54*900, height = 7.5/2.54*900, units = "px", device = "pdf")

  ggpubr::ggarrange(panel_a_census, panel_b_census, panel_c_census, ncol = 3) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  
  ggsave(filename = here::here("out", "other-output", paste0("pyramid_time_", j, "_census_", estimates_years[i, 3], ".pdf")), 
         width = 15/2.54*900, height = 7.5/2.54*900, units = "px", device = "pdf")

  ggpubr::ggarrange(pyramid_MO.., pyramid_.FE., pyramid_..MI_wpp, ncol = 3) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

  ggsave(filename = here::here("out", "other-output", paste0("pyramid_scenario_", j, "_wpp_", estimates_years[i, 2], ".pdf")), 
         width = 15/2.54*900, height = 7.5/2.54*900, units = "px", device = "pdf")
  
  ggpubr::ggarrange(pyramid_MO.., pyramid_.FE., pyramid_..MI_census, ncol = 3) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  
  ggsave(filename = here::here("out", "other-output", paste0("pyramid_scenario_", j, "_census_", estimates_years[i, 2], ".pdf")), 
         width = 15/2.54*900, height = 7.5/2.54*900, units = "px", device = "pdf")

  }
}

###################################
# MISSING POPULATION BY AGE GROUP #
###################################

for(i in estimates_years[, 3]){ ## vary end year in plot

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

## add last year again for nicer rendering of CIs
df_agegrp_sex_2 <-
  df_agegrp_sex %>% 
  filter(Year %in% 2020:i) %>% 
  add_row(df_agegrp_sex %>% filter(Year==i) %>% mutate(Year=Year+1)) 

df_agegrp_sex$Sex <- factor(df_agegrp_sex$Sex, levels = c(1, 0)) 
df_agegrp_sex_2$Sex <- factor(df_agegrp_sex_2$Sex, levels = c(1, 0))

delta_agegrp_sex_absolute <-
  ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "black") + ## add horizontal line
  geom_bar(data = df_agegrp_sex %>% filter(Scenario == 4, Year %in% 2020:i), 
           aes(x = Year, y = Pop_diff_absolute_mid/1000, fill = Sex), 
           color = "transparent", width = 1, stat = "identity") +
  geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
            aes(x = Year-0.5, y = Pop_diff_absolute_lower/1000, color = Sex), 
            linetype = "solid", linewidth = 0.4, show.legend = FALSE) +
  geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
            aes(x = Year-0.5, y = Pop_diff_absolute_upper/1000, color = Sex), 
            linetype = "solid", linewidth = 0.4, show.legend = FALSE) +
  xlab("") +
  ylab("Change in Population Size (in Thousands)") +
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
  theme(panel.grid = element_blank(),
        panel.spacing = unit(0.75, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.position = c(.94, .15), 
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, 'cm'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        text = element_text(family = "serif")) +
  facet_grid(rows = vars(Sex), cols = vars(AgeGrp)) +
  coord_cartesian(ylim = c(agegrp_sex_label_min_abs, agegrp_sex_label_max_abs))

delta_agegrp_sex_percent <-
  ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "black") +
  geom_bar(data = df_agegrp_sex %>% filter(Scenario == 4, Year %in% 2020:i), 
           aes(x = Year, y =  100 * Pop_diff_percent_mid, fill = Sex), 
           color = "transparent", width = 1, stat = "identity") +
  geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
            aes(x = Year-0.5, y = 100 * Pop_diff_percent_lower, color = Sex), 
            linetype = "solid", linewidth = 0.4, show.legend = FALSE) +
  geom_step(data = df_agegrp_sex_2 %>% filter(Scenario == 4), 
            aes(x = Year-0.5, y = 100 * Pop_diff_percent_upper, color = Sex), 
            linetype = "solid", linewidth = 0.4, show.legend = FALSE) +
  xlab("") +
  ylab("Change in Population Size (%)") +
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
  theme(panel.grid = element_blank(),
        panel.spacing = unit(0.75, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.position = c(.94, .15), 
        legend.text = element_text(size=12),
        legend.key.size = unit(1, 'cm'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        text = element_text(family="serif")) +
  facet_grid(rows = vars(Sex), cols = vars(AgeGrp)) +
  coord_cartesian(ylim=c(agegrp_sex_label_min_per, agegrp_sex_label_max_per))

ggsave(plot = delta_agegrp_sex_absolute,
       filename = here::here("out", "other-output", paste0("delta_agegrp_absolute_", i, ".pdf")), 
       width = 10/2.54*900, height = 7/2.54*900, units = "px", device = "pdf")

ggsave(plot = delta_agegrp_sex_percent,
       filename = here::here("out", "other-output", paste0("delta_agegrp_percent_", i, ".pdf")), 
       width = 10/2.54*900, height = 7/2.54*900, units = "px", device = "pdf")

}

############################################
# CHANGES IN POPULATION SHARE BY AGE GROUP #
############################################

for(i in 1:dim(estimates_years)[1]){ ## vary years to be plotted

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
  filter(Year %in% estimates_years[i, ],
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
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75, linewidth = 0.25, color = "black") +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.3, linewidth = 0.25) +
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
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16),
        axis.ticks.x = element_blank(),        
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, hjust = 0.5, vjust = 2),
        legend.position = c(0.835, 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        text = element_text(family = "serif") ) +
  coord_cartesian(ylim = c(popshare_label_min, popshare_label_max))

ggsave(plot = delta_popshare,
       filename = here::here("out", "other-output", paste0("delta_popshare_", estimates_years[i, 3], ".pdf")), 
       width = 9/2.54*900, height = 7/2.54*900, units = "px", device = pdf)

}

##############################################################
# TOTAL POPULATION SIZE AND CHANGES IN TOTAL POPULATION SIZE #
##############################################################

for(i in estimates_years[, 3]){ ## vary end year in plot

## determine axis labels
popsize_label_min <-
  plyr::round_any(min(df_total$Pop_cf_lower[df_total$Scenario %in% c(1, 2, 3, 4) &
                                            df_total$Year<=i]/1000000),
                  f=floor, accuracy=10)

popsize_label_max <-
  plyr::round_any(max(df_total$Pop_cf_upper[df_total$Scenario %in% c(1, 2, 3, 4) &
                                            df_total$Year<=i]/1000000),
                  f=ceiling, accuracy=10)

delta_popsize_label_min_abs <-
  plyr::round_any(min(df_total$Pop_diff_absolute_lower[df_total$Scenario %in% c(1, 2, 3, 4) &
                                                       df_total$Year<=i]/1000000),
                  f=floor, accuracy=0.5)

delta_popsize_label_max_abs <-
  plyr::round_any(max(0, df_total$Pop_diff_absolute_upper[df_total$Scenario %in% c(1, 2, 3, 4) &
                                                          df_total$Year<=i]/1000000),
                  f=ceiling, accuracy=0.5)

delta_popsize_label_min_per <-
  plyr::round_any(min(df_total$Pop_diff_percent_lower[df_total$Scenario %in% c(1, 2, 3, 4) &
                                                      df_total$Year<=i]) * 100,
                  f=floor, accuracy=0.5)

delta_popsize_label_max_per <-
  plyr::round_any(max(0, df_total$Pop_diff_percent_upper[df_total$Scenario %in% c(1, 2, 3, 4) &
                                                         df_total$Year<=i] * 100),
                  f=ceiling, accuracy=0.5)

## population size
popsize <-
  df_total %>% 
  filter(Year %in% 2020:i,
         Scenario %in% c(1, 2, 3, 4)) %>%
  mutate(Scenario=factor(Scenario, 
                         levels = c(4, 1, 2, 3),
                         labels = c("All", "Mortality", "Fertility", "Migration"))) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Pop_bl_mid/1000000, color = "Baseline"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = Year, y = Pop_cf_mid/1000000, color = "Counterfactual"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = Year, y = Pop_bl_lower/1000000), linetype = "dashed", color = pal_f[2], linewidth = 0.5) +
  geom_line(aes(x = Year, y = Pop_bl_upper/1000000), linetype = "dashed", color = pal_f[2], linewidth = 0.5) +
  geom_line(aes(x = Year, y = Pop_cf_lower/1000000), linetype = "dashed", color = pal_m[2], linewidth = 0.5) +
  geom_line(aes(x = Year, y = Pop_cf_upper/1000000), linetype = "dashed", color = pal_m[2], linewidth = 0.5) +
  labs(x="Year",
       y="Population Size\n(in Millions)") +
  scale_color_manual("",
                     values = c("Baseline" = pal_f[1], "Counterfactual" = pal_m[1])) +
  theme_bw() +
  scale_x_continuous(limits = c(2020, i),
                     breaks = seq(2020, i, 10)) +
  scale_y_continuous(limits = c(popsize_label_min, popsize_label_max),
                     breaks = seq(popsize_label_min, popsize_label_max, 10)) +
  geofacet::facet_geo(vars(Scenario), grid = grid) +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust=-1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, hjust=0.5, vjust=2),
        legend.position = c(0.835, 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        text = element_text(family = "serif")) +
  coord_cartesian(xlim = c(2020, i+1),
                  ylim = c(popsize_label_min, popsize_label_max)) 

## change in population size, absolute
delta_popsize_absolute <-
  df_total %>% 
  filter(Year %in% 2020:i,
         Scenario %in% c(1, 2, 3, 4)) %>%
  mutate(Scenario=factor(Scenario, 
                         levels = c(4, 1, 2, 3),
                         labels = c("All", "Mortality", "Fertility", "Migration"))) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "black") +
  geom_line(aes(x = Year, y = Pop_diff_absolute_mid/1000000), linetype = "solid", color = pal_m[1], linewidth = 1) +
  geom_line(aes(x = Year, y = Pop_diff_absolute_lower/1000000), linetype = "dashed", color = pal_m[2], linewidth = 0.5) +
  geom_line(aes(x = Year, y = Pop_diff_absolute_upper/1000000), linetype = "dashed", color = pal_m[2], linewidth = 0.5) +
  labs(x="Year",
       y="Change in Population Size\n(in Millions)") +
  theme_bw() +
  scale_x_continuous(limits = c(2020, i),
                     breaks = seq(2020, i, 10)) +
  scale_y_continuous(limits = c(delta_popsize_label_min_abs, delta_popsize_label_max_abs),
                     breaks = seq(delta_popsize_label_min_abs, delta_popsize_label_max_abs, 0.25))  +
  geofacet::facet_geo(vars(Scenario), grid = grid) +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust=-1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, hjust=0.5, vjust=2),
        legend.position = c(0.835, 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        text = element_text(family = "serif")) +
  coord_cartesian(xlim=c(2020, i+1),
                  ylim=c(delta_popsize_label_min_abs, delta_popsize_label_max_abs))

## change in population size, relative
delta_popsize_percent <-
  df_total %>% 
  filter(Year %in% 2020:i,
         Scenario %in% c(1, 2, 3, 4)) %>%
  mutate(Scenario=factor(Scenario, 
                         levels = c(4, 1, 2, 3),
                         labels = c("All", "Mortality", "Fertility", "Migration"))) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "black") +
  geom_line(aes(x = Year, y = Pop_diff_percent_mid * 100), linetype = "solid", color = pal_m[1], linewidth = 1) +
  geom_line(aes(x = Year, y = Pop_diff_percent_lower * 100), linetype = "dashed", color = pal_m[2], linewidth = 0.5) +
  geom_line(aes(x = Year, y = Pop_diff_percent_upper * 100), linetype = "dashed", color = pal_m[2], linewidth = 0.5) +
  labs(x="Year",
       y="Change in Population Size\n(%)") +
  theme_bw() +
  scale_x_continuous(limits = c(2020, i),
                     breaks = seq(2020, i, 10)) +
  scale_y_continuous(limits = c(delta_popsize_label_min_per, delta_popsize_label_max_per),
                     breaks = seq(delta_popsize_label_min_per, delta_popsize_label_max_per, 0.25)) +
  geofacet::facet_geo(vars(Scenario), grid = grid) +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust=-1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, hjust=0.5, vjust=2),
        legend.position = c(0.835, 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        text = element_text(family = "serif")) +
  coord_cartesian(xlim=c(2020, i+1),
                  ylim=c(delta_popsize_label_min_per, delta_popsize_label_max_per))

ggsave(plot = popsize,
       filename = here::here("out", "other-output", paste0("popsize_", i, ".pdf")), 
       width = 9/2.54*900, height = 7/2.54*900, units = "px", device = pdf)

ggsave(plot = delta_popsize_absolute,
       filename = here::here("out", "other-output", paste0("delta_popsize_absolute_", i, ".pdf")), 
       width = 9/2.54*900, height = 7/2.54*900, units = "px", device = pdf)

ggsave(plot = delta_popsize_percent,
       filename = here::here("out", "other-output", paste0("delta_popsize_percent_", i, ".pdf")), 
       width = 9/2.54*900, height = 7/2.54*900, units = "px", device = pdf)

}

###########################
# TABLE WITH MAIN OUTPUTS #
###########################

for(j in 1:dim(estimates_years)[1]){ ## vary years to be plotted

wb <- openxlsx::createWorkbook()  ## one separate excel file for each combination of years

for(i in c(4, 1, 2, 3)){

## total
estimates_table <-
  df_total %>% 
  filter(Scenario==i) %>% 
  filter(Year %in% estimates_years[j, ]) %>% 
  select(Year, 
         Pop_bl_mid, Pop_bl_lower, Pop_bl_upper,
         Pop_cf_mid, Pop_cf_lower, Pop_cf_upper,
         Pop_diff_absolute_mid, Pop_diff_absolute_lower, Pop_diff_absolute_upper,
         Pop_diff_percent_mid, Pop_diff_percent_lower, Pop_diff_percent_upper) %>% 
  mutate(across(-c(Year, Pop_diff_percent_mid, Pop_diff_percent_lower, Pop_diff_percent_upper),
                ~ trunc(./1000))) %>%
  mutate(across(c(Pop_diff_percent_mid, Pop_diff_percent_lower, Pop_diff_percent_upper),
                ~ round(.*100, 2))) %>%
  mutate(AgeGrp = "Total") %>% 
  
  add_row(

## age-specific
df_agegrp_total %>% 
  filter(Scenario==i) %>% 
  filter(Year %in% estimates_years[j, ]) %>%
  select(Year, AgeGrp,
         Pop_bl_mid, Pop_bl_lower, Pop_bl_upper,
         Pop_cf_mid, Pop_cf_lower, Pop_cf_upper,
         Pop_diff_absolute_mid, Pop_diff_absolute_lower, Pop_diff_absolute_upper,
         Pop_diff_percent_mid, Pop_diff_percent_lower, Pop_diff_percent_upper) %>% 
  mutate(across(-c(Year, AgeGrp, Pop_diff_percent_mid, Pop_diff_percent_lower, Pop_diff_percent_upper),
                ~ trunc(./1000))) %>%
  mutate(across(c(Pop_diff_percent_mid, Pop_diff_percent_lower, Pop_diff_percent_upper),
                ~ round(.*100, 2)))

) %>% 
  relocate(Year, AgeGrp) %>% 
  mutate(AgeGrp = factor(AgeGrp, levels = c("Total", "0-14", "15-49", "50-64", "65-84", "85+"))) %>% 
  arrange(Year, AgeGrp)

colnames(estimates_table) <-
  c("Year", "Age",
    "Population (Baseline)",
    "Population (Baseline), lower",
    "Population (Baseline), upper",
    "Population (Counterfactual)",
    "Population (Counterfactual), lower",
    "Population (Counterfactual), upper",
    "Difference (Absolute)",
    "Difference (Absolute), lower",
    "Difference (Absolute), upper",
    "Difference (Percent)",
    "Difference (Percent), lower",
    "Difference (Percent), upper")

scenario_name <- scenarios[which(scenarios[, 1]==i), 2] ## pull name of scenario

openxlsx::addWorksheet(wb, scenario_name)
openxlsx::writeData(wb, scenario_name, estimates_table)
    
style1 <- openxlsx::createStyle(numFmt = "#,###,##0")
style2 <- openxlsx::createStyle(numFmt = "#,###,##0.00")
openxlsx::addStyle(wb, scenario_name, style = style1, rows = c(2:19), cols = c(3:11), gridExpand = TRUE)
openxlsx::addStyle(wb, scenario_name, style = style2, rows = c(2:19), cols = c(12:14), gridExpand = TRUE)

}

openxlsx::saveWorkbook(wb, here::here("out", "other-output", paste0("Estimates_", estimates_years[j, 3], ".xlsx")), overwrite = TRUE)

}

#####################
# DEPENDENCY RATIOS #
#####################

## see this blog for code: https://themockup.blog/posts/2020-09-04-10-table-rules-in-r
for(j in c("wpp", "census")){
  for(i in 1:dim(estimates_years)[1]){
    
    DR_table <- 
      dependency_ratio %>% 
      filter(Year <= estimates_years[i, 3]) %>% ## vary end year 
      filter(if(j=="wpp"){!grepl("Census", Scenario)}else{Scenario!="Migration"}) %>% ## use only wpp or census migration counts
      mutate(Scenario = case_when(grepl("Migration", Scenario) ~ "Migration",
                                  TRUE ~ Scenario)) %>% 
      mutate(Scenario = factor(Scenario, levels=c("All", "Mortality", "Fertility", "Migration"))) %>% 
      group_by(Year, DR, Scenario) %>% 
      summarize(across(delta, ~ quantile(.x, probs = c(0.5), na.rm = TRUE, names = FALSE))) %>% ## display only median values
      ungroup()
    
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
        geom_line(linewidth=10, color=RColorBrewer::brewer.pal(5, "RdBu")[5]) +
        geom_point(data=data %>% filter(Year %in% estimates_years[i, ]),
                   aes(x=Year, y=delta), size=20, color=RColorBrewer::brewer.pal(5, "RdBu")[1]) +
        geom_text(data=data %>% filter(delta==DR_minmax[1]), aes(x=Year, y=0, label=round(delta,2)), size=40, hjust=0, vjust=-0.5, color="#6E6E6E") +
        geom_text(data=data %>% filter(delta==DR_minmax[2]), aes(x=Year, y=0, label=round(delta,2)), size=40, hjust=0.25, vjust=+1.5, color="#6E6E6E") +    
        theme_void() +
        scale_color_identity() +
        theme(legend.position="none") +
        coord_cartesian(ylim=DR_limits)
      
    }
    
    plots <- 
      DR_table %>% 
      select(DR, Scenario, Year, delta) %>% 
      nest(trend=c(Year, delta)) %>% 
      mutate(plot=map(trend, plot_spark)) %>% 
      select(-trend)
    
    DR_trends <-
      DR_table %>% 
      filter(Year %in% estimates_years[i, ]) %>% 
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
        label = "Change in Dependency Ratio",
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
        table.border.bottom.style = "double",
        table_body.hlines.color="white",
        column_labels.border.bottom.width=px(2),
        column_labels.border.bottom.color="black",
        row_group.border.top.width=px(2),
        row_group.border.top.color="black",
        row_group.border.bottom.width=px(2),
        row_group.border.bottom.color="black",
        source_notes.font.size=px(16)
      ) 
    
    gtsave(DR_trends, here::here("out", "other-output", paste0("dependency-ratio_", estimates_years[i, 3], "_", j, ".png")), expand=25) ## requires google chrome
    gtsave(DR_trends, here::here("out", "other-output", paste0("dependency-ratio_", estimates_years[i, 3], "_", j, ".pdf")), expand=25)
    
  }
}

#################
# GENERATE GIFs #
#################

## generate images for each year
for(j in unique(df_age$Scenario)){ 
  
  scenario_name <- scenarios[which(scenarios[, 1]==j), 2] ## pull name of scenario
  
  for(i in c(2020:2060)){
    
    absolute <-
      pyramid(mid = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_absolute_upper), 
              year = i, 
              title = NULL, 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE)
    
    percent <-
      pyramid(mid = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_percent_upper), 
              year = i, 
              title = NULL, 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE)
    
    ggsave(filename=here::here("out", "gif", "absolute", "with year", scenario_name, paste0("pyramid", i, ".png")), 
           width = 7, height = 9, absolute, device=png)
    
    ggsave(filename=here::here("out", "gif", "percent", "with year", scenario_name, paste0("pyramid", i, ".png")), 
           width = 7, height = 9, percent, device=png)
    
    absolute <-
      pyramid(mid = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_absolute_mid), 
              lower = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_absolute_lower), 
              upper = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_absolute_upper), 
              year = i, 
              title = "", 
              limit.1 = 50, 
              limit.2 = 10, 
              percent = FALSE)
    
    percent <-
      pyramid(mid = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_percent_mid), 
              lower = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_percent_lower), 
              upper = df_age %>% filter(Year==i, Scenario==j) %>% pull(Pop_diff_percent_upper), 
              year = i, 
              title = "", 
              limit.1 = 8, 
              limit.2 = 2, 
              percent = TRUE)
    
    ggsave(filename=here::here("out", "gif", "absolute", "without year", scenario_name, paste0("pyramid", i, ".png")), 
           width = 7, height = 9, absolute, device=png)
    
    ggsave(filename=here::here("out", "gif", "percent", "without year", scenario_name, paste0("pyramid", i, ".png")), 
           width = 7, height = 9, percent, device=png)

  }
}

## combine images
for(j in unique(df_age$Scenario)){ 
  
  scenario_name <- scenarios[which(scenarios[, 1]==j), 2] ## pull name of scenario
  
  for(i in c("absolute", "percent")){
    
    imgs <- 
      list.files(here::here("out", "gif", paste0(i), "with year", scenario_name),
                 full.names = TRUE, 
                 pattern = ".png$")
    
    gifski::gifski(imgs, 
                   gif_file = here::here("out", "gif", paste0(i), "with year", scenario_name, "pyramid.gif"),
                   width = 2800, 
                   height = 3600, 
                   delay = 0.5)
    
    rm(imgs)
    
  }
}

#####################
# CLEAR ENVIRONMENT #
#####################

rm(list=ls())