
BioEconomicModelPlots <- function(dat,
                                  spatial_res = "global", # or regional
                                  temporal_res = "all", # or last
                                  start_year = 2018,
                                  end_year = 2100,
                                  variables_keep = c("Biomass" = "biomass",
                                                     "Catches" = "catches_total",
                                                     "Revenue" = "revenue_total",
                                                     "Mortality" = "u_mort_total"),
                                  fleets_keep = c("Total" = "Total"),
                                  plot_name = "plot",
                                  plot_width = 8,
                                  plot_height = 6.5,
                                  results_dir,
                                  group_var = "run_group",
                                  color_var = "run_group",
                                  size_var = "Mortality",
                                  legend_pos_manual = "none",
                                  legend_name = "",
                                  plot_type = "point"){
  
  ### General theme
  plot_theme <- theme_linedraw()+
    theme(legend.position="right",
          legend.box="vertical")
    
  ### Aggregate data
  if(spatial_res == "global"){
    
  plot_dat <- dat %>%
    dplyr::filter(Variable %in% variables_keep) %>%
    dplyr::filter(Fleet %in% fleets_keep) %>%
    group_by(Year, Variable, Fleet, run_id, run_name, run_group) %>%
    summarize(BAU_tot = sum(BAU, na.rm = T),
              Reform_tot = sum(Reform, na.rm = T),
              Diff_tot = case_when(BAU_tot != 0 ~ (Reform_tot - BAU_tot)/abs(BAU_tot),
                            TRUE ~ 0)) %>%
    ungroup() %>%
    mutate(Region = "Global") %>%
    mutate(Variable = case_when(Variable == "biomass" ~ "Biomass",
                                Variable == "catches_total" ~ "Catches",
                                Variable == "revenue_total" ~ "Revenue",
                                Variable == "u_mort_total" ~ "Mortality"))
    
  }else if(spatial_res == "regional"){
    
    plot_dat <- dat %>%
      dplyr::filter(Variable %in% variables_keep) %>%
      dplyr::filter(Fleet %in% fleets_keep) %>%
      group_by(Year, Variable, Fleet, Region, run_id, run_name, run_group) %>%
      mutate(BAU_tot = sum(BAU, na.rm = T),
             Reform_tot = sum(Reform, na.rm = T),
             Diff_tot = case_when(BAU_tot != 0 ~ (Reform_tot - BAU_tot)/abs(BAU_tot),
                                     TRUE ~ 0)) %>%
      ungroup() %>%
      mutate(Region = case_when(Region == "atlantic" ~ "Atlantic Ocean",
                                Region == "indian" ~ "Indian Ocean",
                                Region == "pacific" ~ "Pacific Ocean")) %>%
      mutate(Variable = case_when(Variable == "biomass" ~ "Biomass",
                                  Variable == "catches_total" ~ "Catches",
                                  Variable == "revenue_total" ~ "Revenue",
                                  Variable == "u_mort_total" ~ "Mortality"))
    
  }

  if(temporal_res == "all"){
    
    plot <- plot_dat %>%
      dplyr::filter(Year >= start_year & Year <= end_year) %>%
      ggplot()+
      aes(x = Year, y = Diff_tot*100)+
      aes_string(color = color_var, group = group_var)+
      geom_line(size = 1)+
      geom_hline(yintercept = 0)+
      scale_x_continuous(expand = c(0,0))+
      labs(x = "Year", y = "Change relative to BAU (%)", color = legend_name)+
      plot_theme +
      theme(legend.position = legend_pos_manual)+
      facet_grid(Variable~Region)
    
  }else if(temporal_res == "last"){
    
    if(plot_type == "point"){
      
      plot_dat <- plot_dat %>%
        dplyr::filter(Year == end_year) %>%
        group_by(Year, Variable, Fleet, Region, run_id, run_name, run_group) %>%
        summarize(Value = unique(Diff_tot)) %>%
        ungroup() %>%
        spread(Variable, Value)
    
    x_limits <- c(min(0,plyr::round_any(min(plot_dat$Biomass), 0.05, f = floor)), 
                  plyr::round_any(max(plot_dat$Biomass), 0.05, f = ceiling))
    y_limits <- c(min(0,plyr::round_any(min(plot_dat$Catches), 0.01, f = floor)), 
                  plyr::round_any(max(plot_dat$Catches), 0.05, f = ceiling))
    
    plot <- plot_dat %>%
      ggplot()+
      aes(x = Biomass, y = Catches)+
      aes_string(color = color_var, group = group_var, size = size_var)+
      geom_point(alpha = 0.6)+
      scale_x_continuous(labels = percent, limits = x_limits)+
      scale_y_continuous(labels = percent, limits = y_limits)+
      geom_hline(yintercept = 0, linetype = 2)+
      geom_vline(xintercept = 0, linetype = 2)+
      plot_theme+
      labs(x = "Change in biomass relative to BAU (%)",
           y = "Change in catch relative to BAU (%)",
           color = legend_name)+
      scale_size(trans = 'reverse',
        # breaks = seq(rev_limits[1], rev_limits[2], length.out = 5),
        #          labels = percent(seq(rev_limits[1], rev_limits[2], length.out = 5)),
        #          limits = rev_limits,
                 name = "Change in fishing mortality\nrelative to BAU (%)")+
      #guides(size = guide_legend(reverse = F))+
      theme(legend.position = legend_pos_manual)+
      facet_grid(~Region)
    
    }else if(plot_type == "box"){
      
      plot_dat_out <- plot_dat %>%
        dplyr::filter(Year == end_year) %>%
        group_by(Year, Variable, Fleet, Region, run_id, run_name, run_group) %>%
        summarize(Value = unique(Diff_tot)) %>%
        ungroup() 
      
      plot <- plot_dat_out %>%
        #dplyr::filter(run_id != "Ambitious Reform") %>%
        ggplot()+
        aes(x = Value, y = run_group)+
        aes_string(color = color_var, group = group_var)+
        geom_boxplot()+
        geom_vline(xintercept = 0, linetype = 2)+
        scale_x_continuous(labels = percent)+
        plot_theme+
        labs(x = "Change relative to BAU (%)",
             y = "",
             color = legend_name)+
        theme(legend.position = legend_pos_manual)+
        facet_grid(Variable~Region, scales = "free_x") +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
        #coord_flip()
      
      
    }
    
  }
  
  ### Save plot 
  ggsave(paste0(results_dir, plot_name, ".png"), plot, width = plot_width, height = plot_height)
  
  return(plot)
  
}