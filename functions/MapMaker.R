
MapMaker <- function(data,
                     res = "grid",
                     x = NULL,
                     x_limits = NULL,
                     x_name = NULL,
                     y = NULL,
                     y_limits = NULL,
                     y_name = NULL,
                     fill,
                     fill_var,
                     fill_name,
                     fill_breaks,
                     fill_limits,
                     fill_labels,
                     land_sf = NULL,
                     region_sf = NULL,
                     eez_sf = NULL,
                     file_name = "map",
                     file_dir = here::here("results/"),
                     plot_labels = NULL){
  
  ### Themes
  dark_theme <- theme(panel.background = element_rect(fill = "black"),
                     plot.background = element_rect(fill = "white"),
                     legend.background=element_rect(fill="white"),
                     legend.text = element_text(color = "black", size = 16),
                     legend.title = element_text(color = "black", size = 16),
                     legend.title.align = 1,
                     legend.direction="horizontal",
                     legend.position = "bottom",
                     legend.justification = "center",
                     axis.text = element_text(color = "white", size = rel(1)),
                     panel.grid.major = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  
  plot_theme <- theme_bw()+
    theme(legend.text = element_text(color = "black", size = 16),
          legend.title = element_text(color = "black", size = 16),
          legend.title.align = 1,
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.justification = "center",
          axis.text = element_text(color = "black", size = rel(1)))
  
  ### Make land layer
  land <- geom_sf(data=land_sf, color = "grey10", fill="grey30", size = 0.25)
  
  if(res == "grid"){
    
    median <- data %>%
      ggplot()+
      geom_tile(aes(x = get(x), y = get(y), fill = get(paste0(fill, "_median")))) +
      land + 
      scale_fill_distiller(type = "seq", palette = "RdYlBu",
                           name = paste0(fill_name),
                           na.value = "#000000", 
                           limits = fill_limits,
                           breaks = fill_breaks,
                           labels = fill_labels,
                           oob = scales::squish)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-80,90))+
      xlab("") +
      ylab("") +
      dark_theme + 
      guides(fill=guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"))
    
    mean <- data %>%
      ggplot()+
      geom_tile(aes(x = get(x), y = get(y), fill = get(paste0(fill, "_mean")))) +
      land + 
      scale_fill_distiller(type = "seq", palette = "RdYlBu",
                           name = paste0(fill_name),
                           na.value = "#000000", 
                           limits = fill_limits,
                           breaks = fill_breaks,
                           labels = fill_labels,
                           oob = scales::squish)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-80,90))+
      xlab("") +
      ylab("") +
      dark_theme + 
      guides(fill=guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"))
    
    w_mean <- data %>%
      ggplot()+
      geom_tile(aes(x = get(x), y = get(y), fill = get(paste0(fill, "_w_mean")))) +
      land + 
      scale_fill_distiller(type = "seq", palette = "RdYlBu",
                           name = paste0(fill_name),
                           na.value = "#000000", 
                           limits = fill_limits,
                           breaks = fill_breaks,
                           labels = fill_labels,
                           oob = scales::squish)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-80,90))+
      xlab("") +
      ylab("") +
      dark_theme + 
      guides(fill=guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"))
    
    # Combine into three panel
    legend <- get_legend(w_mean)
    
    all <- plot_grid(median + theme(legend.position = "none"),
                     mean + theme(legend.position = "none"),
                     w_mean + theme(legend.position = "none"),
                     legend,
                     ncol = 1,
                     rel_heights = c(1, 1, 1, 0.2),
                     labels = plot_labels,
                     label_x = c(-0.04, -0.03, -0.1, 0),
                     align = "v")
    
    ggsave(paste0(file_dir, file_name, ".png"), dpi = 200, width = 7, height = 12.5)
    
    return(list(median = median,
                mean = mean,
                w_mean = w_mean,
                all = all))
    
  }else if(res == "region"){
    
    median <- data %>%
      right_join(region_sf, by = "fao_region") %>%
      ggplot()+
      geom_sf(aes(geometry = geometry, fill = get(paste0(fill, "_median"))), color = "#000000") +
      land + 
      scale_fill_distiller(type = "seq", palette = "RdYlBu",
                           name = fill_name,
                           na.value = "#000000", 
                           limits = fill_limits,
                           breaks = fill_breaks,
                           labels = fill_labels,
                           oob = scales::squish)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-80,90))+
      xlab("") +
      ylab("") +
      dark_theme + 
      guides(fill=guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"))
    
    mean <- data %>%
      right_join(region_sf, by = "fao_region") %>%
      ggplot()+
      geom_sf(aes(geometry = geometry, fill = get(paste0(fill, "_mean"))), color = "#000000") +
      land + 
      scale_fill_distiller(type = "seq", palette = "RdYlBu",
                           name = fill_name,
                           na.value = "#000000", 
                           limits = fill_limits,
                           breaks = fill_breaks,
                           labels = fill_labels,
                           oob = scales::squish)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-80,90))+
      xlab("") +
      ylab("") +
      dark_theme + 
      guides(fill=guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"))
    
    w_mean <- data %>%
      right_join(region_sf, by = "fao_region") %>%
      ggplot()+
      geom_sf(aes(geometry = geometry, fill = get(paste0(fill, "_w_mean"))), color = "#000000") +
      land + 
      scale_fill_distiller(type = "seq", palette = "RdYlBu",
                           name = fill_name,
                           na.value = "#000000", 
                           limits = fill_limits,
                           breaks = fill_breaks,
                           labels = fill_labels,
                           oob = scales::squish)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-80,90))+
      xlab("") +
      ylab("") +
      dark_theme + 
      guides(fill=guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"))
    
    # Combine into three panel
    legend <- get_legend(w_mean)
    
    all <- plot_grid(median + theme(legend.position = "none"),
                     mean + theme(legend.position = "none"),
                     w_mean + theme(legend.position = "none"),
                     legend,
                     ncol = 1,
                     rel_heights = c(1, 1, 1, 0.2),
                     labels = plot_labels,
                     label_x = c(-0.04, -0.03, -0.1, 0),
                     align = "v")
    
    ggsave(paste0(file_dir, file_name, ".png"), dpi = 200, width = 7, height = 12.5)
    
    return(list(median = median,
                mean = mean,
                w_mean = w_mean,
                all = all))
    
  }else if(res == "corr"){
    
    # This one isn't a map, but putting it here for now
    median <- data %>%
      ggplot()+
      aes(x = get(x), y = get(paste0(y, "_median")))+
      geom_smooth(method = "lm", na.rm = T, se = F, color = "black")+
      geom_point(aes(size = get(fill)), color = "black")+
      scale_size(name = fill_name,
                 limits = fill_limits,
                 breaks = fill_breaks,
                 labels = fill_labels)+
      scale_x_continuous(limits = x_limits)+
      scale_y_continuous(limits = y_limits)+
      xlab(x_name) +
      ylab(paste0("Median ", y_name)) +
      geom_hline(yintercept = 1, linetype = "dashed")+
      plot_theme+
      guides(size=guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"),
        color = "none")
    
    mean <- data %>%
      ggplot()+
      aes(x = get(x), y = get(paste0(y, "_mean")))+
      geom_smooth(method = "lm", na.rm = T, se = F, color = "black")+
      geom_point(aes(size = get(fill)), color = "black")+
      scale_size(name = fill_name,
                 limits = fill_limits,
                 breaks = fill_breaks,
                 labels = fill_labels)+
      scale_x_continuous(limits = x_limits)+
      scale_y_continuous(limits = y_limits)+
      xlab(x_name) +
      ylab(paste0("Mean ", y_name)) +
      geom_hline(yintercept = 1, linetype = "dashed")+
      plot_theme+
      guides(size=guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"),
        color = "none")
    
    w_mean <- data %>%
      ggplot()+
      aes(x = get(x), y = get(paste0(y, "_w_mean")))+
      geom_smooth(method = "lm", na.rm = T, se = F, color = "black")+
      geom_point(aes(size = get(fill)), color = "black")+
      scale_size(name = fill_name,
                 limits = fill_limits,
                 breaks = fill_breaks,
                 labels = fill_labels)+
      scale_x_continuous(limits = x_limits)+
      scale_y_continuous(limits = y_limits)+
      xlab(x_name) +
      ylab(paste0("Weighted Mean ", y_name)) +
      geom_hline(yintercept = 1, linetype = "dashed")+
      plot_theme+
      guides(size=guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"),
        color = "none")
    
    #Combine into three panel
    all <- plot_grid(
      
      plot_grid(median + theme(legend.position = "none", axis.title.x = element_blank()),
                mean + theme(legend.position = "none", axis.title.x = element_blank()),
                w_mean + theme(legend.position = "none"),
                ncol = 1,
                align = "hv"),
      get_legend(w_mean),
      ncol = 1,
      rel_heights = c(3, 0.2))
    
    ggsave(paste0(file_dir, file_name, ".png"), dpi = 200, width = 7, height = 12.5)
    
    return(list(median = median,
           mean = mean,
           w_mean = w_mean,
           all = all))
    
  }else{
    
    print("Not a valid resolution")
    
  }
  
}