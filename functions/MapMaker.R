
MapMaker <- function(data,
                     res = "grid",
                     x = NULL,
                     x_limits = NULL,
                     x_name = NULL,
                     y = NULL,
                     y_limits = NULL,
                     y_name = NULL,
                     fill,
                     fill_name,
                     fill_breaks,
                     fill_limits,
                     fill_labels,
                     land_sf = NULL,
                     region_sf = NULL,
                     eez_sf = NULL){
  
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
    
    out <- data %>%
      ggplot()+
      geom_tile(aes(x = get(x), y = get(y), fill = get(fill))) +
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
    
    return(out)
    
  }else if(res == "region"){
    
    out <- data %>%
      right_join(region_sf, by = "fao_region") %>%
      ggplot()+
      geom_sf(aes(geometry = geometry, fill = get(fill)), color = "#000000") +
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
    
    return(out)
    
  }else if(res == "corr"){
    
    # This one isn't a map, but puttin it here for now
    out <- data %>%
      ggplot()+
      aes(x = get(x), y = get(y), size = get(fill))+
      geom_point(color = "black")+
      scale_size(name = fill_name,
                 limits = fill_limits,
                 breaks = fill_breaks,
                 labels = fill_labels)+
      geom_smooth(method = "lm", na.rm = T, se = F, color = "black")+
      # scale_color_distiller(type = "seq", palette = "RdYlBu",
      #                      name = fill_name,
      #                      na.value = "#000000", 
      #                      limits = fill_limits,
      #                      breaks = fill_breaks,
      #                      labels = fill_labels,
      #                      oob = scales::squish)+
      scale_x_continuous(limits = x_limits)+
      scale_y_continuous(limits = y_limits)+
      xlab(x_name) +
      ylab(y_name) +
      geom_hline(yintercept = 1, linetype = "dashed")+
      geom_vline(xintercept = 1, linetype = "dashed")+
      plot_theme+
      guides(size=guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        frame.colour = "black",
        barwidth=4,
        barheight=0.25,
        default.unit="inch"),
        color = "none")
    
    return(out)
    
  }else{
    
    print("Not a valid resolution")
    
  }
  
}