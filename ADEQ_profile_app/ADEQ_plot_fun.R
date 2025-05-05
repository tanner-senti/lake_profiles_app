# Plotting function from lake_profiles_graphing project
# modified to work with app

site_plottingAR <- function(site_data) {
  
  site_data <- site_data %>% 
    mutate(across(Date:SiteID, as.character),
           across(Temp_Inst:Depth, as.numeric))
  
  site_id <- unique(site_data$SiteID)
  
  # Creating a custom theme for text size:
  my_theme <- theme_classic(12) +
    theme(
      axis.title = element_text(size = 16),  # Larger axis titles
      axis.text = element_text(size = 14),                  # Larger tick labels
      plot.title = element_text(size = 18),  # Larger plot titles
      legend.title = element_text(size = 14),               # Larger legend title
      legend.text = element_text(size = 12)                 # Larger legend text
    )
  
  
  # Plotting:
  
  # DO:
  do_p <- ggplot(site_data) +
    aes(x = DO_Inst, y = Depth, color = Date) +
    geom_point() +
    geom_line(orientation = "y") +
    scale_y_reverse() +
    scale_x_continuous(position = "top")  +
    labs(x = "Dissolved oxygen (mg/L)", y = "Depth (m)") +
    my_theme
  
  # pH:
  ph_p <- ggplot(site_data) + 
    aes(x=pH_Inst, y=Depth, color = Date) +
    geom_point() + 
    geom_line(orientation = "y") +
    scale_y_reverse() + 
    scale_x_continuous(position = "top")  +
    labs(x="pH", y="Depth (m)") +
    my_theme
  
  # Temp:
  temp_p <- ggplot(site_data) + 
    aes(x=Temp_Inst, y=Depth, color = Date) +
    geom_point() + 
    geom_line(orientation = "y") +
    scale_y_reverse() + 
    scale_x_continuous(position = "top")  +
    labs(x="Temperature (\u00B0C)", y="Depth (m)") +
    my_theme
  
  # Add title to the grid with larger font size
  title <- grid::textGrob(
    paste(site_id),
    gp = grid::gpar(fontsize = 20)  # Increase fontsize as needed
  )
  
  # Display plots in a grid:
  grid.arrange(do_p, ph_p, temp_p, ncol = 2, top = title)
  
}
