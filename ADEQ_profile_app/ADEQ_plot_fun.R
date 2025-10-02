# Single profile plotting function, modified from Utah's app. Initially takes
# long data from app server selection: -------------------------------------------
profilePlotAR_test <- function(
  data,
  parameter = "CharacteristicName",
  depth = "Depth, data-logger (ported)",
  do = "Dissolved oxygen (DO)",
  temp = "Temperature, water",
  pH = "pH",
  value_var = "ResultMeasureValue",
  line_no = "Time",
  do_crit,
  temp_crit,
  pH_crit
) {
  data <- data[!is.na(data[, line_no]), ]
  data <- droplevels(data[data[, parameter] %in% c(depth, do, temp, pH), ])
  if (all(data[, parameter] != depth)) {
    stop("No depth values associated with this profile")
  }

  param_names <- data.frame(rbind(depth, do, temp, pH))
  colnames(param_names)[1] <- parameter
  param_names$param_name <- row.names(param_names)
  data <- merge(data, param_names, all.x = TRUE)

  site_id <- unique(data$SiteID)
  lk_name <- unique(data$Lake_Name)

  prof_matrix <- reshape2::dcast(
    SiteID + ActivityIdentifier + Date + get(line_no) ~ param_name,
    value.var = value_var,
    fun.aggregate = mean,
    data = data
  )
  prof_matrix <- prof_matrix[order(prof_matrix$depth), ]

  # Convert to long format for ggplot
  prof_long <- prof_matrix %>%
    tidyr::pivot_longer(
      cols = c(do, temp, pH),
      names_to = "Parameter",
      values_to = "Value"
    ) %>%
    mutate(
      Parameter = case_when(
        Parameter == "do" ~ "DO (mg/L)",
        Parameter == "temp" ~ "Temp (°C)",
        TRUE ~ Parameter
      )
    ) %>%
    mutate(
      tooltip = paste0(
        "Depth (m): ",
        depth,
        "<br>Parameter: ",
        Parameter,
        "<br>Value: ",
        round(Value, 2),
        "<br>Date: ",
        Date
      )
    )

  # Build interactive ggplot
  p <- ggplot(
    prof_long,
    aes(x = Value, y = depth)
  ) +
    # Lines (black)
    geom_path_interactive(
      aes(group = Parameter),
      linewidth = 0.6,
      colour = "black"
    ) +
    # Points (filled color, black outline)
    geom_point_interactive(
      aes(fill = Parameter, shape = Parameter, tooltip = tooltip),
      size = 3,
      colour = "black",
      stroke = 0.6
    ) +
    scale_y_reverse(name = "Depth (m)") +
    scale_x_continuous(position = "top", name = "") +
    scale_fill_manual(
      values = c(
        "DO (mg/L)" = "deepskyblue3",
        "Temp (°C)" = "orange",
        "pH" = "green"
      ),
      labels = c(
        "DO (mg/L)" = "Dissolved Oxygen (mg/L)",
        "Temp (°C)" = "Temperature (°C)",
        "pH" = "pH"
      )
    ) +
    scale_shape_manual(
      values = c("DO (mg/L)" = 24, "Temp (°C)" = 21, "pH" = 22),
      labels = c(
        "DO (mg/L)" = "Dissolved Oxygen (mg/L)",
        "Temp (°C)" = "Temperature (°C)",
        "pH" = "pH"
      )
    ) +
    labs(title = paste0(site_id, " - ", lk_name)) +
    theme_bw(base_size = 16) +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      legend.box = "vertical",
      legend.direction = "vertical"
    )
  return(p)
}


# Plotting function from lake_profiles_graphing project, modified to work with app
# This produces the site profiles (all dates) plots
# Takes profiles_wide data from app server ---------------------------------------------
site_plottingAR_interactive <- function(site_data) {
  site_data <- site_data %>%
    mutate(
      across(Date:SiteID, as.character),
      across(`Temp (°C)`:Depth, as.numeric),
      tooltip = paste0(
        "Date: ",
        Date,
        "<br>Depth: ",
        Depth,
        " m",
        "<br>DO: ",
        `DO (mg/L)`,
        "<br>Temp: ",
        `Temp (°C)`,
        "<br>pH: ",
        pH
      )
    )

  site_id <- unique(site_data$SiteID)
  lk_name <- unique(site_data$Lake_Name)

  my_theme <- theme_classic(10) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.3, "cm"), # Smaller legend keys/symbols
      legend.spacing.y = unit(0.2, "cm"), # Tighter spacing between items
      legend.margin = margin(0, 0, 0, 0), # Remove legend margins
      legend.box.spacing = unit(0.2, "cm"), # Less space around legend box
      legend.position = "none" # Remove legends from individual plots
    )

  do_p <- ggplot(site_data, aes(x = `DO (mg/L)`, y = Depth, color = Date)) +
    geom_line_interactive(aes(group = Date), orientation = "y") +
    geom_point_interactive(aes(tooltip = tooltip), size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    labs(x = "Dissolved oxygen (mg/L)", y = "Depth (m)") +
    my_theme

  ph_p <- ggplot(site_data, aes(x = pH, y = Depth, color = Date)) +
    geom_line_interactive(aes(group = Date), orientation = "y") +
    geom_point_interactive(aes(tooltip = tooltip), size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    labs(x = "pH", y = "Depth (m)") +
    my_theme

  temp_p <- ggplot(site_data, aes(x = `Temp (°C)`, y = Depth, color = Date)) +
    geom_line_interactive(aes(group = Date), orientation = "y") +
    geom_point_interactive(aes(tooltip = tooltip), size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    labs(x = "Temperature (°C)", y = "Depth (m)") +
    my_theme

  # Extract legend from temp_p (before removing it)
  temp_p_with_legend <- temp_p +
    theme(legend.position = "right", legend.text = element_text(size = 8))
  legend <- cowplot::get_legend(temp_p_with_legend)

  # Create the grid layout
  cowplot::plot_grid(
    cowplot::ggdraw() +
      cowplot::draw_label(
        paste0(site_id, " - ", lk_name),
        size = 14,
        hjust = 0.5
      ),
    cowplot::plot_grid(
      do_p,
      ph_p,
      temp_p,
      legend,
      ncol = 2,
      rel_widths = c(1, 1)
    ),
    ncol = 1,
    rel_heights = c(0.08, 1)
  )
}

# -------------------------------------------------------------------------------------
# Slightly customzied for the "by year" tab - just different color gradient
site_plottingAR_year_interactive <- function(site_data) {
  site_data <- site_data %>%
    mutate(
      across(Date:SiteID, as.character),
      across(`Temp (°C)`:Depth, as.numeric),
      Date = as.Date(Date),
      Date_f = factor(Date, levels = sort(unique(Date))),
      tooltip = paste0(
        "Date: ",
        Date,
        "<br>Depth: ",
        Depth,
        " m",
        "<br>DO: ",
        `DO (mg/L)`,
        "<br>Temp: ",
        `Temp (°C)`,
        "<br>pH: ",
        pH
      )
    )

  site_id <- unique(site_data$SiteID)
  lk_name <- unique(site_data$Lake_Name)

  n_dates <- length(unique(site_data$Date_f))
  date_colors <- colorRampPalette(
    viridisLite::mako(n_dates, begin = 0.9, end = 0.1)
  )(n_dates)

  my_theme <- theme_classic(10) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.3, "cm"), # Smaller legend keys/symbols
      legend.spacing.y = unit(0.2, "cm"), # Tighter spacing between items
      legend.margin = margin(0, 0, 0, 0), # Remove legend margins
      legend.box.spacing = unit(0.2, "cm"), # Less space around legend box
      legend.position = "none" # Remove legends from individual plots
    )

  do_p <- ggplot(site_data, aes(x = `DO (mg/L)`, y = Depth, color = Date_f)) +
    geom_line_interactive(
      aes(group = Date_f),
      orientation = "y"
    ) +
    geom_point_interactive(aes(tooltip = tooltip), size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(values = date_colors, name = "Date") +
    labs(x = "Dissolved Oxygen (mg/L)", y = "Depth (m)") +
    my_theme

  ph_p <- ggplot(site_data, aes(x = pH, y = Depth, color = Date_f)) +
    geom_line_interactive(
      aes(group = Date_f),
      orientation = "y"
    ) +
    geom_point_interactive(aes(tooltip = tooltip), size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(values = date_colors, name = "Date") +
    labs(x = "pH", y = "Depth (m)") +
    my_theme

  temp_p <- ggplot(site_data, aes(x = `Temp (°C)`, y = Depth, color = Date_f)) +
    geom_line_interactive(
      aes(group = Date_f),
      orientation = "y"
    ) +
    geom_point_interactive(aes(tooltip = tooltip), size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(values = date_colors, name = "Date") +
    labs(x = "Temperature (°C)", y = "Depth (m)") +
    my_theme

  # Extract legend from temp_p (before removing it)
  temp_p_with_legend <- temp_p +
    theme(legend.position = "right", legend.text = element_text(size = 8))
  legend <- cowplot::get_legend(temp_p_with_legend)

  # Create plot grid:
  cowplot::plot_grid(
    cowplot::ggdraw() +
      cowplot::draw_label(
        paste0(site_id, " - ", lk_name),
        size = 14,
        hjust = 0.5
      ),
    cowplot::plot_grid(
      do_p,
      ph_p,
      temp_p,
      legend,
      ncol = 2,
      rel_widths = c(1, 1)
    ),
    ncol = 1,
    rel_heights = c(0.08, 1)
  )
}

#####################################################################################
# non-interactive versions of the sites profiles (all dates) and site profiles by year plot grids
# for the download button handlers:
site_plottingAR <- function(site_data) {
  site_data <- site_data %>%
    mutate(
      across(Date:SiteID, as.character),
      across(`Temp (°C)`:Depth, as.numeric)
    )

  site_id <- unique(site_data$SiteID)
  lk_name <- unique(site_data$Lake_Name)

  my_theme <- theme_classic(10) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.3, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0.2, "cm"),
      legend.position = "none"
    )

  do_p <- ggplot(site_data, aes(x = `DO (mg/L)`, y = Depth, color = Date)) +
    geom_line(aes(group = Date), orientation = "y") +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    labs(x = "Dissolved oxygen (mg/L)", y = "Depth (m)") +
    my_theme

  ph_p <- ggplot(site_data, aes(x = pH, y = Depth, color = Date)) +
    geom_line(aes(group = Date), orientation = "y") +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    labs(x = "pH", y = "Depth (m)") +
    my_theme

  temp_p <- ggplot(site_data, aes(x = `Temp (°C)`, y = Depth, color = Date)) +
    geom_line(aes(group = Date), orientation = "y") +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    labs(x = "Temperature (°C)", y = "Depth (m)") +
    my_theme

  temp_p_with_legend <- temp_p +
    theme(legend.position = "right", legend.text = element_text(size = 8))
  legend <- cowplot::get_legend(temp_p_with_legend)

  cowplot::plot_grid(
    cowplot::ggdraw() +
      cowplot::draw_label(
        paste0(site_id, " - ", lk_name),
        size = 14,
        hjust = 0.5
      ) +
      theme(plot.background = element_rect(fill = "white", color = NA)),
    cowplot::ggdraw() +
      cowplot::draw_plot(
        cowplot::plot_grid(
          do_p,
          ph_p,
          temp_p,
          legend,
          ncol = 2,
          rel_widths = c(1, 1)
        )
      ) +
      theme(plot.background = element_rect(fill = "white", color = NA)),
    ncol = 1,
    rel_heights = c(0.08, 1)
  )
}
#-------------
site_plottingAR_year <- function(site_data) {
  site_data <- site_data %>%
    mutate(
      across(Date:SiteID, as.character),
      across(`Temp (°C)`:Depth, as.numeric),
      Date = as.Date(Date),
      Date_f = factor(Date, levels = sort(unique(Date))),
    )

  site_id <- unique(site_data$SiteID)
  lk_name <- unique(site_data$Lake_Name)

  n_dates <- length(unique(site_data$Date_f))
  date_colors <- colorRampPalette(
    viridisLite::mako(n_dates, begin = 0.9, end = 0.1)
  )(n_dates)

  my_theme <- theme_classic(10) +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.3, "cm"), # Smaller legend keys/symbols
      legend.spacing.y = unit(0.2, "cm"), # Tighter spacing between items
      legend.margin = margin(0, 0, 0, 0), # Remove legend margins
      legend.box.spacing = unit(0.2, "cm"), # Less space around legend box
      legend.position = "none" # Remove legends from individual plots
    )

  do_p <- ggplot(site_data, aes(x = `DO (mg/L)`, y = Depth, color = Date_f)) +
    geom_line(
      aes(group = Date_f),
      orientation = "y"
    ) +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(values = date_colors, name = "Date") +
    labs(x = "Dissolved Oxygen (mg/L)", y = "Depth (m)") +
    my_theme

  ph_p <- ggplot(site_data, aes(x = pH, y = Depth, color = Date_f)) +
    geom_line(
      aes(group = Date_f),
      orientation = "y"
    ) +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(values = date_colors, name = "Date") +
    labs(x = "pH", y = "Depth (m)") +
    my_theme

  temp_p <- ggplot(site_data, aes(x = `Temp (°C)`, y = Depth, color = Date_f)) +
    geom_line(
      aes(group = Date_f),
      orientation = "y"
    ) +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    scale_color_manual(values = date_colors, name = "Date") +
    labs(x = "Temperature (°C)", y = "Depth (m)") +
    my_theme

  # Extract legend from temp_p (before removing it)
  temp_p_with_legend <- temp_p +
    theme(legend.position = "right", legend.text = element_text(size = 8))
  legend <- cowplot::get_legend(temp_p_with_legend)

  # Create plot grid:
  cowplot::plot_grid(
    cowplot::ggdraw() +
      cowplot::draw_label(
        paste0(site_id, " - ", lk_name),
        size = 14,
        hjust = 0.5
      ) +
      theme(plot.background = element_rect(fill = "white", color = NA)),
    cowplot::ggdraw() +
      cowplot::draw_plot(
        cowplot::plot_grid(
          do_p,
          ph_p,
          temp_p,
          legend,
          ncol = 2,
          rel_widths = c(1, 1)
        )
      ) +
      theme(plot.background = element_rect(fill = "white", color = NA)),
    ncol = 1,
    rel_heights = c(0.08, 1)
  )
}

#####################################################################################

# Framework for a tab with a separate plot for each year, and the dropdown
# selection is parameter (opposite of above)
# site_plottingAR_paramyear <- function(site_data, sel_param) {
#   site_data <- site_data %>%
#     mutate(
#       across(Date:SiteID, as.character),
#       across(`Temp (°C)`:Depth, as.numeric),
#       Date = as.Date(Date),
#       Year = format(Date, "%Y"),
#       Month = format(Date, "%b"), # abbreviated month for legend
#       Month_num = as.numeric(format(Date, "%m")), # 1–12 for ordering
#       DayOfYear = as.numeric(format(Date, "%j"))
#     )
#
#   site_id <- unique(site_data$SiteID)
#
#   # fixed 12-color palette across ALL plots (Jan → Dec, light→dark)
#   month_colors <- colorRampPalette(
#     viridisLite::mako(12, begin = 0.9, end = 0)
#   )(12)
#
#   my_theme <- theme_classic(12) +
#     theme(
#       axis.title = element_text(size = 16),
#       axis.text = element_text(size = 14),
#       plot.title = element_text(size = 18),
#       legend.title = element_text(size = 14),
#       legend.text = element_text(size = 12)
#     )
#
#   # build a plot for each Year
#   year_plots <- lapply(split(site_data, site_data$Year), function(df) {
#     # create a mapping of Month_num to the first date for each month
#     date_labels <- df %>%
#       group_by(Month_num) %>%
#       summarise(first_date = min(Date), .groups = "drop") %>%
#       arrange(Month_num)
#
#     ggplot(
#       df,
#       aes(
#         x = .data[[sel_param]],
#         y = Depth,
#         color = factor(Month_num, levels = 1:12),
#         group = interaction(Year, DayOfYear)
#       )
#     ) +
#       geom_point() +
#       geom_line(orientation = "y") +
#       scale_y_reverse() +
#       scale_x_continuous(position = "top") +
#       scale_color_manual(
#         values = month_colors,
#         labels = format(date_labels$first_date, "%Y-%m-%d"),
#         breaks = date_labels$Month_num,
#         name = "Date",
#         drop = TRUE
#       ) +
#       labs(
#         x = sel_param,
#         y = "Depth (m)",
#         title = paste("Year", unique(df$Year))
#       ) +
#       my_theme
#   })
#
#   # grid title
#   title <- grid::textGrob(
#     paste(site_id, "-", sel_param),
#     gp = grid::gpar(fontsize = 20)
#   )
#
#   # arrange all year plots in grid
#   gridExtra::grid.arrange(grobs = year_plots, ncol = 2, top = title)
# }

# site_plottingAR_interactive <- function(site_data) {
#   site_data <- site_data %>%
#     mutate(
#       across(Date:SiteID, as.character),
#       across(`Temp (°C)`:Depth, as.numeric),
#       tooltip = paste0(
#         "Date: ",
#         Date,
#         "<br>Depth: ",
#         Depth,
#         " m",
#         "<br>DO: ",
#         `DO (mg/L)`,
#         "<br>Temp: ",
#         `Temp (°C)`,
#         "<br>pH: ",
#         pH
#       )
#     )
#
#   site_id <- unique(site_data$SiteID)
#   lk_name <- unique(site_data$Lake_Name)
#
#   my_theme <- theme_classic(10) +
#     theme(
#       legend.text = element_text(size = 8),
#       legend.title = element_text(size = 8), # Smaller legend title
#       legend.key.size = unit(0.3, "cm"), # Smaller legend keys/symbols
#       legend.spacing.y = unit(0.15, "cm"), # Tighter spacing between items
#       legend.margin = margin(0, 0, 0, 0), # Remove legend margins
#       legend.box.spacing = unit(0.2, "cm") # Less space around legend box
#     )
#
#   do_p <- ggplot(site_data, aes(x = `DO (mg/L)`, y = Depth, color = Date)) +
#     geom_line_interactive(
#       aes(group = Date),
#       orientation = "y"
#     ) +
#     geom_point_interactive(aes(tooltip = tooltip), size = 1) +
#     scale_y_reverse() +
#     scale_x_continuous(position = "top") +
#     labs(x = "Dissolved oxygen (mg/L)", y = "Depth (m)") +
#     my_theme
#
#   ph_p <- ggplot(site_data, aes(x = pH, y = Depth, color = Date)) +
#     geom_line_interactive(
#       aes(group = Date),
#       orientation = "y"
#     ) +
#     geom_point_interactive(aes(tooltip = tooltip), size = 1) +
#     scale_y_reverse() +
#     scale_x_continuous(position = "top") +
#     labs(x = "pH", y = "Depth (m)") +
#     my_theme
#
#   temp_p <- ggplot(site_data, aes(x = `Temp (°C)`, y = Depth, color = Date)) +
#     geom_line_interactive(
#       aes(group = Date),
#       orientation = "y"
#     ) +
#     geom_point_interactive(aes(tooltip = tooltip), size = 1) +
#     scale_y_reverse() +
#     scale_x_continuous(position = "top") +
#     labs(x = "Temperature (°C)", y = "Depth (m)") +
#     my_theme
#
#   cowplot::plot_grid(
#     cowplot::ggdraw() +
#       cowplot::draw_label(
#         paste0(site_id, " - ", lk_name),
#         size = 14,
#         hjust = 0.5
#       ),
#     cowplot::plot_grid(do_p, ph_p, temp_p, ncol = 2, rel_widths = c(1, 1)),
#     ncol = 1,
#     rel_heights = c(0.08, 1)
#   )
# }
