# Testing

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
    labs(title = site_id) +
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
