## Modified app by Tanner Senti (2024-12-31)

# Data come from "lake_profiles_graphing" project at
# E:\LAKES\Quarterly Lakes Study\2023 - 2026 Lakes Study\Lake Profiles

# Function to check and install packages
check_and_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of packages needed (instead of using library()):
required_packages <- (c(
  "magrittr",
  "dplyr",
  "ggplot2",
  "leaflet",
  "shiny",
  "markdown",
  "gridExtra",
  "bslib",
  "ggiraph"
))

# Check and install
check_and_install(required_packages)

# Only install if running locally
if (Sys.getenv("SHINY_PORT") == "") {
  # Not on shinyapps.io
  check_and_install <- function(packages) {
    for (pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      }
    }
  }

  required_packages <- c(
    "magrittr",
    "dplyr",
    "ggplot2",
    "leaflet",
    "shiny",
    "markdown",
    "gridExtra",
    "bslib"
  )
  check_and_install(required_packages)
} else {
  # On shinyapps.io, just load packages
  library(magrittr)
  library(dplyr)
  library(leaflet)
  library(markdown)
  library(ggplot2)
  library(gridExtra)
  library(bslib)
  library(ggiraph)
}

#library(wqTools)

# Custom map function for AR instead of Utah's map function in wqTools:
source("map_fun_redo.R")
# Custom plot function for AR instead of Utah's profilePlot() function in wqTools:
source("plot_fun.R")
# Custom plot function for AR from lake_profiles_graphing project:
source("ADEQ_plot_fun.R")

# Testing interactive plot function:
source("plot_fun_test.R")

## UI -----------------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(
    version = 5, # Bootstrap 5
    bootswatch = "flatly", # Modern, clean look
    primary = "#0DA5B5", # Match ADEQ teal
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto Slab")
  ),

  div(
    style = "margin: 15px;", # space on all sides

    tags$head(
      tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
        type = "text/javascript"
      )
    ),

    # Header
    headerPanel(
      title = tags$a(
        href = 'https://www.adeq.state.ar.us/water/',
        tags$img(
          src = 'adeq_logo.png',
          height = 125,
          width = 100 * 2.85 * 1.75
        ),
        target = "_blank"
      ),
    ),
    # Heading under the logo:
    fluidRow(
      column(
        12,
        h4(
          "Lake Vertical Profiles Dashboard",
          style = "margin-top: 10px; margin-left: 60px; color: #0DA5B5;"
        ),
        p(
          "Disclaimer: Data are provisional and not for official use",
          style = "margin-left: 60px; margin-top: -5px; font-size: 90%; color: #D9534F;"
        )
      )
    ),
    # Disclaimer text if needed:
    # fluidRow(column(
    #   p(
    #     "Lakes Profiles Dashboard"
    #   ),
    #   width = 2
    # )),
    # User guide button:
    fluidRow(column(
      2,
      actionButton("show_guide", "Show User Guide", style = "margin-top: 10px;")
    )),

    br(),

    # Input widgets
    fluidRow(
      column(
        5,
        conditionalPanel(
          condition = "input.plot_tabs!='User guide'",
          tabsetPanel(
            id = "ui_tab",
            tabPanel(
              "Map",
              column(
                12,
                h5(
                  "Click a site to display plots",
                  style = "color: #666666; margin-top: 10px;"
                )
              ),
              column(
                12,
                shinycssloaders::withSpinner(
                  leaflet::leafletOutput("map", height = "440px"),
                  size = 2,
                  color = "#0080b7"
                )
              )
            ),
            tabPanel(
              "Table",
              column(
                12,
                h5(
                  "Click a site to display plots",
                  style = "color: #666666; margin-top: 10px;"
                ),
                div(DT::dataTableOutput("table_input"), style = "font-size:70%")
              )
            )
          ),

          # Always-visible profile table under both tabs
          column(
            12,
            h5(
              "Profile data table:",
              style = "color: #666666; margin-top: 10px;"
            ),
            div(
              DT::dataTableOutput("profile_table"),
              style = "font-size:80%;"
            ),
            downloadButton(
              "download_profile_csv",
              "Download CSV",
              style = "margin-top: 5px;  background-color: #0080b7; color: white;"
            )
          )
        ),
        conditionalPanel(
          condition = "input.plot_tabs=='User guide'",
          column(12)
        )
      ),
      column(
        7,
        tabsetPanel(
          id = "plot_tabs",
          tabPanel(
            "Individual profiles",
            fluidRow(column(
              4,
              div(
                uiOutput("date_select"),
                style = "font-family: 'Roboto Slab', serif; font-size: 22px; color: #666666; margin-top: 10px;"
              )
            )),
            fluidRow(
              column(
                12,
                h4("Profile plot:", style = "color: #666666;"),
                div(
                  plotOutput("ind_prof_plot", height = "500px"),
                  style = "max-width: 600px; margin-top: -8px;"
                ),
                downloadButton(
                  "download_profile_plot",
                  "Download Plot",
                  style = "margin-top: 5px; margin-left: 10px; background-color: #0080b7; color: white;"
                )
              ),
              # TESTING, DELETE THIS EXTRA STUFF-----------------
              column(
                12,
                h4("Profile plot:", style = "color: #666666;"),
                div(
                  girafeOutput("ind_prof_plot_test", height = "500px"),
                  style = "max-width: 600px; margin-top: -8px;"
                )
              )
              #--------------------------------------------------
            ),
            br()
          ),
          tabPanel(
            "Site profiles (all dates)",
            # fluidRow(column(4, uiOutput("date_slider"))),
            fluidRow(
              column(
                4,
                div(
                  uiOutput("start_date_select"),
                  style = "font-family: 'Roboto Slab', serif; font-size: 22px; color: #666666; margin-top: 10px;"
                )
              ),
              column(
                4,
                div(
                  uiOutput("end_date_select"),
                  style = "font-family: 'Roboto Slab', serif; font-size: 22px; color: #666666; margin-top: 10px;"
                )
              )
            ),
            fluidRow(column(
              12,
              h4("Parameter profiles:", style = "color: #666666;"),
              div(
                plotOutput("site_prof_plot", height = "600px"),
                style = "max-width: 800px"
              ),
              downloadButton(
                "download_site_plot",
                "Download Plots",
                style = "margin-top: 5px; margin-left: 10px; background-color: #0080b7; color: white;"
              )
            )),
            br()
          ),
        )
      )
    ),

    uiOutput("data_notice"),

    tags$footer(
      "Arkansas Division of Environmental Quality - Lake Vertical Profiles Dashboard",
      style = "text-align: center; padding: 10px; font-size: 80%; color: #777;"
    )
  )
)
# end UI, Server below -----------------------------------------------------------------

server <- function(input, output, session) {
  # Loading modal to keep user out of trouble while map draws...
  showModal(
    modalDialog(
      title = "MAP LOADING - PLEASE WAIT...",
      "Please wait for map to draw before proceeding.",
      size = "l",
      footer = NULL
    )
  )

  # Remove modal when app is ready
  observe({
    req(map, sites_table)
    removeModal()
  })

  # Observer for the user guide button
  observeEvent(input$show_guide, {
    showModal(modalDialog(
      title = "User Guide",
      includeMarkdown('./user_guide/user_guide.rmd'),
      size = "l",
      easyClose = TRUE
    ))
  })

  # Data work -------------------------------------------------------------------

  # Find the latest file ending with date pattern _YYYY-MM-DD.csv
  latest_file <- list.files(
    path = "./data/",
    pattern = "_\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = TRUE
  ) %>%
    sort(decreasing = TRUE) %>% # Sort in descending order (latest first)
    head(1) # Take the first (most recent) file

  # Load the latest file
  profiles_wideAR <- read.csv(latest_file)

  # Fix AR wide data
  profiles_wideAR <- profiles_wideAR %>%
    mutate(SiteID = toupper(SiteID)) %>%
    mutate(ActivityIdentifier = paste(SiteID, Date, sep = "-")) %>%
    mutate(Date = as.Date(Date)) %>%
    rename(
      `DO (mg/L)` = DO_Inst,
      `pH` = pH_Inst,
      `Temp (°C)` = Temp_Inst
    )

  # Create AR long data
  profiles_longAR <- profiles_wideAR %>%
    tidyr::pivot_longer(
      cols = c("Temp (°C)", "DO (mg/L)", "pH", "Depth"),
      names_to = "Parameter",
      values_to = "IR_Value"
    ) %>%
    mutate(
      Parameter = case_when(
        Parameter == "Depth" ~ "Depth (m)",
        #Parameter == "DO_Inst" ~ "DO (mg/L)",
        #Parameter == "pH_Inst" ~ "pH",
        # Parameter == "Temp_Inst" ~ "Temp (°C)",
        TRUE ~ Parameter
      )
    )

  # Temporary fix to issues with plotting and reactive_objects$selectActID -
  # Long term solution needed to preserve tibble and make code work:
  profiles_longAR <- as.data.frame(profiles_longAR)
  profiles_longAR$ActivityIdentifier <-
    as.factor(profiles_longAR$ActivityIdentifier)

  # Load Arkansas sites data:
  # (used in buildMapAR function):
  all_sitesAR <- read.csv("./data/all_site_info.csv")

  filtered_sitesAR <- all_sitesAR %>%
    filter(MonitoringLocationIdentifier %in% profiles_wideAR$SiteID)

  # List of locations with no data (for optional map layer):
  sites_nodataAR <- all_sitesAR %>%
    filter(!(MonitoringLocationIdentifier %in% profiles_wideAR$SiteID))

  # New map/site table:
  sites_table <- filtered_sitesAR %>%
    rename(
      StationID = MonitoringLocationIdentifier,
      Lake_Name = MonitoringLocationName,
      Type = MonitoringLocationTypeName,
      Latitude = LatitudeMeasure,
      Longitude = LongitudeMeasure
    )

  # Complete data work ----

  # Empty reactive values object
  reactive_objects = reactiveValues(
    sel_mlid = NULL,
    selectedActID = NULL,
    sel_profiles = NULL,
    profile_dates = NULL,
    sel_profs_wide = NULL
  )
  observe({
    req(filtered_sitesAR) # Ensure sites data is available
    if (is.null(reactive_objects$sel_mlid)) {
      # Set default site to the first site in the list
      default_site <-
        filtered_sitesAR$MonitoringLocationIdentifier[1]
      reactive_objects$sel_mlid <- default_site
    }
  })

  # Resources for returning site info on click:
  ## https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
  ## https://stackoverflow.com/questions/42613984/how-to-implement-inputmap-marker-click-correctly?noredirect=1&lq=1

  # Select map set up
  map = leaflet::createLeafletMap(session, 'map')

  # Here, buildMapAR() is a function customized from buildMap() in Utah's
  # wqTools package that will build a map from prof_sites data using
  # au_poly polygons (here these are lake polygons)
  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      buildMapAR_redo(sites = filtered_sitesAR, sites_nodataAR)
    })
  })

  # Table interface:
  output$table_input <- DT::renderDataTable({
    req(sites_table) # Ensure data is available
    DT::datatable(
      sites_table,
      selection = 'single',
      rownames = FALSE,
      filter = 'top',
      options = list(
        scrollY = '500px',
        paging = FALSE,
        scrollX = TRUE,
        dom = "ltipr"
      )
    )
  })

  # Ensure that when switching to the Table tab, the selected site is set
  observeEvent(input$ui_tab, {
    if (input$ui_tab == "Table") {
      # This triggers the selection from the map again (if applicable)
      if (!is.null(input$map_marker_click)) {
        site_click <- input$map_marker_click
        siteid = site_click$id
        reactive_objects$sel_mlid = siteid
      }
    }
  })

  # Map marker click (to identify selected site)
  observe({
    req(profiles_longAR)
    site_click <- input$map_marker_click
    if (is.null(site_click)) {
      return()
    }
    siteid = site_click$id
    reactive_objects$sel_mlid = siteid
  })

  # Observer for table row selection to update plots
  observeEvent(input$table_input_rows_selected, {
    req(sites_table) # Ensure data is available
    selected_row <- input$table_input_rows_selected

    if (length(selected_row) > 0) {
      # Get the SiteID of the selected row
      selected_site <- sites_table[selected_row, "StationID"]
      # Update reactive state to reflect selected site
      reactive_objects$sel_mlid <- selected_site
    }
  })

  # Observer to zoom map to the selected site when a row is clicked in the table
  observeEvent(input$table_input_rows_selected, {
    req(sites_table) # Ensure data is available
    selected_row <- input$table_input_rows_selected

    if (length(selected_row) > 0) {
      # Get the selected site's latitude and longitude
      selected_site <- sites_table[selected_row, ]
      lat <- selected_site$Latitude
      lon <- selected_site$Longitude

      # Zoom the map to the selected site
      leafletProxy("map") %>%
        setView(lng = lon, lat = lat, zoom = 12) # Adjust zoom level as needed
    }
  })

  # Select profiles & date options based on selected site ID
  observe({
    req(reactive_objects$sel_mlid)
    reactive_objects$sel_profiles = profiles_longAR[
      profiles_longAR$SiteID == reactive_objects$sel_mlid,
    ]
    profile_dates = unique(reactive_objects$sel_profiles$Date)
    profile_dates = profile_dates[order(profile_dates)]
    reactive_objects$profile_dates = profile_dates
  })

  # Profile date selection
  output$date_select <- renderUI({
    req(reactive_objects$profile_dates)
    selectInput(
      "date_select",
      "Profile date:",
      choices = reactive_objects$profile_dates,
      selected = max(reactive_objects$profile_dates)
    )
  })

  # Date slider for combined profile plots (original usage, converted to two box drop downs):
  # output$date_slider <- renderUI({
  #   req(reactive_objects$profile_dates)
  #   date_min = min(reactive_objects$profile_dates)
  #   date_max = max(reactive_objects$profile_dates)
  #   sliderInput(
  #     "date_slider",
  #     "Date range:",
  #     min = date_min,
  #     max = date_max,
  #     value = c(date_min, date_max)
  #   )
  # })

  # NEW date drop downs instead of slider:
  # Render start date dropdown
  output$start_date_select <- renderUI({
    req(reactive_objects$profile_dates)
    date_choices <- reactive_objects$profile_dates
    selectInput(
      "start_date",
      "Start date:",
      choices = date_choices,
      selected = min(date_choices)
    )
  })

  # Render end date dropdown with filtered choices
  output$end_date_select <- renderUI({
    req(reactive_objects$profile_dates, input$start_date)
    date_choices <- reactive_objects$profile_dates

    # Filter dates to only show those after start_date
    valid_end_dates <- date_choices[date_choices >= input$start_date]

    selectInput(
      "end_date",
      "End date:",
      choices = valid_end_dates,
      selected = max(valid_end_dates)
    )
  })

  # Generate selected aid
  observe({
    req(input$date_select)
    reactive_objects$selectedActID = reactive_objects$sel_profiles[
      reactive_objects$sel_profiles$Date == input$date_select,
      "ActivityIdentifier"
    ][1]
  })

  # Profile plot output (uses long data) ----
  output$ind_prof_plot = renderPlot({
    req(reactive_objects$sel_profiles, reactive_objects$selectedActID)
    one_profile = reactive_objects$sel_profiles[
      reactive_objects$sel_profiles$ActivityIdentifier ==
        reactive_objects$selectedActID,
    ]

    one_profile = unique(one_profile[, c(
      #"DataLoggerLine",
      "ActivityIdentifier",
      "Date",
      "Parameter",
      "IR_Value",
      #"IR_Unit",
      "SiteID",
      "Time"
    )])

    # OG profilePlot() function from wqTools package by Utah DEQ
    # Modified for AR data setup
    profilePlotAR(
      data = one_profile,
      parameter = "Parameter",
      depth = "Depth (m)",
      do = "DO (mg/L)",
      temp = "Temp (°C)",
      pH = "pH",
      value_var = "IR_Value",
      line_no = "Time"
      #pH_crit = c(6.5, 9),
      #do_crit = do_crit,
      #temp_crit = temp_crit

      # customize or remove these:
      #units = "IR_Unit",
      #line_no = "DataLoggerLine",
    )
    box()
  })

  # Download plot button for single plot ---------------------------------------
  output$download_profile_plot <- downloadHandler(
    filename = function() {
      paste0(reactive_objects$sel_mlid, "_", input$date_select, ".png")
    },
    content = function(file) {
      req(reactive_objects$sel_profiles, reactive_objects$selectedActID)
      one_profile <- reactive_objects$sel_profiles[
        reactive_objects$sel_profiles$ActivityIdentifier ==
          reactive_objects$selectedActID,
      ]
      png(file, width = 700, height = 500)
      profilePlotAR(
        data = one_profile,
        parameter = "Parameter",
        depth = "Depth (m)",
        do = "DO (mg/L)",
        temp = "Temp (°C)",
        pH = "pH",
        value_var = "IR_Value",
        line_no = "Time"
      )
      dev.off()
    }
  )

  # TESTING PLOT, DELETE THIS ----------------------------
  # Profile plot output (uses long data) ----
  output$ind_prof_plot_test <- renderGirafe({
    req(reactive_objects$sel_profiles, reactive_objects$selectedActID)

    one_profile <- reactive_objects$sel_profiles[
      reactive_objects$sel_profiles$ActivityIdentifier ==
        reactive_objects$selectedActID,
    ]

    one_profile <- unique(one_profile[, c(
      "ActivityIdentifier",
      "Date",
      "Parameter",
      "IR_Value",
      "SiteID",
      "Time"
    )])

    profilePlotAR_test(
      data = one_profile,
      parameter = "Parameter",
      depth = "Depth (m)",
      do = "DO (mg/L)",
      temp = "Temp (°C)",
      pH = "pH",
      value_var = "IR_Value",
      line_no = "Time"
      # pH_crit = c(6.5, 9),
      # do_crit = do_crit,
      # temp_crit = temp_crit
    )
  })

  #-----------------------------------------------------------

  #Data table output (uses profiles_Wide):----------------------------------------
  observe({
    req(reactive_objects$selectedActID)
    table_data = profiles_wideAR[
      profiles_wideAR$ActivityIdentifier == reactive_objects$selectedActID,
      c("SiteID", "Date", "Depth", "DO (mg/L)", "pH", "Temp (°C)")
    ]
    reactive_objects$table_data = table_data[order(table_data$Depth), ]
  })

  output$profile_table = DT::renderDataTable({
    if (input$plot_tabs == "Individual profiles") {
      req(reactive_objects$table_data)
      dat <- reactive_objects$table_data
      filename <- paste0(reactive_objects$sel_mlid, "_", input$date_select)
    } else if (input$plot_tabs == "Site profiles (all dates)") {
      req(reactive_objects$sel_profs_wide)
      dat <- reactive_objects$sel_profs_wide[, c(
        "SiteID",
        "Date",
        "Depth",
        "DO (mg/L)",
        "pH",
        "Temp (°C)"
      )]
      filename <- paste0(reactive_objects$sel_mlid, "_all_dates")
    } else {
      return(NULL)
    }
    DT::datatable(
      dat,
      selection = 'multiple',
      options = list(
        scrollY = '500px',
        paging = FALSE,
        scrollX = TRUE,
        searching = FALSE
      )
    ) %>%
      DT::formatStyle(
        "DO (mg/L)",
        backgroundColor = DT::styleEqual(1, "orange")
      ) %>%
      DT::formatStyle(
        "pH",
        backgroundColor = DT::styleEqual(1, "orange")
      ) %>%
      DT::formatStyle(
        "Temp (°C)",
        backgroundColor = DT::styleEqual(1, "orange")
      )
  })

  # Download CSV button for profile data table (uses profiles wide): ---------------
  output$download_profile_csv <- downloadHandler(
    filename = function() {
      if (input$plot_tabs == "Individual profiles") {
        paste0(reactive_objects$sel_mlid, "_", input$date_select, ".csv")
      } else {
        paste0(reactive_objects$sel_mlid, "_all_dates.csv")
      }
    },
    content = function(file) {
      if (input$plot_tabs == "Individual profiles") {
        dat <- reactive_objects$table_data
      } else {
        dat <- reactive_objects$sel_profs_wide[, c(
          "SiteID",
          "Date",
          "Depth",
          "DO (mg/L)",
          "pH",
          "Temp (°C)"
        )]
      }
      write.csv(dat, file, row.names = FALSE)
    }
  )

  # Site profiles (all dates) plotting - uses profiles wide (OLD, for slider):----
  # observe({
  #   req(reactive_objects$sel_mlid, input$date_slider)
  #
  #   reactive_objects$sel_profs_wide = profiles_wideAR[profiles_wideAR$SiteID == reactive_objects$sel_mlid &
  #                                                       profiles_wideAR$Date >= input$date_slider[1] &
  #                                                       profiles_wideAR$Date <= input$date_slider[2]
  #                                                     ,]
  # })

  # Site profiles (all dates) plotting - uses profiles wide (NEW): ---------------------
  observe({
    req(reactive_objects$sel_mlid, input$start_date, input$end_date)

    reactive_objects$sel_profs_wide = profiles_wideAR[
      profiles_wideAR$SiteID == reactive_objects$sel_mlid &
        profiles_wideAR$Date >= input$start_date &
        profiles_wideAR$Date <= input$end_date,
    ]
  })

  output$site_prof_plot = renderPlot({
    req(reactive_objects$sel_profs_wide)
    site_data_wide <- reactive_objects$sel_profs_wide

    # plotting function with args
    site_plottingAR(site_data_wide)
    #box()
  })

  # Download plots button for all dates plots -------------------------------------
  output$download_site_plot <- downloadHandler(
    filename = function() {
      paste0(reactive_objects$sel_mlid, "_all_dates_plot.png")
    },
    content = function(file) {
      req(reactive_objects$sel_profs_wide)
      site_data_wide <- reactive_objects$sel_profs_wide
      png(file, width = 1000, height = 800) # Adjust size as needed
      site_plottingAR(site_data_wide)
      dev.off()
    }
  )

  ## Data notice at bottom of app ----------------------------------------------------
  output$data_notice <- renderUI({
    req(profiles_wideAR)
    latest_date <- max(profiles_wideAR$Date, na.rm = TRUE)
    p(
      paste0(
        "Data available through ",
        latest_date,
        ". Data are typically uploaded on a quarterly basis."
      ),
      style = "text-align: center; font-size: 80%; color: #666666; margin-bottom: 2px;"
    )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
