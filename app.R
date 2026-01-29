
library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(lubridate)
library(htmlwidgets)
library(shinyjs)
library(tidyr)
library(RColorBrewer)


detect_dataset <- function(df){
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  
  binary_vars <- numeric_vars[sapply(df[numeric_vars], function(x)
    all(na.omit(unique(x)) %in% c(0, 1))
  )]
  
  time_vars <- names(df)[
    sapply(seq_along(df), function(i) {
      inherits(df[[i]], c("POSIXct", "POSIXt", "Date", "POSIXlt", "hms", "difftime")) ||
        (is.numeric(df[[i]]) &&
           grepl("time|sec|sample|min|hour", tolower(names(df)[i]))) ||
        (is.character(df[[i]]) && 
           any(grepl("\\d{2}:\\d{2}", df[[i]][1:min(10, nrow(df))]), na.rm = TRUE))
    })
  ]
  
  list(
    numeric = numeric_vars,
    binary  = binary_vars,
    time    = time_vars,
    n_rows  = nrow(df),
    n_cols  = ncol(df)
  )
}

extract_event_windows_idx <- function(event){
  r <- rle(event)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  idx <- which(r$values == 1)
  data.frame(start = starts[idx], end = ends[idx])
}

expand_timeseries <- function(data, id_var, var_name, start_time_var, end_time_var, time_unit) {
  
  start_col <- data[[start_time_var]]
  end_col <- data[[end_time_var]]
  
  # Parse Start Time
  if (is.character(start_col)) {
    start_col <- parse_date_time(start_col, orders = c("ymd HMS", "ymd HM", "HMS", "HM", "ymd"), quiet = TRUE)
  }
  # Parse End Time
  if (is.character(end_col)) {
    end_col <- parse_date_time(end_col, orders = c("ymd HMS", "ymd HM", "HMS", "HM", "ymd"), quiet = TRUE)
  }
  
  # Update data frame
  data[[start_time_var]] <- start_col
  data[[end_time_var]] <- end_col
  
  # Renaming
  working_df <- data %>%
    rename(
      internal_id = all_of(id_var),
      internal_activity = all_of(var_name),
      internal_start = all_of(start_time_var),
      internal_end = all_of(end_time_var)
    ) %>%
    filter(!is.na(internal_start), !is.na(internal_end))
  
  if(nrow(working_df) == 0) stop("No valid rows found after date parsing.")
  
  # Expand 
  expanded <- working_df %>%
    rowwise() %>%
    filter(internal_end >= internal_start) %>%
    mutate(time_seq = list(seq(from = internal_start, to = internal_end, by = time_unit))) %>%
    unnest(time_seq) %>%
    select(internal_id, time_seq, internal_activity) %>%
    ungroup()
  
  # Handle Overlaps 
  # Aggregate duplicates (max wins for binary/categorical)
  unique_time_df <- expanded %>%
    group_by(internal_id, time_seq) %>%
    summarise(internal_activity = max(internal_activity, na.rm = TRUE), .groups = "drop")
  
  # Fill with 0s
  fill_val <- if(is.numeric(unique_time_df$internal_activity)) 0 else "0"
  
  final_df <- unique_time_df %>%
    group_by(internal_id) %>%
    complete(
      time_seq = seq(from = min(time_seq), to = max(time_seq), by = time_unit),
      fill = list(internal_activity = fill_val)
    ) %>%
    ungroup()
  
  # Renaming
  final_df <- final_df %>%
    rename(
      !!id_var := internal_id,
      !!var_name := internal_activity,
      time = time_seq
    )
  
  return(final_df)
}

get_burstiness <- function(vector){
  clean_vec <- na.omit(vector)
  if (!is.numeric(clean_vec) || !all(unique(clean_vec) %in% c(0, 1))) {
    return(NA)
  }
  onsets <- which(clean_vec == 1)
  if (length(onsets) < 2) return(NA)  
  
  IOIs <- diff(onsets)    
  l <- length(IOIs)
  m <- mean(IOIs)
  s <- sd(IOIs)
  if (m == 0) return(NA)
  
  r <- s / m
  sqrt.np <- sqrt(l + 1)
  sqrt.nn <- sqrt(l - 1)
  
  burstiness <- ((sqrt.np * r) - sqrt.nn) / (((sqrt.np - 2) * r) + sqrt.nn)
  return(burstiness)
}

validate_id_variable <- function(df, col_name) {
  # Robust check for empty/null selections or length 0 vectors
  if (is.null(col_name) || length(col_name) == 0 || is.na(col_name) || col_name == "") {
    return("No ID variable selected.")
  }
  
  # Check if column exists
  if (!col_name %in% names(df)) {
    return("Selected ID variable not found in dataset.")
  }
  
  vals <- na.omit(df[[col_name]])
  n_unique <- length(unique(vals))
  n_rows <- nrow(df)
  
  # 1. Check if it's a constant (only 1 ID for the whole file)
  if (n_unique <= 1) {
    return(paste0("Error: The variable '", col_name, "' has only one unique value. Please select a variable that distinguishes between participants."))
  }
  
  # 2. Check if it's a row index (unique value for every single row)
  # However, we must allow for the case where n_rows is small (e.g. 2 rows, 2 participants)
  if (n_unique == n_rows && n_rows > 10) {
    return(paste0("Error: The variable '", col_name, "' has a unique value for every row. This looks like a row index, not a Participant ID."))
  }
  
  return(NULL)
}
# UI

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive Time-Series Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      
      # DATA OPTIONS 
      conditionalPanel(
        condition = "input.sidebar_state == 'data'",
        
        h4("Step 1: Upload Data"),
        fileInput("file", "Upload CSV", accept = ".csv"),
        
        conditionalPanel(
          condition = "output.hasData",
          # hr(),
          # h4("Dataset Diagnostics"),
          # verbatimTextOutput("diagnostics"),
          
          hr(),
          h4("Data Format Conversion"),
          checkboxInput("is_interval_data", "Data has start time and end time or duration columns", FALSE),          
          conditionalPanel(
            condition = "input.is_interval_data",
            uiOutput("interval_conversion_ui"),
            actionButton("convert_data", "Convert to Continuous Format", class = "btn-success"),
            br(), br(),
            textOutput("conversion_status")
          )
        ),
        
        conditionalPanel(
          condition = "output.hasData",
          hr(),
          h4("Step 2: Describe Dataset"),
          
          selectInput(
            "data_structure",
            "Primary data type",
            c(
              "Continuous time series",
              "Binary / event-coded",
              "Mixed (continuous + events)"
            )
          ),
          
          checkboxInput("use_id", "Multiple participants", FALSE),
          
          conditionalPanel(
            condition = "input.use_id",
            uiOutput("idvar_ui")
          ),
          
          br(),
          actionButton("go_viz", "Go to Visualizations", class = "btn-primary")
        )
      ),
      
      # VISUALIZATIONS
      conditionalPanel(
        condition = "input.sidebar_state == 'viz'",
        
        h4("Step 3: Visualization"),
        
        selectInput(
          "viz_mode",
          "Visualization type",
          c(
            "Raw time series",
            "Event + Continuous Overlay",
            "Event-locked average",
            "Event-locked single event",
            "Event durations (barcode)"
          )
        ),
        
        # Participant controls 
        conditionalPanel(
          condition = "input.use_id == true",
          hr(),
          h4("Participants"),
          
          checkboxInput("step_through", "Step through participants", FALSE),
          
          conditionalPanel(
            condition = "input.step_through == false && input.viz_mode != 'Event-locked single event'",
            uiOutput("id_select_ui")
          ),
          
          conditionalPanel(
            condition = "input.step_through == true",
            fluidRow(
              column(6, actionButton("prev_id", "Previous")),
              column(6, actionButton("next_id", "Next"))
            ),
            textOutput("current_participant")
          )
        ),
        
        # Event-level controls
        conditionalPanel(
          condition = "input.viz_mode == 'Event-locked single event'",
          hr(),
          fluidRow(
            column(6, actionButton("prev_event", "Previous Event")),
            column(6, actionButton("next_event", "Next Event"))
          ),
          textOutput("current_event"),
          textOutput("event_onset_time")
        ),
        
        hr(),
        
        conditionalPanel(
          condition = "input.viz_mode == 'Raw time series'",
          uiOutput("var_ui"),
          selectInput("plot_type", "Plot type", c("Line", "Scatter"))
        ),
        
        conditionalPanel(
          condition = "input.viz_mode == 'Event + Continuous Overlay'",
          uiOutput("overlay_ui")
        ),
        
        conditionalPanel(
          condition = "input.viz_mode == 'Event durations (barcode)'",
          uiOutput("barcode_ui")
        ),
        
        conditionalPanel(
          condition = "input.viz_mode == 'Event-locked average' || input.viz_mode == 'Event-locked single event'",
          uiOutput("event_ui"),
          numericInput("pre", "Seconds before event", 5, min = 1),
          numericInput("post", "Seconds after event", 5, min = 1)
        ),
        
        conditionalPanel(
          condition = "input.viz_mode == 'Event-locked average'",
          checkboxInput(
            "overlay_events",
            "Overlay individual events",
            FALSE
          )
        ),
        hr(),
        
        # Custom labels section
        h4("Custom Labels"),
        textInput("custom_title", "Plot Title", placeholder = "Auto-generated"),
        textInput("custom_xlab", "X-Axis Label", placeholder = "Variable name"),
        textInput("custom_ylab", "Y-Axis Label", placeholder = "Variable name"),
        
        conditionalPanel(
          condition = "input.viz_mode == 'Raw time series' || input.viz_mode == 'Event + Continuous Overlay'",
          textInput("custom_legend", "Legend Title", placeholder = "Auto-generated")
        ),
        
        hr(),
        actionButton("back_data", "Back to Data Options")
      ),
      
      hidden(textInput("sidebar_state", "", value = "data"))
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "550px"),
      uiOutput("stats_section")
    )
  )
)

# SERVER 

server <- function(input, output, session){
  
  options(shiny.maxRequestSize = 100*1024^2)
  
  # Sidebar navigation 
  observeEvent(input$go_viz,{
    
    # Check ID before moving to visualizations 
    if (input$use_id) {
      err <- validate_id_variable(data_reactive(), input$idvar)
      if (!is.null(err)) {
        showNotification(err, type = "error", duration = 10)
        return() # Stop execution
      }
    }
    
    updateTextInput(session, "sidebar_state", value = "viz")
    
    # Determine which visualizations are allowed based on data structure
    allowed_choices <- switch(input$data_structure,
                              "Continuous time series" = c("Raw time series"),
                              "Binary / event-coded"   = c("Event durations (barcode)"),
                              "Mixed (continuous + events)" = c(
                                "Raw time series",
                                "Event + Continuous Overlay",
                                "Event-locked average",
                                "Event-locked single event",
                                "Event durations (barcode)"
                              )
    )
    
    updateSelectInput(session, "viz_mode",
                      choices = allowed_choices,
                      selected = allowed_choices[1])
  })
  
  observeEvent(input$back_data,{
    updateTextInput(session,"sidebar_state",value="data")
  })
  
  output$hasData <- reactive({ !is.null(input$file) })
  outputOptions(output,"hasData", suspendWhenHidden = FALSE)
  
  # Store both original and converted data
  data_original <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    # Auto-detect and parse datetime columns
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        if (any(grepl("\\d{4}-\\d{2}-\\d{2}", df[[col]][1:min(10, nrow(df))]), na.rm = TRUE) ||
            any(grepl("\\d{2}:\\d{2}:\\d{2}", df[[col]][1:min(10, nrow(df))]), na.rm = TRUE)) {
          
          parsed <- suppressWarnings(
            parse_date_time(df[[col]], 
                            orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM", 
                                       "mdy HMS", "mdy HM", "HMS", "HM", 
                                       "ymd", "dmy", "mdy"),
                            quiet = TRUE)
          )
          if (sum(!is.na(parsed)) > 0.5 * length(parsed)) {
            df[[col]] <- parsed
          }
        }
      }
    }
    df
  })
  
  data_converted <- reactiveVal(NULL)
  conversion_done <- reactiveVal(FALSE)
  
  data_reactive <- reactive({
    if (conversion_done() && !is.null(data_converted())) {
      data_converted()
    } else {
      data_original()
    }
  })
  
  observeEvent(input$file, {
    data_converted(NULL)
    conversion_done(FALSE)
    updateCheckboxInput(session, "is_interval_data", value = FALSE)
  })
  
  diagnostics <- reactive({
    detect_dataset(data_reactive())
  })
  
  output$diagnostics <- renderPrint({
    d <- diagnostics()
    cat(
      "Rows:", d$n_rows, "\n",
      "Columns:", d$n_cols, "\n\n",
      "Numeric variables:\n", paste(d$numeric, collapse=", "), "\n\n",
      "Binary/event variables:\n", paste(d$binary, collapse=", "), "\n\n",
      "Candidate time variables:\n", paste(d$time, collapse=", ")
    )
  })
  
  # Interval data conversion UI
  output$interval_conversion_ui <- renderUI({
    df <- data_original()
    all_vars <- names(df)
    
    tagList(
      radioButtons("interval_format", "Input Data Format:",
                   choices = c("Start Time + End Time" = "start_end", 
                               "Start Time + Duration" = "start_dur"),
                   inline = TRUE),
      
      selectInput("start_time_col", "Start time column", all_vars),
      
      conditionalPanel(
        condition = "input.interval_format == 'start_end'",
        selectInput("end_time_col", "End time column", all_vars)
      ),
      
      conditionalPanel(
        condition = "input.interval_format == 'start_dur'",
        fluidRow(
          column(8, selectInput("duration_col", "Duration column", all_vars)),
          column(4, selectInput("duration_unit_input", "Unit", 
                                choices = c("Seconds" = 1, "Minutes" = 60, "Hours" = 3600),
                                selected = 1))
        )
      ),
      
      hr(),
      # Independent Participant control for this step
      checkboxInput("conv_has_id", "Dataset contains multiple participants", FALSE),
      
      conditionalPanel(
        condition = "input.conv_has_id",
        selectInput("conv_id_col", "Participant ID Variable", all_vars)
      ),
      
      selectInput("event_var_col", "Event/Activity variable", all_vars),
      numericInput("time_unit_val", "Output Time Step (resolution in seconds)", 1, min = 0.001, step = 0.1)
    )
  })
  
  output$conversion_status <- renderText({
    if (conversion_done()) {
      paste("✓ Data converted successfully! New dataset has", nrow(data_converted()), "rows.")
    } else {
      ""
    }
  })
  
  # Convert interval data
  observeEvent(input$convert_data, {
    req(input$start_time_col, input$event_var_col, input$time_unit_val)
    
    if (input$interval_format == "start_end") req(input$end_time_col)
    if (input$interval_format == "start_dur") req(input$duration_col)
    
    # VALIDATION: Check ID variable if selected
    # Performed OUTSIDE tryCatch to ensure it stops execution visibly
    if (input$conv_has_id) {
      req(input$conv_id_col)
      df <- data_original()
      err <- validate_id_variable(df, input$conv_id_col)
      if (!is.null(err)) {
        showNotification(err, type = "error", duration = 10)
        return() # Stop conversion immediately
      }
    }
    
    tryCatch({
      df <- data_original()
      
      # PRE-PROCESSING FOR DURATION FORMAT
      target_end_col <- input$end_time_col 
      
      if (input$interval_format == "start_dur") {
        s_time <- df[[input$start_time_col]]
        if (is.character(s_time)) {
          s_time <- parse_date_time(s_time, orders = c("ymd HMS", "ymd HM", "HMS", "HM", "ymd"), quiet = TRUE)
        }
        
        dur_val <- suppressWarnings(as.numeric(df[[input$duration_col]]))
        multiplier <- as.numeric(input$duration_unit_input)
        df$calculated_end_time <- s_time + (dur_val * multiplier)
        target_end_col <- "calculated_end_time"
        df[[input$start_time_col]] <- s_time
      }
      
      # STANDARD EXPANSION
      df_to_process <- df
      
      # Use the ID selection from THIS step (Step 1)
      if (input$conv_has_id && !is.null(input$conv_id_col)) {
        chosen_id <- input$conv_id_col
        # Auto-update Step 2 UI settings
        updateCheckboxInput(session, "use_id", value = TRUE)
      } else {
        df_to_process$temp_id <- 1
        chosen_id <- "temp_id"
        updateCheckboxInput(session, "use_id", value = FALSE)
      }
      
      converted <- expand_timeseries(
        data = df_to_process,
        id_var = chosen_id,
        var_name = input$event_var_col,
        start_time_var = input$start_time_col,
        end_time_var = target_end_col, 
        time_unit = input$time_unit_val
      )
      
      data_converted(converted)
      conversion_done(TRUE)
      
    }, error = function(e) {
      # UPDATED ERROR MESSAGE HERE
      showNotification(
        paste0("Conversion Failed: ", e$message, "\n Please check your variable selections and try again."),
        type = "error", 
        duration = 15
      )
    })
  })
  
  # Participant handling
  output$idvar_ui <- renderUI({
    df <- data_reactive()
    # FIX: Use isTRUE() to safely handle cases where the conversion UI (input$conv_has_id) is NULL/hidden
    sel <- if(isTRUE(input$conv_has_id) && !is.null(input$conv_id_col)) input$conv_id_col else NULL
    
    selectInput("idvar", "Participant ID variable", names(df), selected = sel)
  })
  
  all_ids <- reactive({
    req(input$idvar)
    unique(data_reactive()[[input$idvar]])
  })
  
  id_index <- reactiveVal(1)
  event_index <- reactiveVal(1)
  
  # Persistent variable selections
  selected_time <- reactiveVal(NULL)
  selected_signal <- reactiveVal(NULL)
  selected_event <- reactiveVal(NULL)
  
  observeEvent(input$viz_mode, {
    if (input$viz_mode == "Event-locked single event") {
      updateCheckboxInput(session, "step_through", value = TRUE)
    }
  })
  
  observeEvent(input$next_id,{
    id_index(ifelse(id_index() == length(all_ids()), 1, id_index() + 1))
    event_index(1)
  })
  observeEvent(input$prev_id,{
    id_index(ifelse(id_index() == 1, length(all_ids()), id_index() - 1))
    event_index(1)
  })
  
  observeEvent(input$next_event,{
    event_index(event_index() + 1)
  })
  observeEvent(input$prev_event,{
    event_index(max(1, event_index() - 1))
  })
  
  output$current_participant <- renderText({
    paste("Participant:", all_ids()[id_index()])
  })
  
  output$current_event <- renderText({
    paste("Event:", event_index())
  })
  
  output$event_onset_time <- renderText({
    req(input$viz_mode == "Event-locked single event")
    df <- data_reactive()
    
    if (isTRUE(input$use_id)) {
      df <- df[df[[input$idvar]] == all_ids()[id_index()], ]
    }
    
    req(input$event_var)
    windows <- extract_event_windows_idx(df[[input$event_var]])
    
    if (nrow(windows) > 0 && event_index() <= nrow(windows)) {
      d <- diagnostics()
      if (length(d$time) > 0) {
        time_var <- d$time[1]
        onset_time <- df[[time_var]][windows$start[event_index()]]
        
        if (inherits(onset_time, c("POSIXct", "POSIXt", "POSIXlt"))) {
          paste("Event onset time:", format(onset_time, "%Y-%m-%d %H:%M:%S"))
        } else if (is.numeric(onset_time)) {
          paste("Event onset time:", round(onset_time, 2), "seconds")
        } else {
          paste("Event onset time:", as.character(onset_time))
        }
      } else {
        paste("Event onset index:", windows$start[event_index()])
      }
    }
  })
  
  output$id_select_ui <- renderUI({
    selectInput(
      "selected_ids",
      "Select participant(s)",
      choices = all_ids(),
      selected = all_ids(),
      multiple = TRUE
    )
  })
  
  # Variable selection
  output$var_ui <- renderUI({
    d <- diagnostics()
    tagList(
      selectInput("xvar", "Time (x) variable", d$time, selected = selected_time()),
      selectInput("yvar", "Signal (y) variable", d$numeric, selected = selected_signal(), multiple = T)
    )
  })
  
  output$event_ui <- renderUI({
    d <- diagnostics()
    tagList(
      selectInput("event_var", "Event variable (0/1)", d$binary, selected = selected_event()),
      selectInput("signal_var", "Signal variable", d$numeric, selected = selected_signal())
    )
  })
  
  output$overlay_ui <- renderUI({
    d <- diagnostics()
    df <- data_reactive()
    
    # Identify categorical/event-like variables
    all_vars <- names(df)
    cat_vars <- all_vars[sapply(df, function(x) {
      is.factor(x) || is.character(x) || 
        (is.numeric(x) && length(unique(na.omit(x))) <= 20)
    })]
    
    tagList(
      selectInput("time_overlay", "Time variable", d$time, selected = selected_time()),
      selectInput("signal_overlay", "Continuous signal(s)", d$numeric, selected = selected_signal(), multiple = TRUE),
      
      # CHANGE: Added multiple = TRUE so you can pick "Freezing" AND "Tremor"
      selectInput("event_overlay", "Event variable(s)", cat_vars, 
                  selected = selected_event(), multiple = TRUE)
    )
  })
  output$barcode_ui <- renderUI({
    d <- diagnostics()
    df <- data_reactive()
    
    all_vars <- names(df)
    cat_vars <- all_vars[sapply(df, function(x) {
      is.factor(x) || is.character(x) || 
        (is.numeric(x) && length(unique(na.omit(x))) <= 20)
    })]
    
    tagList(
      selectInput("barcode_time", "Time variable", d$time, selected = selected_time()),
      selectInput("barcode_var", "Categorical/Event variable", cat_vars,
                  selected = if(!is.null(selected_event()) && selected_event() %in% cat_vars) 
                    selected_event() else NULL)
    )
  })
  
  observeEvent(input$xvar, { selected_time(input$xvar) }, ignoreNULL = TRUE)
  observeEvent(input$time_overlay, { selected_time(input$time_overlay) }, ignoreNULL = TRUE)
  observeEvent(input$barcode_time, { selected_time(input$barcode_time) }, ignoreNULL = TRUE)
  observeEvent(input$yvar, { selected_signal(input$yvar) }, ignoreNULL = TRUE)
  observeEvent(input$signal_var, { selected_signal(input$signal_var) }, ignoreNULL = TRUE)
  observeEvent(input$signal_overlay, { selected_signal(input$signal_overlay) }, ignoreNULL = TRUE)
  observeEvent(input$event_var, { selected_event(input$event_var) }, ignoreNULL = TRUE)
  observeEvent(input$event_overlay, { selected_event(input$event_overlay) }, ignoreNULL = TRUE)
  observeEvent(input$barcode_var, { selected_event(input$barcode_var) }, ignoreNULL = TRUE)
  
  # Plot
  output$plot <- renderPlotly({
    
    req(input$viz_mode)
    df <- data_reactive()
    
    # 1. Standardize Participant Filtering
    if (isTRUE(input$use_id)) {
      req(input$idvar)
      if (isTRUE(input$step_through)) {
        req(length(all_ids()) > 0)
        df <- df[df[[input$idvar]] == all_ids()[id_index()], ]
      } else {
        req(input$selected_ids)
        df <- df[df[[input$idvar]] %in% input$selected_ids, ]
      }
    }
    
    # 2. Define Helper for Custom Labels
    # This function grabs the user input or falls back to the default
    get_labels <- function(default_title, default_x, default_y, default_legend) {
      list(
        title = if(isTruthy(input$custom_title)) input$custom_title else default_title,
        x = if(isTruthy(input$custom_xlab)) input$custom_xlab else default_x,
        y = if(isTruthy(input$custom_ylab)) input$custom_ylab else default_y,
        legend = if(isTruthy(input$custom_legend)) input$custom_legend else default_legend
      )
    }
    
    # Raw time series
    if (input$viz_mode == "Raw time series") {
      req(input$xvar, input$yvar)
      is_single_view <- !isTRUE(input$use_id) || isTRUE(input$step_through)
      
      # Get Labels
      labs <- get_labels(
        default_title = "Raw Time Series",
        default_x = input$xvar,
        default_y = "Value",
        default_legend = if(is_single_view) "Variable" else "Participant"
      )
      
      p <- plot_ly()
      
      if (is_single_view) {
        for (var in input$yvar) {
          p <- add_trace(p, x = df[[input$xvar]], y = df[[var]], name = var,
                         type = "scatter", mode = ifelse(input$plot_type == "Line", "lines", "markers"))
        }
      } else {
        for (var in input$yvar) {
          p <- add_trace(p, x = df[[input$xvar]], y = df[[var]], 
                         name = paste(df[[input$idvar]], "-", var),
                         type = "scatter", mode = ifelse(input$plot_type == "Line", "lines", "markers"))
        }
      }
      
      return(p %>% layout(
        title = labs$title,
        xaxis = list(title = labs$x),
        yaxis = list(title = labs$y),
        legend = list(title = list(text = labs$legend))
      ))
    }
    
    # Event + Continuous Overlay
    if (input$viz_mode == "Event + Continuous Overlay") {
      req(input$time_overlay, input$signal_overlay, input$event_overlay)
      
      time_vec <- df[[input$time_overlay]]
      
      # Calculate y-axis range for the rectangles
      all_vals <- unlist(lapply(input$signal_overlay, function(v) df[[v]]))
      y_min <- min(all_vals, na.rm = TRUE)
      y_max <- max(all_vals, na.rm = TRUE)
      
      # We build a list of "targets" to plot. 
      # Each target has: Column Name, Value to match, Color, and Label.
      
      plot_targets <- list()
      
      # Helper for colors
      get_palette <- function(n) {
        if(n <= 8) RColorBrewer::brewer.pal(max(3, n), "Set2")[1:n]
        else colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)
      }
      
      # 1. Analyze selected columns to determine total colors needed
      total_items <- 0
      for(col in input$event_overlay){
        vals <- na.omit(unique(df[[col]]))
        # If binary (0/1), we count it as 1 item (the "1" state)
        if(all(vals %in% c(0,1))) {
          total_items <- total_items + 1
        } else {
          # If categorical, we count the unique values (excluding 0)
          vals <- vals[vals != "0" & vals != 0]
          total_items <- total_items + length(vals)
        }
      }
      
      # 2. Generate Palette
      master_pal <- get_palette(total_items)
      color_idx <- 1
      
      # 3. Build Targets
      for(col in input$event_overlay){
        raw_vals <- df[[col]]
        unique_vals <- unique(raw_vals[!is.na(raw_vals)])
        
        # Check if strictly binary 0/1
        is_binary <- all(unique_vals %in% c(0,1))
        
        if(is_binary) {
          # CASE: Binary Column (e.g. "Freezing") -> One color
          plot_targets[[length(plot_targets)+1]] <- list(
            col = col,
            val = 1,
            color = master_pal[color_idx],
            label = col # Label is just the column name
          )
          color_idx <- color_idx + 1
        } else {
          # CASE: Categorical Column (e.g. "Activity") -> Multiple colors
          unique_vals <- sort(unique_vals)
          unique_vals <- unique_vals[unique_vals != 0 & unique_vals != "0"]
          
          for(uv in unique_vals){
            plot_targets[[length(plot_targets)+1]] <- list(
              col = col,
              val = uv,
              color = master_pal[color_idx],
              label = paste(col, "-", uv) # Label is "Col - Value"
            )
            color_idx <- color_idx + 1
          }
        }
      }
      
      hex_to_rgba <- function(hex, alpha = 0.2) {
        rgb_vals <- col2rgb(hex)
        paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
      }
      
      shapes <- list()
      legend_traces <- list() # To store info for dummy legend entries
      
      for(target in plot_targets) {
        # Extract binary vector for this specific target
        vec <- df[[target$col]]
        is_active <- vec == target$val
        # Handle NAs as false
        is_active[is.na(is_active)] <- FALSE
        is_active <- as.numeric(is_active)
        
        windows <- extract_event_windows_idx(is_active)
        
        if (nrow(windows) > 0) {
          rgba_col <- hex_to_rgba(target$color, alpha = 0.2)
          
          # Create rectangles
          for (i in seq_len(nrow(windows))) {
            shapes[[length(shapes) + 1]] <- list(
              type = "rect", 
              x0 = time_vec[windows$start[i]], 
              x1 = time_vec[windows$end[i]],
              y0 = y_min, 
              y1 = y_max, 
              fillcolor = rgba_col, 
              line = list(width = 0),
              layer = "below"
            )
          }
          
          # Add to legend list (so we only add one legend entry per event type)
          legend_traces[[length(legend_traces)+1]] <- target
        }
      }
      
      # Get Labels
      labs <- get_labels(
        default_title = paste("Event Overlay"),
        default_x = input$time_overlay,
        default_y = "Value",
        default_legend = "Legend"
      )
      
      p <- plot_ly()
      
      # 1. Add Continuous Lines
      for (var in input$signal_overlay) {
        p <- add_trace(p, x = time_vec, y = df[[var]], name = var, type = "scatter", mode = "lines")
      }
      
      # 2. Add Dummy Legend Entries
      for (tr in legend_traces) {
        p <- add_trace(p, 
                       x = time_vec[1], 
                       y = y_min, 
                       type = "scatter", 
                       mode = "markers", 
                       marker = list(color = tr$color, symbol = "square"),
                       name = tr$label,
                       visible = "legendonly" 
        )
      }
      
      return(p %>% layout(
        title = labs$title,
        xaxis = list(title = labs$x),
        yaxis = list(title = labs$y),
        legend = list(title = list(text = labs$legend)),
        shapes = shapes
      ))
    }
    
    # Event-locked average 
    if (input$viz_mode == "Event-locked average") {
      req(input$event_var, input$signal_var)
      windows <- extract_event_windows_idx(df[[input$event_var]])
      req(nrow(windows) > 0)
      
      win <- (-input$pre):input$post
      extract_event <- function(i){
        idx <- windows$start[i] + win
        idx <- idx[idx > 0 & idx <= nrow(df)]
        df[[input$signal_var]][idx]
      }
      mat <- do.call(rbind, lapply(seq_len(nrow(windows)), extract_event))
      avg <- colMeans(mat, na.rm = TRUE)
      
      # Get Labels
      labs <- get_labels(
        default_title = paste("Average Trajectory:", input$event_var),
        default_x = "Time relative to event",
        default_y = input$signal_var,
        default_legend = ""
      )
      
      p <- plot_ly(x = win, y = avg, type = "scatter", mode = "lines", name = "Mean", line = list(width = 3, color = "blue"))
      
      if (input$overlay_events){
        for (i in seq_len(nrow(mat))){
          p <- add_lines(p, x = win, y = mat[i,], opacity = 0.3, line = list(color = "gray"), showlegend = FALSE)
        }
      }
      
      return(p %>% layout(
        title = labs$title,
        xaxis = list(title = labs$x),
        yaxis = list(title = labs$y)
      ))
    }
    
    # event durations (barcode)
    if (input$viz_mode == "Event durations (barcode)") {
      req(input$barcode_time, input$barcode_var)
      df_clean <- df[!is.na(df[[input$barcode_time]]) & !is.na(df[[input$barcode_var]]), ]
      req(nrow(df_clean) > 0)
      
      time_vec <- df_clean[[input$barcode_time]]
      event_vec <- df_clean[[input$barcode_var]]
      unique_vals <- unique(sort(event_vec))
      
      # Palette Logic
      n_colors <- length(unique_vals)
      if(n_colors <= 8) {
        pal <- RColorBrewer::brewer.pal(max(3, n_colors), "Set2")
      } else {
        pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
      }
      pal <- pal[1:n_colors]
      names(pal) <- as.character(unique_vals)
      if ("0" %in% names(pal)) pal[["0"]] <- "#FFFFFF"
      
      plot_shapes <- list()
      for (val in unique_vals) {
        is_active <- event_vec == val
        windows <- extract_event_windows_idx(as.numeric(is_active))
        if (nrow(windows) > 0) {
          for (i in seq_len(nrow(windows))) {
            plot_shapes[[length(plot_shapes) + 1]] <- list(
              type = "rect", x0 = time_vec[windows$start[i]], x1 = time_vec[windows$end[i]],
              y0 = 0, y1 = 1, fillcolor = pal[[as.character(val)]], line = list(width = 0), layer = "below"
            )
          }
        }
      }
      
      # Get Labels
      labs <- get_labels(
        default_title = paste("Event Barcode:", input$barcode_var),
        default_x = input$barcode_time,
        default_y = "",
        default_legend = input$barcode_var
      )
      
      p <- plot_ly()
      for (val in unique_vals) {
        p <- add_trace(p, x = head(time_vec, 1), y = 0, type = "bar", name = as.character(val),
                       marker = list(color = pal[[as.character(val)]]), showlegend = TRUE, hoverinfo = "name")
      }
      p <- add_segments(p, x = min(time_vec), xend = max(time_vec), y = 0.5, yend = 0.5,
                        line = list(color = "transparent"), showlegend = FALSE, hoverinfo = "none")
      
      return(p %>% layout(
        title = labs$title,
        shapes = plot_shapes,
        xaxis = list(title = labs$x),
        yaxis = list(title = labs$y, range = c(0, 1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(title = list(text = labs$legend)),
        barmode = "stack"
      ))
    }
  })
  
  # Dynamic stats section container
  output$stats_section <- renderUI({
    req(input$viz_mode)
    should_show <- FALSE
    
    if (input$viz_mode == "Raw time series") {
      if (isTruthy(input$yvar)) should_show <- TRUE
    } else if (input$viz_mode == "Event durations (barcode)") {
      if (isTruthy(input$barcode_var)) should_show <- TRUE
    } else if (input$viz_mode == "Event + Continuous Overlay") {
      if (isTruthy(input$signal_overlay) && isTruthy(input$event_overlay)) should_show <- TRUE
    } else if (grepl("Event-locked", input$viz_mode)) {
      if (isTruthy(input$signal_var)) should_show <- TRUE
    }
    
    if (should_show) {
      tagList(hr(), h4("Descriptive Statistics"), verbatimTextOutput("desc_stats"))
    } else {
      NULL
    }
  })
  
  # Descriptive statistics
  # Descriptive statistics
  output$desc_stats <- renderPrint({
    req(input$viz_mode)
    df <- data_reactive()
    ids_to_process <- list()
    
    # Determine which IDs to process
    if (isTRUE(input$use_id)) {
      req(input$idvar)
      if (isTRUE(input$step_through)) {
        ids_to_process <- all_ids()[id_index()]
      } else {
        ids_to_process <- input$selected_ids
      }
    } else {
      ids_to_process <- "All Data"
      df$temp_id <- "All Data"
    }
    
    id_col <- if(isTRUE(input$use_id)) input$idvar else "temp_id"
    
    # Initialize variables to hold selection names
    cont_vars <- NULL
    event_vars <- NULL
    calc_type <- NULL 
    
    # 1. Determine Calculation Type based on Viz Mode
    if (input$viz_mode == "Event + Continuous Overlay") {
      req(input$signal_overlay, input$event_overlay)
      cont_vars <- input$signal_overlay
      event_vars <- input$event_overlay # Now a vector of potential multiple events
      calc_type <- "both"
    } else if (input$viz_mode == "Raw time series") {
      req(input$yvar)
      cont_vars <- input$yvar
      calc_type <- "continuous"
    } else if (input$viz_mode == "Event durations (barcode)") {
      req(input$barcode_var)
      event_vars <- input$barcode_var
      calc_type <- "event"
    } else if (grepl("Event-locked", input$viz_mode)) {
      req(input$signal_var)
      cont_vars <- input$signal_var
      calc_type <- "continuous"
    }
    
    if (is.null(calc_type) || length(ids_to_process) == 0) {
      cat("No statistics available for this view.")
      return()
    }
    
    # 2. Print General Header
    cat(paste(rep("=", 60), collapse = ""), "\n")
    if (calc_type == "both") {
      cat("  Comparison: Continuous Signals vs Event Variables\n")
    } else if (calc_type == "continuous") {
      cat("  Continuous Signal Statistics\n")
    } else if (calc_type == "event") {
      cat("  Event Statistics (Burstiness)\n")
    }
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    
    # 3. Loop through Participants
    for (id in ids_to_process) {
      sub_df <- df[df[[id_col]] == id, ]
      
      cat(paste("Participant:", id, "\n"))
      cat(paste(rep("-", 40), collapse = ""), "\n")
      
      if (calc_type == "both") {
        
        # NESTED LOOPS: Loop through every selected Event AND every selected Signal
        for(e_var in event_vars) {
          
          # Calculate Event Stats for this specific event variable
          e_vals <- sub_df[[e_var]]
          
          # Check if strictly binary (0/1) or categorical
          unique_e <- unique(na.omit(e_vals))
          is_binary <- all(unique_e %in% c(0, 1))
          
          b_str <- "N/A"
          note_str <- ""
          
          if(is_binary) {
            b <- get_burstiness(e_vals)
            b_str <- if(is.na(b)) "NA" else sprintf("%.4f", b)
            note_str <- ""
          } else {
            # Categorical logic: count unique types
            clean_vals <- e_vals[e_vals != 0 & e_vals != "0"]
            n_types <- length(unique(na.omit(clean_vals)))
            b_str <- "N/A"
            note_str <- paste("(Categorical:", n_types, "types)")
          }
          
          # Now pair this event with every selected continuous variable
          for(c_var in cont_vars) {
            c_vals <- sub_df[[c_var]]
            m <- mean(c_vals, na.rm = TRUE)
            s <- sd(c_vals, na.rm = TRUE)
            
            # Print Side-by-Side block
            cat(sprintf("Signal: %-25s Event: %-20s\n", c_var, e_var))
            cat(sprintf("  Mean: %-24.4f  Burstiness: %s\n", m, b_str))
            cat(sprintf("  SD:   %-24.4f %s\n", s, note_str))
            cat("\n")
          }
        }
        
      } else if (calc_type == "continuous") {
        
        for (var_name in cont_vars) {
          vals <- sub_df[[var_name]]
          m <- mean(vals, na.rm = TRUE)
          s <- sd(vals, na.rm = TRUE)
          
          cat(paste("  Variable:", var_name, "\n"))
          cat(sprintf("    Mean: %.4f,  SD: %.4f\n", m, s))
        } 
        cat("\n")
        
      } else if (calc_type == "event") {
        # Loop through event vars (usually just 1 for barcode, but safe to loop)
        for(e_var in event_vars){
          vals <- sub_df[[e_var]]
          b <- get_burstiness(vals)
          
          cat(paste("  Variable:", e_var, "\n"))
          
          if (is.na(b)) {
            clean_vals <- na.omit(vals)
            if (!all(unique(clean_vals) %in% c(0, 1))) {
              cat("    Burstiness: NA (Categorical/Multiclass)\n")
            } else if (sum(clean_vals == 1) < 2) {
              cat("    Burstiness: NA (<2 events detected)\n")
            } else {
              cat("    Burstiness: NA\n")
            }
          } else {
            cat(sprintf("    Burstiness: %.4f\n", b))
          }
        }
        cat("\n")
      }
    }
  })
}

shinyApp(ui, server)