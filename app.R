library(shiny)
library(lubridate)
library(shinyjs)
library(plotly)
library(readr)
library(htmlwidgets)
library(dplyr)

get_burstiness <- function(vector){
  if (!is.numeric(vector) || !all(na.omit(unique(vector)) %in% c(0, 1))) {
    return(NA)
  }
  
  onsets <- which(vector == 1)
  if (length(onsets) < 2) {
    return(NA)  
  }
  
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


options(shiny.error = NULL)

ui <- fluidPage(
  useShinyjs(), 
  titlePanel("Testing Interactive Plots"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("var_select_ui"),
      checkboxInput("show_descriptives", "Display descriptive statistics", FALSE),
      uiOutput("plot_type_ui"),
      checkboxInput("use_id", "My data has multiple participants", FALSE),
      conditionalPanel(
        condition = "input.use_id == true",
        uiOutput("idvar_select_ui"),
        uiOutput("id_select_ui"),
        checkboxInput("use_1_id", "View mutliple participants at once", F),
        # conditionalPanel(
        #   condition = "input.use_1_id == true",
        #   numericInput("num_participants", "Number of Participants to Display", value = 2, min = 1, step = 1)
        # )
      ),
      tags$script(HTML("
    $(document).on('keydown', function(e) {
      if (e.key === 'ArrowRight') {
        $('#goButton').click();
      } else if (e.key === 'ArrowLeft') {
        $('#prevButton').click();
      }
    });
  ")),
      checkboxInput("show_plot2", "Show another plot", FALSE),
      conditionalPanel(
        condition = "input.show_plot2 == true",
        #uiOutput("plot_type_ui"),
        uiOutput("plot_type2_ui")
      ),
      br(),
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    mainPanel(
      plotlyOutput("interactive_plot"),
      #br(),
      textOutput("current_participant"),
      #br(),
      conditionalPanel(
        condition = "input.show_descriptives == true",
        htmlOutput("descriptives")
      ),
      textOutput("burstiness"),
      conditionalPanel(
        condition = "input.show_plot2 == true",
        plotlyOutput("interactive_plot2"),
        textOutput("current_participant2"),
        textOutput("burstiness2"),
        #br(),
        textOutput("mean_value2")
      )
    )
  )
)

server <- function(input, output, session) {
  plotly_obj <- reactiveVal(NULL)
  current_index <- reactiveVal(1)
  plotly_obj2 <- reactiveVal(NULL)

    data_reactive <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  # UI for selecting variables once data is uploaded
  output$var_select_ui <- renderUI({
    df <- data_reactive()
    numeric_vars <- names(df)
    #[sapply(df, is.numeric)]
    vars = names(df)
    tagList(
      selectInput("xvar", "X-axis variable", choices = c("Please select"="", numeric_vars), selected = ""),
      selectInput("yvar", "Y-axis variable", choices = c("Please select"="", numeric_vars), selected = "")
    )
  })
  output$idvar_select_ui <- renderUI({
    df <- data_reactive()    
    vars = names(df)
    selectInput("idvar", "Participant ID variable", choices = c("Please select"="", vars), selected = "")
  })
  
  output$goButton_ui <- renderUI({
    req(input$use_id)
    actionButton("goButton", "Next")
  })
  
  output$prevButton_ui <- renderUI({
    req(input$use_id)
    actionButton("prevButton", "Previous")
  })
  
  output$id_select_ui <- renderUI({
    req(input$idvar)
    df       <- data_reactive()
    all_ids  <- unique(df[[ input$idvar ]])
    
    # if you're in multi‐participant mode, allow multiple selections
    tagList(
    selectInput(
      "id",
      "Select participant(s) to display",
      choices  = all_ids,
      selected = all_ids[current_index()],
      multiple = isTRUE(input$use_1_id)  # TRUE when multi, FALSE when single
    ),
    fluidRow(
      uiOutput('prevButton_ui', style = 'display: inline-block; margin-left: 15px;'),
      uiOutput('goButton_ui', style = 'display: inline-block;')
    )
    )
  })
  output$plot_type_ui <- renderUI({
    selectInput("plot_type", "Select Plot Type", choices = c("Scatter", "Line", "Bar Code"))
  })
  output$plot_type2_ui <- renderUI({
    selectInput("plot_type2", "Select Plot Type", choices = c("Scatter", "Line"))
  })
  visible_data <- reactiveVal(NULL)
  visible_data2 <- reactiveVal(NULL)
  
  
  # observeEvent(input$id, {
  #   req(input$idvar, input$id, input$use_id)
  #   df <- data_reactive()
  #   unique_ids <- unique(df[[input$idvar]])
  #   idx <- match(input$id, unique_ids)
  #   if (!is.na(idx)) current_index(idx)
  # })
  observeEvent(input$goButton, {
    req(input$use_id, input$idvar)
    df       <- data_reactive()
    all_ids  <- unique(df[[ input$idvar ]])
    
    step     <- if (isTRUE(input$use_1_id)) input$num_participants else 1
    new_idx  <- (current_index() - 1 + step) %% length(all_ids) + 1
    if (new_idx > length(all_ids)) new_idx <- 1
    current_index(new_idx)
    
    # now compute the IDs in your display window
    if (isTRUE(input$use_1_id)) {
      start_i <- new_idx
      end_i   <- min(start_i + step - 1, length(all_ids))
      sel_ids <- all_ids[start_i:end_i]
    } else {
      sel_ids <- all_ids[new_idx]
    }
    
    updateSelectInput(
      session,
      "id",
      selected = sel_ids
    )
  })
  observeEvent(input$prevButton, {
    req(input$use_id, input$idvar)
    df       <- data_reactive()
    all_ids  <- unique(df[[input$idvar]])
    
    step     <- if (isTRUE(input$use_1_id)) input$num_participants else 1
    new_idx  <- (current_index() - 1 - step) %% length(all_ids) + 1
    if (new_idx > length(all_ids)) new_idx <- 1
    current_index(new_idx)
    
    # now compute the IDs in your display window
    if (isTRUE(input$use_1_id)) {
      start_i <- new_idx
      end_i   <- min(start_i + step - 1, length(all_ids))
      sel_ids <- all_ids[start_i:end_i]
    } else {
      sel_ids <- all_ids[new_idx]
    }
    
    updateSelectInput(
      session,
      "id",
      selected = sel_ids
    )
  })
  
  
  output$interactive_plot <- renderPlotly({
    req(input$xvar, input$yvar, input$plot_type)
    df <- data_reactive()
    
    # If user selected multiple participants mode
    if (isTRUE(input$use_id) && nzchar(input$idvar)) {
      if (isTRUE(input$use_1_id)) {
        # multi: filter by *all* selected IDs
        df <- df[df[[input$idvar]] %in% input$id, ]
      } else {
        # single: filter by exactly one
        df <- df[df[[input$idvar]] == input$id, ]
      }
    }
    
    if (input$plot_type == "Scatter" | input$plot_type == "Line"){
      plt <- plot_ly(
        df,
        x = ~get(input$xvar),
        y = ~get(input$yvar),
        source="myplot",
        color = if (input$use_id) ~factor(df[[input$idvar]]),
        colors = if (input$use_id) "Dark2",
        type = 'scatter',
        mode = ifelse(input$plot_type == "Scatter", "markers", "lines")
      ) %>% layout(
        title = paste(input$plot_type, "Plot"), 
        xaxis = list(title = input$xvar), 
        yaxis = list(title = input$yvar)
      )} else {
        time_numeric <- as.numeric(df[[input$xvar]])
        tick_interval <- 120
        tickvals <- seq(min(time_numeric), max(time_numeric), by = tick_interval)
        ticktext <- format(seq(min(df[[input$xvar]]), max(df[[input$xvar]]), by = "2 min"), "%H:%M:%S")
        
        plt<-    plot_ly(df,
          x = time_numeric, 
          y = rep(1, length(time_numeric)),
          z = df[[input$yvar]], 
          source = "myplot",
          type = "heatmap"
        )%>%
          layout(
            xaxis = list(
              title = "Time",
              tickvals = tickvals,
              ticktext = ticktext,
              rangeslider = list(visible = TRUE,
                                 tickvals = tickvals,  
                                 ticktext = ticktext) 
              # rangeslider = list(visible = TRUE),
              #range = c(min(time_numeric), min(time_numeric) + 600) 
            ),
            yaxis = list(title = "", showticklabels = FALSE)
          )
        
        #plt = ggplotly(p)
      }
    #event_register("plotly_relayout", source = "myplot")
    plt<-event_register(plt, "plotly_relayout")
    
    plotly_obj(plt)
    visible_data(df)
    plt
  })
  
  output$interactive_plot2 <- renderPlotly({
    req(input$xvar, input$yvar, input$plot_type2)
    
    df <- data_reactive()
    
    if (input$use_id && !is.null(input$idvar)) {
      unique_ids <- unique(df[[input$idvar]])
      sel_id <- unique_ids[current_index()]
      df <- df[df[[input$idvar]] == sel_id, ]
    }
    
    plt2 <- plot_ly(
      df,
      x = ~get(input$xvar),
      y = ~get(input$yvar),
      source="myplot2",
      color = if (input$use_id) ~factor(df[[input$idvar]]),
      colors = if (input$use_id) "Dark2",
      type = 'scatter',
      mode = ifelse(input$plot_type2 == "Scatter", "markers", "lines")
    ) %>% layout(title = paste(input$plot_type2, "Plot 2"), xaxis =list(title=input$xvar), yaxis =list(title=input$yvar))
    plt2<-event_register(plt2, "plotly_relayout")
    #event_register("plotly_relayout", source = "myplot2")
    plotly_obj2(plt2)
    visible_data2(df)
    plt2
  })
  
  observe({
    df <- data_reactive()
    req(input$xvar, input$yvar)
    
    # Helper function to get axis range
    get_range <- function(event_data, axis = "x") {
      if (is.null(event_data)) return(NULL)
      
      direct_range <- event_data[[paste0(axis, "axis.range")]]
      if (!is.null(direct_range) && length(direct_range) == 2) {
        return(as.numeric(direct_range))
      }
      
      range_start <- event_data[[paste0(axis, "axis.range[0]")]] %||%
        event_data[[paste0(axis, "axis.rangeslider.range[0]")]]
      range_end <- event_data[[paste0(axis, "axis.range[1]")]] %||%
        event_data[[paste0(axis, "axis.rangeslider.range[1]")]]
      
      if (!is.null(range_start) && !is.null(range_end)) {
        return(as.numeric(c(range_start, range_end)))
      }
      
      return(NULL)
    }
    
    # ---- Plot 1 ----
    relayout1 <- event_data("plotly_relayout", source = "myplot")
    x_range1 <- get_range(relayout1, "x")
    y_range1 <- get_range(relayout1, "y")
    
    filtered1 <- df
    if (!is.null(x_range1)) {
      xvar_data <- filtered1[[input$xvar]]
      filtered1 <- filtered1[xvar_data >= x_range1[1] & xvar_data <= x_range1[2], ]
    }
    if (!is.null(y_range1)) {
      yvar_data <- filtered1[[input$yvar]]
      filtered1 <- filtered1[yvar_data >= y_range1[1] & yvar_data <= y_range1[2], ]
    }
    visible_data(filtered1)
    
    # ---- Plot 2 ----
    relayout2 <- event_data("plotly_relayout", source = "myplot2")
    x_range2 <- get_range(relayout2, "x")
    y_range2 <- get_range(relayout2, "y")
    
    filtered2 <- df
    if (!is.null(x_range2)) {
      xvar_data <- filtered2[[input$xvar]]
      filtered2 <- filtered2[xvar_data >= x_range2[1] & xvar_data <= x_range2[2], ]
    }
    if (!is.null(y_range2)) {
      yvar_data <- filtered2[[input$yvar]]
      filtered2 <- filtered2[yvar_data >= y_range2[1] & yvar_data <= y_range2[2], ]
    }
    visible_data2(filtered2)
  })
  
  output$current_participant <- renderText({
    if (isTRUE(input$use_id) && nzchar(input$idvar)) {
      if (isTRUE(input$use_1_id)) {
        paste0("Currently viewing participants: ", paste(input$id, collapse = ", "))
      } else {
        paste0("Currently viewing participant: ", input$id)
      }
    } else {
      "Currently viewing all data (no participant selection)"
    }
  })
  output$burstiness <- renderText({
    df_visible <- visible_data()
    req(input$yvar, input$xvar)
    req(nrow(df_visible) > 0)
    if (lubridate::is.timepoint(df_visible[[input$xvar]]) == TRUE){
    time_numeric = as.numeric(df_visible[[input$xvar]])
    bursty <- get_burstiness(df_visible[[input$yvar]])
    bursty_start = as.POSIXct(min(time_numeric)+60*60*4, origin = "1970-01-01 UTC")
    bursty_end = as.POSIXct(max(time_numeric)+60*60*4, origin = "1970-01-01 UTC")
    paste0("Burstiness of displayed data (from ",format(bursty_start, "%m/%d/%Y  %H:%M:%S"), " to ", format(bursty_end, "%m/%d/%Y %H:%M:%S"),"): ", round(bursty, 2))
    }else{
        bursty <- get_burstiness(df_visible[[input$yvar]])
        paste0("Burstiness of displayed data (from ",round(min(df_visible[[input$xvar]]),2), " to ", round(max(df_visible[[input$xvar]]),2),"): ", round(bursty, 2))
    }
  })
  
  # output$burstiness <- renderText({
  #   df_visible <- visible_data()
  #   req(input$yvar, input$xvar)
  #   req(nrow(df_visible) > 0)
  #   bursty <- get_burstiness(df_visible[[input$yvar]])
  #   paste0("Burstiness of displayed data (from ",round(min(df_visible[[input$xvar]]),2), " to ", round(max(df_visible[[input$xvar]]),2),"): ", round(bursty, 2))
  # })
  
  
  output$descriptives <- renderUI({
    df <- visible_data()
    req(input$yvar, input$xvar)
    
    if (input$use_1_id) {
      # figure out which IDs to summarize
      # unique_ids <- unique(df[[input$idvar]])
      # start_index <- current_index()
      # end_index   <- min(start_index + input$num_participants - 1, length(unique_ids))
      #selected_ids <- unique_ids[start_index:end_index]
      selected_ids <- input$id
      
      # build a summary df
      summary_df <- df %>%
        filter(.data[[input$idvar]] %in% selected_ids) %>%
        group_by(pid = .data[[input$idvar]]) %>%
        summarise(
          mean   = mean(.data[[input$yvar]], na.rm = TRUE),
          sd     = sd(.data[[input$yvar]],   na.rm = TRUE),
          n      = n(),
          .groups = "drop"
        )
      
      html_lines <- summary_df %>%
        mutate(
          line = paste0(
            "Participant ", pid, ": ",
            "<B>M</B> = ", round(mean, 2), 
            ", <B>SD</B> = ", round(sd, 2),
            " (n=", n, ")"
          )
        ) %>%
        pull(line) %>%
        paste(collapse = "<br/>")
      
      HTML(paste0(
        "From ", input$xvar, " ", 
        round(min(df[[input$xvar]]), 2), " to ", 
        round(max(df[[input$xvar]]), 2), ":<br/>"))
      HTML(html_lines)
      
    } else {
      df_vis <- visible_data()
      req(nrow(df_vis) > 0)
      mean_val   <- mean(df_vis[[input$yvar]], na.rm = TRUE)
      sd_val     <- sd(df_vis[[input$yvar]],   na.rm = TRUE)
      median_val <- median(df_vis[[input$yvar]], na.rm = TRUE)
      
      HTML(paste0(
        "From ", input$xvar, " ", 
        round(min(df_vis[[input$xvar]]), 2), " to ", 
        round(max(df_vis[[input$xvar]]), 2), ":<br/>",
        "<B>M</B> = ", round(mean_val, 2), 
        ", <B>Mdn</B> = ", round(median_val, 2),
        ", <B>SD</B> = ", round(sd_val, 2)
      ))
    }
  })
  
  output$current_participant2 <- renderText({
    req(input$use_id, input$show_plot2)
    df <- visible_data2()
    unique_ids <- unique(df[[input$id]])
    paste("Currently viewing participant:", unique_ids[current_index()])
  })
  output$burstiness2 <- renderText({
    df_visible <- visible_data2()
    req(df_visible, input$yvar, input$xvar)
    bursty <- get_burstiness(df_visible[[input$yvar]])
    paste0("Burstiness of displayed data (from ",round(min(df_visible[[input$xvar]]),2), " to ", round(max(df_visible[[input$xvar]]),2),"): ", round(bursty, 2))
  })
  
  output$mean_value2 <- renderText({
    df_visible <- visible_data2()
    req(df_visible, input$yvar, input$xvar)
    mean_val <- mean(df_visible[[input$yvar]], na.rm = TRUE)
    paste0("Mean value of displayed data (from ",round(min(df_visible[[input$xvar]]),2), " to ", round(max(df_visible[[input$xvar]]),2),"): ", round(mean_val, 2))
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("interactive_plot_", Sys.Date(), ".html")
    },
    content = function(file) {
      saveWidget(plotly_obj(), file)
    }
  )
  
}

shinyApp(ui, server)

