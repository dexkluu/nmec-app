# Load necessary libraries
library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(plotly)
library(openxlsx)
library(shinyWidgets)
library(rlang)
library(shinyjs)
library(nmecr)
source("helper.R")

# Define the UI of the application
ui = navbarPage("My App",
                 tabPanel("Table Display",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file", "Choose CSV or Excel File",
                                        accept = c(".csv", ".xlsx")),  # File input for CSV or Excel files
                              checkboxInput("header", "Header", TRUE),  # Checkbox to include header in file
                              uiOutput("datetime_column"),  # UI output for selecting datetime column
                              actionButton("clean_datetime", "Clean SkySpark Datetime"),  # Button to clean datetime format
                              actionButton("add_holidays", "Add Working Holidays"), # Button to add work holiday indicator column
                              numericInput("decimals", "Decimal Places", value = 2, min = 0),  # Input for decimal places
                              actionButton("round_numeric", "Round Numeric Columns"),  # Button to round numeric columns
                              dateRangeInput("drop_date_range", "Drop Observations Between Dates"),  # Input for date range to drop observations
                              actionButton("drop_date_range_btn", "Drop Date Range"),  # Button to drop date range
                              uiOutput("rename_column"),  # UI output for renaming column
                              textInput("new_column_name", "New Column Name"),  # Input for new column name
                              actionButton("rename_column_btn", "Rename Column"),  # Button to rename column
                              actionButton("rollback_1", "Rollback to Last Change"),  # Button to rollback to last change
                              actionButton("rollback_2", "Rollback to Second Last Change"),  # Button to rollback to second last change
                              actionButton("rollback_3", "Rollback to Third Last Change"),  # Button to rollback to third last change
                              dateInput("navigate_date", "Navigate to Date"),  # Input to navigate to a specific date
                              actionButton("go_to_date", "Go to Date")  # Button to go to the specified date
                            ),
                            mainPanel(
                              dataTableOutput("table")  # Output for displaying the data table
                            )
                          )
                 ),
                 tabPanel("Plotting",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("x_axis_column"),  # UI output for selecting X-axis column
                              uiOutput("y_axis_columns"),  # UI output for selecting Y-axis columns
                              uiOutput("datetime_aggregation"),  # UI output for selecting datetime aggregation
                              actionButton("plot_graph", "Plot Graph"),  # Button to plot the graph
                              numericInput("y_axis_min", "Primary Y-Axis Min", value = NULL),  # Input for primary Y-axis minimum value
                              numericInput("y_axis_max", "Primary Y-Axis Max", value = NULL),  # Input for primary Y-axis maximum value
                              numericInput("y2_axis_min", "Secondary Y-Axis Min", value = NULL),  # Input for secondary Y-axis minimum value
                              numericInput("y2_axis_max", "Secondary Y-Axis Max", value = NULL)  # Input for secondary Y-axis maximum value
                            ),
                            mainPanel(
                              plotlyOutput("plot")  # Output for displaying the plot
                            )
                          )
                 ),
                 tabPanel("Modeling",
                          useShinyjs(),  # Initialize shinyjs
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("dr_analysis", "Is this a demand response analysis?", value = FALSE),
                              uiOutput("date_selector"),
                              uiOutput("dr_date_plotter_ui"),
                              selectInput("regression_type", "Regression Type", choices = c("TOWT", "SLR", "Three Parameter Cooling", "Three Parameter Heating", "Four Parameter Linear Model", "Five Parameter Linear Model"), multiple = TRUE),
                              uiOutput("y_var_ui"),  # UI output for y_var selection
                              uiOutput("temp_var_ui"),  # UI output for temp_var selection
                              uiOutput("time_var_ui"),  # UI output for time_var selection
                              uiOutput("additional_vars_ui"),  # UI output for additional variables multiselect
                              uiOutput("additional_var_agg_ui"),  # UI output for additional variable aggregation
                              textInput("baseline_start", "Baseline Start", value = "01/01/2024 00:00"),
                              textInput("baseline_end", "Baseline End", value = "12/31/2024 23:59"),
                              textInput("performance_start", "Performance Start", value = "01/01/2025 00:00"),
                              textInput("performance_end", "Performance End", value = "12/31/2025 23:59"),
                              selectInput("convert_to_data_interval", "Convert to Data Interval", choices = c("Hourly", "Daily")),
                              actionButton("toggle_advanced", "Show Advanced Options"),  # Button to toggle advanced options
                              div(id = "advanced_options", style = "display: none;",  # Initially hidden div for advanced options
                                  numericInput("timescale_days", "Timescale Days", value = NULL),
                                  checkboxInput("has_temp_knots_defined", "Has Temp Knots Defined", FALSE),
                                  checkboxInput("equal_temp_segment_points", "Equal Temp Segment Points", TRUE),
                                  numericInput("temp_segments_numeric", "Temp Segments Numeric", value = 6),
                                  textInput("temp_knots_value", "Temp Knots Value", value = "40, 55, 65, 80, 90"),
                                  textInput("initial_breakpoints", "Initial Breakpoints", value = "50, 65"),
                                  numericInput("occupancy_threshold", "Occupancy Threshold", value = 0.65),
                                  checkboxInput("day_normalized", "Day Normalized", FALSE)
                              ),
                              actionButton("generate_inputs", "Generate Inputs"),  # Button to generate modeling inputs
                              actionButton("run_model", "Run Model")  # Button to run the model
                            ),
                            mainPanel(
                              verbatimTextOutput("modeling_inputs_output"),  # Output for displaying the modeling inputs
                              dataTableOutput("model_stats_table"),  # Output for displaying the model stats table
                              downloadButton("download_data", "Download Data as Excel"),  # Button to download data as Excel
                              plotlyOutput("model_plot"),  # Output for displaying the plot
                              uiOutput("totals")
                            )
                          )
                 )
)
# Define the server logic
server = function(input, output, session) {
  data = reactiveVal(NULL)  # Reactive value to store data
  history = reactiveValues(states = list())  # Reactive values to store history of data states
  
  # Function to add state to history
  add_to_history = function(state) {
    if (length(history$states) >= 3) {
      history$states = history$states[-1]  # Keep only the last 3 states
    }
    history$states = c(history$states, list(state))
  }
  
  # Function to rollback to a specific state
  rollback_to_state = function(index) {
    if (index <= length(history$states)) {
      data(history$states[[length(history$states) - index + 1]])
      history$states = history$states[1:(length(history$states) - index)]
      update_table(data())  # Update the table with the rolled back state
    }
  }
  
  # Observe the file input and load data
  observeEvent(input$file, {
    ext = tools::file_ext(input$file$name)
    df = switch(ext,
                 csv = read_csv(input$file$datapath, col_names = input$header),
                 xlsx = read_excel(input$file$datapath, col_names = input$header),
                 stop("Invalid file type")
    )
    data(df)
    history$states = list(df)  # Initialize history with the loaded data
    update_table(df)  # Update the table with the loaded data
  })
  
  # UI output for datetime column selection
  output$datetime_column = renderUI({
    req(data())
    selectInput("datetime_col", "Select Datetime Column", choices = names(data()))
  })
  
  # UI output for column renaming
  output$rename_column = renderUI({
    req(data())
    selectInput("column_to_rename", "Select Column to Rename", choices = names(data()))
  })
  
  # UI output for X-axis column selection
  output$x_axis_column = renderUI({
    req(data())
    selectInput("x_axis", "Select X-Axis Column", choices = names(data()))
  })
  
  # UI output for Y-axis columns selection
  output$y_axis_columns = renderUI({
    req(data())
    tagList(
      selectInput("primary_y_axes", "Select Primary Y-Axis Columns", choices = names(data()), multiple = TRUE),
      selectInput("secondary_y_axes", "Select Secondary Y-Axis Columns", choices = names(data()), multiple = TRUE),
      uiOutput("plot_type_primary"),
      uiOutput("plot_type_secondary")
    )
  })
  
  # UI output for plot type selection for primary Y-axis columns
  output$plot_type_primary = renderUI({
    req(input$primary_y_axes)
    lapply(input$primary_y_axes, function(col) {
      selectInput(paste0("plot_type_", col, "_primary"), paste0("Plot Type for ", col, " (Primary)"), choices = c("line", "scatter"))
    })
  })
  
  # UI output for plot type selection for secondary Y-axis columns
  output$plot_type_secondary = renderUI({
    req(input$secondary_y_axes)
    lapply(input$secondary_y_axes, function(col) {
      selectInput(paste0("plot_type_", col, "_secondary"), paste0("Plot Type for ", col, " (Secondary)"), choices = c("line", "scatter"))
    })
  })
  
  # UI output for datetime aggregation selection
  output$datetime_aggregation = renderUI({
    req(data())
    tagList(
      selectInput("aggregation_type", "Select Aggregation Type", choices = c("Original", "Hourly", "Daily")),
      selectInput("aggregation_func", "Select Aggregation Function", choices = c("mean", "sum", "max", "median"))
    )
  })
  # Observe the clean datetime button and clean the datetime column
  observeEvent(input$clean_datetime, {
    req(input$datetime_col)
    df = data()
    add_to_history(df)
    df[[input$datetime_col]] = as.POSIXct(df[[input$datetime_col]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")  # Convert to POSIXct
    data(df)
    update_table(df)  # Update the table with cleaned datetime
  })
  
  # Observe the add holidays button and add holiday indicator to data
  observeEvent(input$add_holidays, {
    req(input$datetime_col)
    df = data()
    holidays = readRDS("data/holidays.Rds")
    holidays$holiday_ind = 1
    df = add_holidays(df, holidays, input$datetime_col)
    df$holiday_ind[is.na(df$holiday_ind)] = 0
    data(df)
    update_table(df)
  })
  
  # Observe the drop date range button and drop observations within the date range
  observeEvent(input$drop_date_range_btn, {
    req(data(), input$datetime_col, input$drop_date_range)
    df = data()
    add_to_history(df)
    df = df %>% filter(!(as.Date(df[[input$datetime_col]]) >= input$drop_date_range[1] & as.Date(df[[input$datetime_col]]) <= input$drop_date_range[2]))
    data(df)
    update_table(df)  # Update the table with dropped date range
  })
  
  # Observe the round numeric button and round numeric columns
  observeEvent(input$round_numeric, {
    req(data())
    df = data()
    add_to_history(df)
    num_cols = sapply(df, is.numeric)
    df[num_cols] = lapply(df[num_cols], round, digits = input$decimals)
    data(df)
    update_table(df)  # Update the table with rounded numeric columns
  })
  
  # Observe the rename column button and rename the selected column
  observeEvent(input$rename_column_btn, {
    req(input$column_to_rename, input$new_column_name)
    df = data()
    add_to_history(df)
    colnames(df)[colnames(df) == input$column_to_rename] = input$new_column_name
    data(df)
    update_table(df)  # Update the table with renamed column
  })
  
  # Observe rollback buttons and rollback to the specified state
  observeEvent(input$rollback_1, {
    rollback_to_state(1)
  })
  observeEvent(input$rollback_2, {
    rollback_to_state(2)
  })
  observeEvent(input$rollback_3, {
    rollback_to_state(3)
  })
  
  update_table = function(df) {
    # Get column data types and append to column names
    data_types = sapply(df, class)
    colnames(df) = paste0(colnames(df), " (", data_types, ")")
    
    output$table = renderDataTable({
      datatable(df, options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px"
      ))
    })
  }
  
  observeEvent(input$go_to_date, {
    req(input$navigate_date, input$datetime_col)
    df = data()
    dt_col = input$datetime_col
    if (dt_col %in% colnames(df)) {
      target_row = which(format(df[[dt_col]], "%Y-%m-%d") == format(input$navigate_date, "%Y-%m-%d"))
      if (length(target_row) > 0) {
        page_length = 10
        target_page = (target_row[1] - 1) %/% page_length + 1
        proxy = dataTableProxy('table')
        proxy %>% selectPage(target_page)
      }
    }
  })
  
  ####################################### Plotting Page ########################
  
  observeEvent(input$plot_graph, {
    req(input$x_axis, input$datetime_col, input$aggregation_type, input$aggregation_func)
    
    df_orig = data()
    x_col = input$x_axis
    agg_x_col = if (is.numeric(df_orig[[input$x_axis]])) input$x_axis else NULL
    datetime_col = input$datetime_col
    aggregation_type = input$aggregation_type
    aggregation_func = input$aggregation_func
    
    # Aggregate data based on user selection
    if (aggregation_type != "Original") {
      df = df_orig %>%
        mutate(temp_datetime = case_when(
          aggregation_type == "Hourly" ~ floor_date(!!sym(datetime_col), "hour"),
          aggregation_type == "Daily" ~ floor_date(!!sym(datetime_col), "day")
        )) %>%
        group_by(temp_datetime) %>%
        summarise(across(all_of(c(input$primary_y_axes, input$secondary_y_axes, agg_x_col)), match.fun(aggregation_func), na.rm = TRUE)) %>%
        ungroup() %>%
        rename(!!datetime_col := temp_datetime)
    } else {
      df = df_orig
    }
    
    plot = plot_ly(df, x = ~get(x_col))
    
    # Add traces for primary y-axis columns
    lapply(input$primary_y_axes, function(col) {
      plot_type = input[[paste0("plot_type_", col, "_primary")]]
      if (plot_type == "line") {
        plot <<- plot %>% add_lines(y = ~get(col), name = col, yaxis = "y1")
      } else {
        plot <<- plot %>% add_markers(y = ~get(col), name = col, yaxis = "y1")
      }
    })
    
    # Add traces for secondary y-axis columns
    lapply(input$secondary_y_axes, function(col) {
      plot_type = input[[paste0("plot_type_", col, "_secondary")]]
      if (plot_type == "line") {
        plot <<- plot %>% add_lines(y = ~get(col), name = col, yaxis = "y2")
      } else {
        plot <<- plot %>% add_markers(y = ~get(col), name = col, yaxis = "y2")
      }
    })
    
    plot = plot %>%
      layout(
        xaxis = list(title = input$x_axis),
        yaxis = list(title = "Primary Y-Axis", range = c(input$y_axis_min, input$y_axis_max)),
        yaxis2 = list(title = "Secondary Y-Axis", overlaying = "y", side = "right", range = c(input$y2_axis_min, input$y2_axis_max))
      )
    
    output$plot = renderPlotly({
      plot
    })
  })
  
  # Download data as Excel file
  output$download_data = downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(data(), file)
    }
  )
  
  ####################################### Modeling Page ########################
  # Advanced options button
  observeEvent(input$toggle_advanced, {
    toggle("advanced_options")
    if (input$toggle_advanced %% 2 == 1) {
      updateActionButton(session, "toggle_advanced", label = "Hide Advanced Options")
    } else {
      updateActionButton(session, "toggle_advanced", label = "Show Advanced Options")
    }
  })
  
  # Define data that can change
  data = reactiveVal(NULL)  # Reactive value to store data
  model_output = reactiveVal(NULL)  # Reactive value to store model output
  model_stats = reactiveVal(NULL)  # Reactive value to store model stats
  
  # UI for dr date selector
  output$date_selector = renderUI({
    if (input$dr_analysis) {
      airDatepickerInput("dr_dates", "Select DR Dates", multiple = TRUE)
    }
  })
  
  observeEvent(input$dr_dates, {
    dr_dates = as.character(input$dr_dates)
    output$dr_date_plotter_ui = renderUI({
      selectInput("dr_date_plotter", "Date to Plot", choices = c("Any", dr_dates))
    })
  })
  
  ##### Need to deal with how data is flowing from first page to this page

  # Update the y_var, temp_var, time_var, and additional_vars UI with column names from the uploaded dataframe
  output$y_var_ui = renderUI({
    req(data())
    selectInput("y_var", "Y Variable", choices = names(data()))
  })
  
  output$temp_var_ui = renderUI({
    req(data())
    selectInput("temp_var", "Temp Variable", choices = names(data()))
  })
  
  output$time_var_ui = renderUI({
    req(data())
    selectInput("time_var", "Time Variable", choices = names(data()))
  })
  
  output$additional_vars_ui = renderUI({
    req(data())
    selectInput("additional_vars", "Additional Variables", choices = names(data()), multiple = TRUE)
  })

# Observe additional_vars input and update additional_var_agg UI
observeEvent(input$additional_vars, {
  output$additional_var_agg_ui = renderUI({
    req(input$additional_vars)
    lapply(seq_along(input$additional_vars), function(i) {
      selectInput(paste0("additional_var_agg_", i), paste0("Aggregation for ", input$additional_vars[i]), choices = c("mean", "median", "max", "min"))
    })
  })
})
  
  # Function to get modeling inputs
  get_modeling_inputs = reactive({
    timescale_days = if (is.null(input$timescale_days) || is.numeric(input$timescale_days)) {
      input$timescale_days
    } else {
      NULL
    }
    
    additional_vars = if (is.character(input$additional_vars)) {
      input$additional_vars
    } else {
      NULL
    }
    
    additional_var_agg = if (!is.null(additional_vars)) {
      sapply(seq_along(additional_vars), function(i) {
        match.fun(input[[paste0("additional_var_agg_", i)]])
      })
    } else {
      NULL
    }
    
    modeling_inputs(
      timescale_days = timescale_days,
      has_temp_knots_defined = input$has_temp_knots_defined,
      equal_temp_segment_points = input$equal_temp_segment_points,
      temp_segments_numeric = input$temp_segments_numeric,
      temp_knots_value = as.numeric(unlist(strsplit(input$temp_knots_value, ","))),
      initial_breakpoints = as.numeric(unlist(strsplit(input$initial_breakpoints, ","))),
      regression_type = input$regression_type,
      occupancy_threshold = input$occupancy_threshold,
      day_normalized = input$day_normalized,
      y_var = input$y_var,
      temp_var = input$temp_var,
      time_var = input$time_var,
      additional_vars = additional_vars,
      additional_var_agg = additional_var_agg,
      baseline_start = input$baseline_start,
      baseline_end = input$baseline_end,
      performance_start = input$performance_start,
      performance_end = input$performance_end,
      convert_to_data_interval = input$convert_to_data_interval,
      dr_analysis = input$dr_analysis,
      dr_dates = input$dr_dates
    )
  })
  
  # Observe the generate inputs button and display modeling inputs
  observeEvent(input$generate_inputs, {
    modeling_inputs = get_modeling_inputs()
    output$modeling_inputs_output = renderPrint({
      modeling_inputs
    })
  })
  
  # Observe the run model button and display model output
  observeEvent(input$run_model, {
    tryCatch({
      req(data())
      modeling_inputs = get_modeling_inputs()
      model_output(mv(data(), modeling_inputs))
      
      # Extract model stats and combine into a single dataframe
      stats = do.call(rbind, lapply(names(model_output()), function(name) {
        stats = model_output()[[name]]$baseline_stats
        stats = as.data.frame(lapply(stats, type.convert, as.is = TRUE))
        stats[] = lapply(stats, function(x) {
          if (is.numeric(x)) {
            ifelse(abs(x) < 0.01, format(x, scientific = TRUE, digits = 4), format(x, digits = 4))
          } else {
            x
          }
        })
        cbind(Model = name, stats)
      }))
      model_stats(stats)
      
      output$model_stats_table = renderDataTable({
        datatable(model_stats(), options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "200px"
        ))
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Observe row selection in the model stats table and create plot
  observeEvent(input$model_stats_table_rows_selected, {
    req(model_output())
    selected_model = model_stats()[input$model_stats_table_rows_selected, "Model"]
    performance_predictions = model_output()[[selected_model]]$performance_prediction
    performance_predictions$date = date(performance_predictions[[input$time_var]])
    performance_predictions$hour = hour(performance_predictions[[input$time_var]])
    performance_predictions = tibble(performance_predictions)
    y_var_sym = sym(input$y_var)
    df_react = reactive({
      if (input$dr_date_plotter=="Any"){
        subset(performance_predictions, date %in% input$dr_dates) %>% group_by(hour) %>%
          summarise(
            !!input$y_var := sum(!!y_var_sym, na.rm = TRUE),
            predictions = sum(predictions, na.rm = TRUE)
          )
        
      } else {
        subset(performance_predictions, date %in% as.Date(input$dr_date_plotter)) %>% group_by(hour) %>%
          summarise(
            !!input$y_var := sum(!!y_var_sym, na.rm = TRUE),
            predictions = sum(predictions, na.rm = TRUE)
          )
      }
    })
    
    if (input$dr_analysis){
      output$model_plot = renderPlotly({
        df_dr = df_react()
        min_val = floor(min(df_dr[[input$y_var]], df_dr$predictions))
        max_val = ceiling(max(df_dr[[input$y_var]], df_dr$predictions))
        range_val = max_val - min_val
        
        # Calculate tick interval using Plotly's default logic
        tick_interval = pretty(range_val, n = 10)[2] - pretty(range_val, n = 10)[1]
        tickvals = seq(min_val, max_val, by = tick_interval)
        
        plot_ly(df_dr, x = df_dr$hour) %>%
          add_lines(y = ~get(input$y_var), name = input$y_var, yaxis = "y1") %>%
          add_lines(y = ~predictions, name = "Predictions", yaxis = "y2") %>%
          layout(
            title = "Performance Period",
            xaxis = list(
              title = input$time_var
            ),
            yaxis = list(
              title = input$y_var,
              range = c(min_val, max_val),
              tickvals = tickvals
            ),
            yaxis2 = list(
              title = "Predictions",
              overlaying = "y",
              side = "right",
              range = c(min_val, max_val),
              tickvals = tickvals
            )
          )
      })
    } else{
      output$model_plot = renderPlotly({
        min_val = floor(min(performance_predictions[[input$y_var]], performance_predictions$predictions))
        max_val = ceiling(max(performance_predictions[[input$y_var]], performance_predictions$predictions))
        range_val = max_val - min_val
        
        # Calculate tick interval using Plotly's default logic
        tick_interval = pretty(range_val, n = 10)[2] - pretty(range_val, n = 10)[1]
        tickvals = seq(min_val, max_val, by = tick_interval)
        
        plot_ly(performance_predictions, x = ~get(input$time_var)) %>%
          add_lines(y = ~get(input$y_var), name = input$y_var, yaxis = "y1") %>%
          add_lines(y = ~predictions, name = "Predictions", yaxis = "y2") %>%
          layout(
            title = "Performance Period",
            xaxis = list(
              title = input$time_var
            ),
            yaxis = list(
              title = input$y_var,
              range = c(min_val, max_val),
              tickvals = tickvals
            ),
            yaxis2 = list(
              title = "Predictions",
              overlaying = "y",
              side = "right",
              range = c(min_val, max_val),
              tickvals = tickvals
            )
          )
      })
    }
    output$totals = renderUI({
      totals_df = if (input$dr_analysis) df_react() else performance_predictions
      totals_df$savings = totals_df$predictions - totals_df[[input$y_var]]
      total_actual_usage = round(sum(totals_df[[input$y_var]], na.rm = TRUE), 2)
      total_predicted_usage = round(sum(totals_df$predictions, na.rm = TRUE), 2)
      total_savings = round(sum(totals_df$savings, na.rm = TRUE), 2)
      
      tagList(
        h4("Total Actual Usage: ", total_actual_usage),
        h4("Total Predicted Usage: ", total_predicted_usage),
        h4("Total Savings: ", total_savings)
      )
    })
  })
  
  # Download data as Excel file
  output$download_data = downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Ensure the required inputs are available
      req(model_output())
      req(model_stats)
      selected_model = model_stats()[input$model_stats_table_rows_selected, "Model"]
      baseline_df_output = model_output()[[selected_model]]$baseline_mod$training_data
      baseline_mod_inputs = model_output()[[selected_model]]$baseline_mod$model_input_options
      
      performance_predictions = model_output()[[selected_model]]$performance_prediction
      performance_predictions$date = date(performance_predictions[[input$time_var]])
      performance_predictions$hour = hour(performance_predictions[[input$time_var]])
      performance_predictions = tibble(performance_predictions)
      performance_predictions$savings = performance_predictions$predictions - performance_predictions[[input$y_var]]
      
      # Create a new workbook
      wb = createWorkbook()
      
      # Add the first sheet with model stats
      addWorksheet(wb, "Model Stats")
      writeData(wb, "Model Stats", model_stats()[input$model_stats_table_rows_selected,])
      
      # Add sheet with input options and write model inputs
      addWorksheet(wb, "Model options")
      row = 1  # Start at row 1
      for (name in names(baseline_mod_inputs)) {
        item = baseline_mod_inputs[[name]]
        
        # Write the name of the list element
        writeData(wb, "Model options", name, startRow = row, startCol = 1)
        
        if (is.data.frame(item)) {
          # Write data frames starting in the next row
          writeData(wb, "Model options", item, startRow = row + 1, startCol = 2)
          row = row + nrow(item) + 2  # Move to next row (leave a blank row)
        } else if (is.vector(item) || is.list(item)) {
          # Convert vectors/lists to a column and write them
          writeData(wb, "Model options", t(data.frame(item)), startRow = row + 1, startCol = 2, colNames = FALSE)
          row = row + length(item) + 2
        } else {
          # Write single values (numbers, strings, etc.)
          writeData(wb, "Model options", as.character(item), startRow = row + 1, startCol = 2)
          row = row + 2
        }
      }
      
      # Add sheet with baseline data
      addWorksheet(wb, "Baseline Model Data")
      writeData(wb, "Baseline Model Data", baseline_df_output)
      
      # Add sheet with performance predictions
      addWorksheet(wb, "performance_period")
      writeData(wb, "performance_period", performance_predictions)
      
      # Save the workbook to the specified file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)