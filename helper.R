library(dplyr)
library(readxl)
library(lubridate)
library(nmecr)

add_holidays = function(df, holidays_df, time_var){
  # Function to add UCSF work holidays to data
  df$date = as.Date(df[[time_var]])
  out = df %>% left_join(holidays_df, by = "date")
  out$date = NULL
  return(out)
}

modeling_inputs = function(timescale_days = NULL,
                           has_temp_knots_defined = FALSE,
                           equal_temp_segment_points = TRUE,
                           temp_segments_numeric = 6,
                           temp_knots_value = c(40, 55, 65, 80, 90),
                           initial_breakpoints = c(50,65),
                           regression_type = c("TOWT", "SLR",
                                               "Three Parameter Cooling",
                                               "Three Parameter Heating",
                                               "Four Parameter Linear Model",
                                               "Five Parameter Linear Model"),
                           occupancy_threshold = 0.65,
                           day_normalized = FALSE,
                           y_var = "eload",
                           temp_var = "temp",
                           time_var = "time",
                           additional_vars = NULL,
                           additional_var_agg = c(mean),
                           baseline_start = "01/01/2024 00:00",
                           baseline_end = "12/31/2024 23:59",
                           performance_start = "01/01/2025 00:00",
                           performance_end = "12/31/2025 23:59",
                           convert_to_data_interval = "Hourly",
                           dr_analysis = FALSE,
                           dr_dates = NULL){
  if (length(regression_type) > 1){
    reg_placeholder = regression_type[1]
  } else {
    reg_placeholder = regression_type
  }
  nmec_model_options = assign_model_inputs(timescale_days,
                                           has_temp_knots_defined,
                                           equal_temp_segment_points,
                                           temp_segments_numeric,
                                           temp_knots_value,
                                           initial_breakpoints,
                                           reg_placeholder,
                                           occupancy_threshold,
                                           day_normalized)
  out = list()
  out$nmecr_model_options = nmec_model_options
  out$y_var = y_var
  out$temp_var = temp_var
  out$time_var = time_var
  out$additional_vars = additional_vars
  out$additional_var_agg = additional_var_agg
  out$baseline_start = baseline_start
  out$baseline_end = baseline_end
  out$performance_start = performance_start
  out$performance_end = performance_end
  out$data_interval = convert_to_data_interval
  out$regression_type = regression_type
  out$dr_analysis = dr_analysis
  out$dr_dates = dr_dates
  
  return(out)
}

mv = function(df, nmec_options){
  # df is the cleaned dataframe
  # options is nmec arguments to run models
  
  df = df %>% rename(eload = all_of(nmec_options$y_var),
                     temp = all_of(nmec_options$temp_var),
                     time = all_of(nmec_options$time_var))
  load_data = df %>% select(time, eload)
  temp_data = df %>% select(time, temp)
  if (is.null(nmec_options$additional_vars)){
    additional_vars_df = NULL
  } else {
    additional_vars_df =  df %>% select(time, all_of(nmec_options$additional_vars))
  }

  baseline_df = create_dataframe(eload_data = load_data, temp_data = temp_data,
                                 additional_independent_variables = additional_vars_df,
                                 additional_variable_aggregation = nmec_options$additional_var_agg,
                                 convert_to_data_interval = nmec_options$data_interval,
                                 start_date = nmec_options$baseline_start,
                                 end_date = nmec_options$baseline_end)
  performance_df = create_dataframe(eload_data = load_data, temp_data = temp_data,
                                    additional_independent_variables = additional_vars_df,
                                    additional_variable_aggregation = nmec_options$additional_var_agg,
                                    convert_to_data_interval = nmec_options$data_interval,
                                    start_date = nmec_options$performance_start,
                                    end_date = nmec_options$performance_end)
  
  subset_performance_df <- function(performance_df, dr_dates) {
    # Convert the time column to Date format
    performance_df$date <- as.Date(performance_df$time)
    
    # Subset the dataframe
    subset_df <- subset(performance_df, date %in% dr_dates)
    
    # Remove the extra date column
    subset_df$date <- NULL
    
    return(subset_df)
  }
  
  subset_baseline_df = function(baseline_df, dr_dates){
    baseline_df$date = as.Date(baseline_df$time)
    subset_df = subset(baseline_df, !(date %in% dr_dates))
    subset_df$date = NULL
    return(subset_df)
  }
  if (nmec_options$dr_analysis){
    performance_df = subset_performance_df(performance_df, nmec_options$dr_dates)
    baseline_df = subset_baseline_df(baseline_df, nmec_options$dr_dates)
  }
  
  models = list()
  
  nmec_savings = function(model_algo, baseline_df, performance_df, model_inputs){
    out_list = list()
    baseline_mod = model_algo(training_data = baseline_df, model_input_options = model_inputs)
    baseline_stats = calculate_summary_statistics(baseline_mod)
    performance_prediction = calculate_model_predictions(baseline_df, performance_df, baseline_mod, FALSE)
    aeu = calculate_savings_and_uncertainty(performance_prediction, savings_fraction=0.1, modeled_object=baseline_mod, model_summary_statistics = baseline_stats)
    out_list[["baseline_mod"]] = baseline_mod
    out_list[["baseline_stats"]] = baseline_stats
    out_list[["performance_prediction"]] = aeu$savings_df
    return(out_list)
  }
  
  if ("TOWT" %in% nmec_options$regression_type){
    nmec_opts = nmec_options$nmecr_model_options
    nmec_opts[["regression_type"]] = "TOWT"
    models[["TOWT"]] = nmec_savings(model_with_TOWT, baseline_df, performance_df, nmec_opts)
  }
  
  if ("Five Parameter Linear Model" %in% nmec_options$regression_type){
    nmec_opts = nmec_options$nmecr_model_options
    nmec_opts[["regression_type"]] = "5P"
    models[["5P"]] = nmec_savings(model_with_CP, baseline_df, performance_df, nmec_opts)
  }
  
  if ("Four Parameter Linear Model" %in% nmec_options$regression_type){
    nmec_opts = nmec_options$nmecr_model_options
    nmec_opts[["regression_type"]] = "4P"
    models[["4P"]] = nmec_savings(model_with_CP, baseline_df, performance_df, nmec_opts)
  }
  
  if ("Three Parameter Cooling" %in% nmec_options$regression_type){
    nmec_opts = nmec_options$nmecr_model_options
    nmec_opts[["regression_type"]] = "3PC"
    models[["3PC"]] = nmec_savings(model_with_CP, baseline_df, performance_df, nmec_opts)
  }
  
  if ("Three Parameter Heating" %in% nmec_options$regression_type){
    nmec_opts = nmec_options$nmecr_model_options
    nmec_opts[["regression_type"]] = "3PH"
    models[["3PH"]] = nmec_savings(model_with_CP, baseline_df, performance_df, nmec_opts)
  }
  
  if ("SLR" %in% nmec_options$regression_type){
    nmec_opts = nmec_options$nmecr_model_options
    nmec_opts[["regression_type"]] = "SLR"
    models[["SLR"]] = nmec_savings(model_with_SLR, baseline_df, performance_df, nmec_opts)
  }
  
  ct = 1
  for (i in models){
    # Rename baseline model training data
    colnames(i$baseline_mod$training_data)[colnames(i$baseline_mod$training_data) == "eload"] = nmec_options$y_var
    colnames(i$baseline_mod$training_data)[colnames(i$baseline_mod$training_data) == "temp"] = nmec_options$temp_var
    colnames(i$baseline_mod$training_data)[colnames(i$baseline_mod$training_data) == "time"] = nmec_options$time_var
    models[[ct]]$baseline_mod$training_data = i$baseline_mod$training_data
    
    # Remap names back for performance predictions
    colnames(i$performance_prediction)[colnames(i$performance_prediction) == "eload"] = nmec_options$y_var
    colnames(i$performance_prediction)[colnames(i$performance_prediction) == "temp"] = nmec_options$temp_var
    colnames(i$performance_prediction)[colnames(i$performance_prediction) == "time"] = nmec_options$time_var
    models[[ct]]$performance_prediction = i$performance_prediction
    ct = ct + 1
  }
  
  return(models)
}