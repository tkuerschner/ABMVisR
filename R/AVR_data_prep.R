#' Summarize Simulation Data with Dynamic Grouping
#'
#' This function processes simulation data, either count-based or individual-based,
#' and summarizes it based on the presence of id and scenario columns.
#'
#' @param data A dataframe containing raw input data.
#' @param data_type A string indicating the type of data: either "count" or "individual".
#' @param time_col The name of the column representing time or iteration steps.
#' @param id_col (Optional) The name of the column representing the run identifier. Default is NULL.
#' @param scenario_col (Optional) The name of the column representing the scenario identifier. Default is NULL.
#' @param target_cols A vector of column names to summarize or count over time.
#' @param group_cols (Optional) A vector of columns to group by before summarizing. Default is NULL.
#' @return A summarized dataframe showing the requested summaries over time.

summarize_simulation_data <- function(data,
                                      data_type,
                                      time_col,
                                      id_col = NULL,
                                      scenario_col = NULL,
                                      target_cols,
                                      group_cols = NULL) {
  
  library(dplyr)
  
  if (data_type == "count") {
    
    # Step 1: Group by the necessary columns
    grouping_vars <- c(group_cols, time_col)
    if (!is.null(scenario_col)) {
      grouping_vars <- c(scenario_col, grouping_vars)
    }
    
    # Step 2: Summarize
    summarized_data <- data %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarize(across(all_of(target_cols), sum, .names = "sum_{col}"), .groups = "drop")
    
    # Step 3: If id_col is present, further summarize by scenario
    if (!is.null(id_col)) {
      summarized_data <- summarized_data %>%
        group_by(across(all_of(c(scenario_col, group_cols, time_col)))) %>%
        summarize(across(starts_with("sum_"), 
                         list(mean = ~ mean(.), sd = ~ sd(.)), 
                         .names = "{col}_{fn}"), .groups = "drop")
    }
    
  } else if (data_type == "individual") {
    
    # Step 1: Group by the necessary columns
    grouping_vars <- c(group_cols, time_col)
    if (!is.null(scenario_col)) {
      grouping_vars <- c(scenario_col, grouping_vars)
    }
    
    # Step 2: Summarize
    summarized_data <- data %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarise(n = n(), .groups = "drop")
    
    # Step 3: If id_col is present, further summarize by scenario
    if (!is.null(id_col)) {
      summarized_data <- summarized_data %>%
        group_by(across(all_of(c(scenario_col, group_cols, time_col)))) %>%
        summarize(n_mean = mean(n), n_sd = sd(n), .groups = "drop")
    }
    
  } else {
    stop("Invalid data_type. Please specify 'count' or 'individual'.")
  }
  
  return(summarized_data)
}



count_data <- read.csv("test_data/wolf_model_spatial_data/Scenario1_36x_run1.csv", header = TRUE, skip = 6)
# Example of using the function with count data
summarized_counts <- summarize_simulation_data(
  data = count_data, 
  data_type = "count",
  time_col = "X.step.",
  id_col = "runName",
  scenario_col = "myScenario",
  target_cols = c("count.turtles", "totalInd", "reproMales", "reproFemales"),
  group_cols = c("reproProb")
)

print(summarized_counts)


individual_data <- read.csv("test_data/asf_abm_data/all_groups_400.csv", header = TRUE)
# Example of using the function with individual data
summarized_individuals <- summarize_simulation_data(
  data = individual_data,
  data_type = "individual",
  time_col = "iteration",
  id_col = NULL,  # No id_col
  scenario_col = NULL,
  target_cols = NULL,        # Not needed for individual type
  group_cols = c("age_class", "sex") # Group by age_class
)

print(summarized_individuals)


