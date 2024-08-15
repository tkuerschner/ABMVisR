#' Summarize Simulation Data with Dynamic Grouping
#'
#' This function processes simulation data, either count-based or individual-based,
#' and summarizes it based on the presence of id, scenario and grouping columns.
#'
#' @param data A dataframe containing raw input data.
#' @param data_type A string indicating the type of data: either "count" or "individual".
#' @param time_col The name of the column representing time or iteration steps.
#' @param id_col (Optional) The name of the column representing the run identifier. Default is NULL.
#' @param scenario_col (Optional) The name of the column representing the scenario identifier. Default is NULL.
#' @param target_cols A vector of column names to summarize or count over time.
#' @param group_cols (Optional) A vector of columns to group by before summarizing. Default is NULL.
#' @return A list of summarized dataframe showing the requested summaries over time.
summarize_simulation_data <- function(data,
                    data_type,
                    time_col,
                    id_col = NULL,
                    scenario_col = NULL,
                    target_cols,
                    group_cols = NULL) {
  library(dplyr)
  #REMOVE ME
    data_by_id <- NULL
    summary_by_scenario <- NULL
    summary_by_group <- NULL
    summary_by_scenario_and_group <- NULL
  #REMOVE ME
  if (data_type == "count") {
  cols_to_keep <- c(id_col, time_col, target_cols, group_cols, scenario_col)
  if (!is.null(id_col)) {
    data_by_id <- data %>%
    dplyr::group_by(across(all_of(c(id_col, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col))
  }
  if (!is.null(scenario_col)) {
    summary_by_scenario <- data %>%
    dplyr::group_by(across(all_of(c(scenario_col, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(across(all_of(target_cols), list(mean = ~ mean(.), sd = ~ sd(.), sum = ~ sum(.)), .names = "{fn}_{col}"), .groups = "drop")
  }
  if (!is.null(group_cols)) {
    summary_by_group <- data %>%
    dplyr::group_by(across(all_of(c(group_cols, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(across(all_of(target_cols), list(mean = ~ mean(.), sd = ~ sd(.), sum = ~ sum(.)), .names = "{fn}_{col}"), .groups = "drop")
  }
  if (!is.null(scenario_col) & !is.null(group_cols)) {
    summary_by_scenario_and_group <- data %>%
    dplyr::group_by(across(all_of(c(scenario_col, group_cols, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(across(all_of(target_cols), list(mean = ~ mean(.), sd = ~ sd(.), sum = ~ sum(.)), .names = "{fn}_{col}"), .groups = "drop")
  }
  } else if (data_type == "individual") {
  cols_to_keep <- c(id_col, time_col, target_cols, group_cols, scenario_col)
  if (!is.null(id_col)) {
    data_by_id <- data %>%
    dplyr::group_by(across(all_of(c(id_col, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(n = n(), .groups = "drop")
  }
  if (!is.null(scenario_col)) {
    summary_by_scenario <- data %>%
    dplyr::group_by(across(all_of(c(scenario_col, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(n = n(), .groups = "drop")
  }
  if (!is.null(group_cols)) {
    summary_by_group <- data %>%
    dplyr::group_by(across(all_of(c(group_cols, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(n = n(), .groups = "drop")
  }
  if (!is.null(scenario_col) & !is.null(group_cols)) {
    summary_by_scenario_and_group <- data %>%
    dplyr::group_by(across(all_of(c(scenario_col, group_cols, time_col)))) %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    dplyr::rename(run_id = all_of(id_col),
            time = all_of(time_col),
            scenario = all_of(scenario_col)) %>%
    dplyr::summarize(n = n(), .groups = "drop")
  }
  } else {
  stop("Invalid data_type. Please specify 'count' or 'individual'.")
  }
  return(list(data_by_id = data_by_id,
        summary_by_scenario = summary_by_scenario,
        summary_by_group = summary_by_group,
        summary_by_scenario_and_group = summary_by_scenario_and_group))
}


#REMOVE ME
count_data <- read.csv("test_data/wolf_model_spatial_data/Scenario1_36x_run1.csv", header = TRUE, skip = 6)

count_data <- count_data %>% 
  group_by(runName) %>%
  mutate(grouptester = sample(c("A", "B", "C"), 1)) %>%
  mutate(scenarioTester = sample(c("scenarioA", "scenarioB"), 1)) %>%
  ungroup()


#(count_data %>% filter(runName == "scenario1_77337344_typeOfCore_incMort_0") )$grouptester
#
#
#head(count_data)
#head(as.data.frame(count_data), 20)
#
#unique(count_data$runName)

# Example of using the function with count data
summarized_counts <- summarize_simulation_data(
  data = count_data, 
  data_type = "count",
  time_col = "X.step.",
  id_col = "runName",
  scenario_col = "scenarioTester",
  target_cols = c("count.turtles", "totalInd", "reproMales", "reproFemales")#,
  #group_cols = c("grouptester")
)

print(summarized_counts)
colnames(summarized_counts)

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

#REMOVE ME