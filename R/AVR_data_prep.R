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
