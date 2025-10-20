#' Plotting simulated data over time
#'
#' This function plots responses in the the summarised over time, either for individual runs or as an average.
#' It also allows to use the grouping variables.
#'
#' @param plotdata A summarized object from the functions XXXX or XXX.
#' @param response The name of the variable to plot over time.
#' @param time The name of the variable to use as time steps.
#' @param plot_type A string specifying what data to plot: all the runs "all", "group_avg" or "both".
#' Group_avg represents the mean and sd of the selecting groups, if any.
#' Default is "all"
#' @param id_col The name of the column representing the run identifier. Default is "run_id"
#' @param group (Optional) The name of the grouping variable.
#' @param scenario (Optional) The name of the scenarios to plot in facets.
#' @param time_scale (Optional) Integer to summarise the time steps into different time units. Default is NULL
#' For example if a time step represents a month, time_scale = 12, will summarize across 12 time steps giving a yearly output
#'
#' @return: a ggplot of the selected variables over time.


time_plot <- function(plotdata, response, time, plot_type = "all",
                      id_col = "run_id", group, scenario, time_scale){

  ## prepare data for plots

  # all runs
  data_allruns <- plotdata %>%
    group_by(group, id_col, time) %>%
    summarise(response = mean(response))

  # scenario avg
  meandata_group <- plotdata %>%
    group_by(time, group) %>%
    summarise(mean = mean(response),
              sd = sd(response))

  ## create plots
  if(plot_type == "all"){

    myplot <- ggplot(data_allruns, aes(x = time, y = response, colour = as.factor(id_col))) +
      geom_line(show.legend=FALSE) +
      scale_colour_brewer() +
      scale_fill_brewer() +
      ylab(response) +
      xlab(time) +
      theme_bw()
  }

  if(plot_type == "group_avg"){

    myplot <- ggplot(meandata_group, aes(x = time, y = mean, colour = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), col = "transparent", alpha = 0.3) +
      geom_line(lwd = 2) +
      scale_colour_brewer() +
      scale_fill_brewer() +
      ylab(response) +
      xlab(time) +
      theme_bw()
  }

  if(plot_type == "both"){

    myplot <- ggplot(meandata_group, aes(x = time, y = mean)) +
      geom_line(data = data_allruns, aes(x = time, y = response, group = as.factor(id_col), colour = scenario),
                lwd = 0.1) +
      # geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = scenario), col = "transparent", alpha = 0.3) +
      geom_line(aes(, colour = scenario), lwd = 2) +
      scale_colour_brewer() +
      scale_fill_brewer() +
      ylab(response) +
      xlab(time) +
      theme_bw()
  }

  return(myplot)
}





