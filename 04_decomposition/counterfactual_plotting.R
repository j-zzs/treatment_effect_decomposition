################################################
# This file contains the plotting routine for 
# the counterfactual analysis plots.
################################################


# load relevant libraries
library(tidyverse)
library(ggpubr)
library(scales)



#' Plot Distribution
#' 
#' Function to plot quantiles of treatment effect distribution alongside the
#' quantiles of the counterfactual distribution. Only for the counterfactual 
#' distribution we get confidence intervals, as the other quantiles can be 
#' observed and are not estimated (when taking the observed treatment effect).
#' 
#' All dataframes come in the following input format:
#'  - each row represents result of a random split
#'  - first column shows distribution, either "distr", or "upper" and "lower" 
#'    confidence interval results
#'  - second column indicate the group of interest
#'  - followed by the value at each quantile
#'  
#' @param taus quantile points where counterfactual is evaluated at
#' @param quantiles_1_df result df for quantiles of group distributions and corresponding CI
#' @param quantiles_0_df result df for quantiles of reference group distribution and corresponding CI
#' @param quantiles_counterfactual_df result df for quantile of counterfactual distribution and CI
#' @param group_names names that should be assigned to groups in plot legend
#' @param reference_group name of reference group for plot legend
#' @param filter_low where to filter distributions, defaults to 0
#' @param filter_high upper filter for distribution, defaults to 1
#' 
#' @returns quantile plot
#'  
plot_quantiles = function(taus, quantiles_1_df, quantiles_0_df,
                          quantiles_counterfactual_df, group_names, reference_group,
                          filter_low = 0, filter_high = 1, lower_lim = NA, upper_lim = NA){
  
  # rename columns of all dataframes 
  col_names = c("distr","group", taus)
  colnames(quantiles_1_df) = col_names
  colnames(quantiles_0_df) = col_names
  colnames(quantiles_counterfactual_df) = col_names
  
  
  # prepare dataframes
  # change format from wide to long, make sure types are set correctly
  # get mean for each group at each tau 
  
  # confidence interval
  counter_lower = quantiles_counterfactual_df %>%
    filter(distr == "lower") %>% 
    select(-distr) %>% 
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(count_lower = mean(value))
  counter_upper = quantiles_counterfactual_df %>%
    filter(distr == "upper") %>% 
    select(-distr) %>% 
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(count_upper = mean(value))
  ci_data = counter_lower %>% 
    inner_join(counter_upper, by = c("group", "taus")) %>% 
    ungroup() %>% 
    mutate(group = factor(group, levels=seq(1, (length(group_names))), labels=group_names))
  
  #quantiles
  q1 = quantiles_1_df %>%
    filter(distr == "distr") %>% 
    select(-distr) %>% 
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(marginal_1 = mean(value))
  # quantiles_0_df is save for every group (makes look simpler) but does not differ by group!
  q0 = quantiles_0_df %>% 
    filter(distr == "distr") %>% 
    select(-distr) %>%
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(taus) %>% 
    summarise(marginal_0 = mean(value)) %>% 
    mutate(group = reference_group) %>% 
    mutate(group = as.factor(group))
  counterfact = quantiles_counterfactual_df %>% 
    filter(distr == "distr") %>% 
    select(-distr) %>%
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(counterfactual_x = mean(value)) 
  
  # plots
  
  if(is.na(lower_lim) & is.na(upper_lim)){
    quantile_plot = counterfact %>%  inner_join(q1, by = c("taus", "group")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>%
      gather(key = key, value =value, -c(group, taus)) %>% 
      separate(key, into = c("dist", "var"), sep = "_") %>%
      filter(taus < filter_high, taus > filter_low) %>%
      ggplot(aes(x=taus, colour=group)) +
      xlim(0, 1) +
      geom_line(aes(linetype=dist, y= value), size=1) +
      scale_linetype_manual(name = "", values=c("twodash", "solid")) +
      geom_line(data = q0 %>% filter(taus < filter_high, taus > filter_low) , aes(x=taus, y= marginal_0), size=1)+
      geom_ribbon(data = ci_data %>% filter(taus < filter_high, taus > filter_low), aes(ymin=count_lower, ymax=count_upper, fill=group),linetype=2,alpha=0.1, show.legend = FALSE)+
      scale_fill_manual(values = hue_pal()(length(group_names)+1)[-1]) +
      xlab("Quantile") +
      ylab("Estimated Treatment Effect") +
      ggtitle("Quantiles of Cumulative Distribution Functions")+
      theme_minimal()
  } else {
    quantile_plot = counterfact %>%  inner_join(q1, by = c("taus", "group")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>%
      gather(key = key, value =value, -c(group, taus)) %>% 
      separate(key, into = c("dist", "var"), sep = "_") %>%
      filter(taus < filter_high, taus > filter_low) %>%
      ggplot(aes(x=taus, colour=group)) +
      xlim(0, 1) +
      ylim(lower_lim, upper_lim) +
      geom_line(aes(linetype=dist, y= value), size=1) +
      scale_linetype_manual(name = "", values=c("twodash", "solid")) +
      geom_line(data = q0 %>% filter(taus < filter_high, taus > filter_low) , aes(x=taus, y= marginal_0), size=1)+
      geom_ribbon(data = ci_data %>% filter(taus < filter_high, taus > filter_low), aes(ymin=count_lower, ymax=count_upper, fill=group),linetype=2,alpha=0.1, show.legend = FALSE)+
      scale_fill_manual(values = hue_pal()(length(group_names)+1)[-1]) +
      xlab("Quantile") +
      ylab("Estimated Treatment Effect") +
      ggtitle("Quantiles of Cumulative Distribution Functions")+
      theme_minimal()
  }
  
  return(quantile_plot)
}


#' Plot for Total Effect
#' 
#' Function to plot Total Effect, which is simply the difference of the 
#' observed distributions or quantiles of distributions in out context.
#' 
#' All dataframes come in the following input format:
#'  - each row represents result of a random split
#'  - first column shows distribution, either "effect", or "upper" and "lower" 
#'    confidence interval results
#'  - second column indicate the group of interest
#'  - followed by the value at each quantile
#'  
#' @param total_effect_df result df for total effect, 
#'                        each row represents result of a random split,
#'                        first row shows group, followed by the total effect at each tau and CI
#' @param taus quantile points where counterfactual is evaluated at 
#' @param group_names names that should be assigned to groups in plot legend
#' @param filter_low where to filter distributions, defaults to 0
#' @param filter_high upper filter for distribution, defaults to 1
#' 
#' @returns total effect plot
#' 
plot_total = function(taus, total_effect_df, group_names,
                      filter_low = 0, filter_high = 1, lower_lim = NA, upper_lim = NA, y_lab = "Difference of Treatment Effects in gram"){
  
  # rename columns of all dataframes 
  # (up for improvement: do that as first step of data preparation below!)
  col_names = c("distr","group", taus)
  
  colnames(total_effect_df) = col_names
  #colnames(te_ci_upper_df)  = col_names
  #colnames(te_ci_lower_df)  = col_names
  
  # plot for total effect
  # prepare total effect dataframe, get mean by group and taus
  te = total_effect_df %>%
    filter(distr == "effect") %>% 
    select(-distr) %>%  
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(te = mean(value))
  # prepare total effect confidence intervals, get mean by group and taus
  te_ci_1 = total_effect_df %>%
    filter(distr == "upper") %>% 
    select(-distr) %>%   
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(upper = mean(value))
  te_ci_2 = total_effect_df %>%
    filter(distr == "lower") %>% 
    select(-distr) %>%  
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(lower = mean(value))
  
  # join prepared frames and plot in case of automatic ylim
  if(is.na(lower_lim) & is.na(upper_lim)){
    total_plot = te %>% inner_join(te_ci_1, by = c("group", "taus")) %>% 
      inner_join(te_ci_2, by = c("group", "taus")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>% 
      filter(taus < filter_high, taus >filter_low) %>% 
      ggplot(aes(x=taus,colour=group, group = group )) +
      xlim(0, 1) +
      geom_line(aes(x=taus, y=te), size = 1) +
      ggtitle("Total Effect") +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=group),linetype=2,alpha=0.1)+
      xlab("Quantile") +
      ylab(y_lab) +
      theme_minimal()
  } else{
    total_plot = te %>% inner_join(te_ci_1, by = c("group", "taus")) %>% 
      inner_join(te_ci_2, by = c("group", "taus")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>% 
      filter(taus < filter_high, taus >filter_low) %>% 
      ggplot(aes(x=taus,colour=group, group = group )) +
      xlim(0, 1) +
      ylim(lower_lim, upper_lim) +
      geom_line(aes(x=taus, y=te), size = 1) +
      ggtitle("Total Effect") +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=group),linetype=2,alpha=0.1)+
      xlab("Quantile") +
      ylab(y_lab) +
      theme_minimal()
  }
  
  
  return(total_plot)
}

#' Plot for Structure Effect
#' 
#' Function to plot Structure Effect, which is the difference ???
#' 
#' All dataframes come in the following input format:
#'  - each row represents result of a random split
#'  - first column shows distribution, either "effect", or "upper" and "lower" 
#'    confidence interval results
#'  - second column indicate the group of interest
#'  - followed by the value at each quantile
#'  
#' @param structure_effect_df result df for structure effect, 
#'                            each row represents result of a random split,
#'                            first row shows group, followed by the structure effect at each tau and CI
#' @param taus quantile points where counterfactual is evaluated at 
#' @param group_names names that should be assigned to groups in plot legend
#' @param filter_low where to filter distributions, defaults to 0
#' @param filter_high upper filter for distribution, defaults to 1
#' 
#' @returns struvture effect plot
#' 
plot_structure = function(taus, structure_effect_df, group_names,
                          filter_low = 0, filter_high = 1,
                          lower_lim = NA, upper_lim = NA, y_lab = "Difference of Treatment Effects in gram",
                          single = FALSE){
  
  # rename columns of all dataframes 
  # (up for improvement: do that as first step of data preparation below!)
  col_names = c("distr","group", taus)
  
  colnames(structure_effect_df) = col_names
  
  
  # plot for structural effect
  # change format from wie to long and set correct types
  #get mean by group and tau
  se = structure_effect_df %>%
    filter(distr == "effect") %>% 
    select(-distr) %>%
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(se = mean(value))
  se_ci_1 = structure_effect_df %>%
    filter(distr == "upper") %>% 
    select(-distr) %>%  
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(upper = mean(value))
  se_ci_2 = structure_effect_df %>%
    filter(distr == "lower") %>% 
    select(-distr) %>%   
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(lower = mean(value))
  
  # set title based on `single` flag
  plot_title = ifelse(single, "", "Structural Effect")
  
  # join dataframes together, add group names, plot effects by group
  if(is.na(lower_lim) & is.na(upper_lim)){
    structural_plot = se %>% inner_join(se_ci_1, by = c("group", "taus")) %>% 
      inner_join(se_ci_2, by = c("group", "taus")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>% 
      filter(taus < filter_high, taus >filter_low) %>% 
      ggplot(aes(x=taus,colour=group, group = group )) +
      xlim(0, 1) +
      geom_line(aes(x=taus, y=se), size = 1) +
      ggtitle(plot_title) +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=group),linetype=2,alpha=0.1)+
      xlab("Quantile") +
      ylab(y_lab) +
      theme_minimal()
  } else {
    structural_plot = se %>% inner_join(se_ci_1, by = c("group", "taus")) %>% 
      inner_join(se_ci_2, by = c("group", "taus")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>% 
      filter(taus < filter_high, taus >filter_low) %>% 
      ggplot(aes(x=taus,colour=group, group = group )) +
      xlim(0, 1) +
      ylim(lower_lim, upper_lim) +
      geom_line(aes(x=taus, y=se), size = 1) +
      ggtitle("Structural Effect") +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=group),linetype=2,alpha=0.1)+
      xlab("Quantile") +
      ylab(y_lab) +
      theme_minimal()
  }
  
  
  return(structural_plot)
}


#' Plot for Composition Effect
#' 
#' Function to plot Composition Effect, which is the difference ???
#' 
#' All dataframes come in the following input format:
#'  - each row represents result of a random split
#'  - first column shows distribution, either "effect", or "upper" and "lower" 
#'    confidence interval results
#'  - second column indicate the group of interest
#'  - followed by the value at each quantile
#'  
#' @param composition_effect_df result df for composition effect, 
#'                              each row represents result of a random split,
#'                              first row shows group, followed by the composition effect at each tau and CI
#' @param taus quantile points where counterfactual is evaluated at
#' @param group_names names that should be assigned to groups in plot legend
#' @param filter_low where to filter distributions, defaults to 0
#' @param filter_high upper filter for distribution, defaults to 1
#' 
#' @returns composition effect plot
#' 
plot_composition = function(taus, composition_effect_df, group_names,
                            filter_low = 0, filter_high = 1, 
                            lower_lim = NA, upper_lim = NA, y_lab = "Difference of Treatment Effects in gram"){
  
  # rename columns of all dataframes 
  # (up for improvement: do that as first step of data preparation below!)
  col_names = c("distr","group", taus)
  colnames(composition_effect_df) = col_names
  
  
  # plot for composition effect
  # change format from wie to long and set correct types
  #get mean by group and tau
  ce = composition_effect_df %>%
    filter(distr == "effect") %>% 
    select(-distr) %>%  
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(ce = mean(value))
  ce_ci_1 = composition_effect_df %>%
    filter(distr == "upper") %>% 
    select(-distr) %>%  
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(upper = mean(value))
  ce_ci_2 = composition_effect_df %>%
    filter(distr == "lower") %>% 
    select(-distr) %>% 
    gather(taus, value, -c(group)) %>%
    mutate(value = as.numeric(value), taus = as.numeric(taus), group = as.factor(group) )%>% 
    group_by(group, taus) %>% 
    summarise(lower = mean(value))
  
  if(is.na(lower_lim) & is.na(upper_lim)){
    composition_plot = ce %>% inner_join(ce_ci_1, by = c("group", "taus")) %>% 
      inner_join(ce_ci_2, by = c("group", "taus")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>% 
      filter(taus < filter_high, taus > filter_low) %>% 
      ggplot(aes(x=taus,colour=group, group = group )) +
      xlim(0, 1) +
      geom_line(aes(x=taus, y=ce), size = 1) +
      ggtitle("Compositional Effect") +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=group),linetype=2,alpha=0.1)+
      xlab("Quantile") +
      ylab(y_lab) +
      theme_minimal()
  } else {
    composition_plot = ce %>% inner_join(ce_ci_1, by = c("group", "taus")) %>% 
      inner_join(ce_ci_2, by = c("group", "taus")) %>% 
      ungroup() %>% 
      mutate(group = factor(group, levels=seq(1, length(group_names)), labels=group_names)) %>% 
      filter(taus < filter_high, taus > filter_low) %>% 
      ggplot(aes(x=taus,colour=group, group = group )) +
      xlim(0, 1) +
      ylim(lower_lim, upper_lim) +
      geom_line(aes(x=taus, y=ce), size = 1) +
      ggtitle("Compositional Effect") +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=group),linetype=2,alpha=0.1)+
      xlab("Quantile") +
      ylab(y_lab) +
      theme_minimal()
  }
  
  return(composition_plot)
}



#' Plot effect for counterfactual analysis
#' 
#' This function is used to automate the plotting for the counterfactual analysis.
#' It takes in all dataframes from the splitting routine and prepares data accordingly.
#' All dataframes come in the following input format:
#'  - each row represents result of a random split
#'  - first row shows group
#'  - followed by the total effect at each tau
#' 
#' @param total_effect_df result df for total effect, 
#'                        each row represents result of a random split,
#'                        first row shows group, followed by the total effect at each tau
#' @param structure_effect_df result df for structure effect, 
#'                            each row represents result of a random split,
#'                            first row shows group, followed by the structure effect at each tau
#' @param composition_effect_df result df for composition effect, 
#'                              each row represents result of a random split,
#'                              first row shows group, followed by the composition effect at each tau
#' @param taus quantile points where counterfactual is evaluated at
#' @param quantiles_1_df result df for quantiles of group distributions
#' @param quantiles_0_df result df for quantiles of reference group distribution
#' @param quantiles_counterfactual_df result df for quantile of counterfactual distribution
#' @param group_names names that should be assigned to groups in plot legend
#' @param reference_group name of reference group for plot legend
#' @param plot_annotation variable of interests name to be used in plot annotation
#' @param save_figure boolean, whether to save figure or not, defaults to FALSE
#' @param path where figure should be saved at, defaults to NULL
#' @param name name of plot, need to contain file ending, such as ".png", defaults to NULL
#' @param filter_low where to filter distributions, defaults to 0
#' @param filter_high upper filter for distribution, defaults to 1
#' 
#' @returns combined plot
#' 
plot_effects = function(total_effect_df, structure_effect_df, composition_effect_df, taus,
                        quantiles_1_df, quantiles_0_df, quantiles_counterfactual_df,
                        group_names, 
                        plot_annotation, save_figure = FALSE, path = NULL, name = NULL,
                        filter_low = 0, filter_high = 1, limits_quant = c(NA, NA), 
                        limits_total = c(NA, NA), limits_structure = c(NA, NA), 
                        limits_composition = c(NA, NA),
                        y_lab = "Difference of Treatment Effects in gram",
                        plot = "all") {
  
  # Make sure plot is one of the allowed options
  plot <- match.arg(plot, choices = c("all", "structure", "composition"))
  
  if (plot == "all") {
    
    quantile_plot = plot_quantiles(
      taus, quantiles_1_df, 
      quantiles_0_df,  
      quantiles_counterfactual_df, group_names[-1], 
      group_names[1], 
      filter_low = filter_low, filter_high = filter_high,
      lower_lim = limits_quant[1], upper_lim = limits_quant[2]
    )
    
    total_plot = plot_total(
      taus, total_effect_df, group_names[-1],
      filter_low = filter_low, filter_high = filter_high, 
      lower_lim = limits_total[1], upper_lim = limits_total[2],
      y_lab = y_lab
    )
    
    structure_plot = plot_structure(
      taus, structure_effect_df, 
      group_names[-1],
      filter_low = filter_low, filter_high = filter_high,
      lower_lim = limits_structure[1], upper_lim = limits_structure[2],
      y_lab = y_lab,
      single = FALSE
    )
    
    composition_plot = plot_composition(
      taus, composition_effect_df,
      group_names[-1],
      filter_low = filter_low, filter_high = filter_high,
      lower_lim = limits_composition[1], upper_lim = limits_composition[2],
      y_lab = y_lab
    )
    
    # Arrange 4 subplots
    figure <- ggarrange(
      quantile_plot, total_plot, structure_plot, composition_plot,
      ncol = 2, nrow = 2, legend = "right"
    )
    
    figure <- annotate_figure(
      figure,
      top = text_grob(
        paste0("Effect Decomposition by ", plot_annotation),
        face = "bold", size = 14
      )
    )
    
    if (save_figure == TRUE) {
      ggsave(paste0(path, name), plot = figure, width = 16, height = 8.5)
    }
    
  } else if (plot == "structure") {
    
    structure_plot = plot_structure(
      taus, structure_effect_df, 
      group_names[-1],
      filter_low = filter_low, filter_high = filter_high,
      lower_lim = limits_structure[1], upper_lim = limits_structure[2],
      y_lab = y_lab,
      single = TRUE
    )
    
    figure <- ggarrange(
      structure_plot,
      ncol = 1, nrow = 1, legend = "right"
    )
    
    figure <- annotate_figure(
      figure,
      top = text_grob(
        paste0("Structural Effect of ", plot_annotation),
        face = "bold", size = 14
      )
    )
    
    if (save_figure == TRUE) {
      ggsave(paste0(path, name), plot = figure, width = 8, height = 4.25)
    }
    
  } else if (plot == "composition") {
    
    composition_plot = plot_composition(
      taus, composition_effect_df,
      group_names[-1],
      filter_low = filter_low, filter_high = filter_high,
      lower_lim = limits_composition[1], upper_lim = limits_composition[2],
      y_lab = y_lab
    )
    
    figure <- ggarrange(
      composition_plot,
      ncol = 1, nrow = 1, legend = "right"
    )
    
    figure <- annotate_figure(
      figure,
      top = text_grob(
        paste0("Composition Effect of ", plot_annotation),
        face = "bold", size = 14
      )
    )
    
    if (save_figure == TRUE) {
      ggsave(paste0(path, name), plot = figure, width = 8, height = 4.25)
    }
  }
  
  return(figure)
}
