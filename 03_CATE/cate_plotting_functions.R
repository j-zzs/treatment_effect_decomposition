library(tidyverse)
library(grf)
library(roll)


train_causal_forest <- function(data, 
                                treatment, 
                                outcome, 
                                covariates, 
                                num.trees = 5000, 
                                honesty = TRUE, 
                                predict.on.training = TRUE,
                                test.size = 0.5,
                                seed = 1234) {
  
  set.seed(seed)
  
  # 1. Extract variables
  W <- data[[treatment]]
  Y <- data[[outcome]]
  X <- data %>% select(all_of(covariates))
  
  # 2. Create train-test split
  n <- nrow(data)
  test_indices <- sample(seq_len(n), size = floor(test.size * n))
  train_indices <- setdiff(seq_len(n), test_indices)
  
  # 3. Split data
  W_train <- W[train_indices]
  Y_train <- Y[train_indices]
  X_train <- X[train_indices, , drop = FALSE]
  
  W_test <- W[test_indices]
  Y_test <- Y[test_indices]
  X_test <- X[test_indices, , drop = FALSE]
  
  # 4. One-hot encode X_train
  X_train_encoded <- model.matrix(~ 0 + ., X_train) %>% as.data.frame()
  
  # 5. Apply same encoding to X_test (ensure same columns)
  X_test_encoded <- model.matrix(~ 0 + ., X_test) %>% as.data.frame()
  
  # Ensure test set has the same columns as training set
  missing_cols <- setdiff(names(X_train_encoded), names(X_test_encoded))
  for (col in missing_cols) {
    X_test_encoded[[col]] <- 0
  }
  # Also ensure the order of columns matches
  X_test_encoded <- X_test_encoded[, names(X_train_encoded)]
  
  # 6. Train causal forest
  cf <- causal_forest(X_train_encoded, Y_train, W_train, 
                      honesty = honesty, num.trees = num.trees)
  
  # 7. Get ATE (training set)
  ate_train <- average_treatment_effect(cf)
  

  
  # 8. Predictions
  train_predictions <- NULL
  if (predict.on.training) {
    train_predictions <- predict(cf, estimate.variance = TRUE)
  }
  test_predictions <- predict(cf, newdata = X_test_encoded, estimate.variance = TRUE)
  
  # 9. Return as list
  list(
    model = cf,
    ate = ate_train,
    train_predictions = train_predictions,
    test_predictions = test_predictions,
    train_indices = train_indices,
    test_indices = test_indices
  )
}


plot_causal_forest_results <- function(data,
                                       causal_forest_model = NULL,
                                       predictions = NULL,
                                       ate = NULL,
                                       plot_title = "CATE Plot",
                                       width = 2000,
                                       min_obs = 100,
                                       p_low = 0.02,
                                       p_up = 0.98,
                                       tau_filter_low = 0.005,
                                       tau_filter_high = 0.995,
                                       plot_path = NULL,
                                       plot_filename = "CATE_CI.png",
                                       plot_width_cm = 24,
                                       plot_height_cm = 12) {
  
  # Predict if predictions are not provided but model is
  if (is.null(predictions)) {
    if (is.null(causal_forest_model)) {
      stop("Either 'predictions' or 'causal_forest_model' must be provided.")
    } else {
      message("No predictions supplied; predicting on training data...")
      predictions <- predict(causal_forest_model, estimate.variance = TRUE)
    }
  }
  
  # Get ATE if not supplied
  if (is.null(ate)) {
    if (is.null(causal_forest_model)) {
      stop("To compute ATE, the 'causal_forest_model' must be provided.")
    } else {
      ate <- average_treatment_effect(causal_forest_model)
    }
  }
  
  # Add tau + CI to data
  data$tau <- predictions$predictions
  data$tau_ci_low <- predictions$predictions - 1.96 * sqrt(predictions$variance.estimates)
  data$tau_ci_up  <- predictions$predictions + 1.96 * sqrt(predictions$variance.estimates)
  
  # Determine 1% and 99% quantiles of estimated CATEs
  tau_limits <- quantile(
    data$tau,
    probs = c(tau_filter_low, tau_filter_high),
    na.rm = TRUE
  )
  
  # Prepare data for plotting
  data_plot <- data %>% 
    select(tau, tau_ci_low, tau_ci_up) %>%
    filter(
      tau >= tau_limits[1],
      tau <= tau_limits[2]
    ) %>% 
    arrange(tau)
  
  #  Smooth confidence intervals
  data_plot$tau_ci_up_smooth <- roll::roll_quantile(data_plot$tau_ci_up, width = width, min_obs = min_obs, p = p_low)
  data_plot$tau_ci_low_smooth <- roll::roll_quantile(data_plot$tau_ci_low, width = width, min_obs = min_obs, p = p_up)
  
  # # Filter tau range
  # data_plot <- data_plot %>% filter(tau > tau_filter_low, tau < tau_filter_high)

  
  
  # Plot
  plt <- ggplot(data_plot, aes(y = tau, x = seq(0, 1, length.out = nrow(data_plot)))) + 
    geom_ribbon(aes(ymin = tau_ci_low_smooth, ymax = tau_ci_up_smooth), fill = "grey", show.legend = FALSE) +
    geom_line(aes(linetype = "CATE")) +
    xlab('Quantile') +
    ylab('Estimated Treatment Effect') +
    ggtitle(plot_title) +
    geom_hline(aes(yintercept = ate[1], linetype = "ATE"), color = "red") +
    geom_hline(aes(yintercept = ate[1] - ate[2]), linetype = 2, color = "red") +
    geom_hline(aes(yintercept = ate[1] + ate[2]), linetype = 2, color = "red") +
    scale_linetype_manual(name = "", values = c(1, 1),
                          guide = guide_legend(override.aes = list(color = c("red", "black")))) +
    theme_minimal()
  
  print(plt)
  
  # Save plot if requested
  if (!is.null(plot_path)) {
    ggsave(
      filename = plot_filename,
      plot = plt,
      device = "png",
      path = plot_path,
      scale = 1,
      width = plot_width_cm,
      height = plot_height_cm,
      units = "cm",
      dpi = 300,
      limitsize = TRUE
    )
  }
  
  # Return both plot + smoothed data
  list(
    plot = plt,
    data_plot = data_plot
  )
}



generate_ate_latex <- function(ate_list,
                               caption = "Average Treatment Effects (ATE) with Standard Errors",
                               label = "tab:ate_results",
                               file = "ate_table.tex") {
  lines <- c()
  lines <- c(lines, "\\begin{table}[ht]")
  lines <- c(lines, "\\centering")
  lines <- c(lines, "\\begin{tabular}{lc}")
  lines <- c(lines, "\\toprule")
  lines <- c(lines, "\\textbf{Outcome} & \\textbf{ATE Estimate (SE)} \\\\")
  lines <- c(lines, "\\midrule")
  
  for (name in names(ate_list)) {
    ate <- ate_list[[name]]$ate
    estimate <- sprintf("%.3f", ate["estimate"])
    std_err <- sprintf("%.3f", ate["std.err"])
    est_se <- sprintf("%s (%s)", estimate, std_err)
    
    line <- sprintf("%s & %s \\\\", name, est_se)
    lines <- c(lines, line)
  }
  
  lines <- c(lines, "\\bottomrule")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, sprintf("\\caption{%s}", caption))
  lines <- c(lines, sprintf("\\label{%s}", label))
  lines <- c(lines, "\\end{table}")
  
  # Ensure path exists
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  writeLines(lines, con = file)
  message(sprintf("ATE LaTeX table saved to '%s'", file))
}