library(tidyverse)
library(furrr)
library(purrr)
plan(multisession, workers = 10) 
library(tictoc)
library(grf)
library(Counterfactual)
set.seed(123)


compute_summary <- function(estimates_mat, se_mat, true_mat, quantiles) {
  
  # Ensure all inputs are matrices (row = runs, col = quantiles)
  if (is.vector(estimates_mat)) estimates_mat <- matrix(estimates_mat, nrow = 1)
  if (is.vector(se_mat)) se_mat <- matrix(se_mat, nrow = 1)
  if (is.vector(true_mat)) true_mat <- matrix(true_mat, nrow = 1)
  
  n_runs <- nrow(estimates_mat)
  n_quant <- ncol(estimates_mat)
  
  # Bias and squared errors
  bias_mat <- estimates_mat - true_mat
  sq_error_mat <- bias_mat^2
  
  # Standard deviations of estimates (across runs)
  sd_vec <- apply(estimates_mat, 2, sd)
  
  # Coverage calculation (assumes compute_coverage handles matrices)
  cov_80 <- compute_coverage(estimates_mat, se_mat, true_mat, alpha = 0.2)
  cov_95 <- compute_coverage(estimates_mat, se_mat, true_mat, alpha = 0.05)
  cov_90 <- compute_coverage(estimates_mat, se_mat, true_mat, alpha = 0.1)
  
  # Average true values across runs
  mean_true <- colMeans(true_mat)
  
  data.frame(
    Quantile = paste0("Q", quantiles),
    True_value = mean_true,
    Mean_Estimate = colMeans(estimates_mat),
    Bias = colMeans(bias_mat),
    MSE = colMeans(sq_error_mat),
    SD = sd_vec,
    cov_80 = cov_80,
    cov_95 = cov_95,
    cov_90 = cov_90
  )
}


combine_total_se <- function(point_estimates, bootstrap_ses) {
  # Check dimensions
  if (!all(dim(point_estimates) == dim(bootstrap_ses))) {
    stop("Dimensions of point_estimates and bootstrap_ses must match.")
  }
  
  B <- nrow(point_estimates)  # number of repetitions
  P <- ncol(point_estimates)  # number of parameters
  
  se_total <- numeric(P)
  
  for (p in 1:P) {
    theta_p <- point_estimates[, p]         # point estimates for parameter p
    se_p <- bootstrap_ses[, p]              # bootstrap SEs for parameter p
    
    within_var <- mean(se_p^2)              # mean within-bootstrap variance
    between_var <- var(theta_p)             # between-replication variance
    
    total_var <- (within_var + (1+1/B)* between_var)
    se_total[p] <- sqrt(total_var)
  }
  
  names(se_total) <- colnames(point_estimates)
  return(se_total)
}

compute_coverage <- function(estimates_mat, se_mat, true_mat, alpha = 0.025) {
  
  # Convert vectors to 1-row matrices
  if (is.vector(estimates_mat)) estimates_mat <- matrix(estimates_mat, nrow = 1)
  if (is.vector(se_mat)) se_mat <- matrix(se_mat, nrow = 1)
  if (is.vector(true_mat)) true_mat <- matrix(true_mat, nrow = 1)
  
  z <- qnorm(1 - alpha / 2)
  lower <- estimates_mat - z * se_mat
  upper <- estimates_mat + z * se_mat
  
  coverage <- (lower <= true_mat) & (upper >= true_mat)
  
  # Compute coverage per column (quantile)
  colMeans(coverage)
}


# Helper: extract matrix from the list of lists
extract_matrix <- function(results, key) {
  do.call(rbind, lapply(results, function(x) x[[key]]))
}


################# Simulation ##################

run_simulation <- function(m, B, quantiles,
                           n, p,linear_cate, comp_effect,
                           shift_mode, delta_tau) {
  message('Replication ', m)
  set.seed(100 + m)
  
  # Simulate data
  data <- simulate_dgp_confounded(n = n, p = p, 
                                  linear_cate = linear_cate, 
                                  comp_effect = comp_effect, 
                                  shift_mode = shift_mode,
                                  delta_tau = delta_tau,
                                  seed = 100 + m)
  
  # store true decomposition
  true_decomp <- compute_true_quantile_decomposition(data, quantiles = quantiles)
  true_structure     <- true_decomp$structural_effect
  true_composition   <- true_decomp$compositional_effect
  true_total         <- true_decomp$total_effect
  true_iate_q        <- true_decomp$IATE
  true_iate_G0_q     <- true_decomp$iate_G0
  true_iate_G1_q     <- true_decomp$iate_G1
  
  
  structure_boot <- matrix(NA, nrow = B, ncol = length(quantiles))
  composition_boot <- matrix(NA, nrow = B, ncol = length(quantiles))
  total_boot <- matrix(NA, nrow = B, ncol = length(quantiles))
  
  structure_boot_se <- matrix(NA, nrow = B, ncol = length(quantiles))
  composition_boot_se <- matrix(NA, nrow = B, ncol = length(quantiles))
  total_boot_se <- matrix(NA, nrow = B, ncol = length(quantiles))
  
  for (b in 1:B) {
    set.seed(10000 + 100*m + b)
    
    # Split sample
    idx <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE)
    aux_sample <- data[idx,]
    main_sample <- data[!idx,]
    
    # Train causal forest
    W_train <- aux_sample$D
    Y_train <- aux_sample$Y
    X_train <- aux_sample %>% dplyr::select(dplyr::starts_with("X"), G)
    X_train_encoded <- model.matrix(~ 0 + ., X_train) %>% data.frame()
    
    
    cf <- causal_forest(X_train_encoded, Y_train, W_train, 
                        honesty = TRUE)
    
    # Predict tau on main sample
    X_pred <- model.matrix(~ 0 + ., main_sample %>% dplyr::select(dplyr::starts_with("X"), G)) %>% data.frame()
    main_sample$tau <- predict(cf, newdata = X_pred)$predictions
    
    # prepare analysis of IATE
    q_iate = quantile(main_sample$tau, probs = quantiles)
    q_iate_G1 = quantile(main_sample[main_sample$G == 1, ]$tau, probs = quantiles)
    q_iate_G0 = quantile(main_sample[main_sample$G == 0, ]$tau, probs = quantiles)
    
    # Prepare for counterfactual decomposition
    data_decomp <- main_sample %>% dplyr::select(dplyr::starts_with("X"), G, tau)
    X_vars <- setdiff(names(data_decomp), c("G", "tau", "IATE"))  # you can include X1 if desired
    
    # Decompose
    decomp <- counterfactual(as.formula(paste("tau ~", paste(X_vars, collapse = " + "))),
                             data = data_decomp,
                             group = data_decomp$G,
                             treatment = TRUE,
                             quantiles = quantiles,
                             method = "logit",
                             nreg = 50,
                             weightedboot = TRUE,
                             printdeco = FALSE,
                             decomposition = TRUE,
                             sepcore = TRUE)
    
    structure_boot[b, ] <- as.vector(decomp$resSE[,1])
    composition_boot[b, ] <- as.vector(decomp$resCE[,1])
    total_boot[b, ] <- as.vector(decomp$resTE[,1])
    
    structure_boot_se[b, ] <- as.vector(decomp$resSE[,2])
    composition_boot_se[b, ] <- as.vector(decomp$resCE[,2])
    total_boot_se[b, ] <- as.vector(decomp$resTE[,2])
    
  }
  
  structure_se = combine_total_se(structure_boot, structure_boot_se)
  composition_se = combine_total_se(composition_boot, composition_boot_se)
  total_se = combine_total_se(total_boot, total_boot_se)
  
  structure = apply(structure_boot, 2, mean)
  composition = apply(composition_boot, 2, mean)
  total = apply(total_boot, 2, mean)
  
  list(
    # estimated
    IATE_q = as.vector(as.vector(q_iate)),
    IATE_G0_q = as.vector(as.vector(q_iate_G0)), 
    IATE_G1_q = as.vector(as.vector(q_iate_G1)), 
    structure = structure,
    composition = composition,
    total = total,
    structure_se = structure_se,
    composition_se = composition_se,
    total_se = total_se,
    
    # Truth
    true_structure = as.vector(true_structure),
    true_composition = as.vector(true_composition),
    true_total = as.vector(true_total),
    true_iate_q = as.vector(true_iate_q),
    true_iate_G0_q = as.vector(true_iate_G0_q),
    true_iate_G1_q = as.vector(true_iate_G1_q)
  )
  
}




# one replicate, returns either result or error
run_one <- function(i, B, quantiles, n, p,
                    linear_cate, comp_effect, shift_mode, delta_tau,
                    outdir = "results") {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  fname <- file.path(outdir, paste0(n, "_result_", i, ".rds"))
  if (file.exists(fname)) {
    message("Skipping ", i, " (already saved)")
    return(NULL)
  }
  
  out <- tryCatch({
    run_simulation(i, B = B, quantiles = quantiles,
                   n = n, p = p,
                   linear_cate = linear_cate,
                   comp_effect = comp_effect,
                   shift_mode = shift_mode,
                   delta_tau = delta_tau)
  }, error = function(e) {
    structure(list(error = conditionMessage(e)), class = "sim_error")
  })
  
  saveRDS(out, fname)
  message(if (inherits(out, "sim_error")) 
    paste0("Error at ", i, ": ", out$error)
    else 
      paste0("Saved result ", i))
  
  NULL
}

# run all, then retry failed
run_all_with_retry <- function(indices, B, quantiles, n, p,
                               linear_cate, comp_effect, shift_mode,
                               delta_tau,
                               outdir = "results", retries = 2) {
  for (attempt in 1:(retries + 1)) {
    message("Attempt ", attempt)
    
    future_walk(indices, ~ run_one(.x, B, quantiles, n, p,
                                   linear_cate, comp_effect, shift_mode,
                                   delta_tau,
                                   outdir = outdir),
                .progress = TRUE)
    
    # check which failed
    files <- list.files(outdir, pattern = "^result_\\d+\\.rds$", full.names = TRUE)
    failed_ids <- map(files, readRDS) |>
      map_lgl(~ inherits(.x, "sim_error")) |>
      which()
    if (length(failed_ids) == 0) {
      message("All runs successful after ", attempt, " attempt(s).")
      break
    } else {
      failed_files <- files[failed_ids]
      failed_nums <- as.integer(gsub("\\D", "", basename(failed_files)))
      # restrict failures to the indices we’re actually running
      failed_nums <- intersect(failed_nums, indices)
      if (length(failed_nums) == 0) {
        message("All requested runs succeeded after ", attempt, " attempt(s).")
        break
      }
      message(" ", length(failed_nums), " failures remain: ", paste(failed_nums, collapse = ", "))
      
      # remove failed files so they get retried
      file.remove(file.path(outdir, paste0("result_", failed_nums, ".rds")))
      indices <- failed_nums  # retry only failed runs
    }
  }
}



