library(Counterfactual)
library(grf)

# example for shift in structural effect:
# delta_fun <- function(X, G) {
# 0.2 + 0.4 * X[,1]   # returns length-n vector. can be any R expression using X and G
#}
#d <- simulate_dgp_confounded_v2(delta_tau = delta_fun)


simulate_dgp_confounded <- function(
    n = 1000,
    p = 5,
    delta_tau = 0.5,        # can be:
    #  - numeric scalar (original behavior)
    #  - numeric vector length n (elementwise shifts)
    #  - a function(X, G) returning length-n vector
    delta_X = 0.5, # magnitude used by shift modes
    linear_cate = FALSE, # if TRUE, cate is linear, else non-linear
    comp_effect = FALSE, # whether covariate shift affects tau (we assume shift_cols chosen accordingly)
    seed = 123,
    shift_mode = c("mult"),
    clip_quantiles = c(0.005, 0.995),
    return_diag = FALSE
) {
  set.seed(seed)
  shift_mode <- match.arg(shift_mode)
  
  # Covariates
  X <- matrix(rnorm(n * p), nrow = n)
  colnames(X) <- paste0("X", 1:p)
  
  # Group indicator
  G <- rbinom(n, 1, 0.5)
  
  # shift columns
  if (comp_effect) {
    shift_cols <- c(1, 2, 3)
  } else {
    shift_cols <- 4:min(6, p)
  }
  
  X_shifted <- X
  for (j in shift_cols) {
    if (shift_mode == "mult") {
      X_shifted[G == 1, j] <- X_shifted[G == 1, j] * (1 + delta_X)
    }
  }
  
  # Clip to maintain overlap
  if (!is.null(clip_quantiles)) {
    lo <- clip_quantiles[1]; hi <- clip_quantiles[2]
    for (j in 1:ncol(X_shifted)) {
      qlow <- as.numeric(quantile(X[, j], lo))
      qhigh <- as.numeric(quantile(X[, j], hi))
      X_shifted[, j] <- pmin(pmax(X_shifted[, j], qlow), qhigh)
    }
  }
  
  # CATE (base)
  if (linear_cate) {
    TAU_base <- 0.5 * X_shifted[, 3] + 0.1 * X_shifted[, 4]
  } else {
    TAU_base <- 1 / (1 + exp(-X_shifted[, 3]))
  }
  
  # ---- Build the group-specific structural shift per unit (delta_tau_i) ----
  # 1) Start from delta_tau argument:
  #    - if numeric scalar -> replicate
  #    - if function -> call function(X_shifted, G) -> expect length-n numeric
  if (is.function(delta_tau)) {
    delta_tau_vec <- delta_tau(X_shifted, G)
    if (length(delta_tau_vec) != n) stop("delta_tau(X, G) must return a length-n numeric vector.")
  } else if (is.numeric(delta_tau) && length(delta_tau) == 1) {
    delta_tau_vec <- rep(delta_tau, n)
  } else {
    stop("delta_tau must be numeric scalar, numeric vector length n, or function(X, G).")
  }
  # Final IATE: TAU_base + group-specific delta for units in G (we apply delta only when G==1)
  TAU <- TAU_base + delta_tau_vec * G
  IATE_cf_G0 <- TAU_base  # counterfactual under G==0
  
  # Baseline outcome
  Y0 <- pmax(X_shifted[, 2] + X_shifted[, 3], 0) +
    rowMeans(X_shifted[, shift_cols, drop = FALSE]) / 2 +
    rnorm(n)
  
  # Treated (add extra noise)
  Y1 <- Y0 + TAU + rnorm(n)
  
  # Confounded treatment assignment (depends on X1, X2 possibly shifted)
  D <- rbinom(n, 1, 1 / (1 + exp(-X_shifted[, 1] - X_shifted[, 2])))
  Y <- D * Y1 + (1 - D) * Y0
  
  out_df <- data.frame(
    Y = Y, D = D, Y0 = Y0, Y1 = Y1, IATE = TAU, IATE_cf_G0 = IATE_cf_G0, G = G, X_shifted
  )
  colnames(out_df)[8:ncol(out_df)] <- paste0("X", 1:p)
  
  if (!return_diag) return(out_df)
  
  # Diagnostics
  ps <- glm(D ~ ., data = data.frame(X_shifted[, 1:2, drop = FALSE]), family = binomial)$fitted.values
  diag <- list(
    propensities = ps,
    mean_ps_by_G = tapply(ps, G, mean),
    min_ps_by_G = tapply(ps, G, min),
    max_ps_by_G = tapply(ps, G, max),
    smd = sapply(shift_cols, function(j) {
      (mean(X_shifted[G == 1, j]) - mean(X_shifted[G == 0, j])) / sd(X_shifted[, j])
    }),
    delta_tau_vector = delta_tau_vec
  )
  
  return(list(data = out_df, diag = diag))
}




compute_true_quantile_decomposition <- function(df, quantiles = seq(0.1, 0.9, by = 0.1)) {
  # Split data by group
  df0 <- df[df$G == 0, ]
  df1 <- df[df$G == 1, ]
  
  # Extract IATEs
  iate_00 <- df0$IATE            # τ⟨0|0⟩
  iate_11 <- df1$IATE            # τ⟨1|1⟩
  iate_01 <- df1$IATE_cf_G0      # τ⟨0|1⟩: counterfactual IATEs using group 0 model
  
  # Compute quantiles
  q_00 <- quantile(iate_00, probs = quantiles)
  q_11 <- quantile(iate_11, probs = quantiles)
  q_01 <- quantile(iate_01, probs = quantiles)
  
  # Decomposition
  total_effect <- q_11 - q_00
  structural_effect <- q_11 - q_01
  compositional_effect <- q_01 - q_00
  
  # Assemble results
  decomp_df <- data.frame(
    quantile = quantiles,
    total_effect = as.numeric(total_effect),
    structural_effect = as.numeric(structural_effect),
    compositional_effect = as.numeric(compositional_effect),
    iate_G0 = q_00,
    iate_G1 = q_11,
    IATE = quantile(df$IATE,probs = quantiles)
  )
  
  return(decomp_df)
}


plot_true_iate_ecdfs <- function(df) {
  df0 <- df[df$G == 0, ]
  df1 <- df[df$G == 1, ]
  
  iate_00 <- df0$IATE
  iate_11 <- df1$IATE
  iate_01 <- df1$IATE_cf_G0
  
  # Combine all for x-axis range
  all_iates <- c(iate_00, iate_11, iate_01)
  xlim_range <- range(all_iates)
  
  # Plot ECDF for τ⟨1|1⟩
  plot(ecdf(iate_11), 
       main = "True Distribution of IATEs by Group",
       xlab = "Individual Treatment Effect",
       ylab = "Cumulative Distribution Function",
       col = "blue", lwd = 2, xlim = xlim_range,
       do.points = FALSE, verticals = TRUE)
  
  # Add others
  lines(ecdf(iate_00), col = "red", lwd = 2, do.points = FALSE, verticals = TRUE)
  lines(ecdf(iate_01), col = "darkgreen", lwd = 2, do.points = FALSE, verticals = TRUE)
  
  # Add legend
  legend("topleft", 
         legend = c(expression(tau["⟨1|1⟩"]), 
                    expression(tau["⟨0|0⟩"]),
                    expression(tau["⟨0|1⟩"] ~ "(Counterfactual)")),
         col = c("blue", "red", "darkgreen"), 
         lty = 1, lwd = 2, bty = "n", cex = 0.9)
  
  # Grid for readability
  grid()
}



plot_true_iate_quantiles <- function(df) {
  df0 <- df[df$G == 0, ]
  df1 <- df[df$G == 1, ]
  
  iate_00 <- df0$IATE
  iate_11 <- df1$IATE
  iate_01 <- df1$IATE_cf_G0
  
  # Quantile grid
  probs <- seq(0, 1, length.out = 200)
  q_11 <- quantile(iate_11, probs, na.rm = TRUE)
  q_00 <- quantile(iate_00, probs, na.rm = TRUE)
  q_01 <- quantile(iate_01, probs, na.rm = TRUE)
  
  # Determine x-axis range
  xlim_range <- range(c(q_11, q_00, q_01))
  
  # Plot τ⟨1|1⟩
  plot(probs, q_11,
       type = "l", col = "blue", lwd = 2,
       main = "True Quantile Functions of IATEs by Group",
       xlab = "Quantile",
       ylab = "Individual Treatment Effect",
       ylim = xlim_range)
  
  # Add others
  lines(probs, q_00, col = "red", lwd = 2)
  lines(probs, q_01, col = "darkgreen", lwd = 2)
  
  # Legend
  legend("topleft", 
         legend = c(expression(tau["⟨1|1⟩"]), 
                    expression(tau["⟨0|0⟩"]),
                    expression(tau["⟨0|1⟩"] ~ "(Counterfactual)")),
         col = c("blue", "red", "darkgreen"), 
         lty = 1, lwd = 2, bty = "n", cex = 0.9)
  
  # Grid
  grid()
}

