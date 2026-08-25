
source("./simulation/dgp.R")
source("./simulation/simulation_engine.R")

# fixed structural effect change
delta_tau <- 0.5


# Set parameters
M <- 80  # number of iterations
quantiles <- seq(0.1, 0.9, 0.1)
B <- 50 # repeated sample splitting replications

# dgp details
n = 2500
# n= 10000
# n= 40000
p = 6


run_all_with_retry(
  indices = 1:M,
  B = B,
  quantiles = quantiles,
  n = n,
  p = p,
  linear_cate = FALSE,
  comp_effect = TRUE,
  shift_mode = "mult",
  delta_tau = delta_tau,
  outdir = "results/fixed"
)
