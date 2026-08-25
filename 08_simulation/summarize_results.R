#############################################
# summarize_results.R
#############################################

library(tidyverse)


source("./simulation/simulation_engine.R")



#############################################
# Choose design
#############################################

n = 2500

#design <- "fixed"
design <- "covariate"

result_dir <- file.path("./simulation/results", design)

#############################################
# Load simulation results
#############################################

results <- list.files(
  result_dir,
  pattern = paste0("^", n, "_result_\\d+\\.rds$"),
  full.names = TRUE
) |>
  lapply(readRDS)

#############################################
# Remove failed runs
#############################################

results <- results[
  !sapply(results, function(x)
    inherits(x, "sim_error"))
]

cat("Successful runs:", length(results), "\n")

#############################################
# Extract truth
#############################################

true_struc <- extract_matrix(results, "true_structure")
true_total <- extract_matrix(results, "true_total")
true_comp  <- extract_matrix(results, "true_composition")

#############################################
# Extract estimates
#############################################

structure_mat  <- extract_matrix(results, "structure")
composition_mat <- extract_matrix(results, "composition")
total_mat <- extract_matrix(results, "total")

#############################################
# Extract standard errors
#############################################

structure_ses <- extract_matrix(results, "structure_se")
composition_ses <- extract_matrix(results, "composition_se")
total_ses <- extract_matrix(results, "total_se")

#############################################
# Extract IATE quantities
#############################################

true_iate_q <- extract_matrix(results, "true_iate_q")
true_iate_G0_q <- extract_matrix(results, "true_iate_G0_q")
true_iate_G1_q <- extract_matrix(results, "true_iate_G1_q")

IATE_q <- extract_matrix(results, "IATE_q")
IATE_G0_q <- extract_matrix(results, "IATE_G0_q")
IATE_G1_q <- extract_matrix(results, "IATE_G1_q")

#############################################
# Compute summary tables
#############################################

quantiles <- seq(0.1, 0.9, 0.1)

structure_summary <- compute_summary(
  structure_mat,
  structure_ses,
  true_struc,
  quantiles
)

composition_summary <- compute_summary(
  composition_mat,
  composition_ses,
  true_comp,
  quantiles
)

total_summary <- compute_summary(
  total_mat,
  total_ses,
  true_total,
  quantiles
)

#############################################
# Save csv tables
#############################################

write.csv(
  structure_summary,
  file.path(result_dir, paste0(n, "_structure_summary.csv")),
  row.names = FALSE
)

write.csv(
  composition_summary,
  file.path(result_dir, paste0(n, "_composition_summary.csv")),
  row.names = FALSE
)

write.csv(
  total_summary,
  file.path(result_dir, paste0(n, "_total_summary.csv")),
  row.names = FALSE
)

#############################################
# Save paper-ready object
#############################################

results_summary <- list(
  true_effects = list(
    structure = true_struc,
    composition = true_comp,
    total = true_total
  ),
  estimates = list(
    structure = structure_mat,
    composition = composition_mat,
    total = total_mat
  ),
  ses = list(
    structure = structure_ses,
    composition = composition_ses,
    total = total_ses
  ),
  summaries = list(
    structure = structure_summary,
    composition = composition_summary,
    total = total_summary
  ),
  cate = list(
    true_q = true_iate_q,
    true_G0_q = true_iate_G0_q,
    true_G1_q = true_iate_G1_q,
    IATE_q = IATE_q,
    IATE_G0_q = IATE_G0_q,
    IATE_G1_q = IATE_G1_q
  )
)

saveRDS(
  results_summary,
  file.path(result_dir, paste0(n, "_summary_results.rds"))
)

cat("Finished summarising", design, "design.\n")