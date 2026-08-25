library(glue)
library(dplyr)
library(broom)
library(purrr)
library(robomit)
library(tidyverse)

compute_oster_results <- function(outcomes, treatment, controls, data, id = "none", time = "none") {
  results <- list()
  
  for (y in outcomes) {
    # Raw model
    formula_raw <- as.formula(glue("{y} ~ {treatment}"))
    model_raw <- lm(formula_raw, data = data)
    
    # Controlled model
    formula_ctrl <- as.formula(glue("{y} ~ {treatment} + {controls}"))
    model_ctrl <- lm(formula_ctrl, data = data)
    
    # Rmax for Oster
    rmax <- 1.3 * summary(model_ctrl)$r.squared
    
    # Oster bounds (delta = 1)
    bound <- o_beta(
      y = y, x = treatment, con = controls, w = NULL,
      id = id, time = time, delta = 1,
      R2max = rmax, type = "lm", data = data
    )
    
    # Extract stats
    coef_raw  <- coef(summary(model_raw))[treatment, "Estimate"]
    se_raw    <- coef(summary(model_raw))[treatment, "Std. Error"]
    r2_raw    <- summary(model_raw)$r.squared
    
    coef_ctrl <- coef(summary(model_ctrl))[treatment, "Estimate"]
    se_ctrl   <- coef(summary(model_ctrl))[treatment, "Std. Error"]
    r2_ctrl   <- summary(model_ctrl)$r.squared
    
    beta_star <- bound$Value[bound$Name == "beta*"]
    
    results[[y]] <- list(
      raw = list(beta = coef_raw, se = se_raw, r2 = r2_raw),
      ctrl = list(beta = coef_ctrl, se = se_ctrl, r2 = r2_ctrl),
      oster = list(beta_star = beta_star)
    )
  }
  
  return(results)
}

oster_results_to_latex <- function(results, caption = "Bias-adjusted estimates for different outcomes", label = "tab:oster_summary", file = NULL) {
  table_header <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    glue("\\caption{{{caption}}}"),
    glue("\\label{{{label}}}"),
    "\\begin{tabular}{lccc}",
    "\\toprule",
    "Outcome & Raw & Controlled & Oster $\\beta^*$ \\\\",
    " & (SE) [$R^2$] & (SE) [$R^2$] &  \\\\",
    "\\midrule"
  )
  
  table_rows <- purrr::map_chr(names(results), function(outcome) {
    r <- results[[outcome]]
    
    line1 <- glue("{outcome} & ",
                  "{sprintf('%.4f', r$raw$beta)} & ",
                  "{sprintf('%.4f', r$ctrl$beta)} & ",
                  "{sprintf('%.4f', r$oster$beta_star)} \\\\")
    
    line2 <- glue(" & ({sprintf('%.4f', r$raw$se)}) [{sprintf('%.2f', r$raw$r2)}] & ",
                  "({sprintf('%.4f', r$ctrl$se)}) [{sprintf('%.2f', r$ctrl$r2)}] & \\\\")
    
    paste(line1, line2, sep = "\n")
  })
  
  table_footer <- c(
    "\\bottomrule",
    "\\end{tabular}",
    "\\vspace{0.2cm}",
    "\\footnotesize Notes: Each row corresponds to a different dependent variable. Columns report the treatment coefficient from the raw regression (without controls), the controlled regression, and the bias-adjusted coefficient ($\\beta^*$) using the Oster method with $\\delta = 1$ and $R_{\\max} = \\min\\{1.3R^2_{\\text{controlled}}, 1\\}$.",
    "\\end{table}"
  )
  
  full_table <- paste(c(table_header, table_rows, table_footer), collapse = "\n\n")
  
  if (!is.null(file)) {
    writeLines(full_table, con = file)
    message(glue("LaTeX table written to: {file}"))
  }
  
  return(full_table)
}

source("./data_preparation/load_data.R")


# load prepared + recoded data
data = combine_data(years = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))
setDF(data)

outcomes = c("birwt", "bw_standardized", "apgar5", "gestat", "LBW")
treatment = "tobacco"
controls <- "sex + wtgain + mage + meduc + mrace + ormoth + mar + paternity + fage + feduc + frace + orfath + monpre + nprevis + priorterm + dlivord + wic + prepreg_diabetes + gest_diabetes + prepreg_hypertension + gest_hypertension + preterm + infertility_treatment + fertility_drugs + reproductive_assistance + cesarean + cesarean_n + payment + bmi + interval_last_livebirth + interval_last_otherbirth + mothers_height + birmon + birthplace + chlamydia + gonorrhea + syphilis + hepatitis_c + hepatitis_b"


# Get results
oster_results <- compute_oster_results(outcomes, treatment, controls, data = data)

# Write LaTeX table
oster_results_to_latex(oster_results, file = "./robustness/output/oster_summary.tex")

