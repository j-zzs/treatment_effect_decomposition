generate_stacked_threepart_latex <- function(res_2500, res_10000, res_40000, components = c("structure", "composition", "total")) {
  library(dplyr)
  
  format_two_decimals <- function(x) {
    if (is.numeric(x)) {
      return(formatC(x, format = "f", digits = 2))
    } else {
      return(x)
    }
  }
  
  create_table <- function(comp) {
    df_list <- list(
      `2500` = res_2500$summaries[[comp]],
      `10000` = res_10000$summaries[[comp]],
      `40000` = res_40000$summaries[[comp]]
    )
    
    # Round all numeric columns to 2 decimals
    df_list <- lapply(df_list, function(df) {
      df %>% mutate(across(where(is.numeric), format_two_decimals))
    })
    
    ncol_table <- 7  # Quantile + Bias + MSE + SD + cov_80 + cov_90 + cov_95
    caption_text <- paste0("Simulation results for ", comp, " effect")
    
    latex <- paste0("\\begin{table}[htbp]\n",
                    "\\centering\n",
                    "\\begin{threeparttable}\n",
                    "\\caption{", caption_text, "}\n",
                    "\\begin{tabular}{", paste(rep("c", ncol_table), collapse=" "), "}\n",
                    "\\toprule\n")
    
    # First header row
    latex <- paste0(latex,
                    "Quantile & Bias & MSE & SD & \\multicolumn{3}{c}{Coverage} \\\\\n")
    # Second header row for coverage
    latex <- paste0(latex,
                    " &  &  &  & 80\\% & 90\\% & 95\\% \\\\\n",
                    "\\midrule\n")
    
    # Loop over sample sizes
    for(nm in names(df_list)) {
      latex <- paste0(latex, "\\multicolumn{", ncol_table, "}{c}{\\textbf{Sample size: ", nm, "}} \\\\\n")
      latex <- paste0(latex, "\\midrule\n")
      df <- df_list[[nm]]
      for(i in 1:nrow(df)) {
        latex <- paste0(latex,
                        df$Quantile[i], " & ",
                        df$Bias[i], " & ",
                        df$MSE[i], " & ",
                        df$SD[i], " & ",
                        df$cov_80[i], " & ",
                        df$cov_90[i], " & ",
                        df$cov_95[i], " \\\\\n")
      }
      latex <- paste0(latex, "\\midrule\n")
    }
    
    latex <- paste0(latex, "\\bottomrule\n",
                    "\\end{tabular}\n",
                    "\\begin{tablenotes}\n",
                    "\\item[] \n",  # Empty notes
                    "\\end{tablenotes}\n",
                    "\\end{threeparttable}\n",
                    "\\end{table}\n")
    
    return(latex)
  }
  
  tables <- lapply(components, create_table)
  names(tables) <- components
  return(tables)
}

generate_side_by_side_latex <- function(res_2500, res_10000, res_40000) {
  library(dplyr)
  
  format_two_decimals <- function(x) {
    if (is.numeric(x)) {
      return(formatC(x, format = "f", digits = 2))
    } else {
      return(x)
    }
  }
  
  create_side_table <- function() {
    comps <- c("structure", "composition")
    
    # Collect data for both structure and composition
    df_list <- lapply(comps, function(comp) {
      list(
        `2500` = res_2500$summaries[[comp]],
        `10000` = res_10000$summaries[[comp]],
        `40000` = res_40000$summaries[[comp]]
      )
    })
    names(df_list) <- comps
    
    # Apply consistent formatting (2 decimals)
    df_list <- lapply(df_list, function(comp_list) {
      lapply(comp_list, function(df) {
        df %>% mutate(across(where(is.numeric), format_two_decimals))
      })
    })
    
    ncol_table <- 13  # Quantile + 6 for structure + 6 for composition
    caption_text <- "Simulation results: Structure vs Composition effects"
    
    latex <- paste0("\\begin{table}[htbp]\n",
                    "\\centering\n",
                    "\\scriptsize\n",
                    "\\begin{threeparttable}\n",
                    "\\caption{", caption_text, "}\n",
                    "\\begin{tabular}{", paste(rep("c", ncol_table), collapse=" "), "}\n",
                    "\\toprule\n")
    
    # First header row
    latex <- paste0(latex,
                    " & \\multicolumn{6}{c}{Structure} & \\multicolumn{6}{c}{Composition} \\\\\n")
    # Second header row
    latex <- paste0(latex,
                    "Quantile & Bias & MSE & SD & 80\\% & 90\\% & 95\\% & ",
                    "Bias & MSE & SD & 80\\% & 90\\% & 95\\% \\\\\n",
                    "\\midrule\n")
    
    # Loop over sample sizes
    for(nm in names(df_list[[1]])) {
      latex <- paste0(latex, "\\multicolumn{", ncol_table, "}{c}{\\textbf{Sample size: ", nm, "}} \\\\\n")
      latex <- paste0(latex, "\\midrule\n")
      
      df_s <- df_list$structure[[nm]]
      df_c <- df_list$composition[[nm]]
      
      for(i in 1:nrow(df_s)) {
        latex <- paste0(latex,
                        df_s$Quantile[i], " & ",
                        df_s$Bias[i], " & ",
                        df_s$MSE[i], " & ",
                        df_s$SD[i], " & ",
                        df_s$cov_80[i], " & ",
                        df_s$cov_90[i], " & ",
                        df_s$cov_95[i], " & ",
                        df_c$Bias[i], " & ",
                        df_c$MSE[i], " & ",
                        df_c$SD[i], " & ",
                        df_c$cov_80[i], " & ",
                        df_c$cov_90[i], " & ",
                        df_c$cov_95[i], " \\\\\n")
      }
      latex <- paste0(latex, "\\midrule\n")
    }
    
    latex <- paste0(latex, "\\bottomrule\n",
                    "\\end{tabular}\n",
                    "\\begin{tablenotes}\n",
                    "\\item[] \n",
                    "\\end{tablenotes}\n",
                    "\\end{threeparttable}\n",
                    "\\end{table}\n")
    
    return(latex)
  }
  
  return(create_side_table())
}


