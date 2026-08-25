library(tidyverse)

source('code_rewrite/simulation/results/evaluation_function.R')

# evaluaiton of constnat structural effect
res_2500= readRDS('code_rewrite/simulation/results/constant_SE/NL_2500.rds')
res_10000= readRDS('code_rewrite/simulation/results/constant_SE/NL_10000.rds')
res_40000= readRDS('code_rewrite/simulation/results/constant_SE/NL_40000.rds')


latex_sbs = generate_side_by_side_latex(res_2500, res_10000, res_40000)


cat(latex_sbs)


latex = generate_stacked_threepart_latex(res_2500, res_10000, res_40000)

cat(latex$structure)

cat(latex$total)

cat(latex$composition)










# evaluaiton of funcitonal structural effect
res_2500= readRDS('code_rewrite/simulation/results/functional_SE/NL_fun_se_2500.rds')
res_10000= readRDS('code_rewrite/simulation/results/functional_SE/NL_fun_se_10000.rds')
res_40000= readRDS('code_rewrite/simulation/results/functional_SE/NL_fun_se_40000.rds')



latex = generate_stacked_threepart_latex(res_2500, res_10000, res_40000)

cat(latex$structure)

cat(latex$total)

cat(latex$composition)