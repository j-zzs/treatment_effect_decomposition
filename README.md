# treatment_effect_decomposition

This code reproduces the results in **“Uncovering Sources of Heterogeneity in the Effects of Maternal Smoking on Infants' Health at Birth.”**

The code is organized into separate folders corresponding to the main steps of the analysis. The scripts should be run in the order described below.

## 0. Raw data

The raw data are available from:

**https://data.nber.org/nvss/natality/csv/**

Download the data for **2011–2018** and place the files in the `raw_data` folder.

## 1. Data preparation

Folder: `01_data_preparation`

Run:

`prepare_raw_data.R`

This script cleans and prepares the raw data and saves the resulting analysis dataset in the `analysis_data` folder.

## 2. Data overview

Folder: `02_data_overview`

Run:

`summary_tables.R`

This script generates:

- Table 3
- Table C.1

Then run:

`overview_plots.R`

This script generates:

- Figure C.1
- Figure C.2

## 3. Conditional average treatment effects (CATE)

Folder: `03_CATE`

Run:

`plot_cate.R`

This script generates:

- Table 4
- Figure 1
- Figure E.1
- Figure E.11

## 4. Decomposition analysis

Folder: `04_decomposition`

Run:

`decomposition_call.R`

Before running the script, select the appropriate outcome as indicated in the file.

For each decomposition, the script produces:

- an `.RData` file containing the decomposition results
- a corresponding plot

If necessary, use the following scripts to rescale the plots:

- `rescale_plot.R`
- `rescale_plots_composition.R`
- `rescale_plots_structure.R`

These scripts are used to produce:

- Figures 2–10
- Figures E.2–E.10
- Figures E.12–E.20

## 5. Comparison with simpler methods

Folder: `05_simpler_methods`

Run:

`comparison_simpler_methods.R`

This script generates:

- Table E.1
- Table E.2

## 6. Robustness checks

Folder: `06_robustness`

Run:

`oster_bound.R`

This script generates:

- Table D.1

Then run:

`placebo_infection_outcomes.R`

This script generates:

- Table D.2

## 7. Policy exercise

Folder: `07_policy_exercise`

Run:

`sorting_costs.R`

This script generates:

- Figure F.1

## 8. Simulation study

Folder: `08_simulation`

Run:

`run_covariate_structural.R`

Select the desired sample size as indicated in the script before running it.

Then run:

`run_fixed_structural.R`

Again, select the desired sample size before running the script.

After all simulation runs have completed successfully, run:

`summarize_results.R`

This script summarizes the simulation results and generates the results reported in:

- Table 1
- Table 2
- Tables B.1–B.4