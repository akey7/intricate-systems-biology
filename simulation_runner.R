library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(readr)
library(doParallel)
library(here)
source("simulation_core.R")

# Setup Initial Conditions and Run Specs

initial_condition_df <- cross_initial_conditions(
  x1 = c(1),
  x2 = c(0.75, 1, 1.25),
  x3 = c(1),
  x4 = c(0.75, 1, 1.25),
  x5 = c(1),
  h42 = c(0.5, 0.75, 1),
  input2_off_at_min = c(10, 20, 30),
  input2_on_at_min = c(40, 50, 60)
)

# run_specs <- create_run_specs(initial_condition_df)
run_specs <- create_run_specs(head(initial_condition_df))

# Use doParallel to Create All Plots

cl <- makeCluster(3, type='PSOCK')
registerDoParallel(cl)

results <- foreach(i=1:length(run_specs), .packages = "tidyverse") %dopar% {
  initial_conditions_run_and_plot(
    run_name = run_specs[[i]][["run_name"]],
    x1_initial = run_specs[[i]][["ics"]][[1]],
    x2_initial = run_specs[[i]][["ics"]][[2]],
    x3_initial = run_specs[[i]][["ics"]][[3]],
    x4_initial = run_specs[[i]][["ics"]][[4]],
    x5_initial = run_specs[[i]][["ics"]][[5]],
    h42 = run_specs[[i]][["h42"]],
    input2_off_at_min = run_specs[[i]][["input2_off_at_min"]],
    input2_on_at_min = run_specs[[i]][["input2_on_at_min"]]
  )
}

registerDoSEQ()

# Write star schema csvs

star_schema_dfs <- star_schema(results)
write_csv(star_schema_dfs[["run_timeseries_wide"]], here("output", "run_timeseries_wide.csv"), col_names = TRUE, na = "")
write_csv(star_schema_dfs[["run_timeseries_long"]], here("output", "run_timeseries_long.csv"), col_names = TRUE, na = "")
write_csv(star_schema_dfs[["run_metadata"]], here("output", "run_metadata.csv"), col_names = TRUE, na = "")

# Set theme for the plots

theme_set(
  theme_linedraw() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 9)
    )
)

# Render the plots as pngs

write_plots(results)

