# dplyr, tidyr, ggplot must be loaded before these functions.

initial_conditions <- function(
    length_minutes = 100, 
    step_size_minutes = 0.01, 
    x1_initial = 1.0, 
    x2_initial = 1.0, 
    x3_initial = 1.0, 
    x4_initial = 1.0, 
    x5_initial = 1.0,
    input1_initial = 0.025,
    input2_initial = 0.975,
    h42 = 0.75,
    gamma_1 = 1.0
  ) {
  t_minutes <- seq(0, length_minutes, by = step_size_minutes)
  
  x1 <- numeric(length(t_minutes))
  x2 <- numeric(length(t_minutes))
  x3 <- numeric(length(t_minutes))
  x4 <- numeric(length(t_minutes))
  x5 <- numeric(length(t_minutes))
  
  x1[1] <- x1_initial
  x2[1] <- x2_initial
  x3[1] <- x3_initial
  x4[1] <- x4_initial
  x5[1] <- x5_initial
  
  data.frame(
    t_minutes = t_minutes,
    input1 = rep(input1_initial, length(t_minutes)),
    input2 = rep(input2_initial, length(t_minutes)),
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4,
    x5 = x5,
    h42 = h42,
    gamma_1 = gamma_1
  )
}

turn_input2_off_and_on <- function(simulation_df, off_at_min = 10, on_at_min = 60, cutoff_level = 0.0) {
  timestep <- simulation_df[2, "t_minutes"] - simulation_df[1, "t_minutes"]
  off_index <- off_at_min / timestep
  on_index <- on_at_min / timestep
  before_cutoff <- simulation_df$input2[1:off_index - 1]
  after_cutoff <- simulation_df$input2[on_index:nrow(simulation_df)]
  downtime <- rep_len(cutoff_level, length.out = on_index - off_index)
  new_input2 <- c(before_cutoff, downtime, after_cutoff)

  simulation_df %>%
    mutate(input2 = new_input2)
}

run_euler <- function(simulation_df) {
  timestep <- simulation_df[2, "t_minutes"] - simulation_df[1, "t_minutes"]
  
  x1 <- rep(0, nrow(simulation_df))
  x2 <- rep(0, nrow(simulation_df))
  x3 <- rep(0, nrow(simulation_df))
  x4 <- rep(0, nrow(simulation_df))
  x5 <- rep(0, nrow(simulation_df))
  
  x1[1] <- simulation_df[1, "x1"]
  x2[1] <- simulation_df[1, "x2"]
  x3[1] <- simulation_df[1, "x3"]
  x4[1] <- simulation_df[1, "x4"]
  x5[1] <- simulation_df[1, "x5"]
  
  h42 <- simulation_df[1, "h42"]
  gamma_1 <- simulation_df[1, "gamma_1"]
  
  for(i in 2:length(x1)) {
    input1 <- simulation_df[i, "input1"]
    input2 <- simulation_df[i, "input2"]

    # x1[i] <- x1[i-1]+timestep*(input1+input2*x4[i-1]^0.5-x1[i-1]^0.5)
    # x2[i] <- x2[i-1]+timestep*(x1[i-1]^0.5-x2[i-1]^0.75)
    # x3[i] <- x3[i-1]+timestep*(2*x2[i-1]^0.75-2*x3[i-1]^0.4)
    # x4[i] <- x4[i-1]+timestep*(2*x3[i-1]^0.4-input2*x4[i-1]^0.5-x2[i-1]^h42*x4[i-1]^0.5)
    # x5[i] <- x5[i-1]+timestep*(x2[i-1]^h42*x4[i-1]^0.5-x5[i-1]^0.5)
    
    x1[i] <- x1[i-1]+timestep*(input1+input2*x4[i-1]^0.5-gamma_1*x1[i-1]^0.5)
    x2[i] <- x2[i-1]+timestep*(gamma_1*x1[i-1]^0.5-x2[i-1]^0.75)
    x3[i] <- x3[i-1]+timestep*(2*x2[i-1]^0.75-2*x3[i-1]^0.4)
    x4[i] <- x4[i-1]+timestep*(2*x3[i-1]^0.4-input2*x4[i-1]^0.5-x2[i-1]^h42*x4[i-1]^0.5)
    x5[i] <- x5[i-1]+timestep*(x2[i-1]^h42*x4[i-1]^0.5-x5[i-1]^0.5)
  }
  
  data.frame(
    t_minutes = simulation_df$t_minutes,
    x1 = x1, 
    x2 = x2, 
    x3 = x3, 
    x4 = x4, 
    x5 = x5, 
    input1 = simulation_df$input1, 
    input2 = simulation_df$input2
  )
}

cross_initial_conditions <- function(x1, x2, x3, x4, x5, h42, input2_off_at_min, input2_on_at_min, gamma_1) {
  x1_df <- data.frame(x1 = x1)
  x2_df <- data.frame(x2 = x2)
  x3_df <- data.frame(x3 = x3)
  x4_df <- data.frame(x4 = x4)
  x5_df <- data.frame(x5 = x5)
  h42_df <- data.frame(h42 = h42)
  input2_off_at_min_df <- data.frame(input2_off_at_min = input2_off_at_min)
  input2_on_at_min_df <- data.frame(input2_on_at_min = input2_on_at_min)
  gamma_1_df <- data.frame(gamma_1 = gamma_1)
  
  initial_condition_df <- crossing(
    x1_df, 
    x2_df, 
    x3_df,
    x4_df,
    x5_df,
    h42_df,
    input2_off_at_min_df,
    input2_on_at_min_df,
    gamma_1_df
  )
}

create_run_specs <- function(initial_condition_df) {
  run_specs <- vector(mode = "list", length = nrow(initial_condition_df))
  
  serial <- 1
  
  for (i in 1:nrow(initial_condition_df)) {
    initial_condition_vec <- numeric(5)
    initial_condition_vec[1] <- initial_condition_df[i, "x1"]
    initial_condition_vec[2] <- initial_condition_df[i, "x2"]
    initial_condition_vec[3] <- initial_condition_df[i, "x3"]
    initial_condition_vec[4] <- initial_condition_df[i, "x4"]
    initial_condition_vec[5] <- initial_condition_df[i, "x5"]
    
    run_specs[[i]] <- vector(mode = "list", length = 2)
    run_specs[[i]][["ics"]] <- initial_condition_vec
    run_specs[[i]][["run_name"]] <- paste("run", serial, sep = "_")
    run_specs[[i]][["h42"]] <- as.double(initial_condition_df[i, "h42"])
    run_specs[[i]][["input2_off_at_min"]] <- as.double(initial_condition_df[i, "input2_off_at_min"])
    run_specs[[i]][["input2_on_at_min"]] <- as.double(initial_condition_df[i, "input2_on_at_min"])
    run_specs[[i]][["gamma_1"]] <- as.double(initial_condition_df[i, "gamma_1"])
    
    serial <- serial + 1
  }
  
  run_specs
}

initial_conditions_run_and_plot <- function(run_name, x1_initial, x2_initial, x3_initial, x4_initial, x5_initial, h42, input2_off_at_min, input2_on_at_min, gamma_1) {
  simulation_df <- initial_conditions(
      x1_initial = x1_initial, 
      x2_initial = x2_initial, 
      x3_initial = x3_initial, 
      x4_initial = x4_initial, 
      x5_initial = x5_initial,
      h42 = h42,
      gamma_1 = gamma_1
    ) %>%
    turn_input2_off_and_on(off_at_min = input2_off_at_min, on_at_min = input2_on_at_min) %>%
    run_euler()
  
  simulation_df_long <- simulation_df %>%
    pivot_longer(starts_with("x"), values_to = "concentration", names_to = "xi") %>%
    transmute(
      t_minutes,
      input1,
      input2,
      concentration,
      "metabolite" = case_when(
        xi == "x1" ~ "G6P",
        xi == "x2" ~ "FBP",
        xi == "x3" ~ "3_PGA",
        xi == "x4" ~ "PEP",
        xi == "x5" ~ "pyruvate",
        TRUE ~ "unknown"  # This condition should never be reached
      )
    )
  
  simulation_df_wide <- simulation_df_long %>%
    pivot_wider(names_from = "metabolite", values_from = "concentration")
  
  simulation_plot <- simulation_df_long %>%
    ggplot(aes(x = t_minutes, y = concentration, color = metabolite)) +
    geom_line(linewidth = 1) +
    labs(
      x = "t (minutes)",
      y = "concentration (au)",
      title = run_name
    )
  
  list(
    simulation_df_wide = simulation_df_wide,
    simulation_df_long = simulation_df_long,
    simulation_plot = simulation_plot, 
    run_name = run_name,
    h42 = h42,
    gamma_1 = gamma_1,
    input2_off_at_min = input2_off_at_min,
    input2_on_at_min = input2_on_at_min
  )
}

write_plots <- function(results_list) {
  walk(results_list, function(.x) {
    plot_filename <- here("output", "plots", paste(.x[["run_name"]], "png", sep = "."))
    ggsave(plot_filename, plot = .x[["simulation_plot"]], units = "in", dpi = 300, height = 3, width = 5)
  })
}

star_schema <- function(results_list) {
  run_timeseries_wide <- list_rbind(
    map(results_list, function(.x) {
      .x[["simulation_df_wide"]] %>%
        mutate(run_name = .x[["run_name"]])
    })
  )
  
  run_timeseries_long <- list_rbind(
    map(results_list, function(.x) {
      .x[["simulation_df_long"]] %>%
        mutate(run_name = .x[["run_name"]])
    })
  )
  
  run_metadata <- list_rbind(
    map(results_list, function(.x) {
      .x[["simulation_df_wide"]] %>%
        arrange(t_minutes) %>%
        head(1) %>%
        transmute(
          run_name = .x[["run_name"]],
          h42 = .x[["h42"]],
          input2_off_at_min = .x[["input2_off_at_min"]],
          input2_on_at_min = .x[["input2_on_at_min"]],
          initial_G6P = G6P,
          intial_FBP = FBP,
          intial_3_PGA = `3_PGA`,
          initial_PEP = PEP,
          initial_pyruvate = pyruvate,
          gamma_1 = .x[["gamma_1"]]
        )
    })
  )
  
  list(
    run_timeseries_long = run_timeseries_long,
    run_timeseries_wide = run_timeseries_wide,
    run_metadata = run_metadata
  )
}

