# dplyr, tidyr, ggplot must be loaded before these functions.

initial_conditions <- function(
    length_minutes = 100, 
    step_size_minutes = 0.01, 
    x1_initial = 1.0, 
    x2_initial = 1.0, 
    x3_initial = 1.0, 
    x4_initial = 1.0, 
    x5_initial = 1.0,
    input1_initial = 0.1,
    input2_initial = 0.99
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
    x5 = x5
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

run_euler <- function(simulation_df, h42 = 0.75) {
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
  
  for(i in 2:length(x1)) {
    input1 <- simulation_df[i, "input1"]
    input2 <- simulation_df[i, "input2"]

    x1[i] <- x1[i-1]+timestep*(input1+input2*x4[i-1]^0.5-x1[i-1]^0.5)
    x2[i] <- x2[i-1]+timestep*(x1[i-1]^0.5-x2[i-1]^0.75)
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

cross_initial_conditions <- function(x1, x2, x3, x4, x5) {
  x1_initial_df <- data.frame(x1 = x1)
  x2_initial_df <- data.frame(x2 = x2)
  x3_initial_df <- data.frame(x3 = x3)
  x4_initial_df <- data.frame(x4 = x4)
  x5_initial_df <- data.frame(x5 = x5)
  
  initial_condition_df <- crossing(
    x1_initial_df, 
    x2_initial_df, 
    x3_initial_df,
    x4_initial_df,
    x5_initial_df
  )
}

create_run_specs <- function(initial_condition_df) {
  run_specs <- vector(mode = "list", length = nrow(initial_condition_df))
  
  serial <- 1
  
  for (i in 1:nrow(initial_condition_df)) {
    initial_condition_vec <- vector(mode = "numeric", length = length(initial_condition_df))
    
    initial_condition_vec[1] <- initial_condition_df[i, "x1"]
    initial_condition_vec[2] <- initial_condition_df[i, "x2"]
    initial_condition_vec[3] <- initial_condition_df[i, "x3"]
    initial_condition_vec[4] <- initial_condition_df[i, "x4"]
    initial_condition_vec[5] <- initial_condition_df[i, "x5"]
    
    run_specs[[i]] <- vector(mode = "list", length = 2)
    run_specs[[i]][["ics"]] <- initial_condition_vec
    run_specs[[i]][["run_name"]] <- paste("run", serial, sep = "_")
    
    serial <- serial + 1
  }
  
  run_specs
}

initial_conditions_run_and_plot <- function(run_name, x1_initial, x2_initial, x3_initial, x4_initial, x5_initial) {
  simulation_df <- initial_conditions(
      x1_initial = x1_initial, 
      x2_initial = x2_initial, 
      x3_initial = x3_initial, 
      x4_initial = x4_initial, 
      x5_initial = x5_initial
    ) %>%
    turn_input2_off_and_on() %>%
    run_euler()
  
  simulation_plot_title <- paste(
    "ICs:",
    paste(
      x1_initial,
      x2_initial,
      x3_initial,
      x4_initial,
      x5_initial,
      sep = ", "
    ),
    sep = " "
  )
  
  simulation_plot_df <- simulation_df %>%
    select(-input1, -input2) %>%
    pivot_longer(-t_minutes, values_to = "concentration", names_to = "xi") %>%
    transmute(
      t_minutes,
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
  
  ylim_max <- simulation_plot_df %>%
    summarize(max_concentration = round(max(concentration))) %>%
    pull(max_concentration)
  
  friendly_df <- simulation_plot_df %>%
    pivot_wider(names_from = "metabolite", values_from = "concentration")
  
  simulation_plot <- simulation_plot_df %>%
    ggplot(aes(x = t_minutes, y = concentration, color = metabolite)) +
    geom_line(linewidth = 1) +
    ylim(-0.1, ylim_max) +
    labs(
      x = "t (minutes)",
      y = "concentration (au)",
      title = simulation_plot_title
    )
  
  list(friendly_df = friendly_df, simulation_plot = simulation_plot, run_name = run_name)
}

write_plots <- function(results_list) {
  walk(results_list, function(.x) {
    plot_filename <- here("output", "plots", paste(.x[["run_name"]], "png", sep = "."))
    ggsave(plot_filename, plot = .x[["simulation_plot"]], units = "in", dpi = 300, height = 3, width = 5)
    # print(paste("Wrote", plot_filename, sep = " "))
  })
}

combine_simulations_dfs <- function(results_list) {
  list_rbind(
    map(results_list, function(.x) {
      .x[["friendly_df"]] %>%
        mutate(run_name = .x[["run_name"]])
    })
  )
}
