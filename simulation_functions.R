library(dplyr)

initial_times_and_conditions <- function(
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

turn_input2_off_and_on <- function(simulation_df, off_at_min, on_at_min, cutoff_level = 0.0) {
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

