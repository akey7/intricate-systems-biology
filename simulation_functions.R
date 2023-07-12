library(dplyr)

initial_times_and_conditions <- function(
    length_minutes = 100, 
    step_size_minutes = 0.1, 
    x1_initial = 1., 
    x2_initial = 1., 
    x3_initial = 1., 
    x4_initial = 1., 
    x5_initial = 1.,
    input1_initial = 0.01,
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

turn_input2_off_and_on <- function(simulation_df, off_at_min, on_at_min) {
  timestep <- simulation_df[2, "t_minutes"] - simulation_df[1, "t_minutes"]
  off_index <- off_at_min / timestep
  on_index <- on_at_min / timestep
  before_cutoff <- simulation_df$input2[1:off_index - 1]
  after_cutoff <- simulation_df$input2[on_index:nrow(simulation_df)]
  downtime <- rep_len(0, length.out = on_index - off_index)
  new_input2 <- c(before_cutoff, downtime, after_cutoff)
  
  simulation_df %>%
    mutate(input2 = new_input2)
}
