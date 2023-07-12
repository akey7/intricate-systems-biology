initial_times_and_conditions <- function(
    length_minutes = 10, 
    step_size_minutes = 0.1, 
    x1_initial = 1., 
    x2_initial = 1., 
    x3_initial = 1., 
    x4_initial = 1., 
    x5_initial = 1.,
    input1_initial = 0.01,
    input2_initial = 0.99
  ) {
  t_minutes <- seq(0, 10, by = step_size_minutes)
  
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
