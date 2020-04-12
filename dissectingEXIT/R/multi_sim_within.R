multi_sim_within <- function(task_data, model_function, pars) {
  # Run simulations with all trial sequences using current parameter vector
  n_reps <- length(task_data$exp_list)
  index_vals <- rep(0, n_reps)
  for(i in 1:n_reps) {
    sim_probs <- model_function(experiment = task_data$exp_list[[i]], pars = pars)$sim_data$test_probs
    index_vals[i] <- task_data$index_fun(sim_probs)
  }

  # Compute summary statistics of index_vals
  m <- mean(index_vals)
  s <- sd(index_vals)
  if(s > 0.000001) {
    t_test <- t.test(index_vals)
    l_conf <- t_test$conf.int[1]
    u_conf <- t_test$conf.int[2]
    t_stat <- t_test$statistic
  }
  else {
    l_conf <- m
    u_conf <- m
    t_stat <- NA
  }

  # Gather output
  output <- c(m, s, l_conf, u_conf, t_stat)
  output
}
