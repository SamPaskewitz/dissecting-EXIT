multi_sim_between <- function(task_data, model_function, pars) {
  # Run simulations with all trial sequences using current parameter vector
  n_reps <- length(task_data$exp_list1)
  index_vals1 <- rep(0, n_reps)
  index_vals2 <- rep(0, n_reps)
  for(i in 1:n_reps) {
    sim_probs1 <- model_function(experiment = task_data$exp_list1[[i]], pars = pars)$sim_data$test_probs
    index_vals1[i] <- task_data$index_fun1(sim_probs1)
    sim_probs2 <- model_function(experiment = task_data$exp_list2[[i]], pars = pars)$sim_data$test_probs
    index_vals2[i] <- task_data$index_fun2(sim_probs2)
  }

  # Compute summary statistics of index_vals for each group
  m1 <- mean(index_vals1)
  s1 <- sd(index_vals1)
  m2 <- mean(index_vals2)
  s2 <- sd(index_vals2)
  m_dif <- m1 - m2
  s_pool <- (s1 + s2)/2
  d <- m_dif/s_pool
  if(s1 > 0.000001 & s2 > 0.000001) {
    t_test <- t.test(index_vals1, index_vals2, paired = F)
    l_conf <- t_test$conf.int[1]
    u_conf <- t_test$conf.int[2]
    t_stat <- t_test$statistic
  }
  else {
    l_conf <- m1 - m2
    u_conf <- m1 - m2
    t_stat <- NA
  }

  # Gather output
  output <- c(m1, s1, m2, s2, m_dif, s_pool, d, l_conf, u_conf, t_stat)
  names(output) <- c("m1", "s1", "m2", "s2", "m_dif", "s_pool", "d", "l_conf", "u_conf", "t_stat")
  return(output)
}
