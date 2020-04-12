sim_grid_setup <- function(model = "model1", grid_size = 5, task_data) {
  # Set up grid of possible parameter combinations
  if(model == "model1") {
    sim_grid <- expand.grid(lambda = seq(0.01, task_data$lambda_max, length.out = grid_size))
    sim_grid$phi <- rep(5, nrow(sim_grid))
  }
  if(model == "model2") {
    sim_grid <- expand.grid(lambda = seq(0.01, task_data$lambda_max, length.out = grid_size),
                           mu = seq(0.01, 0.99, length.out = grid_size))
    sim_grid$phi <- rep(5, nrow(sim_grid))
    sim_grid$a0 <- rep(task_data$a0, nrow(sim_grid))
  }
  if(model == "model3") {
    sim_grid <- expand.grid(lambda = seq(0.01, task_data$lambda_max, length.out = grid_size),
                           mu = seq(0.01, 0.99, length.out = grid_size),
                           rho = seq(0.01, 7.5, length.out = grid_size))
    sim_grid$phi <- rep(5, nrow(sim_grid))
    sim_grid$a0 <- rep(task_data$a0, nrow(sim_grid))
  }
  if(model == "model4") {
    sim_grid <- expand.grid(lambda = seq(0.01, task_data$lambda_max, length.out = grid_size),
                           mu = seq(0.01, 0.99, length.out = grid_size),
                           p = seq(1, 10, length.out = grid_size))
    sim_grid$phi <- rep(5, nrow(sim_grid))
  }
  if(model == "model5") {
    sim_grid <- expand.grid(lambda = seq(0.01, task_data$lambda_max, length.out = grid_size),
                           mu = seq(0.01, 0.99, length.out = grid_size),
                           rho = seq(0.01, 7.5, length.out = grid_size),
                           p = seq(1, 10, length.out = grid_size))
    sim_grid$phi <- rep(5, nrow(sim_grid))
  }
  if(model == "EXIT") {
    sim_grid <- expand.grid(lambda = seq(0.01, task_data$lambda_max, length.out = grid_size),
                           mu = seq(0.01, 0.99, length.out = grid_size),
                           rho = seq(0.01, 7.5, length.out = grid_size),
                           p = seq(1, 10, length.out = grid_size),
                           C = seq(0.001, 5, length.out = grid_size))
    sim_grid$phi <- rep(5, nrow(sim_grid))
  }

  if(task_data$task_type == "within") {
    sim_grid$m <- rep(0, nrow(sim_grid))
    sim_grid$s <- rep(0, nrow(sim_grid))
    sim_grid$l_conf <- rep(0, nrow(sim_grid))
    sim_grid$u_conf <- rep(0, nrow(sim_grid))
    sim_grid$t_stat <- rep(0, nrow(sim_grid))
  }
  if(task_data$task_type == "between") {
    sim_grid$m1 <- rep(0, nrow(sim_grid))
    sim_grid$s1 <- rep(0, nrow(sim_grid))
    sim_grid$m2 <- rep(0, nrow(sim_grid))
    sim_grid$s2 <- rep(0, nrow(sim_grid))
    sim_grid$m_dif <- rep(0, nrow(sim_grid))
    sim_grid$s_pool <- rep(0, nrow(sim_grid))
    sim_grid$d <- rep(0, nrow(sim_grid))
    sim_grid$l_conf <- rep(0, nrow(sim_grid))
    sim_grid$u_conf <- rep(0, nrow(sim_grid))
    sim_grid$t_stat <- rep(0, nrow(sim_grid))
  }

  sim_grid
}
