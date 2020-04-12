learning_simulations <- function(task = "cor_effect", n_reps = 10, model_list = c("model1"), grid_size = 5, intercept = F) {
  # Task setup
  task_data <- task_data_setup(task = task, n_reps = n_reps, intercept = intercept)

  # Setup for running simulations
  n_models <- length(model_list)
  sim_grid <- list() # list of simulation data grids (one for each model)
  if(task_data$task_type == "within") {
    multi_sim_function <- multi_sim_within
  }
  if(task_data$task_type == "between") {
    multi_sim_function <- multi_sim_between
  }
  model_function_list <- list(model1, model2, model3, model4, model5, EXIT)
  names(model_function_list) <- c("model1", "model2", "model3", "model4", "model5", "EXIT")

  # Simulate each model in turn
  for(i in 1:n_models) {
    model <- model_list[i] # name of current model to run
    model_function <- model_function_list[[model]] # function for simulating current model
    sim_grid[[i]] <- sim_grid_setup(model = model,
                                    grid_size = grid_size,
                                    task_data = task_data) # set up simulation data grid for current model
    n_grid_rows <- nrow(sim_grid[[i]])
    n_pars <- length(which(colnames(sim_grid[[i]]) %in% c("lambda", "mu", "rho", "p", "C", "phi", "a0"))) # number of parameters for current model
    for(j in 1:n_grid_rows) {
      pars <- as.numeric(sim_grid[[i]][j, 1:n_pars]) # current parameter values

      print(model)
      pct_done <- round(100*j/n_grid_rows, 1)
      print(paste(pct_done, "%"))
      print(pars)

      sim_grid[[i]][j, -c(1:n_pars)] <- multi_sim_function(task_data = task_data,
                                                     model_function = model_function,
                                                     pars = pars)
    }
    write.csv(sim_grid[[i]], file = paste(model, task, date()))
  }
  names(sim_grid) <- model_list

  # Assemble the maximum and minimum index for each model
  results <- list()
  for(i in 1:n_models) {
    dfi <- sim_grid[[i]] # simulation results for current model
    results[[i]] <- list()
    results[[i]]$max <- dfi[which.max(dfi$m), ]
    results[[i]]$min <- dfi[which.min(dfi$m), ]
  }
  print(results)
  names(results) <- model_list

  # Assemble data into a list for output
  output <- list(results = results, task = task, n_reps = n_reps, model_list = model_list, sim_grid = sim_grid, task_data = task_data)
  output
}
