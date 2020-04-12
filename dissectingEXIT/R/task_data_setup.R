task_data_setup <- function(task = "cor_effect", n_reps = 5, intercept = F) {
  # Task setup
  if(task == "cor_effect") {
    experiment <- cor_effect
    task_type <- "within"
    index_fun <- function(sim_probs) {(sim_probs["cat 3", "A + C"] + sim_probs["cat 4", "B + D"]) - (sim_probs["cat 3", "V + X"] + sim_probs["cat 4", "W + Y"])}
    n <- ifelse(intercept == F, 2, 3)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "cor_effect (configural)") {
    experiment <- function() {add_configural(cor_effect())}
    task_type <- "within"
    index_fun <- function(sim_probs) {(sim_probs["cat 3", "A + C"] + sim_probs["cat 4", "B + D"]) - (sim_probs["cat 3", "V + X"] + sim_probs["cat 4", "W + Y"])}
    n <- ifelse(intercept == F, 3, 4)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "cor_effect (old vs new)") {
    experiment1 <- function() {add_configural(cor_effect_oldnew(pairs = "old"))}
    experiment2 <- function() {add_configural(cor_effect_oldnew(pairs = "new"))}
    task_type <- "between"
    index_fun1 <- function(sim_probs) {(sim_probs["cat 3", "A + C"] + sim_probs["cat 4", "B + D"]) - (sim_probs["cat 3", "V + X"] + sim_probs["cat 4", "W + Y"])}
    index_fun2 <- index_fun1
    n <- ifelse(intercept == F, 3, 4)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "ibre") {
    experiment <- ibre
    task_type <- "within"
    index_fun <- function(sim_probs) {(sim_probs["R1", "PC1 + PR1"] - sim_probs["C1", "PC1 + PR1"]) + (sim_probs["R2", "PC2 + PR2"] - sim_probs["C2", "PC2 + PR2"])}
    n <- ifelse(intercept == F, 2, 3)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "ibre (cofed)") {
    experiment <- ibre
    task_type <- "within"
    index_fun <- function(sim_probs) {
      index_ibre <- (sim_probs["R1", "PC1 + PR1"] - sim_probs["C1", "PC1 + PR1"]) + (sim_probs["R2", "PC2 + PR2"] - sim_probs["C2", "PC2 + PR2"])
      index_single <- (sim_probs["C1", "PC1"] - sim_probs["R1", "PR1"]) + (sim_probs["C2", "PC2"] - sim_probs["R2", "PR2"])
      index_cofed <- index_ibre*ifelse(index_single > 0, 1, 0)

      return(index_cofed)
    }
    n <- ifelse(intercept == F, 2, 3)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "ibre (configural)") {
    experiment <- function() {add_configural(ibre())}
    task_type <- "within"
    index_fun <- function(sim_probs) {(sim_probs["R1", "PC1 + PR1"] - sim_probs["C1", "PC1 + PR1"]) + (sim_probs["R2", "PC2 + PR2"] - sim_probs["C2", "PC2 + PR2"])}
    n <- ifelse(intercept == F, 3, 4)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "ibre (shared cues)") {
    experiment <- ibre_shared
    task_type <- "within"
    # The index reflects a base rate effect for disjoined categories conditional on there being an IBRE when there's a common cue.
    index_fun <- function(sim_probs) {index_shared <- sim_probs["R", "PC + PR"] - sim_probs["C", "PC + PR"]
                                      index_disjoined <- (sim_probs["DC", "DC1 + DR1"] - sim_probs["DR", "DC1 + DR1"]) + (sim_probs["DC", "DC2 + DR2"] - sim_probs["DR", "DC2 + DR2"])
                                      index_output <- index_disjoined*ifelse(index_shared > 0, 1, 0)
                                      return(index_output)}
    n <- ifelse(intercept == F, 2, 3)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "highlighting") {
    experiment <- highlighting
    task_type <- "within"
    index_fun <- function(sim_probs) {(sim_probs["L1", "PE1 + PL1"] - sim_probs["E1", "PE1 + PL1"]) + (sim_probs["L2", "PE2 + PL2"] - sim_probs["E2", "PE2 + PL2"])}
    n <- ifelse(intercept == F, 2, 3)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "blocked_cues") {
    experiment <- blocked_cues
    task_type <- "within"
    index_fun <- function(sim_probs) {
      index <- (sim_probs["cat 3", "D + Q"] + sim_probs["cat 3", "H + K"] + sim_probs["cat 3", "D + K"] + sim_probs["cat 3", "H + Q"])
      index <- index - (sim_probs["cat 3", "B + T"] + sim_probs["cat 3", "F + N"] + sim_probs["cat 3", "B + N"] + sim_probs["cat 3", "F + T"])
      index
    }
    n <- ifelse(intercept == F, 2, 3)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "blocked_cues (configural)") {
    experiment <- function(){add_configural(blocked_cues())}
    task_type <- "within"
    index_fun <- function(sim_probs) {
      index <- (sim_probs["cat 3", "D + Q"] + sim_probs["cat 3", "H + K"] + sim_probs["cat 3", "D + K"] + sim_probs["cat 3", "H + Q"])
      index <- index - (sim_probs["cat 3", "B + T"] + sim_probs["cat 3", "F + N"] + sim_probs["cat 3", "B + N"] + sim_probs["cat 3", "F + T"])
      index
    }
    n <- ifelse(intercept == F, 3, 4)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "dif_cue_use") {
    experiment <- function() {add_configural(use_receptors(dif_cue_use(n_blocks = 10), n_points = 8, decay = 15))}
    task_type <- "within"
    index_fun <- function(sim_probs) {sim_probs["cat A", "5-6 (X)"] - sim_probs["cat A", "2-7 (W)"] + sim_probs["cat A", "7-2 (Z)"] - sim_probs["cat A", "6-5 (Y)"]}
    n <- ifelse(intercept == F, 20, 21)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "cond_cor_effect") {
    experiment1 <- function() {add_configural(cond_cor_effect(group = "c1"))}
    experiment2 <- function() {add_configural(cond_cor_effect(group = "c2"))}
    task_type <- "between"
    index_fun1 <- function(sim_probs) {sim_probs["cat 3", "A + X + I"] + sim_probs["cat 3", "A + Y + I"] - sim_probs["cat 3", "B + X + I"] - sim_probs["cat 3", "B + Y + I"]}
    index_fun2 <- function(sim_probs) {sim_probs["cat 3", "A + X + II"] + sim_probs["cat 3", "A + Y + II"] - sim_probs["cat 3", "B + X + II"] - sim_probs["cat 3", "B + Y + II"]}
    n <- ifelse(intercept == F, 6, 7)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "cond_cor_effect (with triplets)") {
    experiment1 <- function() {add_unique(add_configural(cond_cor_effect(group = "c1")))}
    experiment2 <- function() {add_unique(add_configural(cond_cor_effect(group = "c2")))}
    task_type <- "between"
    index_fun1 <- function(sim_probs) {sim_probs["cat 3", "A + X + I"] + sim_probs["cat 3", "A + Y + I"] - sim_probs["cat 3", "B + X + I"] - sim_probs["cat 3", "B + Y + I"]}
    index_fun2 <- function(sim_probs) {sim_probs["cat 3", "A + X + II"] + sim_probs["cat 3", "A + Y + II"] - sim_probs["cat 3", "B + X + II"] - sim_probs["cat 3", "B + Y + II"]}
    n <- ifelse(intercept == F, 6, 7)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "exclusive_or") {
    experiment <- exclusive_or
    task_type <- "within"
    index_fun <- function(sim_probs) {
      index <- (sim_probs["cat 1", "A + X + I"] + sim_probs["cat 1", "A + X + II"] + sim_probs["cat 1", "B + Y + I"] + sim_probs["cat 1", "B + Y + II"])
      index <- index - (sim_probs["cat 1", "A + Y + I"] + sim_probs["cat 1", "A + Y + II"] + sim_probs["cat 1", "B + X + I"] + sim_probs["cat 1", "B + X + II"])
      index
    }
    n <- ifelse(intercept == F, 6, 7)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }
  if(task == "exclusive_or_triplets") {
    experiment <- exclusive_or_triplets
    task_type <- "within"
    index_fun <- function(sim_probs) {
      index <- (sim_probs["cat 1", "A + X + I"] + sim_probs["cat 1", "A + X + II"] + sim_probs["cat 1", "B + Y + I"] + sim_probs["cat 1", "B + Y + II"])
      index <- index - (sim_probs["cat 1", "A + Y + I"] + sim_probs["cat 1", "A + Y + II"] + sim_probs["cat 1", "B + X + I"] + sim_probs["cat 1", "B + X + II"])
      index
    }
    n <- ifelse(intercept == F, 3, 4)
    a0 <- 1/n
    lambda_max <- 2/n - 0.01
  }

  # Generate trial sequences; return list of task data
  if(task_type == "within") {
    exp_list <- list()
    for(i in 1:n_reps) {
      exp_list[[i]] <- experiment()
      if(intercept == T) {
        exp_list[[i]] <- add_intercept(exp_list[[i]])
      }
    }
    task_data <- list(exp_list = exp_list, index_fun = index_fun, a0 = a0, lambda_max = lambda_max)
  }
  if(task_type == "between") {
    exp_list1 <- list()
    for(i in 1:n_reps) {
      exp_list1[[i]] <- experiment1()
      if(intercept == T) {
        exp_list1[[i]] <- add_intercept(exp_list1[[i]])
      }
    }
    exp_list2 <- list()
    for(i in 1:n_reps) {
      exp_list2[[i]] <- experiment2()
      if(intercept == T) {
        exp_list2[[i]] <- add_intercept(exp_list2[[i]])
      }
    }
    task_data <- list(exp_list1 = exp_list1, exp_list2 = exp_list2, index_fun1 = index_fun1, index_fun2 = index_fun2, a0 = a0, lambda_max = lambda_max)
  }
  task_data$task_type <- task_type

  task_data
}
