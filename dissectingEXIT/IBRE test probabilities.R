ibre_probs <- list()
sim_exp <- ibre()
ibre_probs$model3 <- model3(sim_exp, pars = c(0.99, 0.74, 5.63, 5, 0.5))$sim_data$test_probs
ibre_probs$model4 <- model4(sim_exp, pars = c(0.26, 0.99, 1, 5))$sim_data$test_probs
ibre_probs$EXIT <- EXIT(sim_exp, pars = c(0.99, 0.99, 7.50, 1, 0.001, 5))$sim_data$test_probs
for(i in 1:50) {
  sim_exp <- ibre()
  ibre_probs$model3 <- ibre_probs$model3 + model3(sim_exp, pars = c(0.99, 0.74, 5.63, 5, 0.5))$sim_data$test_probs
  ibre_probs$model4 <- ibre_probs$model4 + model4(sim_exp, pars = c(0.26, 0.99, 1, 5))$sim_data$test_probs
  ibre_probs$EXIT <- ibre_probs$EXIT + EXIT(sim_exp, pars = c(0.99, 0.99, 7.50, 1, 0.001, 5))$sim_data$test_probs
}
ibre_probs$model3 <- t(ibre_probs$model3)/50
ibre_probs$model4 <- t(ibre_probs$model4)/50
ibre_probs$EXIT <- t(ibre_probs$EXIT)/50
rm(list = c("sim_exp", "i"))

# Combine test trials
ibre_probs$model3 <- (ibre_probs$model3[1:9, 1:4] + ibre_probs$model3[10:18, c(3, 4, 1, 2)])/2
rownames(ibre_probs$model3) <- c("I", "PC", "PR", "PC + PR", "I + PC + PR", "I + PCo", "I + PRo", "PC + PRo", "I + PC + PRo")
colnames(ibre_probs$model3) <- c("C", "R", "Co", "Ro")
ibre_probs$model4 <- (ibre_probs$model4[1:9,] + ibre_probs$model4[10:18, c(3, 4, 1, 2)])/2
rownames(ibre_probs$model4) <- c("I", "PC", "PR", "PC + PR", "I + PC + PR", "I + PCo", "I + PRo", "PC + PRo", "I + PC + PRo")
colnames(ibre_probs$model4) <- c("C", "R", "Co", "Ro")
ibre_probs$EXIT <- (ibre_probs$EXIT[1:9,] + ibre_probs$EXIT[10:18, c(3, 4, 1, 2)])/2
rownames(ibre_probs$EXIT) <- c("I", "PC", "PR", "PC + PR", "I + PC + PR", "I + PCo", "I + PRo", "PC + PRo", "I + PC + PRo")
colnames(ibre_probs$EXIT) <- c("C", "R", "Co", "Ro")

# Round
ibre_probs$model3 <- round(ibre_probs$model3, 3)
ibre_probs$model4 <- round(ibre_probs$model4, 3)
ibre_probs$EXIT <- round(ibre_probs$EXIT, 3)

# Actual data from Kruschke 1996, Experiment 1
ibre_probs$empirical <- matrix(0, nrow = 9, ncol = 4)
rownames(ibre_probs$empirical) <- c("I", "PC", "PR", "PC + PR", "I + PC + PR", "I + PCo", "I + PRo", "PC + PRo", "I + PC + PRo")
colnames(ibre_probs$empirical) <- c("C", "R", "Co", "Ro")
ibre_probs$empirical["I", 1:4] <- c(0.746, 0.174, 0.049, 0.031)
ibre_probs$empirical["PC", 1:4] <- c(0.933, 0.031, 0.031, 0.004)
ibre_probs$empirical["PR", 1:4] <- c(0.040, 0.911, 0.018, 0.031)
ibre_probs$empirical["PC + PR", 1:4] <- c(0.353, 0.612, 0.022, 0.013)
ibre_probs$empirical["I + PC + PR", 1:4] <- c(0.580, 0.402, 0.013, 0.004)
ibre_probs$empirical["I + PCo", 1:4] <- c(0.406, 0.080, 0.469, 0.045)
ibre_probs$empirical["I + PRo", 1:4] <- c(0.219, 0.085, 0.031, 0.665)
ibre_probs$empirical["PC + PRo", 1:4] <- c(0.353, 0.027, 0.058, 0.563)
ibre_probs$empirical["I + PC + PRo", 1:4] <- c(0.719, 0.036, 0.036, 0.210)
