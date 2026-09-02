#Final data preparation for ITS

#ITS based on a simple linear model would use fitted(), which includes the stochastic ARIMA component (the $p, d, q$ errors and moving averages). In other words it will try to mimic every wiggle in your data.
#To get the "clean" linear trends for an ITS, we need to ignore the ARIMA noise and only plot the structural component (the xreg predictors). 
#We do this by multiplying intervention matrix by the model's coefficients.

# 1. Extract coefficients specifically for the xreg predictors
# We need to match the names in its_predictors_biweekly matrix
beta_REVISED <- coef(arima_model_REVISED)
reg_names_REVISED <- colnames(its_predictors_REVISED)

# 2. Calculate the "Linear Fitted" value (The structural trend)
# This is: Intercept + (Time * b1) + (Int1 * b2) + (PostTime1 * b3) ...
intercept_REVISED <- if("intercept" %in% names(beta_REVISED)) beta_REVISED['intercept'] else 0

#calculate linear prediction
its_REVISED$linear_fitted <- as.matrix(its_predictors_REVISED) %*% beta_REVISED[reg_names_REVISED] + intercept_REVISED

#Calculate counterfactual 1 (time + intercept)
its_REVISED$cf1 <- (its_REVISED$time * beta_REVISED["time"]) + intercept_REVISED

#Calculate counterfactual 2 (where intervention 1 happened, intervention 2 did not)
its_REVISED$cf2 <- (as.matrix(its_REVISED[, c("time", "intervention1", "postintervention1_time")]) %*% 
                                   beta_REVISED[c("time", "intervention1", "postintervention1_time")]) + intercept_REVISED

#Calculate counterfactual 3 (where intervention 1 happened, intervention 2 did not)
its_REVISED$cf3 <- (as.matrix(its_REVISED[, c("time", "intervention1", "postintervention1_time", "interventionSENSITIVITY", "postintervention_time_SENSITIVITY")]) %*% 
                                   beta_REVISED[c("time", "intervention1", "postintervention1_time", "interventionSENSITIVITY", "postintervention_time_SENSITIVITY")]) + intercept_REVISED

#Some additional tidying of the linear fitted bit
its_biweekly_FINAL_REVISED <- its_REVISED %>% 
  mutate(linear_fitted2 = case_when(time <41 | time >56 ~ NA,
                                    T ~ linear_fitted),
         linear_fitted3 = case_when(time >= 57 & time <68 ~ linear_fitted,
                                    T ~ NA),
         linear_fitted4 = case_when(time >=68 ~ linear_fitted),
         linear_fitted = case_when(time <41 ~ linear_fitted,
                                   T ~ NA),
         xaxis = paste0(monthyear, " ", monthcat)) 
