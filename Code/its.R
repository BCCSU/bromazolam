#Interrupted time series analysis

#Adding intervention points
#This analysis adds the third intervention (PRC scheduling), hence the naming
its_REVISED <- revised %>% 
  pivot_longer(10:14, names_to = 'benzo', values_to = 'prop') %>% 
  filter(benzo == "bromazprop") %>% 
  ungroup() %>% 
  mutate(time = row_number(),
         #FIRST INTERVENTION - when scheduling is recommended
         intervention1 = case_when(time < 41 ~ 0,
                                   T ~ 1),
         postintervention1_time = ifelse(row_number() >= 41,
                                         row_number() -40,
                                         0),
         #THIRD INTERVENTION - when scheduling decision comes into force
         intervention2 = case_when(time <68 ~ 0,
                                   T ~ 1),
         postintervention2_time = ifelse(row_number() >= 68,
                                         row_number() -67,
                                         0),
         #SECOND INTERVENTION - when PRC banned bromazolam domestically
         interventionSENSITIVITY = case_when(time <57 ~ 0,
                                             T ~ 1),
         postintervention_time_SENSITIVITY = ifelse(row_number() >=57,
                                                    row_number() -56,
                                                    0)) 

###################################
#creating a time series object and decomposing
bromaz_ts_REVISED <- ts(its_REVISED$prop,
                         start = c(2022, 03),
                         #end = c(2025, 10),
                         frequency = 24)

bromaz_ts_REVISED

plot(decompose(bromaz_ts_REVISED))

#Grabbing predictors 
its_predictors_REVISED <- its_REVISED %>% 
  select(time, intervention1, postintervention1_time, interventionSENSITIVITY, postintervention_time_SENSITIVITY, intervention2, postintervention2_time)

###################################
#MODELLING
#This code returns the best ARIMA model according to specified criteria (in our case BIC) out of all possible models, within the order constraints provided
model_REVISED <- forecast::auto.arima(bromaz_ts_REVISED,
                                          seasonal = T,
                                          xreg = as.matrix(its_predictors_REVISED),
                                          stepwise = F,
                                          ic = 'bic',
                                          approximation = F,
                                          trace = F,
                                          max.p = 30,
                                          max.q = 30,
                                          max.d = 30)

summary(model_REVISED)

#the model indicates p = 0, d = 0, q = 1 (indicating no lag dependence, stationarity and a moving average of 1)
arma_orders_REVISED <- forecast::arimaorder(model_REVISED)

#Using the final ARIMA selected, the R code below uses the arima() function to fit that model into an arima object (easier for R to use).
arima_model_REVISED <- 
  arima(
    bromaz_ts_REVISED,
    order = c(arma_orders_REVISED[1], arma_orders_REVISED[2], arma_orders_REVISED[3]),
    xreg = as.matrix(its_predictors_REVISED)
  )
#coefficients
broom::tidy(arima_model_REVISED, conf.int = T, conf.level = .95)

#diagnostics - The auto.arima function automates a lot, but remember to check the diagnostics plots generated from the final model. It’s a good idea to verify that the model’s residuals are not autocorrelated and meet necessary assumptions.
#Box-Ljung test for normality
forecast::checkresiduals(arima_model_REVISED)

#Autocorrelation function plot
acf(residuals(arima_model_REVISED))

#histogram of residuals
hist(residuals(arima_model_REVISED))

# Kolmogorov-Smirnov test for residual independence 
ks.test(residuals(arima_model_REVISED), 'pnorm', mean = mean(residuals(arima_model_REVISED)), sd = sd(residuals(arima_model_REVISED)))
#there is evidence of validity!
