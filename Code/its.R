#Interrupted time series analysis


#Filtering for first detection of bromazolam and adding intervention points
#This analysis adds the third intervention (PRC scheduling), hence the naming
its_biweekly_SENSITIVITY <- base_descriptive %>% 
  pivot_longer(10:14, names_to = 'benzo', values_to = 'prop') %>% 
  filter(benzo == "bromazolamprop",
         monthyear >= '2022-03-01') %>% 
  ungroup() %>% 
  mutate(time = row_number(),
         #FIRST INTERVENTION - when scheduling is recommended
         intervention1 = case_when(time < 41 ~ 0,
                                   T ~ 1),
         postintervention1_time = ifelse(row_number() >= 41,
                                         row_number() -40,
                                         0),
         #SECOND INTERVENTION - when scheduling decision comes into force
         intervention2 = case_when(time <68 ~ 0,
                                   T ~ 1),
         postintervention2_time = ifelse(row_number() >= 68,
                                         row_number() -67,
                                         0),
         interventionSENSITIVITY = case_when(time <57 ~ 0,
                                             T ~ 1),
         postintervention_time_SENSITIVITY = ifelse(row_number() >=57,
                                                    row_number() -56,
                                                    0))

###################################
#creating a time series object and decomposing
bromaz_ts_biweekly <- ts(its_biweekly_SENSITIVITY$prop,
                start = c(2022, 03),
                #end = c(2025, 10),
                frequency = 24)

bromaz_ts_biweekly

plot(decompose(bromaz_ts_biweekly))

#Grabbing predictors 
its_predictors_biweekly_SENSITIVITY <- its_biweekly_SENSITIVITY %>% 
  select(time, intervention1, postintervention1_time, interventionSENSITIVITY, postintervention_time_SENSITIVITY, intervention2, postintervention2_time)
###################################
#MODELLING
model_SENSITIVITY <- forecast::auto.arima(bromaz_ts_biweekly,
                              seasonal = T,
                              xreg = as.matrix(its_predictors_biweekly_SENSITIVITY),
                              stepwise = F,
                              ic = 'bic',
                              approximation = F,
                              trace = F,
                              max.p = 30,
                              max.q = 30,
                              max.d = 30)

summary(model_SENSITIVITY)
#The auto.arima function automates a lot, but remember to check the diagnostics plots generated from the final model. It’s a good idea to verify that the model’s residuals are not autocorrelated and meet necessary assumptions.
#Using the final ARIMA selected, the R code below uses the arima() function to fit that model into an arima object (easier for R to use).
#the model indicates p = 0, d = 0, q = 1 (indicating no lag dependence, stationarity and a moving average of 1)
arma_orders_SENSITIVITY <- forecast::arimaorder(model_SENSITIVITY)
arima_model_SENSITIVITY <- 
  arima(
    bromaz_ts_biweekly,
    order = c(arma_orders_SENSITIVITY[1], arma_orders_SENSITIVITY[2], arma_orders_SENSITIVITY[3]),
    xreg = as.matrix(its_predictors_biweekly_SENSITIVITY)
  )
#coefficients
broom::tidy(arima_model_SENSITIVITY, conf.int = T, conf.level = .95)

#diagnostics - there is evidence of validity!
#Box-Ljung test for normality
forecast::checkresiduals(arima_model_SENSITIVITY)

#Autocorrelation function plot
acf(residuals(arima_model_SENSITIVITY))

#histogram of residuals
hist(residuals(arima_model_SENSITIVITY))

# Kolmogorov-Smirnov test for residual independence 
ks.test(residuals(arima_model_SENSITIVITY), 'pnorm', mean = mean(residuals(arima_model_SENSITIVITY)), sd = sd(residuals(arima_model_SENSITIVITY)))

