#Post-hoc correlation analysis of bromazolam and desalkylgidazepam, using the base_descriptive df from Descriptive_wrangle_biweekly.R

#grabbing variables of interest
correlation_anal <- base_descriptive %>% 
  select(monthyear, monthcat, `Bromazolam `, `Desalkylgidazepam `) %>% 
  mutate(time = paste0(monthyear, " ", monthcat)) %>% 
  filter(monthyear >= '2022-03-01') 

#initial correlation analysis using Pearson's correlation coefficient
cor.test(correlation_anal$`Bromazolam `, correlation_anal$`Desalkylgidazepam `, method = 'pearson', exact = F)

#cross-correlation analysis
ccf(correlation_anal$`Bromazolam `, correlation_anal$`Desalkylgidazepam `)
#These two results indicate statistical significance, HOWEVER...

#Spearman's rank correlation
cor.test(correlation_anal$`Bromazolam `, correlation_anal$`Desalkylgidazepam `, method = 'spearman', exact = F)

#First-differencing
cor.test(diff(correlation_anal$`Bromazolam `), diff(correlation_anal$`Desalkylgidazepam `), method = 'spearman', exact = F)
#...this relationship was not robust to non-parametric testing nor first-differencing. These results suggest that the observed inverse relationship is likely attributable to non-stationarity, whereby the detection trends appear to be related simply because they changed over time in opposite directions rather than replacing one another.
