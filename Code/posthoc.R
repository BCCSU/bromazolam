#Post-hoc correlation analysis of bromazolam and desalkylgidazepam, using the base_descriptive df from Descriptive_wrangle_biweekly.R

#grabbing variables of interest
correlation_anal_REVISED <- revised %>% 
  select(monthyear, monthcat, bromazprop, desalkylprop) %>% 
  mutate(time = paste0(monthyear, " ", monthcat)) 

#initial correlation analysis using Pearson's correlation coefficient
cor.test(correlation_anal_REVISED$bromazprop, correlation_anal_REVISED$desalkylprop, method = 'pearson', exact = F)

#cross-correlation analysis
ccf(correlation_anal_REVISED$bromazprop, correlation_anal_REVISED$desalkylprop)
#These two results indicate statistical significance, HOWEVER...

#Spearman's rank correlation (not significant)
cor.test(correlation_anal_REVISED$bromazprop, correlation_anal_REVISED$desalkylprop, method = 'spearman', exact = F)

#First-differencing (significant)
cor.test(diff(correlation_anal_REVISED$bromazprop), diff(correlation_anal_REVISED$desalkylprop), method = 'spearman', exact = F)
#...the statstical significance observed for the first-differenced time series suggests a biweekly substitution effect between bromazolam and desalkylgidazepam
