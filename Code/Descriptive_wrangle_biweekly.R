library(tidyverse)

#Reading in main dataset, creating a flag for first half of month and second half
# basedata_biweekly <- read_csv('BC_Main_Data_Report_01-Jan-2019_To_31-Oct-2025.csv') %>%
basedata_biweekly <- read_csv('BC_Main_Data_Report_01-Jan-2019_To_28-Feb-2026.csv') %>%
  mutate(visitdate = as.Date(`Visit Date`, format = "%d-%b-%y"),
         monthyear = as.Date(paste(year(visitdate), month(visitdate), '01', sep = '-')),
         day = str_sub(visitdate, 9,10),
         monthcat = case_when(day <16 ~ "First half",
                              T ~ "Second half")) %>% 
  filter(Drug %in% c("Fentanyl", 'Down (Unknown Opioid)', 'Fentanyl and Heroin', 'Heroin'))

#Extracting number of samples which were opioid positive
exp_opioids_biweekly <- basedata_biweekly %>% 
  select(`Sample ID`, monthyear, monthcat) %>% 
  distinct() %>% 
  group_by(monthyear, monthcat) %>% 
  summarise(exp_opioid_samples = n())

#############################################################
#list of benzos from https://docs.google.com/spreadsheets/d/1VwZT5yBhoY7VeyVe447NiuSbBjIqkYoLpSFHp0rgZBg/edit?gid=0#gid=0

benzo_list <- c(
  #these are the 'Current Supply'
  "3-HO-desalkylgidazepam",
  "4'-Chloro-deschloroalprazolam",
  "Alprazolam",
  "Bromazolam",
  "Clobromazolam",
  "Clonazepam",
  "Desalkylgidazepam",
  "Deschloroetizolam",
  "Diazepam",
  "Diclazepam",
  "Ethylbromazolam",
  "Ethylflualprazolam",
  "Etizolam",
  "Flubromazepam",
  "Flualprazolam",
  "Lorazepam",
  "Medazepam",
  "Methyllclonazepam",
  "Nifoxipam",
  #These are 'Other Benzos'
  "Adinazolam",
  "Avizafone",
  "Bromazepam",
  "Clonazolam",
  "Flubromazolam",
  "Flubrotizolam",
  "Fluclotizolam",
  "Flunitrazepam",
  "Flurazepam",
  "Metizolam",
  "Midazolam",
  "Nimetazepam",
  "Nitrazepam",
  "Nitrazolam",
  "Nordazepam",
  "Norflurazepam",
  "Oxazepam",
  "Phenazepam",
  "Pyrazolam",
  "Rilmazafone",
  "Temazepam",
  "Triazolam"
)
######################################################
#all expected opioid samples where there was a benzo 
benzo_pos_biweekly <- basedata_biweekly %>% 
  group_by(`Sample ID`) %>% 
  filter(
    (`Test Type` == "BENZO" & Results == "POS") |
      (`Test Type` == "FTIR 1" & (Results %in% benzo_list)) |
      (`Test Type` == "FTIR 2" & (Results %in% benzo_list)) |
      (`Test Type` == "FTIR 3" & (Results %in% benzo_list)) |
      (`Test Type` == "FTIR 4" & (Results %in% benzo_list)) |
      (`Test Type` == "FTIR 5" & (Results %in% benzo_list)) |
      (`Test Type` == "FTIR 6" & (Results %in% benzo_list))
  ) %>% 
  distinct(`Sample ID`, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(Results2 = case_when(!str_detect(Results, "Bromazolam|Etizolam|Desalkylgidazepam|POS") ~ 'Any other benzo',
                              T ~ Results))

#creating a baseline descriptivedataset
base_descriptive <- benzo_pos_biweekly %>% 
  group_by(monthyear, monthcat, Results2) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = Results2, values_from = total) %>% 
  replace(is.na(.), 0) %>% 
  left_join(exp_opioids_biweekly, by = c('monthyear', 'monthcat')) %>% 
  mutate(BENZOTOTAL = sum(Etizolam + POS + `Any other benzo` + Bromazolam + Desalkylgidazepam),
         benzoprop = round(BENZOTOTAL/exp_opioid_samples*100,1),
         `Bromazolam ` = round(Bromazolam/exp_opioid_samples*100,1),
         `Desalkylgidazepam ` = round(Desalkylgidazepam/exp_opioid_samples*100,1),
         #flualprop = round(Flualprazolam/exp_opioid_samples*100,1),
         `Etizolam ` = round(Etizolam/exp_opioid_samples*100,1),
         `Any other benzodiazepine` = round(`Any other benzo`/exp_opioid_samples*100,1)) 
