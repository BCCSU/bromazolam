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
test <- basedata_biweekly %>% 
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
  ungroup() %>% 
  filter(monthyear >= '2022-03-01')

#Apparently there are some duplicate records, identified below
test %>% 
  dplyr::summarise(n = dplyr::n(), .by = c(`Visit Date`, `Sample ID`, Drug, Category, monthyear, monthcat, `Test Type`)) %>% 
  dplyr::filter(n > 1L) %>% View()

#Subsets dataframe, retains unique values, and pivots to wide format
benzopos <- test %>% 
  select(`Sample ID`, Drug, Category, `Test Type`, Results, monthyear, visitdate, monthcat, `Health Authority`) %>% 
  unique() %>% 
  pivot_wider(names_from = `Test Type`, values_from = Results) %>% 
  arrange(visitdate) %>% 
  select(1:7, `FTIR 1`, `FTIR 2`, everything()) %>% 
  mutate(POS = case_when(BENZO == 'POS' & if_all(starts_with('FTIR'), ~ is.na(.)) ~ 1 ,
                                T ~ 0),
         Bromazolam = case_when(if_any(starts_with('FTIR'), ~ str_detect(., 'Bromazolam')) ~ 1,
                                T~0),
         Etizolam = case_when(if_any(starts_with('FTIR'), ~ str_detect(., 'Etizolam')) ~ 1,
                                T~0),
         Desalkylgidazepam = case_when(if_any(starts_with('FTIR'), ~ str_detect(., 'Desalkylgidazepam')) ~ 1,
                                T~0),
         Anyother = case_when(if_any(starts_with('FTIR'), ~ . %in% benzo_list[-c(4, 7, 13)]) ~ 1,
                              T ~0),
         ANYNA = case_when(is.na(BENZO) & if_all(starts_with('FTIR'), ~ is.na(.)) ~ 'YES',
                           T ~ 'NO'))
###############################################################################
#Defining a function to extract and concatenate data on individual compounds
extractfun <- function(df, cat) {
  
  cat_name <- deparse(substitute(cat))
  
  df %>% 
    left_join(
      (benzopos %>% 
         group_by(monthyear, monthcat, {{cat}}) %>% 
         summarise("{cat_name}count" := n()) %>% 
         filter({{cat}} == 1) %>% 
         select(-{{cat}})),
      by = c('monthyear', 'monthcat'))
}
##########################################################################
#Merging expected opioids dataframe with individual dataframes corresponding with benzos overall and specific compounds
revised <- left_join(
  
  exp_opioids_biweekly,
  
  (benzopos %>%
     group_by(monthyear, monthcat) %>%
     summarise(BENZOTOTAL = n())),
  by = c('monthyear', 'monthcat')
  
) %>% 
  
  filter(monthyear >= '2022-03-01') %>% 
  
  #Applying extraction function for each compound/category
  extractfun(POS) %>%
  extractfun(Bromazolam) %>% 
  extractfun(Desalkylgidazepam) %>% 
  extractfun(Etizolam) %>% 
  extractfun(Anyother) %>% 
  
  replace(is.na(.), 0) %>% 
  mutate(benzototal = round(BENZOTOTAL/exp_opioid_samples*100,1),
         bromazprop = round(Bromazolamcount/exp_opioid_samples*100,1),
         desalkylprop = round(Desalkylgidazepamcount/exp_opioid_samples*100,1),
         etizolamprop = round(Etizolamcount/exp_opioid_samples*100,1),
         Anyotherprop = round(Anyothercount/exp_opioid_samples*100,1)) 
