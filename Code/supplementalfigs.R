#Supplemental figures

#First grabbing unique sites during study period
sites <- basedata_biweekly %>% 
  filter(monthyear >= '2022-03-01') %>% 
  select(`Collection Site`) %>% 
  distinct() 

#There are quite a few services that were active before the study period began - if you only filter for when the study period began it won't capture these services
sites_samples <- bind_cols(

#Grabbing first date of sample collection at ANY DATE
basedata_biweekly %>% 
  filter(`Collection Site` %in% sites$`Collection Site`) %>% 
  arrange(visitdate) %>% 
  group_by(`Collection Site`) %>% 
  slice(1) %>% 
  select(`Sample ID`, `Collection Site`, monthyear, visitdate, `Health Authority`) %>% 
  arrange(`Collection Site`),

#Grabbing first date of sample after study period commenced
basedata_biweekly %>% 
  select(`Sample ID`, `Collection Site`, monthyear, visitdate, `Health Authority`) %>% 
  filter(monthyear >= '2022-03-01') %>% 
  arrange(visitdate) %>% 
  group_by(`Collection Site`) %>% 
  slice(1) %>% 
  arrange(`Collection Site`)
) %>% 
  mutate(MATCH = case_when(`visitdate...4` == `visitdate...9` ~ 1,
                         T ~ 0)) %>% 
  rename(collectionsite = `Collection Site...2`) %>% 

#Those sites that preceded study period are flagged as such (also added is the total number of opioid samples tested per service)
  left_join((basedata_biweekly %>% 
              filter(monthyear >= '2022-03-01') %>% 
              select(`Sample ID`, `Collection Site`, monthyear, visitdate, `Health Authority`) %>% 
              distinct() %>% 
              group_by(`Collection Site`) %>% 
              summarise(total = n()) %>% 
              rename(collectionsite = `Collection Site`)),
            by = "collectionsite") %>% 
  select(-6:-8, -10) %>% 
  mutate(firstdate = case_when(MATCH == 0 ~ as.Date('2022-03-01'),
                               T ~ `monthyear...3`)) 
####################################################################################################
#Creating a plot of services by date first sample submitted
supp1 <- sites_samples %>% 
  ungroup() %>% 
  group_by(firstdate) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = firstdate, y = total)) +
  geom_col() +
  geom_vline(aes(xintercept = as.Date('2023-11-01')), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = as.Date('2024-07-01')), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = as.Date('2024-12-01')), color = "black", linetype = 'dotted') +
  labs(x = 'Date of first sample recorded',
       y = 'Number of services') +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  geom_text(aes(x = as.Date('2023-05-01'), y = 23.5, label = "Pre-scheduling \nperiod"), size =4, color = "black") + 
  geom_text(aes(x = as.Date('2024-03-01'), y = 23.5, label = "WHO\nrecommendation\nto schedule"), size =4, color = "black") + 
  geom_text(aes(x = as.Date('2024-9-15'), y = 23.5, label = "PRC\nscheduling\nin force"), size =4, color = "black") + 
  geom_text(aes(x = as.Date('2025-05-01'), y = 23.5, label = "UNODC/CND\nscheduling\nin force"), size =4, color = "black")

ggsave('Outputs/supp1.png', height = 5, width = 9, units = 'in', dpi = 600)
####################################################################################################
#Creating a plot of number of samples by health authority (and overall)
bind_rows(
#by HA
basedata_biweekly %>%
  filter(monthyear >= '2022-03-01') %>%
  select(`Sample ID`, `Health Authority`, monthyear) %>%
  distinct() %>%
  group_by(monthyear, `Health Authority`) %>%
  summarise(total = n()), 
#BC overall
basedata_biweekly %>%
  filter(monthyear >= '2022-03-01') %>%
  select(`Sample ID`, `Health Authority`, monthyear) %>%
  distinct() %>%
  group_by(monthyear) %>%
  summarise(total = n()) %>% 
  mutate(`Health Authority` = 'BC OVERALL') %>% 
  select(1,3,2)) %>% 
  
  ggplot(aes(
    x = monthyear,
    y = total,
    color = `Health Authority`,
    group = `Health Authority`
  )) +
  geom_line() +
  geom_vline(aes(xintercept = as.Date('2023-11-01')),
             color = "black",
             linetype = 'dotted') +
  geom_vline(aes(xintercept = as.Date('2024-07-01')),
             color = "black",
             linetype = 'dotted') +
  geom_vline(aes(xintercept = as.Date('2024-12-01')),
             color = "black",
             linetype = 'dotted') +
  theme_bw() +
  scale_colour_brewer(palette = 'Set1') +
  labs(y = 'Expected opioid samples tested', x = 'Month') +
  theme(legend.position = 'bottom',
        panel.grid = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  geom_text(aes(x = as.Date('2023-05-01'), y = 1400, label = "Pre-scheduling \nperiod"), size =4, color = "black") + 
  geom_text(aes(x = as.Date('2024-03-01'), y = 1400, label = "WHO\nrecommendation\nto schedule"), size =4, color = "black") + 
  geom_text(aes(x = as.Date('2024-9-15'), y = 1400, label = "PRC\nscheduling\nin force"), size =4, color = "black") + 
  geom_text(aes(x = as.Date('2025-05-01'), y = 1400, label = "UNODC/CND\nscheduling\nin force"), size =4, color = "black") +
  scale_y_continuous(limits = c(0, 1500))

ggsave('Outputs/supp2.png', height = 5, width = 9, units = 'in', dpi = 600)
