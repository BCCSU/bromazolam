#Figures for the manuscript

#Figure 1
fig1 <- base_descriptive %>% 
  mutate(time = paste0(monthyear, " ", monthcat),
         #opioidonly = exp_opioid_samples - BENZOTOTAL) %>% 
  ) %>% 
  select(monthyear, time, BENZOTOTAL, exp_opioid_samples, Bromazolam) %>% 
  rename(`Expected opioid samples` = exp_opioid_samples,
         `Benzodiazepine detected` = BENZOTOTAL) %>% 
  filter(monthyear >= '2022-03-01') %>% 
  #select(-5) %>% 
  #pivot_longer(4:5, names_to = 'cat', values_to = 'total') %>% 
  # ggplot(aes(x = time, fill = cat, colour = cat)) +
  ggplot(aes(x = time)) +
  geom_col(aes(y = `Expected opioid samples`, fill = 'Expected opioid samples')) +
  geom_line(aes(y = `Benzodiazepine detected`, group = 1, colour = 'Benzodiazepines detected in expected opioid samples'), linetype = 'longdash')+
  #geom_point(aes(y = `Benzodiazepine detected`, group = 1, colour = 'Benzodiazepines detected in expected opioid samples'), shape = 17, size = 2.5) +
  geom_line(aes(y = Bromazolam, group = 2, colour = 'Bromazolam detected in expected opioid samples'),) +
  #geom_point(aes(y = Bromazolam, group = 2, colour = 'Bromazolam detected in expected opioid samples'), shape = 19, size = 2) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  labs(y = 'Number of samples',
       x = '') +
  geom_vline(aes(xintercept = '2023-11-01 First half'), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = '2024-07-01 First half'), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = '2024-12-01 Second half'), color = "black", linetype = 'dotted') +
  scale_x_discrete(breaks = ~ .x[seq(1, length(.x), 6)],
                   labels = function(x) substr(x,1, nchar(x)-14)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,870)) +
  scale_fill_manual(values = 'black') +
  scale_colour_manual(values = c('#A9A9A9', '#D3D3D3')) +
  #scale_shape_manual(values = c('square', 'circle')) +
  geom_text(aes(x = 25, y = 800, label = "Pre-scheduling period"), size =4, color = "black") + 
  geom_text(aes(x = 48, y = 800, label = "UNCND\nscheduling\nrecommended"), size =4, color = "black") + 
  geom_text(aes(x = 62.5, y = 800, label = "PRC\nscheduling\nin force"), size =4, color = "black") + 
  geom_text(aes(x = 80, y = 800, label = "UNCND\n scheduling\nin force"), size =4, color = "black")

ggsave("Outputs/Fig1_REVISED_FEEDBACK.png", plot = fig1, width = 9, height = 6, dpi = 600)

#################################################################################

#Figure 2 (ignore the labelling)
fig1_revised <- its_biweekly_FINAL_SENSITIVITY %>% 
  ggplot(aes(x = xaxis, y = prop)) +
  geom_point(aes(), color = 'black') +
  
  geom_line(aes(y = linear_fitted), color = 'black', size = 1, group =1 )+
  geom_line(aes(y = linear_fitted2), color = 'black', size = 1, group =1 )+
  geom_line(aes(y = linear_fitted3), color = 'black', size = 1, group =1 )+
  geom_line(aes(y = linear_fitted4), color = 'black', size = 1, group =1 )+
  
  geom_line(data = filter(its_biweekly_FINAL_SENSITIVITY, intervention1 == 1), 
            aes(y = cf1), color = 'black', linetype = 'dashed', group = 1) +
  geom_line(data = filter(its_biweekly_FINAL_SENSITIVITY, intervention2 == 1), 
            aes(y = cf3), color = 'black', linetype = "dashed", group = 1) +
  geom_line(data = filter(its_biweekly_FINAL_SENSITIVITY, interventionSENSITIVITY == 1), 
            aes(y = cf2), color = 'black', linetype = "dashed", group = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(limits = c(-.5, 12)) +
  labs(y = 'Opioid samples with bromazolam detected (%)',
       x = 'Month (measured in two-week intervals)') +
  scale_x_discrete(breaks = ~ .x[seq(1, length(.x), 6)],
                   labels = function(x) substr(x,1, nchar(x)-14)) +
  geom_vline(aes(xintercept = '2023-11-01 First half'), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = '2024-07-01 First half'), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = '2024-12-01 Second half'), color = "black", linetype = 'dotted') +
  geom_text(aes(x = 25, y = 10.5, label = "Pre-scheduling period"), size =4, color = "black") + 
  geom_text(aes(x = 48, y = 11.25, label = "UNCND\nscheduling\nrecommended"), size =4, color = "black") + 
  geom_text(aes(x = 62.5, y = 10, label = "PRC\nscheduling\nin force"), size =4, color = "black") + 
  geom_text(aes(x = 80, y = 10.5, label = "UNCND\nscheduling\nin force"), size =4, color = "black")

ggsave("Outputs/fig2_revised.png", plot = fig1_revised, width = 9, height = 4.5, dpi = 600)

#################################################################################
#figure3 alone

fig3 <- base_descriptive %>% 
  pivot_longer(10:14, names_to = 'benzo', values_to = 'prop') %>% 
  mutate(time = paste0(monthyear, " ", monthcat)) %>% 
  filter(benzo !='benzoprop',
         monthyear >= '2022-03-01') %>% 
  ggplot(aes(x = time, y = prop, group = benzo, colour = benzo, linetype = benzo)) +
  #geom_point() +
  geom_line() +
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  labs(y = 'Percent detection in expected opioids',
       x = '') +
  geom_vline(aes(xintercept = '2023-11-01 First half'), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = '2024-07-01 First half'), color = "black", linetype = 'dotted') +
  geom_vline(aes(xintercept = '2024-12-01 Second half'), color = "black", linetype = 'dotted') +
  scale_x_discrete(breaks = ~ .x[seq(1, length(.x), 6)],
                   labels = function(x) substr(x,1, nchar(x)-14)) +
  scale_y_continuous(limits = c(0,12.1)) +
  scale_linetype_manual(values = c('solid', 'solid', 'dashed', 'dashed')) +
  scale_color_manual(values = c('black', '#808080', 'black', '#808080')) +
  geom_text(aes(x = 25, y = 11.5, label = "Pre-scheduling period"), size =4, color = "black") + 
  geom_text(aes(x = 48, y = 11.5, label = "UNCND\nscheduling\nrecommended"), size =4, color = "black") + 
  geom_text(aes(x = 62.5, y = 11.5, label = "PRC\nscheduling\nin force"), size =4, color = "black") + 
  geom_text(aes(x = 80, y = 11.5, label = "UNCND\nscheduling\nin force"), size =4, color = "black") +
  guides(colour = guide_legend(reverse = T),
         linetype = guide_legend(reverse = T))

ggsave("Outputs/Fig3_single.png", plot = fig3, width = 9, height = 5, dpi = 600)
