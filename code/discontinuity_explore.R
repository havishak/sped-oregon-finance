library(tidyverse)
library(lme4)

district_adm <- read.csv("data_clean/district_year_admw.csv") %>%
  select(-1) %>%
  rowwise() %>%
  mutate(district_iep_students = 
           ifelse(grepl("IEP Students", category), parse_number(category),NA)) %>%
  ungroup() %>%
  group_by(district_id, school_year) %>%
  mutate(district_iep_students = sum(district_iep_students, na.rm = T),
         district_iep_percent = district_iep_students/current_year_adm_district, 
         district_iep_11 = sum(current_year_adm[grepl("IEP Students", category)]),
         district_iep_above_11 = sum(current_year_adm[grepl("IEP Above", category)]),
         district_iep_no_extra = district_iep_students - district_iep_11 - district_iep_above_11) %>%
  ungroup() %>%
  mutate(
         district_iep_eff_weight = ifelse(district_iep_students == 0,
                                          0,
                                          (district_iep_11 + district_iep_above_11)/district_iep_students),
         district_iep_add11_weight = ifelse(district_iep_percent < 0.11,
                                            1,
                                            district_iep_above_11/(district_iep_students - district_iep_11)),
         district_iep_add11_weight = ifelse(district_iep_students == 0, 0, district_iep_add11_weight),
         weight_ratio = current_year_effective_admw_entry/current_year_admw_entry
  )

iep_data <- district_adm %>%
  select(school_year, district_id, current_year_adm_district, district_iep_students:district_iep_add11_weight, current_weight_district, weight_ratio) %>%
  distinct() %>%
  mutate(
    iep_less_11 = ifelse(district_iep_percent < 0.11, 1, 0)
  )

weight_data <- district_adm %>%
  select(school_year, district_id, entry_index, current_year_admw_entry, past_year_admw_entry, current_year_admw_district, past_year_adm_district, district_iep_students:district_iep_add11_weight, current_year_eff_admw_district) %>%
  distinct() %>%
  mutate(
    current_year_weight_entry = ifelse(current_year_admw_entry >= past_year_admw_entry, 1, 0),
    current_year_weight_district = ifelse(current_year_admw_district == current_year_eff_admw_district, 1, 0),
    entry_district = paste0(district_id,entry_index)
  ) 

ggplot(weight_data, aes(y = current_year_admw_entry, x = current_year_weight_entry)) +
  geom_jitter(alpha = 0.4)

ggplot(weight_data, aes(y = current_year_admw_entry, x = current_year_weight_district)) +
  geom_jitter(alpha = 0.4)

lmer(current_year_weight_entry ~ 1 | entry_district,
     weight_data)

lmer(district_iep_add11_weight ~ 1 | district_id,
     iep_data)

# Most of variation within district than between
# Random effects:
#   Groups         Name        Std.Dev.
# entry_district (Intercept) 0.04991 
# Residual                   0.49674 
# Number of obs: 4639, groups:  entry_district, 358

# Random effects:
#   Groups                  Name        Std.Dev.
# entry_index:district_id (Intercept) 0.02527 
# district_id             (Intercept) 0.04320 
# Residual                            0.49673 

ggplot(iep_data, aes(iep_less_11)) +
  geom_bar(fill = "cornflowerblue", aes(y = ..prop..))

iep_data %>%
  filter(district_iep_percent < 1) %>%
  ggplot(aes(x = district_iep_percent)) +
  geom_histogram(bins = 60, fill = "cornflowerblue") +
  geom_vline(xintercept = 0.11, color = "darkred") +
  theme_classic()

iep_data %>%
  filter(district_iep_percent < 0.2, school_year == 2018) %>%
  ggplot(aes(x = district_iep_percent, y = current_year_adm_district, color = factor(iep_less_11))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")+
  theme_classic() 

iep_data %>%
  filter(district_iep_percent < 1, school_year == 2018) %>%
  ggplot(aes(x = district_iep_percent, y = district_iep_eff_weight, color = factor(iep_less_11))) +
  geom_point(alpha = 0.5) +
  theme_classic() 

iep_data %>%
  filter(district_iep_percent < 1, district_iep_add11_weight < 1.1, school_year == 2018) %>%
  ggplot(aes(x = district_iep_percent, y = district_iep_add11_weight, color = factor(iep_less_11))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_classic() 




hist(district_adm$district_iep_percent)

temp1 <- temp1 %>%
  mutate(district_iep_prop = district_iep_tot/district_stu,
         district_iep_add_weight = (district_iep_above_11)/(district_iep_tot - district_iep_11))

temp1 <- temp1 %>%
  mutate(district_iep_add_weight = ifelse(is.infinite(district_iep_add_weight)| is.na(district_iep_add_weight), 
                                          1, 
                                          district_iep_add_weight))

# Alternate
temp1 <- temp1 %>%
  mutate(#district_iep_tot2 = district_iep_tot + district_iep_above_11,
         #district_iep_prop2 = district_iep_tot/district_stu,
         district_iep_add_weight2 = (district_iep_11 + district_iep_above_11)/(district_iep_tot))

summary(temp1$district_iep_add_weight, na.rm = T)
summary(temp1$district_iep_add_weight2, na.rm = T)

library(ggplot2)

plt1 <- ggplot(temp1 %>% filter(district_iep_add_weight <= 1), 
               aes(x = district_iep_prop, y = district_iep_add_weight)) +
  geom_point() +
  geom_smooth(data = temp1 %>% filter(district_iep_add_weight == 1))+
  geom_smooth(data = temp1 %>% filter(district_iep_add_weight < 1))

plt1

ggsave("sped_11_discontinuity.jpeg", plt1)

# weight per student for students with IEP.

plt2 <- ggplot(temp1 %>% filter(district_iep_add_weight2 <=1), aes(x = district_iep_prop, y = district_iep_add_weight2)) +
  geom_point() +
  geom_smooth(data = temp1 %>% filter(district_iep_add_weight2 == 1))+
  geom_smooth(data = temp1 %>% filter(district_iep_add_weight2 < 1))

plt2
