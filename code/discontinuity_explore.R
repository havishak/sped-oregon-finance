library(dplyr)
library(readr)
temp <- read.delim("/home/piyush/Downloads/23-24 ADMw breakout 2-22-23.txt")
temp$info <- temp$STATE.SCHOOL.FUND.GRANT
temp$district_fg <- grepl("District ID", temp$info)
# extracted all the district ids and names
district <- temp %>%
  mutate(district_fg = ifelse(lag(district_fg)|lead(district_fg) == TRUE,
                            TRUE,
                            district_fg)) %>%
  filter(district_fg == TRUE, 
  info != "2023-2024", info != "2023-2024 Extended ADMw") %>%
  mutate(group = rep(1:197, each = 2)) %>%
  mutate(district_id = ifelse(grepl("District ID", info), 
                              parse_number(info), NA),
         district_name = ifelse(!grepl("District ID", info), 
                              info, NA)) %>%
  select(-c(1:3)) %>%
  group_by(group) %>%
  summarize(district_id = unique(district_id[!is.na(district_id)]),
            district_name = unique(district_name[!is.na(district_name)]))
  
  

temp$ADM_fg <- grepl("ADMr:", temp$info)
temp$IEP_fg <- grepl("IEP", temp$info)
temp <- temp %>%
  mutate(
    #district_fg = ifelse(lag(district_fg)==TRUE, TRUE, district_fg),
    ADM_fg = ifelse(lag(ADM_fg, 2)==TRUE, TRUE, ADM_fg),
    IEP_fg = ifelse(lag(IEP_fg, 2)==TRUE, TRUE, IEP_fg)
  )

temp1 <- temp %>%
  filter(ADM_fg == TRUE | IEP_fg == TRUE) %>%
  mutate(group = rep(1:333, each = 6)) %>%
  group_by(group) %>%
  mutate(district_stu = parse_number(info[2]),
         district_iep_tot = parse_number(info[3]),
         district_iep_11 = parse_number(info[4]),
         district_iep_above_11 = parse_number(info[6])) %>%
  filter(district_stu > 100, district_iep_tot > 20) %>%
  select(group:district_iep_above_11) %>%
  unique()

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
