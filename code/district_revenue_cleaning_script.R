# revenue from different sources
# code starting with 
# 1: local sources
# 2: intermediate sources
# 3: state sources
# 4: federal
# 5: other (like debt, etc.)

library(tidyverse)
library(readxl)

# read files in folder
file_to_read <- paste0("data_raw/Revenue/",list.files("data_raw/Revenue"))

# other has beginning fund balance too

rev_data_2019_2022 <- map_dfr(file_to_read, 
        ~read_excel(.x, sheet = 1)) %>%
  janitor::clean_names() %>%
  mutate(
    school_year = parse_number(school_year)
  ) %>%
  group_by(school_year, institution_id) %>%
  summarize(
    local = sum(actual_rev_amt[grepl("^1", source_cd)]),
    intermediate = sum(actual_rev_amt[grepl("^2", source_cd)]),
    state = sum(actual_rev_amt[grepl("^3", source_cd)]),
    federal = sum(actual_rev_amt[grepl("^4", source_cd)]),
    other = sum(actual_rev_amt[grepl("^5", source_cd)])
  ) %>%
  pivot_longer(
    cols = local:other,
    names_to = "source",
    values_to = "revenue_dollar"
  ) %>%
  group_by(school_year, institution_id) %>%
  mutate(total_revenue = sum(revenue_dollar),
         per_revenue = revenue_dollar/total_revenue) %>%
  rename("district_id" = "institution_id")

write.csv(rev_data_2019_2022,"data_clean/district_revenue_sources_2019_2022.csv")
