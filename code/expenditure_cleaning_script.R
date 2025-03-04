# Clean annual expenditure files

library(tidyverse)
library(janitor)
library(readxl)

# read files in folder
# by aggregared expenditure
# file_to_read <- paste0("data_raw/Expenditure Aggregated/",list.files("data_raw/Expenditure Aggregated"))
# 
# some issues in compatibility - just reading files with object codes
# 
# expenditure_school_aggregated <-  map2_dfr(
#   file_to_read,
#   c(2, 2, 1, 2, 2),
#   ~ read_excel(.x, sheet = .y) %>%
#     janitor::clean_names() %>%
#     mutate(school_year = parse_number(.x))
# ) %>%
#   filter(!is.na(district_id)) %>%
#   mutate(school_year = case_when(
#     school_year == 1819 ~ 2018,
#     school_year == 1920 ~ 2019,
#     TRUE ~ school_year
#   )) %>%
#   select(district_id:district_expenditures,
#          all_expenditures,
#          esd_internal,
#          school_year)

# note, these have some excluded stuff
# save school_level aggregated expenditure file
#write.csv(expenditure_school_aggregated, "data_clean/expenditure_school_aggregated_2018_22.csv")

# expenditure_district_aggregated <- expenditure_school_aggregated %>%
#   group_by(school_year, district_id, district) %>%
#   summarize(across(direct_classroom_expenditures:esd_direct_support:esd_internal,
#             ~sum(.x, na.rm = T)))

# by object code
file_to_read <- paste0("data_raw/Expenditure/",list.files("data_raw/Expenditure"))

district_expense_df <- map_dfr(file_to_read,
        ~read_excel(.x, sheet = 1) %>%
        janitor::clean_names() %>%
        mutate(school_year = parse_number(.x),
               school_instid = ifelse(school_instid == "NULL", "0", as.character(school_instid))))

## source: https://www.oregon.gov/ode/schools-and-districts/grants/Documents/2022-23%20Actual%20Expenditure%20data.xlsx
## accessed: August 28, 2024

## According to ODE, the expenditured are distributed among 5 categories:
## Instruction - 1000 series, 
## Support Services - 2000 series, 
## Enterprise and Community Services - 3000 series, 
## Facilitied Acquisition and Construction - 4000 series, 
## Other Uses (Debt Service, Transfer of Funds, Apportionment of Funds, Bonds, etc) - 5000 series
## Contigencies and unappropriated ending funds: 6000 and 7000 series
## Code 5400 is a one-time PERS UAL Lump Sum Payment to PERS, which is excluded in expenditure reported by ODE. Also excluding 5100 for Debt Services. Other expenditure includes Transfer of funds and approtionment by ESD.

## Define function code series ranges

instruction_functions <- c(1000:1999)

support_service_functions <- c(2000:2999)

enterprise_community_service_functions <- c(3000:3999)

capital_functions <- c(4000:4999)

other <- c(5000:7999)

# category 2 - used in aggregated ODE reports
direct_classroom <- c(1111,1112,1121,1131,1140, 1200:1499)

classroom_support <- c(1113, 1122 ,1132, 2100:2299, 2400:2499, 3300:3399, 3500:3599)

building_support <- c(2540, 2550, 2570, 2660, 2690, 3100:3199, 3200:3299)

central_support <- c(2510, 2520, 2600, 2610, 2620, 2630, 2640, 2670, 2700:2799, 2300:2399)

# object code
payment_to_others <- c(371, 372, 373, 374)

# rest coded other
district_expense_df <- district_expense_df %>%
  mutate(category = case_when(
    # Debt and pension related
    function_cd %in% c(5100, 5400) ~ "Excluded",
    function_cd %in% instruction_functions ~ "Instruction",
    function_cd %in% support_service_functions ~ "Support Services",
    function_cd %in% enterprise_community_service_functions ~ "Community Service",
    function_cd %in% capital_functions ~ "Capital",
    TRUE ~ "Other Uses"
  ),
  category2 = case_when(
    object_cd %in% payment_to_others ~ "Payment to Others",
    function_cd %in% direct_classroom ~ "Direct Classroom",
    function_cd %in% classroom_support ~ "Classroom Support",
    function_cd %in% building_support ~ "Building Support",
    function_cd %in% central_support ~ "Central Support",
    TRUE ~ "Others"
  ),
  # SpEd flag
  sped_flag = case_when(
    function_cd == "1220" ~ "Restrictive Programs",
    function_cd == "1250" ~ "Less-Restrictive Programs",
    area_of_resp_cd == "320" ~ "Other SpEd",
    TRUE ~ "Not Special Education"
  ),
  # ELL Flag
  el_flag = case_when(
    function_cd %in% c("1291", "1295") ~ "ELL",
    area_of_resp_cd == "280" ~ "Other ELL",
    TRUE ~ "Not ELL"
  ))

district_expense_aggregate <- function(df, ...) {
  
  df <-  df %>%
    group_by(school_year, institution_id, institution_name, ...) %>%
    # total expense by category 1 definition
    summarize(total_group = sum(actual_exp_amt, na.rm = T)) %>%
    group_by(school_year, institution_id) %>%
    mutate(total = sum(total_group, na.rm = T),
           per_total = total_group*100/total) %>%
    ungroup() %>%
    group_by(school_year) %>%
    mutate(
      state_total = sum(total_group, na.rm = T),
      per_state_year = total*100/state_total
    ) %>%
    ungroup()
  
  return(df)
}

# Get things at an aggregated level:
district_expense_category1 <- district_expense_aggregate(district_expense_df, category)
district_expense_category2 <- district_expense_aggregate(district_expense_df, category2)
district_expense_sped_category1 <- district_expense_aggregate(district_expense_df, sped_flag, category)
district_expense_sped_category2 <- district_expense_aggregate(district_expense_df, sped_flag, category2)
district_expense_el_category1 <- district_expense_aggregate(district_expense_df, el_flag, category)
district_expense_el_category2 <- district_expense_aggregate(district_expense_df, el_flag, category2)
district_expense_sped_function <- district_expense_aggregate(filter(district_expense_df, sped_flag == "Other SpEd"),
                                                                    sped_flag, function_cd, function_desc)
district_expense_el_function <- district_expense_aggregate(filter(district_expense_df, el_flag == "Other ELL"), 
                                                           el_flag, function_cd, function_desc)

# write files
write.csv(district_expense_category1,
          "data_clean/district_expenses_aggregate1_1922.csv")
write.csv(district_expense_category2,
          "data_clean/district_expenses_aggregate2_1922.csv")
write.csv(district_expense_sped_category1,
          "data_clean/district_expenses_sped_aggregate1_1922.csv")
write.csv(district_expense_sped_category2,
          "data_clean/district_expenses_sped_aggregate2_1922.csv")
write.csv(district_expense_el_category1,
          "data_clean/district_expenses_el_aggregate1_1922.csv")
write.csv(district_expense_el_category2,
          "data_clean/district_expenses_el_aggregate2_1922.csv")
write.csv(district_expense_sped_function,
          "data_clean/district_expenses_sped_other_1922.csv")
write.csv(district_expense_el_function,
          "data_clean/district_expenses_el_other_1922.csv")