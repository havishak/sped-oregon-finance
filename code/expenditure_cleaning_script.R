# Clean annual expenditure files

library(tidyverse)
library(janitor)

# read files in folder
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

district_expense_df <- district_expense_df %>%
  mutate(category = case_when(
    function_cd %in% c(5100, 5400) ~ "Excluded",
    function_cd %in% instruction_functions ~ "Instruction",
    function_cd %in% support_service_functions ~ "Support Services",
    function_cd %in% enterprise_community_service_functions ~ "Community Service",
    function_cd %in% capital_functions ~ "Capital",
    TRUE ~ "Other Uses"
  ))

district_expense_function <- district_expense_df %>%
  # sped flag is 320
  mutate(sped_flag = ifelse(area_of_resp_cd == 320, 1, 0)) %>%
  group_by(school_year, institution_id, function_desc, category, sped_flag) %>%
  summarize(function_expense = sum(actual_exp_amt, na.rm = T)) %>%
  group_by(school_year, institution_id) %>%
  mutate(total_expense = sum(function_expense, na.rm = T),
         per_function_expense = function_expense*100/total_expense)

# function_desc
# and area of responsibility

combinations <- df %>%
  count(function_desc, object_desc, area_of_resp_desc)

eugene_sd <- df %>%
  filter(institution_id == "2082") %>%
  group_by(function_desc, area_of_resp_desc) %>%
  summarize(
    expense = sum(actual_exp_amt)) %>% 
  ungroup() %>%
  mutate(
    total_expense = sum(expense),
    prop_expense = expense/total_expense
  )
