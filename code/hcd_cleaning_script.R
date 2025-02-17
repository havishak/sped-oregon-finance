# Collection of functions to read and clean HCD files.
library(tidyverse)
library(pdftools)

# read most updated file from year folders when there are year folders
file_to_read <- function(directory_path) {
  # get list of directories inside
  dir_list <- list.dirs(directory_path)
  
  # remove the path we provided as it's added by default
  dir_list <- setdiff(dir_list, directory_path)
  
  # list files in each directory and extract the dates
  file_list <- map_dfr(dir_list,
                       ~ tibble(
                         files = list.files(.x),
                         dir = .x,
                         dates = mdy(gsub(".*?(\\d+-\\d+-\\d+).*", "\\1", files)),
                         is_max = ifelse(dates == max(dates), 1, 0)
                       )) %>%
    filter(is_max == 1) %>%
    mutate(file_path = paste0(dir, "/", files))
  
  return(file_list)
}


# scrape one district estimate files
one_year_hcd_data <- function(file_path) {
  # get year to add to df
  get_year <- str_extract(file_path, "\\d{2}-\\d{2}")
  format_year <- str_replace(get_year, "(\\d{2})-(\\d{2})", "20\\1")
  
  # read document into text
  file_text_lines <- pdf_text(file_path) %>% # character for each page
    # converts to rows per df
    map2_dfr(., 1:length(.), ~ {
      # break new lines into rows
      text <- strsplit(.x, "\n")[[1]]
      # Remove empty lines
      text <- text[!str_detect(text, "^\\s*$")] 
      # Remove extra spaces from beginning
      text <- trimws(text)
      
      tibble(text = text, page = .y) # Convert to dataframe
    })
      
  
  # table header
  header <- file_text_lines %>%
    mutate(table_begin = grepl("Dist_ID", text),
           text = ifelse(table_begin == 1,
                         # addinf space ro identify columns
                         gsub("Dist_ID County", "Dist_ID  County", text),
                         text)) %>%
    filter(table_begin == 1) %>%
    pull(text) %>%
    .[1] %>%
    str_split(., pattern = "\\s{2,}", simplify = T)
           
  # some hand-corrections of header
  header[4] <- "Actual HCD Count"
  header[7] <- "Actual Rate"
  header[9] <- "Estimated HCD Count"
  header[10] <- "Estimated Allowed Expense"
  header[11] <- "Estimated Rate"
  header[12] <- "Preliminary Payment"
  
  header <- header[c(1,4:13)]
  
  # now, split each line into district-level data
  
  year_df_all_entries <- file_text_lines %>%
    mutate(table_begin = grepl("Dist_ID", text),
           row_keep = cumsum(table_begin)) %>%
    filter(row_keep > 0,
           !grepl("Total|Actual|HCD|Estimated", text),
          # grepl("^\\d", text)
          ) %>%
    mutate(
      text_length = nchar(text),
      text_ok = text_length >= 168,
      district_id = cumsum(text_ok),
      # highlight that the district id starts
      district_id_text = grepl("^\\d{4}", text),
      district_id_combine = cumsum(district_id_text)
      ) %>%
    group_by(district_id_combine) %>%
    mutate(
      rows = n(),
      text = ifelse(rows < 4, reduce(text, paste, sep = "  "), text)
    ) %>%
    distinct(district_id_combine,text, text_ok) 
  
  list_df_aligned <- align_cols(year_df_all_entries, format_year)
  
  return(list_df_aligned)
}
  
align_cols <- function(df, format_year, cols = c("V3","V7")){
  
  year_df_good <- df  %>%
    pull(text) %>%
    str_match(., "(\\d{4})\\s(\\w+\\s?[\\w-?/?\\s]+)\\s{2,}(\\d{1,})\\s{2,}(\\$.*)") %>%
    # last row is a character vector with multiple entries
    as.data.frame() %>%
    select(-V1) %>%
    # extra check that there should be no letter in V5
    mutate(V2 = ifelse(grepl("[a-z|A-Z]", V5), NA, V2))
  
  # save for hand-correction
  year_df_bad <- df[is.na(year_df_good$V2),]
  
  if(nrow(year_df_bad) > 0){
    year_df_bad$school_year <- format_year
  }
  
  # remove NA row
  year_df_good <- year_df_good %>%
    filter(!is.na(V2))
  
  # year_df <- file_text_lines %>%
  #   mutate(table_begin = grepl("Dist_ID", text),
  #          row_keep = cumsum(table_begin)) %>%
  #   filter(row_keep > 0,
  #          !grepl("Total", text),
  #          grepl("^\\d", text)) %>%
  #   mutate(
  #     # regular text length is 170+, so if the text_length < 170, the line broke due to some reason 
  #     text_length = nchar(text),
  #     text_ok = text_length > 170,
  #     district_id =)
  #   pull(text) %>%
  #   str_match(., "(\\d{4})\\s(\\w+\\s?[\\w-?/?\\s]+)\\s{2,}(\\d{1,})(.*)") %>%
  #   # last row is a character vector with multiple entries
  #   as.data.frame() %>%
  #   select(-V1) 
  
  # further split V5 into columns
  colV5_details <-   year_df_good %>%
    mutate(V5 = gsub(" ","  ", V5)) %>%
    pull(V5) %>%
    str_split(.,"\\s{2,}", simplify = T) %>%
    as.data.frame()
  # generally problem in rate column so V3 and V[7] 
  # first fix 3rd column
  
  # fix column alignment issues
  for(col in cols){
    # store the correct value
    mode_value <- max(colV5_details[[col]], na.rm = TRUE)
    
    # check if it's correct aka aligned or misaligned
    colV5_details$fix_flag <- ifelse(colV5_details[[col]] == mode_value, 0, 1)
    
    # fix alignment
    for(i in 8:parse_number(col)){
      colV5_details[colV5_details$fix_flag == 1, i+1] <- colV5_details[colV5_details$fix_flag == 1, i]
    }
    
    colV5_details[[col]] <- mode_value
    colV5_details <- select(colV5_details, -fix_flag)
    
  }
  
  # If bracket encountered that's a negative
  colV5_details <- colV5_details %>%
    mutate(V9 = str_replace_all(V9, "\\(\\$([\\d,]+\\.\\d{2})\\)", "-\\1"),
           # change everything to numeric
           across(everything(), parse_number)
    ) 
  
  # put together all parts
  year_df_good <- year_df_good %>%
    select(V2, V4) %>%
    mutate(across(everything(), parse_number)) %>%
    cbind(colV5_details) %>%
    # there are repeated col names
    janitor::clean_names() %>%
    rename_with(~header) %>%
    # making all same case
    janitor::clean_names() %>%
    mutate(school_year = format_year) %>%
    # some year level variables
    mutate(
      year_total_hcd = sum(actual_hcd_count),
      year_total_expense =  sum(actual_expenses),
      year_expense_above_30k = sum(actual_allowed),
      year_total_reimbursement = sum(total_payment),
      rate_diff = estimated_rate - actual_rate
    )
  
  return(list(year_df_good, year_df_bad))
}


all_year_combine <- function(file_path) {
  
  # get all file paths
  file_paths <- file_to_read(file_path)
  
  # get all district-year-level df
  all_year_df_good <- map_dfr(file_paths$file_path[-nrow(file_paths)], 
                         ~one_year_hcd_data(.x)[[1]])
  
  # get all district-year-level df
  all_year_df_bad <- map_dfr(file_paths$file_path[-nrow(file_paths)], 
                              ~one_year_hcd_data(.x)[[2]])
  
  # return admw df
  return(list(file_paths, all_year_df_good, all_year_df_bad))
}

# call function and save clean_df
# okay - doesn't work for 2019 (some weird reading issue - check)
# Add - code for two templated - preliminary and actual payment
# There's only preliminary document for 2023-23

hcd_year_df <- all_year_combine("data_raw/High Cost Disability")

# save 
# write.csv(hcd_year_df[[3]], "data_clean/hcd_year_to_hand_clean.csv")

# after hand correction, read and add to the df
hcd_hand_corrected <- read.csv("data_clean/hcd_year_to_hand_clean.csv") %>%
  select(school_year, text.1) %>%
  rename("text" = text.1)

align_hcd_hand_corrected <- align_cols(df = hcd_hand_corrected, format_year = unique(hcd_hand_corrected$school_year))

# add hand-corrected values to full df and re-calculate the totals

hcd_year_df[[2]] <- hcd_year_df[[2]] %>%
  rbind(align_hcd_hand_corrected[[1]]) %>%
  group_by(school_year) %>%
  mutate(
    year_total_hcd = sum(actual_hcd_count),
    year_total_expense =  sum(actual_expenses),
    year_expense_above_30k = sum(actual_allowed),
    year_total_reimbursement = sum(total_payment),
    rate_diff = estimated_rate - actual_rate
  ) %>%
  ungroup()

# write.csv(hcd_year_df[[1]], "dSata_clean/hcd_file_paths_metadata.csv")
# write.csv(hcd_year_df[[2]], "data_clean/hcd_year_good.csv")
