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
  year_df <- file_text_lines %>%
    mutate(table_begin = grepl("Dist_ID", text),
           row_keep = cumsum(table_begin)) %>%
    filter(row_keep > 0,
           !grepl("Total", text),
           grepl("^\\d", text)) %>%
    pull(text) %>%
    str_match(., "(\\d{4})\\s(\\w+\\s?[\\w-?/?\\s]+)\\s{2,}(\\d{1,})(.*)") %>%
    # last row is a character vector with multiple entries
    as.data.frame() %>%
    select(-V1) 
  
 # further split V5 into columns
 colV5_details <-   year_df %>%
    mutate(V5 = gsub(" ","  ", V5)) %>%
    pull(V5) %>%
    str_split(.,"\\s{2,}", simplify = T) %>%
    .[,-1] %>%
    as.data.frame()
    # generally problem in rate column so V3 and V[7] 
    # first fix 3rd column

 # fix column alignment issues
for(col in c("V3", "V7")){
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
 year_df <- year_df %>%
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
 
  return(year_df)
}


all_year_combine <- function(file_path) {
  
  # get all file paths
  file_paths <- file_to_read(file_path)
  
  # get all district-year-level df
  all_year_df <- map_dfr(file_paths$file_path[-nrow(file_paths)], ~one_year_hcd_data(.x))
  
  # return admw df
  return(list(file_paths, all_year_df))
}

# call function and save clean_df
# okay - doesn't work for 2019 (some weird reading issue - check)
hcd_year_df <- all_year_combine("data_raw/High Cost Disability")

