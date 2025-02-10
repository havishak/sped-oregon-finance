# Collection of functions to read and clean ADMw files.
library(tidyverse)
library(PDFR)
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
    mutate(file_path = paste0(dir,"/",files))
  
  return(file_list)
}

# scrape district estimate files
year_district_data <- function(file_path){
  
  # get year to add to df
  get_year <- str_extract(file_path, "\\d{2}-\\d{2}")
  format_year <- str_replace(get_year, "(\\d{2})-(\\d{2})", "20\\1-20\\2")
    
  # read file text in df format
  file_text <- pdf_data(file_path)
  
  # each line of pdf is a row
  file_text_lines <- map_dfr(1:length(file_text), ~{
    
    # if the y-coordinate of two lines is close to 2 px, then it's just one line
    y_corrected <- file_text[[.x]] %>%
      select(y) %>%
      arrange(y) %>%
      distinct() %>%
      mutate(y_corrected = ifelse(y - lag(y)< 2, lag(y), y),
             y_corrected = ifelse(is.na(y_corrected), y, y_corrected))
    
    # reduce each word of a line into one line
    text <- file_text[[.x]] %>%
      left_join(y_corrected) %>%
      group_by(y_corrected) %>%
      arrange(x) %>%
      summarise(
        text = reduce(text, paste)
      ) %>%
      # add page number and whether a district id was found
      mutate(
        page = .x,
        district_id_found = sum(grepl("District ID:", text))
      )
    
    return(text)
  })
  
  # now, split the data into individual district
  
  # see which pages cover each district
  get_district_page <- file_text_lines %>%
    select(page, district_id_found) %>%
    distinct() %>%
    filter(district_id_found == 1) %>%
    mutate(
      district_index = row_number()
    ) %>%
    select(-district_id_found) %>%
    full_join(tibble(page = 1:max(file_text_lines$page)),
              by = "page") %>%
    arrange(page) %>%
    fill(district_index, .direction = "down") %>%
    mutate(school_year = format_year)
  
  # split
  
  district_year_list <- file_text_lines %>%
    left_join(get_district_page) %>%
    split(., .$district_index)
  
  # remove rows with reduntant information from all dfs
  
  district_year_list <-  map(district_year_list, ~.x %>%
    mutate(text_length = nchar(text),
           remove_text = ifelse(grepl(paste0("^",school_year), text) & text_length < 25, 1,0)) %>%
    filter(!grepl("^STATE|^As|^Page", text),
           remove_text == 0) %>%
    select(text, page, y_corrected))
  
  
  return(district_year_list)
}


one_district_info <- function(district_df){
  
  # claissify info
  
  district_df <- district_df %>% 
    mutate(
      information_type = case_when(
        grepl("District ID", text) ~ "district_info",
        grepl(":", text) ~ "category_info",
        grepl("ADMw", text) ~ "total",
        TRUE ~ "entry_name"
      )
    )  %>%
    mutate(entry_index = cumsum(information_type == "entry_name"))
 
  # grab district level info
 district_details <- district_df %>% 
   filter(information_type == "district_info") %>%
   select(text) %>%
   unlist() %>%
   str_match(., "^(.*?),\\s*(.*?)\\s*District ID:\\s*(\\d+)$")
 
  
}
  
sy11_12 <- file_to_read("data_raw/District Estimates ADMw Breakouts")
sy11_12_districts <- year_district_data(sy11_12$file_path[1])
a <- sy11_12_districts[[1]]

## Template
## Ignore: Starting with State, As, Page, school_year if length < 20

a <- a %>%
  mutate(text_length = nchar(text),
         remove_text = ifelse(grepl("^2011", text) & text_length < 25, 1,0)) %>%
  filter(!grepl("^STATE|^As|^Page", text),
         remove_text == 0) %>%
  select(text, page, y_corrected)

# line 1: location, district name District ID: number
# arrange by y and page entry. categories start at ADMr: 
# one row above is entry name, up until entry name Extended ADMr:
# second last row starts with school_year and ADMw for this and previous row
# Lines in between has categories in the format Category: ADM_current X weight = ADMw_current ADM_past X 1.00 = ADMw_past (so 6 values to extract)

# last row is extended ADMw combined for the district
a <- a %>% 
  mutate(
    information_type = case_when(
      grepl("District ID", text) ~ "district_info",
      grepl(":", text) ~ "category_info",
      grepl("ADMw", text) ~ "total",
      TRUE ~ "entry_name"
    )
  )

# district details
a %>% 
  filter(information_type == "district_info") %>%
  select(text) %>%
  unlist() %>%
  str_match(., "^(.*?),\\s*(.*?)\\s*District ID:\\s*(\\d+)$")

# 
pattern <-  "(.*):\\s*([\\d,]+\\.\\d+)\\s*X\\s*([\\d.]+)\\s*=\\s*([\\d,]+\\.\\d+)\\s*([\\d,]+\\.\\d+)\\s*X\\s*([\\d.]+)\\s*=\\s*([\\d,]+\\.\\d+)"

a %>% 
  filter(information_type == "category_info") %>%
  select(text) %>%
  unlist() %>%
  str_match(., pattern)

b <- a %>%
  mutate(entry_index = cumsum(information_type == "entry_name"))


st

district_row <- grep("District ID", a$text)
district_info <- str_match(a$text[district_row], "^(.*?),\\s*(.*?)\\s*District ID:\\s*(\\d+)$")
district_geo <- district_info[2]
district_name <- district_info[3]
district_id <- district_info[4]
a <- a[-district_row,]
# something like this
district_extended_admw <- parse_number(a$text[grep(paste0("^",district_name," Extended"),a$text)])

# entry id
a <- a %>%
  mutate(
    start_with_adm = as.integer(grepl("^ADMr:", text))
  )
                                       
        
