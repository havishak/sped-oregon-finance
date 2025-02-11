# Collection of functions to read and clean ADMw files.
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

# scrape district estimate files
year_district_data <- function(file_path) {
  # get year to add to df
  get_year <- str_extract(file_path, "\\d{2}-\\d{2}")
  format_year <- str_replace(get_year, "(\\d{2})-(\\d{2})", "20\\1")
  
  # each line of pdf is a row
  file_text_lines <- pdf_text(file_path) %>% # character for each page
    # converts to rows per df
    map2_dfr(., 1:length(.), ~ {
      text <- strsplit(.x, "\n")[[1]]
      # remove empty lines
      text <- text[!grepl("^\\s*$", text)]
      # remove trailing spaces
      text <- gsub("^\\s\\s+", "", text)
      # remove multiple spaces in between
      text <- gsub("\\s+", " ", text)
      
      # there are a few negatives with a symbol other than minus
      text <- gsub("‐", "-", text)
      # convert to df
      page_df <- data.frame(text = text, page = .y)
      
      return(page_df)
    }) %>%
    mutate(district_id_found = as.integer(grepl("District ID:", text)))
  
  # now, split the data into individual district
  
  # see which pages cover each district
  get_district_page <- file_text_lines %>%
    select(page, district_id_found) %>%
    distinct() %>%
    filter(district_id_found == 1) %>%
    mutate(district_index = row_number()) %>%
    select(-district_id_found) %>%
    full_join(tibble(page = 1:max(file_text_lines$page)), by = "page") %>%
    arrange(page) %>%
    fill(district_index, .direction = "down") %>%
    mutate(school_year = format_year)
  
  # split
  
  district_year_list <- file_text_lines %>%
    left_join(get_district_page) %>%
    split(., .$district_index)
  
  # remove rows with reduntant information from all dfs
  ## Template
  ## Ignore: Starting with State, As, Page, school_year if length < 20
  
  district_year_list <-  map(
    district_year_list,
    ~ .x %>%
      mutate(
        text_length = nchar(text),
        remove_text = ifelse(grepl(paste0("^", format_year), text) &
                               text_length < 25, 1, 0)
      ) %>%
      filter(
        !grepl("^STATE|As of|^Page", text),
        remove_text == 0
      ) %>%
      select(text, page, school_year)
  )
  
  
  return(district_year_list)
}


one_district_info <- function(district_df) {
  # classify info
  # line 1: location, district name District ID: number
  # categories have :
  # one row above first category is entry name
  # second last row starts with school_year and ADMw for this and previous row
  # Lines in between has categories in the format Category: ADM_current X weight = ADMw_current ADM_past X 1.00 = ADMw_past (so 6 values to extract)
  # last row is extended ADMw combined for the district
  
  district_df <- district_df %>%
    mutate(
      information_type = case_when(
        grepl("District ID", text) ~ "district_info",
        # adding as files from 2014 onwards have this in the entry name
        grepl("for funding calculation|information only", text) ~ "entry_name",
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
  
  # grab each entry-level information
  entry_category_information <- district_df %>%
    filter(entry_index != 0) %>%
    split(., .$entry_index) %>%
    map_dfr(., ~ {
      # pattern different for 2024, no = sign
      category_pattern <-  "(.*):\\s*(-?[\\d,]+\\.\\d+)?\\s*X\\s*(-?[\\d.]+)\\s*=?\\s*(-?[\\d,]+\\.\\d+)?\\s*(-?[\\d,]+\\.\\d+)?\\s*X\\s*(-?[\\d.]+)\\s*=?\\s*(-?[\\d,]+\\.\\d+)?"
      
      
      .x %>%
        filter(information_type == "category_info") %>%
        select(text) %>%
        unlist() %>%
        str_match(., category_pattern) %>%
        as.data.frame() %>%
        select(-V7) %>%
        rename(
          "org_text" = "V1",
          "category" = "V2",
          "current_year_adm" = "V3",
          "category_weight" = "V4",
          "current_year_admw" = "V5",
          "past_year_adm" = "V6",
          "past_year_admw" = "V8"
        ) %>%
        mutate(entry_name = unique(.x[.x$information_type == "entry_name", ]$text),
               entry_index = unique(.x$entry_index))
    }) %>%
    mutate(
      across(current_year_adm:past_year_admw, ~ gsub(",", "", .x)),
      across(current_year_adm:past_year_admw, ~ ifelse(is.na(.x), 0, .x)),
      across(current_year_adm:past_year_admw, as.numeric)
    )
  
  
  
  # compile a district_level_df
  
  district_level_df <- entry_category_information %>%
    mutate(
      district_geo = district_details[2],
      district_name = district_details[3],
      district_id = district_details[4],
      school_year = unique(district_df$school_year),
      current_year_adm_district = sum(current_year_adm[category == "ADMr"]),
      current_year_admw_district = sum(current_year_admw),
      past_year_adm_district = sum(past_year_adm[category == "ADMr"]),
      past_year_admw_district = sum(past_year_admw)
    ) %>%
    group_by(entry_name) %>%
    mutate(
      current_year_adm_entry = sum(current_year_adm[category == "ADMr"]),
      current_year_admw_entry = sum(current_year_admw),
      past_year_adm_entry = sum(past_year_adm[category == "ADMr"]),
      past_year_admw_entry = sum(past_year_admw),
      current_year_effective_admw_entry = max(current_year_admw_entry, past_year_admw_entry),
      current_weight_entry = ifelse(
        current_year_effective_admw_entry == current_year_adm_entry,
        1,
        0
      )
    ) %>%
    ungroup() %>%
    mutate(
      current_year_eff_admw_district = sum(unique(current_year_effective_admw_entry)),
      current_year_extra_weight_district = current_year_eff_admw_district - current_year_adm_district,
      current_weight_district = ifelse(current_year_extra_weight_district < 1, 1, 0)
    )
  
  return(district_level_df)
}

all_district_year_combine <- function(file_path) {
  # get all file paths
  file_paths <- file_to_read(file_path)
  
  # get all district-year-level df
  district_year_df <- map_dfr(file_paths$file_path, ~ {
    year_district_list <- year_district_data(.x)
    
    print(paste0(.x, " done!"))
    
    year_district_df <- map_dfr(year_district_list, one_district_info)
    
    return(year_district_df)
  })
  
  # return admw df
  return(list(file_paths, district_year_df))
}

# call function and save clean_df
# admw_district_df <- all_district_year_combine("data_raw/District Estimates ADMw Breakouts")
# write.csv(admw_district_df[[1]], "data_clean/district_admw_file_paths_metadata.csv")
# write.csv(admw_district_df[[2]] %>% filter(!is.na(category)), "data_clean/district_year_admw.csv")

