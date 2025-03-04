# script for plots
library(tidyverse)
library(gghighlight)
library(geomtextpath)
library(ggrepel)
library(patchwork)
library(RColorBrewer)
library(ggsankey)
# install.packages("devtools")
#devtools::install_github("davidsjoberg/ggsankey")

# This creates a line plot by district across all years for one continuous variable - related to admission
cont_across_years <- function(df, var_y , var_x, log_on = T,
                              lab_x = "", lab_y = ""){
  
  plot <- df %>%
    distinct(school_year, district_id, district_name, {{var_y}}) %>%
    ggplot(aes(x = school_year, y = {{var_y}})) +
    geom_line(aes(group = district_name, color = district_name)) +
    # geom_smooth(se = F)+
    # scale_y_log10(
    #   breaks = c(1,10,100,1000, 10000, 50000)
    # ) +
    theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line()
    ) +
    scale_x_continuous(breaks = unique(df$school_year))+
    labs(
      x = lab_x,
      y = lab_y
    ) +
    guides(color = "none")
  
  
  if(log_on == TRUE){
    plot <- plot +
      scale_y_continuous(trans = "log10",
                         breaks = c(1, 10, 100, 1000, 10000, 50000),
                         expand = c(0,0))
    
  }else{
    plot <- plot +
      scale_y_continuous(expand = c(0,0))
  }
  
  return(plot)
}

# This creates a line plot by district across all years for one continuous variable - related to admission
cont_across_years_w_mean <- function(df, var_y , var_x, log_on = T,
                              lab_x = "", lab_y = "", mean_numerator, mean_denominator,
                              mean_type = 1, add_dollar = F){
  
  plot_color <- sample(brewer.pal(8, "Dark2"), 1)
  df_mean <- df %>%
    group_by(school_year) %>%
    summarize(
      mean_val = ifelse(mean_type == 1, 
                        sum({{mean_numerator}}, na.rm = T)/sum({{mean_denominator}}, na.rm = T),
                        mean({{var_y}}, na.rm = T))
    )
  
  plot <- df %>%
    distinct(school_year, district_id, district_name, {{var_y}}) %>%
    ggplot(aes(x = school_year, y = {{var_y}})) +
    geom_line(aes(group = district_name),
              color = "gray80", alpha = 0.3) +
    geom_line(data = df_mean,
               aes(x = school_year, y = mean_val), color = plot_color)+
    geom_point(data = df_mean,
               aes(x = school_year, y = mean_val), color = plot_color) +
    geom_text_repel(data = df_mean,
               aes(x = school_year, y = mean_val, label = round(mean_val, 2)), 
              color = plot_color,
              size = 2) +
    # geom_smooth(se = F)+
    # scale_y_log10(
    #   breaks = c(1,10,100,1000, 10000, 50000)
    # ) +
    theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line()
    ) +
    scale_x_continuous(breaks = unique(df$school_year))+
    labs(
      x = lab_x,
      y = lab_y
    ) +
    guides(color = "none")
  
  
  if(log_on == TRUE & add_dollar == FALSE){
    plot <- plot +
      scale_y_continuous(trans = "log10",
                         #breaks = c(1, 10, 100, 1000, 10000, 50000),
                         expand = c(0,0))
    
  }else {
    if(log_on == TRUE & add_dollar == TRUE){
    plot <- plot +
      scale_y_continuous(trans = "log10",
                         #breaks = c(1, 10, 100, 1000, 10000, 50000),
                         expand = c(0,0),
                         labels = scales::dollar_format())
    
  }else{
    plot <- plot +
      scale_y_continuous(expand = c(0,0))
  }
  }
  
  return(plot)
}

# one year scatter plot of two variables
# can also show average proportion of one variable by quintiles of another
# related to admission
one_year_scatterplot_w_bars <- function(df, year = 2022, 
                                 var_y, var_x, log_x_on = T, log_y_on = T,
                                 add_bars = T,
                                 var_x_name = "", var_y_name = "") {
  
  plot_color <- sample(brewer.pal(8, "Dark2"), 1)
  
  plot <- df %>%
    filter(school_year == year) %>%
    distinct(school_year, {{var_x}}, {{var_y}}) %>%
    ggplot(aes(x = {{var_x}}, y = {{var_y}})) +
    geom_point(color = "gray60", alpha = 0.7) +
    geom_smooth(se = F, color = plot_color, method = "lm")+
    labs(
      x = var_x_name,
      y = var_y_name,
      title = paste0("Relationship between ", var_y_name, " and ",var_x_name)
    ) +
    theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line()
    )
  
  if(log_x_on == TRUE){
    plot <- plot +
      scale_x_continuous(trans = "log10",
                         breaks = c(1, 10, 100, 1000, 10000))
  }
  
  if(log_y_on == TRUE){
    plot <- plot +
      scale_y_continuous(trans = "log10",
                         breaks = c(1, 10, 100, 1000, 10000))
  }
  
  if(add_bars == TRUE){
    bar_plot <- df %>%
      filter(school_year == year) %>%
      group_by(school_year) %>%
      mutate(rank_x = ntile({{var_x}}, 5)) %>%
      group_by(school_year, rank_x) %>%
      summarize(
        per_mean = mean({{var_y}})
      ) %>%
      distinct() %>%
      ggplot(
        aes(x = per_mean, y = fct_rev(factor(rank_x)))
      ) +
      geom_col(fill = plot_color) +
      labs(
        x = NULL,
        y = paste0("Quintiles of ", var_x_name),
        title = paste0("Average ",var_y_name)
      ) +
      theme_minimal()+
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_line()
      )
    
    plot <- plot + bar_plot+
      plot_layout(widths = c(2, 1))
    
  }
  
  return(plot)
  
}
