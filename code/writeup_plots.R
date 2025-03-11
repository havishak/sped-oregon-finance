# script for plots
library(tidyverse)
library(gghighlight)
library(geomtextpath)
library(ggrepel)
library(patchwork)
library(RColorBrewer)
library(ggsankey)
library(sf)
library(readxl)
library(plotly)
library(tigris)
# install.packages("devtools")
#devtools::install_github("davidsjoberg/ggsankey")

background_color <- "white"
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
      axis.ticks = element_line(),
      plot.background = element_rect(fill =background_color,color = "transparent"),
      panel.background = element_rect(fill =background_color,color = "transparent")
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
                              mean_type = 1, add_dollar = F,
                              round_pos = 2){
  
  plot_color <- sample(brewer.pal(8, "Dark2"), 1)
  df_mean <- df %>%
    distinct(school_year, district_id, district_name, {{var_y}},
             {{mean_numerator}}, {{mean_denominator}}) %>%
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
              color = "gray80", alpha = 0.5) +
    geom_line(data = df_mean,
               aes(x = school_year, y = mean_val), color = plot_color)+
    geom_point(data = df_mean,
               aes(x = school_year, y = mean_val), color = plot_color) +
    geom_text_repel(data = df_mean,
               aes(x = school_year, y = mean_val, label = round(mean_val, round_pos)), 
              color = plot_color,
              size = 4) +
    # geom_smooth(se = F)+
    # scale_y_log10(
    #   breaks = c(1,10,100,1000, 10000, 50000)
    # ) +
    theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line(),
      plot.background = element_rect(fill =background_color,color = "transparent"),
      panel.background = element_rect(fill =background_color,color = "transparent")
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
    filter(school_year == year, !is.na({{var_x}}), !is.na({{var_y}})) %>%
    distinct(school_year, {{var_x}}, {{var_y}}) %>%
    ggplot(aes(x = {{var_x}}, y = {{var_y}})) +
    geom_point(color = "gray60", alpha = 0.7) +
    geom_smooth(se = F, color = plot_color, method = "lm")+
    labs(
      x = var_x_name,
      y = var_y_name,
      title = str_wrap(paste0("Relationship between ", var_y_name, " and ",var_x_name), 50)
    ) +
    theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line(),
      plot.background = element_rect(fill =background_color,color = "transparent"),
      panel.background = element_rect(fill =background_color,color = "transparent")
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
      filter(school_year == year, !is.na({{var_x}}), !is.na({{var_y}})) %>%
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
        x = var_y_name,
        y = paste0("Quintiles of ", var_x_name),
        title = str_wrap(paste0("Average Rate at Districts"),
                         30)
      ) +
      theme_minimal()+
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.background = element_rect(fill =background_color,color = "transparent"),
        panel.background = element_rect(fill =background_color,color = "transparent")
      )
    
    plot <- plot + bar_plot+
      plot_layout(widths = c(2, 1))
    
  }
  
  return(plot)
  
}

hcd_bar_plot <- function(df, var_x, var_label, title, nudge = 0, seed = 1,
                         position = "left"){
  
  set.seed(seed)
  fill_color <- sample(brewer.pal(12,"Set3"), 1)
  
  plot <- ggplot(df,
         aes(y = fct_rev(factor(school_year)),
             x = {{var_x}})) +
    geom_col(fill = fill_color) +
    geom_text(aes(label = {{var_label}}),
              nudge_x = nudge,
              size = 3) +
    theme_void() +
    ggtitle(title)
  
  if(position == "left"){
    plot <- plot +
      scale_x_reverse() +
      theme(plot.title = element_text(hjust = 1),
    plot.background = element_rect(fill =background_color,color = "transparent"),
    panel.background = element_rect(fill =background_color,color = "transparent"))
  }else{
    plot <- plot +
      theme(axis.text.y = element_text(color = "gray30"),
            plot.background = element_rect(fill =background_color,color = "transparent"),
            panel.background = element_rect(fill =background_color,color = "transparent"))
  }
  
  return(plot)
  
}

# state maps
state_map_var_discrete <- function(df, fill_var, text_var, title, subtitle, return = "plotly"){
  
  # create discrete variable
  
  df <- df %>%
    mutate(
      cat = ntile({{fill_var}}, 5))
  
  # plot
  plot <- ggplot(df,
                 aes(fill = cat, text = {{text_var}})) +
    geom_sf(color = "white") +
    geom_sf_text(aes(label = iso3166_2), size = 3) +
    theme_void() +
    labs(title = title,
         subtitle = subtitle,
         caption = "Source: US 2022 Census Bureau",
         fill = "High to Low") +
    scale_fill_distiller(palette = "PiYG",
                         labels = rep("", 5)) +
    theme(
      text = element_text(family = "serif"),
      axis.line = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
      plot.background = element_rect(fill =background_color,color = "transparent"),
      panel.background = element_rect(fill =background_color,color = "transparent"))
  
  # make a plotly object
  plot_plotly <- ggplotly(plot, tooltip = "text") %>%
    layout(
      annotations = list(
        list(
          x = 0.5, y = 1.05, # Subtitle position
          text = subtitle,
          showarrow = FALSE,
          xref = "paper", yref = "paper",
          font = list(size = 14, color = "black", face = "italic")
        ),
        list(
          x = 0.5, y = .01, # Caption position
          text = "Source: US 2022 Census Bureau", 
          showarrow = FALSE,
          xref = "paper", yref = "paper",
          font = list(size = 12, color = "black")
        )
      )
    ) 
  
  if(return == "plot"){
    return(plot)
  }else{
    return(plot_plotly)
  }
}
