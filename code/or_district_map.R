#trying district map plots
library(sf)
library(tidyverse)
library(plotly)
library(tigris)

or_school_district <- st_read(
  "data_raw/EDGE_SCHOOLDISTRICT_TL24_SY2324/EDGE_SCHOOLDISTRICT_TL24_SY2324.shp") %>%
  janitor::clean_names() %>%
  filter(statefp == 41) %>%
  as.data.frame() %>%
  select(name, lograde, higrade, intptlon, intptlat) %>%
  mutate(across(c(intptlon, intptlat), parse_number),
         hover_text = paste0(name, "\n", "Grades: ", lograde,"-",higrade))

or_counties <- counties(cb = TRUE,
                        resolution = "20m",
                        class = "sf",
                        state = "Oregon") %>%
  janitor::clean_names()

map <- ggplot() +
  geom_sf(data = or_counties, fill = "#FEE123", color = "gray80", alpha = 0.5) +  # Districts
  geom_point(data = or_school_district,
             aes(x = intptlon, y = intptlat, text = name), color = "#154733", size = 1) +
  theme_void() +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Location of Oregon School Districts")

  ggplotly(map, tooltip = "name") %>%
    layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
