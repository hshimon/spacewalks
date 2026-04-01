# Name: Heather Shimon
# Email: heather.shimon@wisc.edu
# Date: 2026-03-31
# Description: learning reproducible software practices in R

# https://data.nasa.gov/resource/eva.json (with modifications)

# Packages
library(tidyverse) #tidyverse "contains" ggplot2
library(jsonlite)
library(lubridate)

# data files
input_file  <- "./eva-data.json"
output_file <- "./eva-data.csv"
graph_file  <- "./cumulative_eva_graph.png"

# Read JSON array into a tibble
eva_tbl <- jsonlite::fromJSON(input_file) %>% 
  as_tibble()

# Convert types and drop missing duration/date
eva_tbl <- eva_tbl %>% 
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE)) %>% 
  filter(!is.na(duration), duration != "", !is.na(date))

# writing the data to an output file
readr::write_csv(eva_tbl, output_file)

# sorting the data by date
eva_tbl <- eva_tbl %>% 
  arrange(date)

# duration_hours and cumulative_time
eva_tbl <- eva_tbl %>% 
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours)
  )

# plotting the data
cumulative_spacetime_plot <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

# saving the plot to a file
ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)
print(cumulative_spacetime_plot)
