# Read in the JSON data from the Climate Reanalyzer API
# By: Bryan M Maitland

# Load libraries
library(tidyverse)
library(jsonlite)

# link to the API output as a JSON file
json_file <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json"

# get the raw json into R
raw_json_list <- read_json(path=json_file)

# View the raw JSON data
View(raw_json_list)

str(raw_json_list, max.level = 2)

# get year
raw_json_list[[2]]$name
raw_json_list |> pluck(2, "name")
year <- raw_json_list |> pluck(2, "name")

# get data for year
raw_json_list[[2]]$data |> unlist()
# same thing with tidy verse
raw_json_list |> pluck(2, "data") |> unlist()
# same thing with tidy verse and cleaned up tibble
raw_json_list |> pluck(2, "data") |> unlist() |>
  tibble() |>
  rownames_to_column(var = "doy") |>
  rename(temp_c=2) |> 
  mutate(doy = as.numeric(doy), year = year) -> data

# plot the data
data |> 
  ggplot(aes(x=doy, y=temp_c, color = year)) +
  geom_line() +
  labs(title = year)


# fx to get the data for each year (row_n = row number of player in the df)
get_data <- function(row_n) {
  # get year
  year <- raw_json_list |> pluck(row_n, "name")
  # get data for year
  data <- raw_json_list |> 
    pluck(row_n, "data") |> 
    unlist() |> 
    tibble() |> 
    rownames_to_column(var = "doy") |> 
    rename(temp_c=2) |> 
    mutate(doy = as.numeric(doy), year = year)
  # return the data
  data
  
}

# test the function
# get_data(2)

# Get the data for all years
sst_df <- 1:length(raw_json_list) %>% 
  map_dfr(get_data)

# Extract mean and standard deviation records
sst_means <- sst_df |> 
  filter(year %in% c('1982-2011 mean', 'minus 2σ', 'plus 2σ'))

# now remove them
sst_df <- sst_df |> 
  filter(! year %in% c('1982-2011 mean', 'minus 2σ', 'plus 2σ'))

## Plot 

# custom scale for the colors
cc <- scales::seq_gradient_pal("lightblue", "blue", "Lab")(seq(0,1,length.out=47))

# plot
p1 <- sst_df |> 
  filter(! year %in% c(2023, 2024)) |> 
  ggplot(aes(x = doy, y = temp_c, color = year)) +
  geom_line() +
  geom_line(data = sst_df |> filter(year == 2023), aes(x=doy, y=temp_c), color = "gold4", linewidth = 1.5) +
  geom_line(data = sst_df |> filter(year == 2024), aes(x=doy, y=temp_c), color = "red", linewidth =2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(19.6, 21.2), breaks = seq(19.6, 21.2, 0.1)) + 
  scale_colour_manual(values = cc, guide="none") +
  labs(
    title = "Global Sea Surface Temperature (60N-60S): 1982-2024", 
    x = "Day of Year",
    y = "Sea Surface Temperature (C)",
    caption = "Data Source: Climate Reanalyzer | By Bryan Maitland"
  ) + 
  theme_classic() + 
  theme(
    axis.text.x = element_blank()
    )
p1

# save the plot
ggsave("plots/sst.png", width = 10, height = 6, units = "in", dpi = 300)


