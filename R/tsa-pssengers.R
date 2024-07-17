# Vizualize TSA passenger data

# Load libraries
library(rvest)
library(tidyverse)

# Read the HTML content of the website for 2024
webpage <- read_html("https://www.tsa.gov/travel/passenger-volumes") 

# Select the table using CSS selector 
table_node <- html_nodes(webpage, "table") 

# Extract the table content 
table_content <- html_table(table_node)[[1]]

# Print the table 
head(table_content)

# clean table to match formate from archived years
data_2024 <- table_content |> 
  select(1:2) |> 
  rename("Numbers" = "2024")


# get data for previous years; test on 2023 then iterate over all available years
webpage <- read_html("https://www.tsa.gov/travel/passenger-volumes/2023")
table_node <- html_nodes(webpage, "table")
table_content <- html_table(table_node)[[1]]
head(table_content)

# map over all years with availavle data
years <- 2019:2023
data <- map(years, ~{
  webpage <- read_html(paste0("https://www.tsa.gov/travel/passenger-volumes/", .x))
  table_node <- html_nodes(webpage, "table")
  table_content <- html_table(table_node)[[1]]
  table_content
})

# combine all data
data_all <- bind_rows(data) |> bind_rows(data_2024)
head(data_all)

# covert date column to date format
data_all <- data_all |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# convert numbers column to numeric and remove commas
data_all <- data_all |> 
  mutate(Numbers = as.numeric(gsub(",", "", Numbers)))

# Visualize TSA passenger data ---------------------------------

# pull out max and min values for highlighting
max <- data_all |> 
  filter(Numbers == max(Numbers)) 
min <- data_all |> 
  filter(Numbers == min(Numbers)) 


data_all |> 
  ggplot(aes(x = Date, y = Numbers)) +
  geom_point(shape = 21) +
  # geom_line(size = 0.1) +
  # geom_smooth() +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, scale = 1e-6)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(title = "Daily US Airline Passengers, Jan 2019 to July 2024", x = "", y = "Number of Passengers (Millions)", 
       caption = "Data: TSA.gov | Vizualization: Bryan M Maitland") +
  theme_classic() + 
  theme(
    plot.caption = element_text(hjust = 0), 
    axis.title.y = element_text(margin = margin(r = 10)), 
    plot.title = element_text(size = 14, face = "bold")
    ) +
  # highlight the highest value
  geom_point(data = max, color = "red", size = 2) + 
  geom_text(data = max, 
            aes(label = scales::number(max(Numbers), accuracy = 1, big.mark = ",")), 
            color = "red", hjust = .8, vjust = -.8, size = 3)  + 
  # highlight the lowest value
  geom_point(data = min, color = "blue", size = 2) + 
  geom_text(data = min, 
            aes(label = scales::number(min(Numbers), accuracy = 1, big.mark = ",")), 
            color = "blue", hjust = -.3, vjust = 1, size = 3)  + 
  # add annotation saying "March 11 2020 WHO declares COVID-19 a global pandemic" on March 11 2020, and break into 3 lines
  annotate("text", x = as.Date("2020-03-20"), y = 3e6, 
           label = "March 11 2020:\nWHO declares COVID-19\na global pandemic", 
           color = "gray3", hjust = 0, vjust = 1, size = 2) +
  # add a line to show the start of the pandemic
  geom_vline(xintercept = as.Date("2020-03-11"), linetype = "dashed", color = "gray3") 

