# Plot of Japan's Chrry Blossom chnaging phenology

# download from https://www.ncei.noaa.gov/access/paleo-search/study/26430

devtools::install_github("dill/emoGG")

library(readxl)
library(ggplot2)
library(emoGG)
library(ggpubr)
library(lubridate)

cherry <- read_xls(
  path = here::here("KyotoFullFlowerW.xls"), 
  col_names = TRUE, col_types = c("numeric", "numeric", "numeric", "text", "text"), 
  range = "A16:E1226"
  )

cherry <- cherry[!is.na(cherry$`Full-flowering date (DOY)`), ]
colnames(cherry)[1:2] <- c("year", "yday")

emoji_search(search = "flower")  # code: 1f338

fuji <- png::readPNG(here::here("fuji.png"))

y.date <- parse_date_time(c("3-20","4-1","4-10","4-20","5-1","5-10"), orders = "md")
y.ticks <- yday(y.date)
y.lab <- paste(month(x = y.date, label = TRUE), day(y.date))

p <- ggplot(data = cherry, aes(x = year, y = yday)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12, family = "Apple Chancery"), 
        axis.title = element_text(size = 12, family = "Apple Chancery")) +  
  background_image(raster.img = fuji) + 
  geom_line() + 
  geom_emoji(emoji = "1f338", size = .025) + 
  geom_smooth(color = "black") + 
  scale_y_continuous(name = element_blank(), breaks = y.ticks, labels = y.lab) + 
  scale_x_continuous(name = "Year", breaks = c(812,1000,1250,1500,1750,2000), expand = c(.01,0))
p

