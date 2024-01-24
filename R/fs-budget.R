# Data Vis - US FS Budget

library(tidyverse)
library(readxl)
library(scales)
library(ggrepel)
library(patchwork)
library(ggsci)
library(magick)



fs_budget <- read_excel("data/fs-budget.xlsx")

fs_budget <- fs_budget |> 
  pivot_longer(-Appropriations, names_to = "year", values_to = "budget") |> 
  mutate(year = as.numeric(year)) |> 
  mutate(label = if_else(year == max(year), 
                         as.character(Appropriations), 
                         NA_character_))


p1 <- fs_budget |> 
  ggplot(aes(x=year, y=budget, color=Appropriations)) +
  geom_vline(xintercept = 2023, linetype="dashed", color = "grey") +
  annotate(
    "text", x = 2022.1, y = 3100000, size = 4,
    label = "Actual", color = "grey") +
  annotate(
    "text", x = 2024.25, y = 3100000, size = 4,
    label = "Estimates", color = "grey") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_lancet() +  
  scale_x_continuous(
    breaks = seq(2011,2024,1),
    limits = c(2011,2034), 
    expand = c(0.01,0)
  ) +
  scale_y_continuous(
    limits = c(0,3250000),
    labels = label_number(scale_cut = cut_short_scale()), 
    expand = c(0.01,0.01)
  ) +
  labs(
    x = "", y = "",
    title = "U.S. Forest Service Budget by appropriations from 2011-2024",
    subtitle = "U.S. Dollars",
    caption = "Data source: U.S. Forest Service | Data viz: Bryan M Maitland"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.length = unit("2", "mm"),
    plot.title.position = "plot",
    plot.title = element_text(face="bold"),
    plot.caption = element_text(hjust = 0), 
    legend.position = "none"
  ) +
  geom_text_repel(
    data = fs_budget |> filter(!is.na(label)),
    aes(label = paste0("  ", label)),
    size = 4,
    segment.curvature = -0.1,
    segment.square = TRUE,
    box.padding = 0.1,
    point.padding = 0.6,
    nudge_x = 0.15,
    nudge_y = 1,
    force = 0.5,
    hjust = 0,
    direction="y",
    na.rm = TRUE, 
    xlim = c(2025,2034),
    ylim = c(0,2970620)
  )
p1

# image_read_svg(here::here("img","USFS-logo.png"))
# logo <- png::readPNG(here::here("img","USFS-logo.png"))
# image <- image_fill(logo, 'none')
# raster <- as.raster(image)
# myplot <- qplot(mpg, wt, data = mtcars)
# myplot + annotation_raster(raster, 25, 35, 3, 5)


ggsave("plots/fs-budget.png",
       width = 5, height = 3, scale = 1.7,
       bg = "white"
)


p2 <- fs_budget |> 
  ggplot(aes(x=year, y=budget, color=Appropriations)) +
  geom_vline(xintercept = 2023, linetype="dashed", color = "grey") +
  annotate(
    "text", x = 2022.3, y = 520000, size = 4,
    label = "Actual", color = "grey") +
  annotate(
    "text", x = 2024, y = 520000, size = 4,
    label = "Estimates", color = "grey") +
  geom_point(size = 2.5) +
  geom_line(size = 1.25) +
  scale_color_lancet() +  
  scale_x_continuous(
    breaks = seq(2011,2024,1),
    limits = c(2011,2034), 
    expand = c(0.01,0)
  ) +
  scale_y_continuous(
    limits = c(0,520000),
    labels = label_number(scale_cut = cut_short_scale()), 
    expand = c(0.01,0)
  ) +
  labs(
    x = "", y = "",
    title = "budget excluding National Forest System and Wildland Fire Management appropriations",
    subtitle = "U.S. Dollars",
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.length = unit("2", "mm"),
    plot.title.position = "plot",
    plot.title = element_text(face="bold"),
    plot.caption = element_text(hjust = 0), 
    plot.background = element_rect(fill = "white", color = "black", size = 2),
    legend.position = "none"
  ) +
  geom_text_repel(
    data = fs_budget_sub |> filter(!is.na(label)),
    aes(label = paste0("  ", label)),
    size = 5,
    segment.curvature = -0.1,
    segment.square = TRUE,
    box.padding = 0.1,
    point.padding = 0.6,
    nudge_x = 0.15,
    nudge_y = 1,
    force = 0.5,
    hjust = 0,
    direction="y",
    na.rm = TRUE, 
    xlim = c(2025,2034),
    ylim = c(0,600000)
  )
p2

# ggsave("plots/fs-budget-sub.png",
#        width = 7, height = 4, scale = 1.5, 
#        bg = "white"
# )

# p1 + p2 + 
#   plot_layout(widths = c(1.3, 1))
# 
# ggsave("plots/fs-budget-panel.png",
#        width = 12, height = 4, scale = 1.7, 
#        bg = "white"
# )
  