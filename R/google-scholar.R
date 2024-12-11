# Analyze and visualize Google Scholar data

# libraries
library(tidyverse) 
library(googlesheets4)
library(scholar) 
library(ggrepel)
library(lemon)
library(patchwork)

# Get data =============================================

# Definitions
id <- "tGn-FzAAAAAJ" 
Author_lastname <- c("Maitland")

# Get profile information
profile <- get_profile(id)

# List of scholar functions
# format_publications(id, "BM Maitland") |> cat(sep='\n\n')
# get_publications(id)
# get_citation_history(id)
# get_num_articles(id)
# get_num_distinct_journals(id)
# get_num_top_journals(id)
# get_oldest_article(id)

# Publication history =====================================================

# Get my publications
pubs <- get_publications(id) |> 
  as_tibble() |> 
  arrange(year) |>
  rownames_to_column()

pubs |> 
  print(n = Inf)

pubs <- pubs |> 
  filter(! rowname %in% c("27"))

# Add a column for the publication type
pubs <- pubs |> 
  mutate(type = case_when(
    rowname %in% c("4","15") ~ "Thesis",
    rowname %in% c("1","18") ~ "Report",
    TRUE ~ "Refereed"
  ))


# Tag first-author papers
# extract list of first authors
lauth <- str_split_fixed(pubs$author, ",", 2)[ ,1]
# add first author tag for matches to my name
pubs$first <- ifelse(grepl(Author_lastname, lauth), 1, 0)
# co-first author
pubs$first[pubs$pubid=="O3NaXMp0MMsC"] <- 1 


# Get journal impact factors
# fix names
pubs$journal <- ifelse(pubs$journal == "Elementa: Science of the Anthropocene", "Elementa", pubs$journal)
pubs$journal <- ifelse(pubs$journal == "Ecology, e", "Ecology", pubs$journal)

# list of journals
journals <- pubs |> distinct(journal) |> pull(journal)

# download ranks
j_ranks <- 
  get_journalrank(journals) |> 
  as_tibble()|> 
  janitor::clean_names() |> 
  select(journal, type, rank, sjr, h_index)

# Join the impact factor to the pubs
pubs <- left_join(pubs, j_ranks, by = c("journal" = "journal")) |> 
  select(-type.y) |> 
  rename(type = type.x) 

# give water research and education a sjr of 0.2
pubs <- mutate(pubs, sjr = ifelse(pubid == "BqipwSGYUEgC", 0.2, sjr))

# # Get IFs from googsheets 
# gs4_deauth()
# j_if_eco <- read_sheet("https://docs.google.com/spreadsheets/d/1uG2Dg0LogysCSAsK51Rh9lD_dxRRFOeo2jq92_TwqF0/edit?gid=0#gid=0", range = "ecology")
# j_if_evo <- read_sheet("https://docs.google.com/spreadsheets/d/1uG2Dg0LogysCSAsK51Rh9lD_dxRRFOeo2jq92_TwqF0/edit?gid=0#gid=0", range = "evolution")
# j_if_con <- read_sheet("https://docs.google.com/spreadsheets/d/1uG2Dg0LogysCSAsK51Rh9lD_dxRRFOeo2jq92_TwqF0/edit?gid=0#gid=0", range = "conservation")
# j_if_aqu <- read_sheet("https://docs.google.com/spreadsheets/d/1uG2Dg0LogysCSAsK51Rh9lD_dxRRFOeo2jq92_TwqF0/edit?gid=0#gid=0", range = "aquatic")
# 
# # j_if_eco |> mutate(across(
# #   where(is.list),
# #   # list conversion to char leaves "NULL" entries > TRICKY: NULLs convert to NA
# #   ~ as.character(.x) %>%
# #     na_if("NULL") %>%
# #     as.numeric))
# 
# j_if_eco |> mutate(`Impact Factor` = map_dbl(`Impact Factor`, as.double)) |> print(n=Inf)
# 
# # bind the sheets
# j_if <- bind_rows(j_if_eco, j_if_evo, j_if_con, j_if_aqu)


# Plots

# plot pub year vs SJR
p1 <- pubs |> 
  mutate(first = as.character(first)) |>
  ggplot(aes(y = sjr, x = year)) +
  geom_point(
    aes(fill = sjr, stroke = first), 
    size=4, shape=21, na.rm = TRUE
    ) +
  scale_discrete_manual(
    aesthetics = "stroke", values = c("0" = 1, "1" = 2), guide = "none") + 
  scale_fill_gradient2(
    low="grey70",mid="khaki3",high="deepskyblue3", guide = "none") +
  cowplot::theme_cowplot() +
  lemon::coord_capped_cart(bottom = 'both') +
  scale_x_continuous(
    breaks = c(min(pubs$year),max(pubs$year)),
    limits = c(min(pubs$year),max(pubs$year))
    ) + 
  labs(
    title = "Journal Rank (SJR)",
    subtitle = "Thick circles = first author papers",
    x = "", y = ""
  ) + 
  theme(
    axis.title.x = element_text(angle=0,color="black",hjust = 0.05,size=11),
    axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(color="grey",linetype = 3),
    axis.title.y.left = element_blank(), 
    axis.text.y = element_text(size=9,color="grey30"),
    plot.title = element_text(color="grey40",face = "bold",size=12),
    plot.subtitle = element_text(color="grey40",face = "bold",size=10, vjust = 0),
    legend.position.inside = c(0.6,0), 
    legend.text= element_text(size=9,color="grey30")
  )
p1

# plot stacked column chart of publication type by year
pub_counts <- pubs |> 
  group_by(type) |>
  count()

p2 <- pubs |> 
  ggplot(aes(x = year, fill = type)) +
  geom_bar(position = "stack", width = 0.5) + 
  scale_fill_manual(
    values = c("Refereed" = "grey10", "Thesis" = "grey90", "Report" = "grey50"),
    label = paste0(pub_counts$type, " (n=", pub_counts$n, ")")
    ) + 
  cowplot::theme_cowplot() +
  coord_capped_cart(bottom='both')+
  scale_x_continuous(
    breaks = c(min(pubs$year),max(pubs$year)),
    limits = c(min(pubs$year)-0.5,max(pubs$year)+0.5)
    ) +
  scale_y_continuous(
    breaks = seq(0, 6, 2),
    limits = c(0, 6)
    ) +
  labs(
    title = "Research Output (2013-2024)",
    x = "", y = "", fill = ""
  ) + 
  theme(
    axis.title.x = element_text(angle=0,color="black",hjust = 0.05,size=11),
    axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(color="grey", linetype = 3),
    axis.title.y.left = element_blank(), 
    axis.text.y = element_text(size=9,color="grey30"),
    plot.title = element_text(color="grey40",face = "bold",size=12),
    legend.position = "inside",
    legend.position.inside = c(0.05, .85), 
    legend.text= element_text(size = 7,color = "black")
  )
p2



# Citation History ===========================================

# get publication history
citation_history <- 
  get_citation_history(id) |> 
  as_tibble()

# plot data
ggplot(citation_history, aes(year, cites)) + 
  geom_line() + 
  geom_point()

# nice plot
p3 <- citation_history |> 
  ggplot() +
  geom_bar(
    aes(x = year, y = cites, fill = cites), 
    stat="identity", position = "dodge", width = 0.5
    ) + 
  geom_text(
    aes(x = year, y = cites + 5, label = cites), 
    size=3, nudge_y = 10)+ 
  annotate(
    "text", x = 2013, y = 82, 
    label = paste(
      "Total citations: ", profile$total_cites, "\n", 
      "h-index: ", profile$h_index, "\n", 
      "i10-index: ", profile$i10_index
      ),
    hjust = 0, vjust = 0, size = 3, color = "black"
    ) +
  labs(
    title = "Citations (2013-2024)", 
  ) +
  scale_fill_gradient2(
    low = "khaki2", mid = "deepskyblue3", high = "dodgerblue4", 
    guide=FALSE, 
    midpoint = quantile(citation_history$cites, probs = 0.85)) + 
  scale_x_continuous(
    expand = c(0,0),
    breaks = c(min(citation_history$year),max(citation_history$year)),
    limits = c(min(citation_history$year)-1,max(citation_history$year)+1)
  ) + 
  cowplot::theme_cowplot() +
  coord_capped_cart(bottom='both')+
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x =element_blank(),
    axis.text.x = element_text(size=9,color="grey30"),
    axis.line.x = element_line(color="gray30"),
    plot.title = element_text(color="grey40",face = "bold",size=12), 
    plot.subtitle = element_text(color="grey40",face = "bold",size=10, vjust = 0)
    )
p3



# Panel plot ===================================

p1 + p3 + plot_layout(ncol = 2)
p2 + p3 + plot_layout(ncol = 2)


ggsave(
  "plots/google-scholar.png",
  width = 6.5, height = 3, scale = 1,
  bg = "white"
)


# Individual article history ================================

## Get article citation history
ach <- get_article_cite_history(id, pubs$pubid[5])

## Plot citation trend
ggplot(ach, aes(year, cites)) +
  geom_segment(aes(xend = year, yend = 0), linewidth = 1, color='darkgrey') +
  geom_point(size=3, color='firebrick')


# now do it for all articles
ach <- 
  pubs$pubid |> 
  map_df(~get_article_cite_history(id, .)) |> 
  as_tibble()

# get journal names for each article
ach <- 
  ach |> 
  left_join(pubs |> select(pubid, journal, type, author), by = c("pubid" = "pubid")) 

# plot citation trend for each article
ach |> 
  ggplot() +
  geom_segment(aes(x=year, xend=year, y=0, yend=cites), color="grey") +
  geom_point(aes(x=year, y=cites, color=type), size=2) +
  facet_wrap(~pubid, scales = "free_y", ncol = 3) +
  labs(
    title = "Citation history by article",
    x = "", y = ""
  ) +
  theme(
    axis.title.x = element_text(angle=0,color="black",hjust = 0.05,size=11),
    axis.text.x = element_text(size=9,color="grey30"),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(color="grey",linetype = 3),
    axis.title.y.left = element_blank(), 
    axis.text.y = element_text(size=9,color="grey30"),
    plot.title = element_text(color="grey40",face = "bold",size=12),
    plot.subtitle = element_text(color="grey40",face = "bold",size=10, vjust = 0)
  )



