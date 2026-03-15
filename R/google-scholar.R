# google-scholar.R
# Analyze and visualize Google Scholar data
# Requires: scholar_helpers.R (sourced below)

# ── Libraries ────────────────────────────────────────────────────────────────

library(tidyverse)
library(scholar)
library(ggrepel)
library(lemon)
library(patchwork)
library(scales)
library(glue)

source("R/scholar_helpers.R")   # helpers, theme, colour palette


# ── Configuration ─────────────────────────────────────────────────────────────
# Edit this section when your publication list changes; nowhere else needs
# touching for routine updates.

SCHOLAR_ID      <- "tGn-FzAAAAAJ"
AUTHOR_LASTNAME <- "Maitland"

# Pubids to exclude: confirmed duplicates or unwanted entries
# yD5IFk8b50cC = duplicate of 35N4QoGY0k4C (same Resist-Accept-Direct paper)
# SeFeTyx0c_EC = "Science to Understand, Manage, and Restore Floodplains" (book chapter / exclude)
EXCLUDE_PUBIDS <- c(
  "yD5IFk8b50cC",  # duplicate RAD paper (citations transferred to 35N4QoGY0k4C)
  "SeFeTyx0c_EC"   # conference special session, not a publication
)

# Citation transfers: named vector of pubid = cites to ADD after excluding duplicates
# Use when Scholar splits citations across duplicate entries
CITE_TRANSFERS <- c(
  "35N4QoGY0k4C" = 6   # absorbs 6 cites from excluded duplicate yD5IFk8b50cC
)

# Classification by stable pubid
# Theses: U Alberta (hC7cP41nSMkC) and U Wyoming (JV2RwH3_ST0C)
THESIS_IDS <- c(
  "-f6ydRqryjwC",  # "Stream Crossings in the Western Boreal Forest" – U Alberta MSc
  "JV2RwH3_ST0C"   # "Isotopic Ecology of Aquatic Communities..." – U Wyoming PhD
)

# Technical reports
# Note: pumped storage hydropower report (2025) not yet on Scholar — add pubid when available
REPORT_IDS <- c(
  "7PzlFSSx8tAC",  # "Status and trends of Lake Huron..." – GLFC report #1
  "pqnbT2bcN3wC"   # "Wisconsin Initiative on Climate Change Impacts..." – UW-Madison report #2
)

# Software (standalone packages/tools)
# Note: fPk4N6BV_jEC is the hatchR journal article (Fisheries) — classified as Refereed Article
# Add the CRAN package pubid here if/when it appears on your Scholar profile
SOFTWARE_IDS <- character(0)

# Data releases (Zenodo archives tied to papers)
DATA_IDS <- c(
  "cFHS6HbyZ2cC",  # bmait101/trophic-expansion: v2.0.0
  "dfsIfKJdRG4C",  # data/scripts repository
  "4OULZ7Gr8RgC"   # bmait101/lake-michigan-foodwebs: v1.0
)

# Co-first-author pubids
CO_FIRST_PUBIDS <- c("O3NaXMp0MMsC","fPk4N6BV_jEC")

# Manual SJR overrides: named vector of pubid = sjr_value
# BqipwSGYUEgC = Journal of Contemporary Water Research & Education (not indexed)
SJR_OVERRIDES <- c(
  "BqipwSGYUEgC" = 0.2
)

# Journal name variants → canonical names (must match xref_journal exactly)
# Add entries here whenever Scholar returns a truncated/variant journal name.
# "Ecology, e" is how Scholar sometimes truncates "Ecology, e02040" article IDs.
JOURNAL_LOOKUP <- c(
  "Ecology, e" = "Ecology"
)


# ── Fetch data ────────────────────────────────────────────────────────────────

profile          <- get_profile(SCHOLAR_ID)
citation_history <- get_citation_history(SCHOLAR_ID) |> as_tibble()

pubs_raw <- get_publications(SCHOLAR_ID) |>
  as_tibble() |>
  arrange(year)


# ── Clean & enrich publications ───────────────────────────────────────────────

xref_journal <- read_csv("data/journal_list.csv")

pubs <- pubs_raw |>
  # Remove known bad entries by stable pubid
  filter(!pubid %in% EXCLUDE_PUBIDS) |>
  
  # Transfer citations from excluded duplicates to canonical pubids
  mutate(cites = cites + ifelse(pubid %in% names(CITE_TRANSFERS),
                                CITE_TRANSFERS[match(pubid, names(CITE_TRANSFERS))],
                                0L)) |>
  
  # Standardise journal names before any joins
  mutate(journal = standardise_journal_names(journal, JOURNAL_LOOKUP)) |>
  
  # Classify publication type via helper (uses pubids, not rownames)
  classify_pub_type(
    thesis_ids   = THESIS_IDS,
    report_ids   = REPORT_IDS,
    software_ids = SOFTWARE_IDS,
    data_ids     = DATA_IDS
  ) |>
  
  # Short journal abbreviations
  left_join(xref_journal, by = "journal") |>
  
  # Extract last name of first-listed author
  mutate(
    first_author_lastname = str_extract(author, "^[^,]+") |>
      str_extract("\\S+$")
  ) |>
  
  # Short pub label — handle NA year (GitHub releases) and NA j_short gracefully
  mutate(
    year_label  = ifelse(is.na(year), "n.d.", as.character(year)),
    venue_label = ifelse(is.na(j_short), as.character(type), j_short),
    pubid_short = paste0(first_author_lastname, " et al. ", year_label, " (", venue_label, ")")
  ) |>
  
  # Flag first / co-first author papers
  mutate(
    first = as.integer(
      grepl(AUTHOR_LASTNAME, str_extract(author, "^[^,]+"), ignore.case = TRUE) |
        pubid %in% CO_FIRST_PUBIDS
    )
  )


# ── Journal impact data ───────────────────────────────────────────────────────

j_ranks <- get_journalrank(unique(pubs$journal)) |>
  as_tibble() |>
  janitor::clean_names() |>
  select(journal, rank, sjr, h_index) |>    # deliberately excludes get_journalrank()'s own `type` column
  distinct(journal, .keep_all = TRUE)        # guard against duplicate journal rows

pubs <- pubs |>
  left_join(j_ranks, by = "journal") |>
  # Apply manual SJR overrides — use match() so named-vector lookup is safe inside mutate()
  mutate(sjr = ifelse(
    pubid %in% names(SJR_OVERRIDES),
    SJR_OVERRIDES[match(pubid, names(SJR_OVERRIDES))],
    sjr
  ))


# ── Summary statistics ────────────────────────────────────────────────────────

pub_summary <- pubs |>
  summarise(
    n_total        = n(),
    n_refereed     = sum(type == "Refereed Article"),
    n_first_author = sum(first == 1 & type == "Refereed Article"),
    mean_sjr       = mean(sjr, na.rm = TRUE),
    median_sjr     = median(sjr, na.rm = TRUE),
    total_cites    = profile$total_cites,
    h_index        = profile$h_index,
    i10_index      = profile$i10_index,
    year_min       = min(year, na.rm = TRUE),
    year_max       = max(year, na.rm = TRUE)
  )

print(pub_summary)


# ── Plot helpers ──────────────────────────────────────────────────────────────

yr_min <- min(pubs$year, na.rm = TRUE)
yr_max <- max(pubs$year, na.rm = TRUE)
yr_label <- year_range_label(pubs$year)

pub_counts <- pubs |> count(type)   # used for legend labels


# ── P1 · Journal rank over time ───────────────────────────────────────────────

p1 <- pubs |>
  ggplot(aes(x = year, y = sjr)) +
  # All points: fill gradient by SJR, thin border
  geom_point(
    aes(fill = sjr),
    size = 4.5, shape = 21, stroke = 0.6,
    na.rm = TRUE, alpha = 0.9
  ) +
  # Overlay thick ring on first-author papers only
  geom_point(
    data = \(d) dplyr::filter(d, first == 1, !is.na(sjr)),
    size = 4.5, shape = 21, stroke = 2.2,
    fill = NA, color = "grey20", na.rm = TRUE
  ) +
  scale_fill_gradient2(
    low = "grey75", mid = "#70b8d4", high = "#1a237e",
    midpoint = median(pubs$sjr, na.rm = TRUE),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(yr_min, yr_max),
    limits = c(yr_min, yr_max),
    expand = expansion(mult = 0.05)
  ) +
  labs(
    title    = "Journal Rank (SJR)",
    subtitle = "Thick border = first-author paper"
  ) +
  theme_scholar() +
  theme(axis.line.x = element_line(color = "grey50"))

p1


# ── P2 · Publication output by type ──────────────────────────────────────────

p2 <- pubs |>
  filter(!is.na(year)) |>           # exclude NA-year software/repo entries
  ggplot(aes(x = year, fill = type)) +
  geom_bar(position = "stack", width = 0.6) +
  scale_fill_manual(
    values = pub_type_colors,
    labels = paste0(pub_counts$type, " (n\u202f=\u202f", pub_counts$n, ")")
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = c(yr_min, yr_max),
    limits = c(yr_min - 0.5, yr_max + 0.5)
  ) +
  scale_y_continuous(
    breaks = integer_breaks(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = paste0("Research Output (", yr_label, ")"),
    fill  = NULL
  ) +
  theme_scholar() +
  theme(
    legend.position       = "inside",
    legend.position.inside = c(0.03, 0.97),
    legend.justification  = c(0, 1),
    legend.text           = element_text(size = 7.5, color = "grey30"),
    legend.key.size       = unit(0.45, "cm")
  )

p2


# ── P3 · Annual citation history ─────────────────────────────────────────────

metrics_label <- glue::glue(
  "Total citations: {profile$total_cites}\n",
  "h-index: {profile$h_index}\n",
  "i10-index: {profile$i10_index}"
)

p3 <- citation_history |>
  ggplot(aes(x = year, y = cites)) +
  geom_col(aes(fill = cites), width = 0.6) +
  geom_text(
    aes(y = cites, label = cites),
    size = 2.8, vjust = -0.6, color = "grey40"
  ) +
  annotate(
    "text",
    x = min(citation_history$year), y = max(citation_history$cites) * 0.72,
    label = metrics_label,
    hjust = 0, vjust = 0, size = 2.9, color = "grey30", lineheight = 1.4
  ) +
  scale_fill_gradient2(
    low      = "#cce3de",
    mid      = "#3a86ff",
    high     = "#1a237e",
    guide    = "none",
    midpoint = quantile(citation_history$cites, 0.6)
  ) +
  scale_x_continuous(
    expand = expansion(add = 0.7),
    breaks = c(min(citation_history$year), max(citation_history$year))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = paste0("Citations (", year_range_label(citation_history$year), ")")) +
  coord_cartesian(clip = "off") +
  theme_scholar() +
  theme(
    axis.ticks.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.line.x   = element_line(color = "grey50")
  )

p3


# ── P4 · NEW: Cumulative publications & citations ────────────────────────────

# Dual y-axis: rescale publications onto citation scale, use sec_axis to relabel
cum_pubs  <- pubs |> filter(!is.na(year)) |> count(year) |>
  arrange(year) |> mutate(cumulative = cumsum(n))
cum_cites <- citation_history |> arrange(year) |>
  mutate(cumulative = cumsum(cites))

# Scale factor maps pub range onto citation range
max_cites <- max(cum_cites$cumulative)
max_pubs  <- max(cum_pubs$cumulative)
pub_scale <- max_cites / max_pubs

p4 <- ggplot() +
  # Citations on left y-axis (orange)
  geom_step(
    data = cum_cites,
    aes(x = year, y = cumulative),
    color = "#e07b39", linewidth = 1, alpha = 0.9
  ) +
  geom_point(
    data = cum_cites,
    aes(x = year, y = cumulative),
    color = "#e07b39", size = 2.5
  ) +
  # Publications rescaled onto citation axis (blue), labeled via sec_axis
  geom_step(
    data = cum_pubs,
    aes(x = year, y = cumulative * pub_scale),
    color = "#3a86ff", linewidth = 1, alpha = 0.9
  ) +
  geom_point(
    data = cum_pubs,
    aes(x = year, y = cumulative * pub_scale),
    color = "#3a86ff", size = 2.5
  ) +
  scale_x_continuous(breaks = integer_breaks()) +
  scale_y_continuous(
    name     = "Cumulative citations",
    breaks   = integer_breaks(),
    sec.axis = sec_axis(
      transform = ~ . / pub_scale,
      name      = "Cumulative publications",
      breaks    = integer_breaks()
    )
  ) +
  labs(title = "Cumulative Output & Impact") +
  theme_scholar() +
  theme(
    axis.title.y.left  = element_text(size = 8, color = "#e07b39"),
    axis.text.y.left   = element_text(color = "#e07b39"),
    axis.line.y.left   = element_line(color = "#e07b39"),
    axis.ticks.y.left  = element_line(color = "#e07b39"),
    axis.title.y.right = element_text(size = 8, color = "#3a86ff", angle = 270, vjust = 2),
    axis.text.y.right  = element_text(color = "#3a86ff"),
    axis.line.y.right  = element_line(color = "#3a86ff"),
    axis.ticks.y.right = element_line(color = "#3a86ff")
  )

p4


# ── P5 · NEW: Citation rate vs SJR bubble plot ───────────────────────────────

# Compute per-article annual citation rate (cites / years since pub)
current_year <- as.integer(format(Sys.Date(), "%Y"))

pubs_bubble <- pubs |>
  filter(type == "Refereed Article", !is.na(sjr)) |>
  mutate(
    age         = pmax(current_year - year, 1),
    cite_rate   = cites / age
  )

p5 <- pubs_bubble |>
  ggplot(aes(x = sjr, y = cite_rate, size = cites, label = pubid_short)) +
  geom_point(
    aes(fill = factor(first)),
    shape = 21, alpha = 0.8, color = "white", stroke = 0.5
  ) +
  ggrepel::geom_text_repel(
    data = \(d) slice_max(d, order_by = cites, n = 10),
    size = 2.5, color = "grey30", box.padding = 0.5, max.overlaps = 10
  ) +
  scale_fill_manual(
    values = c("0" = "#70b8d4", "1" = "#e07b39"),
    labels = c("0" = "Co-author", "1" = "First author"),
    name   = NULL
  ) +
  scale_size_continuous(range = c(2, 12), name = "Total\ncitations") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Citation Rate vs. Journal Rank",
    subtitle = "Bubble size = total citations  |  Orange = first-author papers",
    x = "SJR", y = "Citations / year"
  ) +
  theme_scholar() +
  theme(
    axis.title   = element_text(size = 9, color = "grey40"),
    legend.position = "none"
  )

p5


# ── Panel plots ───────────────────────────────────────────────────────────────

# Summary panel (2x2)
# cowplot::plot_grid avoids the patchwork guide-collection bug entirely
summary_panel <- cowplot::plot_grid(
  p2, p3, p1, p4,
  ncol  = 2,
  align = "hv",
  axis  = "tblr"
)
summary_panel <- cowplot::plot_grid(
  cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Google Scholar Profile \u2013 ", AUTHOR_LASTNAME),
      fontface = "bold", size = 14, color = "grey25", x = 0.02, hjust = 0
    ),
  summary_panel,
  ncol           = 1,
  rel_heights    = c(0.06, 1)
)
summary_panel

ggsave(
  "plots/google-scholar-summary.png",
  plot = summary_panel,
  width = 10, height = 7, scale = 1, dpi = 300, bg = "white"
)

# Citation impact deep-dive (p5 on top, p3 + p4 below)
impact_bottom <- cowplot::plot_grid(p3, p4, ncol = 2, align = "hv", axis = "tblr")
impact_panel  <- cowplot::plot_grid(p5, impact_bottom, ncol = 1, rel_heights = c(1.2, 1))
impact_panel  <- cowplot::plot_grid(
  cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Citation Impact \u2013 ", AUTHOR_LASTNAME),
      fontface = "bold", size = 14, color = "grey25", x = 0.02, hjust = 0
    ),
  impact_panel,
  ncol        = 1,
  rel_heights = c(0.06, 1)
)
impact_panel

ggsave(
  "plots/google-scholar-impact.png",
  plot = impact_panel,
  width = 10, height = 8, scale = 1, dpi = 300, bg = "white"
)


# ── Per-article citation history ─────────────────────────────────────────────

ach <- pubs$pubid |>
  purrr::map(\(pid) {
    tryCatch(
      get_article_cite_history(SCHOLAR_ID, pid),
      error = \(e) {
        message("Skipping pubid ", pid, ": ", conditionMessage(e))
        NULL
      }
    )
  }) |>
  purrr::compact() |>
  purrr::list_rbind() |>
  as_tibble() |>
  left_join(
    pubs |> select(pubid, pubid_short, j_short, type, first),
    by = "pubid"
  )

ach |>
  ggplot() +
  geom_segment(
    aes(x = year, xend = year, y = 0, yend = cites),
    color = "grey80", linewidth = 0.7
  ) +
  geom_point(
    aes(x = year, y = cites, color = type, shape = factor(first)),
    size = 2.2
  ) +
  scale_color_manual(values = pub_type_colors, name = NULL) +
  scale_shape_manual(values = c("0" = 16, "1" = 17), guide = "none") +
  lemon::facet_rep_wrap(~pubid_short, scales = "free_y", ncol = 3) +
  scale_x_continuous(
    breaks = integer_breaks(3),
    limits = c(yr_min - 1, current_year)
  ) +
  scale_y_continuous(breaks = integer_breaks()) +
  labs(
    title    = paste0("Citation History by Article (", yr_label, ")"),
    subtitle = "Triangles = first-author papers"
  ) +
  theme_scholar() +
  theme(
    strip.text      = element_text(size = 7, color = "grey30"),
    legend.position = "bottom",
    legend.text     = element_text(size = 8)
  )

ggsave(
  "plots/google-scholar-articles.png",
  width = 10, height = ceiling(n_distinct(ach$pubid) / 3) * 2.2,
  scale = 1, dpi = 300, bg = "white"
)
