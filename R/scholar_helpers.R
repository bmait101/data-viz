# scholar_helpers.R
# Helper functions for Google Scholar data analysis
# Source this file from google-scholar.R

# ── Utilities ────────────────────────────────────────────────────────────────

#' Integer-only axis breaks for ggplot2
integer_breaks <- function(n = 5, ...) {
  breaker <- scales::pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}

#' Dynamic year-range label, e.g. "2013–2025"
year_range_label <- function(years) {
  paste0(min(years, na.rm = TRUE), "\u2013", max(years, na.rm = TRUE))
}


# ── Publication classification ───────────────────────────────────────────────

#' Classify publications into types.
#'
#' Instead of hardcoding rownames (which shift whenever pubs are added/removed),
#' this function matches on stable pubid values and journal-name patterns.
#'
#' @param pubs   Tibble returned by get_publications()
#' @param thesis_ids       Character vector of pubids to label "Thesis"
#' @param report_ids       Character vector of pubids to label "Technical Report"
#' @param software_ids     Character vector of pubids to label "Software"
#' @param preprint_journals Regex pattern for preprint servers (case-insensitive)
#' @return pubs with a `type` factor column added
classify_pub_type <- function(
    pubs,
    thesis_ids        = character(0),
    report_ids        = character(0),
    software_ids      = character(0),
    data_ids          = character(0),
    preprint_journals = "biorxiv|medrxiv|arxiv|preprint"   # case-insensitive match
) {
  pubs |>
    dplyr::mutate(
      type = dplyr::case_when(
        pubid %in% thesis_ids                                  ~ "Thesis",
        pubid %in% report_ids                                  ~ "Technical Report",
        pubid %in% software_ids                                ~ "Software",
        pubid %in% data_ids                                    ~ "Data Release",
        grepl(preprint_journals, journal, ignore.case = TRUE)  ~ "Preprint",
        TRUE                                                   ~ "Refereed Article"
      ),
      type = factor(
        type,
        levels = c("Refereed Article", "Preprint", "Technical Report",
                   "Software", "Data Release", "Thesis")
      )
    )
}


# ── Journal name standardisation ─────────────────────────────────────────────

#' Standardise journal names using a named lookup vector.
#'
#' Keeps the raw names intact except for known variants, making it easy to
#' maintain the list without touching analysis code.
#'
#' @param journal_vec  Character vector of journal names
#' @param lookup       Named character vector: names = raw variants,
#'                     values = canonical names
#' @return Character vector with names standardised
standardise_journal_names <- function(
    journal_vec,
    lookup = c(
      "Elementa: Science of the Anthropocene" = "Elementa",
      "Ecology, e"                             = "Ecology"
    )
) {
  for (raw in names(lookup)) {
    journal_vec[journal_vec == raw] <- lookup[[raw]]
  }
  journal_vec
}


# ── ggplot2 theme ─────────────────────────────────────────────────────────────

#' Shared base theme for all Scholar plots
theme_scholar <- function(base_size = 11) {
  cowplot::theme_cowplot(font_size = base_size) +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(color = "grey30", face = "bold", size = base_size + 1),
      plot.subtitle     = ggplot2::element_text(color = "grey50", size = base_size - 1, vjust = 0),
      axis.text         = ggplot2::element_text(size = base_size - 2, color = "grey40"),
      axis.line.y       = ggplot2::element_blank(),
      axis.title        = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey88", linetype = "dotted"),
      plot.margin       = ggplot2::margin(8, 12, 8, 8)
    )
}

# Shared publication-type colour palette
pub_type_colors <- c(
  "Refereed Article" = "#1a1a2e",
  "Preprint"         = "#e07b39",
  "Technical Report" = "#3a86ff",
  "Software"         = "#70b8d4",
  "Data Release"     = "#b185db",
  "Thesis"           = "#52b788"
)
