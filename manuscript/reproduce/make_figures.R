# ---------------------------------------------------------------------------
# Builds the manuscript figures from the latest Diebold-Mariano output
# (<root>/outputs/diebold_mariano/diebold_mariano_latest.rds):
#   figures/fig1_dm_outcomes.png     stacked outcome counts by family
#   figures/fig2_map_arima_arfima.png, fig3_map_rf.png, fig4_map_xgb.png
# Run from anywhere:
#   Rscript --vanilla manuscript/reproduce/make_figures.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
PROJECT <- Sys.getenv("COVID_PROJECT_DIR", unset = "")
if (!nzchar(PROJECT)) {
  # folder of this script (when source()d or run with Rscript), two levels below the root
  this_file <- NULL
  for (fr in rev(sys.frames())) if (is.character(fr$ofile) && nzchar(fr$ofile)) { this_file <- fr$ofile; break }
  if (is.null(this_file)) {
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) == 1) this_file <- sub("^--file=", "", fa)
  }
  PROJECT <- if (!is.null(this_file)) {
    normalizePath(file.path(dirname(this_file), "..", ".."), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
}
stopifnot(file.exists(file.path(PROJECT, "global.R")))
setwd(PROJECT)
source("R/diebold_mariano.R")   # compute_dm_status_counts(), compute_dm_map_data()
FIG_MS <- file.path(PROJECT, "manuscript", "figures")
dir.create(FIG_MS, recursive = TRUE, showWarnings = FALSE)

res_dm <- readRDS("outputs/diebold_mariano/diebold_mariano_latest.rds")
bc <- res_dm$by_country

pal_fill <- c("classical better" = "#4575b4", "fractional better" = "#f46d43", "non-significant" = "#d9d9d9")
pal_lab  <- c("classical better" = "Standard model better (p < 0.05)",
              "fractional better" = "Fractional model better (p < 0.05)",
              "non-significant"   = "Non-significant or undefined")
fam_short <- c("ARIMA vs ARFIMA" = "ARIMA vs. ARFIMA",
               "Random Forest vs Fractional Random Forest" = "RF vs. Fractional RF",
               "XGBoost vs Fractional XGBoost" = "XGBoost vs. Fractional XGBoost")

# ---- Figure 1: outcome counts ----
sc <- compute_dm_status_counts(bc)
sc$family_short <- factor(fam_short[sc$family], levels = fam_short)
sc$category <- factor(sc$category, levels = c("non-significant", "fractional better", "classical better"))
p1 <- ggplot(sc, aes(x = family_short, y = countries, fill = category)) +
  geom_col(width = 0.62, colour = "grey25", linewidth = 0.3) +
  geom_text(aes(label = countries), position = position_stack(vjust = 0.5), size = 3.6) +
  scale_fill_manual(values = pal_fill, labels = pal_lab,
                    breaks = c("classical better", "fractional better", "non-significant"), name = NULL) +
  labs(x = "Model family", y = "Number of reporting units") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "vertical",
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(FIG_MS, "fig1_dm_outcomes.png"), p1, width = 6.5, height = 4.6, dpi = 300, bg = "white")

# ---- Figures 2-4: maps ----
# JHU units that correspond to several polygons in maps::world
multi_region <- list(
  "Antigua and Barbuda" = c("Antigua", "Barbuda"),
  "Saint Kitts and Nevis" = c("Saint Kitts", "Nevis"),
  "Saint Vincent and the Grenadines" = c("Saint Vincent", "Grenadines"),
  "Trinidad and Tobago" = c("Trinidad", "Tobago")
)
world <- ggplot2::map_data("world")
fam_files <- c("ARIMA vs ARFIMA" = "fig2_map_arima_arfima.png",
               "Random Forest vs Fractional Random Forest" = "fig3_map_rf.png",
               "XGBoost vs Fractional XGBoost" = "fig4_map_xgb.png")
unmatched_all <- character(0)
for (fam in names(fam_files)) {
  map_df <- compute_dm_map_data(bc, family = fam)      # uses normalize_country_for_world_map()
  extra <- do.call(rbind, lapply(names(multi_region), function(u) {
    row <- map_df[map_df$country == u, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    out <- row[rep(1, length(multi_region[[u]])), , drop = FALSE]
    out$region <- multi_region[[u]]
    out
  }))
  map_df <- rbind(map_df[!(map_df$country %in% names(multi_region)), , drop = FALSE], extra)
  stopifnot(!any(duplicated(map_df$region)))
  unmatched <- setdiff(unique(map_df$region), unique(world$region))
  unmatched_all <- union(unmatched_all, unmatched)
  message(fam, ": units without a map polygon = ", length(unmatched), " (", paste(unmatched, collapse = "; "), ")")
  world2 <- dplyr::left_join(world, map_df[, c("region", "map_status")], by = "region")
  world2$map_status[is.na(world2$map_status)] <- "non-significant"
  pm <- ggplot(world2, aes(long, lat, group = group, fill = map_status)) +
    geom_polygon(colour = "white", linewidth = 0.12) +
    coord_quickmap(ylim = c(-58, 84)) +
    scale_fill_manual(values = pal_fill, labels = pal_lab,
                      breaks = c("classical better", "fractional better", "non-significant"), drop = FALSE, name = NULL) +
    theme_void(base_size = 12) +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(FIG_MS, fam_files[[fam]]), pm, width = 9, height = 4.6, dpi = 300, bg = "white")
}
writeLines(unmatched_all, file.path(PROJECT, "outputs", "diebold_mariano", "map_unmatched_units_latest.txt"))
message("figures written to ", FIG_MS)
