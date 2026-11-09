
# ============================================================
# SETUP.R — LIBRARIES & VISUALIZATION CONFIGURATION
# ============================================================

# 1. PACKAGES
packages <- c("ggplot2")

missing <- setdiff(packages, rownames(installed.packages()))

if (length(missing) > 0) {
  stop(
    "Install missing packages first: ",
    paste(missing, collapse = ", ")
  )
}

invisible(lapply(packages, library, character.only = TRUE))

# 2. COLOR PALETTE
measure_colors <- c(
  "Physical (P)"     = "#2563EB",
  "Risk-neutral (Q)" = "#EA580C",
  "Reweighted P"     = "#059669"
)

# 3. GLOBAL PLOT THEME
theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold", size = 16
      ),
      plot.subtitle = element_text(
        color = "grey40", size = 11
      ),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
)

# 4. OUTPUT CONFIGURATION
fig_dir <- "outputs/figures"

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

save_plot <- function(p, filename, width = 10, height = 5) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

cat("Visualization setup completed.\n")
