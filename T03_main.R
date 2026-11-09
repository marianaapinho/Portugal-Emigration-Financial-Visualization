# VPD project 2026/2027 - Team T03
# Leonor Carvalho, Mariana Pinho, Seyed Mirheydari
#
# Final figure: map panel (T03_map_plot.R) above the Sankey panel
# (T03_sankey_plot.R), with a common title and the team credits.
#
# Run from the project folder, which must contain T03_project_data.xlsx and
# the folder T03_intermediate_Rfiles with the two panel scripts.
# Output: T03_visualization.png
#
# Packages: ggplot2, patchwork, ragg (plus those of the panel scripts)

library(ggplot2)
library(patchwork)

out_file   <- "T03_visualization.png"
map_script <- "T03_intermediate_Rfiles/T03_map_plot.R"
sk_script  <- "T03_intermediate_Rfiles/T03_sankey_plot.R"

required <- c("T03_project_data.xlsx", map_script, sk_script)
missing  <- required[!file.exists(required)]
if (length(missing) > 0) {
  stop("Missing files in ", getwd(), ": ", paste(missing, collapse = ", "))
}

fig_w    <- 16.5   # figure width (in)
sankey_h <- 6      # Sankey panel height (in)


# ---- 1. Panels ---------------------------------------------------------------

# Each script runs in its own environment, since both define objects with
# the same names (cities, df, ...). save_individual stops them from saving
# their own PNG.
env_map <- new.env()
assign("save_individual", FALSE, envir = env_map)
sys.source(map_script, envir = env_map, chdir = FALSE)

env_sankey <- new.env()
assign("save_individual", FALSE, envir = env_sankey)
sys.source(sk_script, envir = env_sankey, chdir = FALSE)

pal <- env_map$pal

# coord_sf fixes the aspect ratio of the map, so its height follows from
# the width (minus the side margins of plot_annotation)
map_aspect <- with(env_map,
                   (canvas_ymax - canvas_ymin) / (canvas_xmax - canvas_xmin))
map_w <- fig_w - 0.5
map_h <- map_w * map_aspect


# ---- 2. Panel titles ---------------------------------------------------------

panel_title <- theme(
  plot.title    = element_text(size = 19, face = "bold", colour = pal$ink,
                               margin = margin(t = 6, b = 7)),
  plot.subtitle = element_text(size = 15.5, colour = pal$muted,
                               margin = margin(b = 15)),
  plot.title.position = "plot"
)

p_map <- env_map$p_map +
  labs(title    = "1. How many workdays a month go to rent?",
       subtitle = "Workdays a full-time worker needs to pay half the rent of a 2-bedroom flat") +
  panel_title +
  theme(plot.margin = margin(0, 0, 6, 0),
        plot.subtitle = element_text(margin = margin(b = 22)))

p_sankey <- env_sankey$p_sankey +
  labs(title    = "2. What is left at the end of the month? Does it match where most Portuguese emigrate?",
       subtitle = "Each band is one city (width = \u20ac per person per month); bars on the left show Portuguese arrivals",
       caption  = if (nzchar(env_sankey$caption_notes)) env_sankey$caption_notes else NULL) +
  panel_title +
  # legend text matched to the map legend
  theme(plot.margin  = margin(12, 0, 6, 0),
        legend.title = element_text(size = env_map$LEGEND_HEADER_SIZE * .pt,
                                    colour = pal$ink,
                                    margin = margin(r = 18)),
        legend.text  = element_text(size = env_map$LEGEND_TEXT_SIZE * .pt,
                                    colour = pal$ink),
        legend.key.spacing.x = unit(16, "pt"),
        legend.key.size = unit(0.6, "cm"),
        plot.subtitle = element_text(margin = margin(b = 22)))


# ---- 3. Layout ---------------------------------------------------------------

final_plot <- (p_map / p_sankey) +
  plot_layout(heights = c(map_h, sankey_h)) +
  plot_annotation(
    title    = "Lisbon has the heaviest rent burden and the least money left over",
    subtitle = paste0("Compared with the main destinations of Portuguese emigration, for a full-time worker on the national average wage (2025)"),
    caption  = paste0(sub("\\.$", "", env_sankey$caption_sources),
                      "  |  Authors: Leonor Carvalho, Mariana Pinho and Seyed Mirheydari",
                      "  |  Date: ", format(Sys.Date(), "%Y-%m-%d")),
    theme = theme(
      plot.title      = element_text(size = 30, face = "bold", colour = pal$ink,
                                     margin = margin(t = 6, b = 7.5)),
      plot.subtitle   = element_text(size = 17, colour = pal$muted,
                                     margin = margin(b = 22)),
      plot.caption    = element_text(size = 11, colour = pal$muted, hjust = 0,
                                     margin = margin(t = 24)),
      plot.background = element_rect(fill = pal$paper, colour = NA),
      plot.margin     = margin(20, 25, 20, 25)
    )
  )

# ---- 4. Save -----------------------------------------------------------------
# The height is the map, the Sankey panel and 4 in for the overall title,
# subtitle and caption.

ragg::agg_png(
  out_file,
  width      = fig_w,
  height     = map_h + sankey_h + 4,
  units      = "in",
  res        = 300,
  background = pal$paper
)

print(final_plot)

invisible(dev.off())
message("Figure saved to ", normalizePath(out_file, winslash = "/"))
