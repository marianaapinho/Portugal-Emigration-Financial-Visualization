# VPDA 2026 - Team T03
# Leonor Carvalho, Mariana Pinho, Seyed Mirheydari
#
# Sankey panel: monthly gross wage per city and what is left after taxes,
# rent and other expenses, next to the number of Portuguese arrivals in
# each destination country.
#
# Input:  T03_project_data.xlsx, sheet "cities" (from T03_data_preparation.R)
# Output: p_sankey, also saved as a PNG when run on its own
#
# Packages: readxl, dplyr, ggplot2, ragg

library(readxl)
library(dplyr)
library(ggplot2)


# ---- 0. Settings -------------------------------------------------------------

font     <- "sans"
out_file <- "T03_sankey_plot.png"

col_bg    <- "#faf7f2"
col_text  <- "#3a3a3a"
col_sub   <- "#7d7a75"
col_bar   <- "#c9c2b8"
col_track <- "#ece7e0"
col_pt    <- "#c0452e"
col_class <- c("Within 40%" = "#A2C597", "Beyond 40%" = "#c0503a")
#ribbon_alpha <- 0.38
ribbon_alpha <- 0.60

# Layout, in plot units
x_stage   <- c(0, 1, 2, 3)     # the four Sankey columns
node_w    <- 0.03
gap       <- 0.018             # vertical gap between nodes
x_bars    <- -0.62
bar_len   <- 0.30              # length of the longest arrivals bar
bar_start <- x_bars - 0.38

city_labels <- c("Bruxelles/Brussel" = "Brussels", "M\u00fcnchen" = "Munich",
                 "Den Haag" = "The Hague", "Lisboa" = "Lisbon",
                 "Bern (Berne)" = "Bern", "Gen\u00e8ve" = "Geneva")

# Euro amounts with a space as thousands separator and a proper minus sign
fmt_eur <- function(v) {
  paste0(ifelse(v < 0, "\u2212", ""), "\u20ac",
         formatC(abs(round(v)), format = "d", big.mark = " "))
}


# ---- 1. Data and nodes -------------------------------------------------------

cities <- read_excel("T03_project_data.xlsx", sheet = "cities") |>
  mutate(
    city       = recode(city, !!!city_labels),
    gross      = gross_wage_eur / 12,
    net        = net_month_eur,
    rent_left  = left_after_rent_eur,
    final      = left_after_expenses_eur,   # NA where Eurostat has no data
    rent_class = cut(round(rent_pct_wage), breaks = c(-Inf, 40, Inf),
                     labels = names(col_class))
  )

stages <- c("gross", "net", "rent_left", "final")

# One euro has the same height in every column, so the bands shrink
# in proportion to what is deducted
scale_k <- 0.8 / sum(cities$gross)

# Stacks the cities of one column from the highest to the lowest value.
# Negative values get zero height.
stack_column <- function(var, s) {
  d <- cities |>
    filter(!is.na(.data[[var]])) |>
    arrange(desc(round(.data[[var]])), country, city) |>
    mutate(h = pmax(.data[[var]], 0) * scale_k)
  total <- sum(d$h) + gap * (nrow(d) - 1)
  top   <- 0.5 + total / 2 - c(0, cumsum(d$h + gap)[-nrow(d)])
  d |> transmute(city, country, rent_class, value = .data[[var]],
                 stage = s, x = x_stage[s], y1 = top, y0 = top - h)
}

nodes <- bind_rows(lapply(seq_along(stages), \(s) stack_column(stages[s], s)))


# ---- 2. Ribbons --------------------------------------------------------------

# Cubic easing between consecutive columns, so ribbons leave and enter
# the nodes horizontally
smooth_step <- function(t) 3 * t^2 - 2 * t^3

ribbons <- nodes |>
  inner_join(nodes |> mutate(stage = stage - 1),
             by = c("city", "stage"), suffix = c("", "_next")) |>
  rowwise() |>
  reframe(
    city, rent_class, stage,
    t    = seq(0, 1, length.out = 80),
    xr   = (x + node_w / 2) + (x_next - node_w / 2 - x - node_w / 2) * t,
    ymin = y0 + (y0_next - y0) * smooth_step(t),
    ymax = y1 + (y1_next - y1) * smooth_step(t)
  ) |>
  mutate(group = paste(city, stage))


# ---- 3. Labels ---------------------------------------------------------------

lab_left <- nodes |>
  filter(stage == 1) |>
  mutate(label = paste0(city, "  ", fmt_eur(value)), y = (y0 + y1) / 2)

lab_mid <- nodes |>
  filter(stage %in% c(2, 3)) |>
  mutate(label = fmt_eur(value), y = (y0 + y1) / 2)

# Right-hand labels are pushed apart when nodes are too close together
lab_right <- nodes |>
  filter(stage == 4) |>
  left_join(select(cities, city, rent_pct_wage), by = "city") |>
  mutate(y = (y0 + y1) / 2) |>
  arrange(y)
for (i in seq_len(nrow(lab_right))[-1]) {
  lab_right$y[i] <- max(lab_right$y[i], lab_right$y[i - 1] + 0.034)
}
lab_right <- lab_right |>
  mutate(value_lab = fmt_eur(value),
         negative  = value < 0,
         is_pt     = city == "Lisbon")

# Cities that stop at the third column
no_expenses <- nodes |>
  filter(stage == 3, !city %in% nodes$city[nodes$stage == 4]) |>
  mutate(y = (y0 + y1) / 2)


# ---- 4. Portuguese arrivals --------------------------------------------------

# One bar per country, centred on its cities in the first column.
# Portugal has no bar and is labelled as the reference.
arrivals <- nodes |>
  filter(stage == 1) |>
  left_join(distinct(cities, country, pt_arrivals, arrivals_year), by = "country") |>
  group_by(country) |>
  summarise(lo = min(y0), hi = max(y1), n_cities = n(),
            arrivals = first(pt_arrivals), year = first(arrivals_year)) |>
  mutate(mid   = (lo + hi) / 2,
         xmin  = bar_start,
         xmax  = bar_start + bar_len * arrivals / max(arrivals, na.rm = TRUE),
         name  = ifelse(is.na(arrivals), paste(country, "(reference)"), country),
         value = ifelse(is.na(arrivals), NA,
                        paste0(round(arrivals / 1000, 1), "k",
                               ifelse(year < max(year, na.rm = TRUE), "*", ""))))

# Brackets for countries with two cities (Germany, Switzerland)
brackets <- arrivals |>
  filter(n_cities > 1) |>
  reframe(country,
          x = x_bars + c(0.06, 0.075, 0.075, 0.06),
          y = c(lo, lo, hi, hi), .by = country)

latest_year <- max(arrivals$year, na.rm = TRUE)
older_years <- sort(unique(arrivals$year[arrivals$year < latest_year]))


# ---- 5. Headers and caption --------------------------------------------------

headers <- tibble(
  x     = c(bar_start + 0.15, x_stage),
  title = c("PORTUGUESE ARRIVALS", "GROSS WAGE", "AFTER TAXES",
            "AFTER RENT", "AFTER OTHER EXPENSES"),
  sub   = c(paste0("per country, ", latest_year,
                   ifelse(length(older_years) > 0, "*", "")),
            "per month", "net wage", "half of a 2-bedroom flat",
            "average spending on food, transport, communication")
)

# Footnote for the arrivals column, drawn under the column itself
arrivals_note <- if (length(older_years) > 0) {
  paste0("*", paste(older_years, collapse = ", "), " for ",
         paste(arrivals$country[arrivals$year %in% older_years],
               collapse = " and "), ".")
} else ""

caption_notes <- if (any(lab_right$negative)) {
  "Negative values: average spending exceeds what is left after rent."
} else ""

caption_sources <- "Sources: Observat\u00f3rio da Emigra\u00e7\u00e3o; OECD; Eurostat."


# ---- 6. Plot -----------------------------------------------------------------

arrivals_bars <- filter(arrivals, !is.na(arrivals))

p_sankey <- ggplot() +
  # arrivals
  geom_rect(data = arrivals_bars,
            aes(xmin = bar_start, xmax = bar_start + bar_len,
                ymin = mid - 0.014, ymax = mid + 0.014),
            fill = col_track) +
  geom_rect(data = arrivals_bars,
            aes(xmin = xmin, xmax = xmax, ymin = mid - 0.014, ymax = mid + 0.014),
            fill = col_bar) +
  geom_segment(data = summarise(arrivals_bars,
                                y0 = min(mid) - 0.025, y1 = max(mid) + 0.025),
               aes(x = bar_start, xend = bar_start, y = y0, yend = y1),
               colour = "#b9b3ab", linewidth = 0.3) +
  geom_text(data = arrivals_bars,
            aes(x = bar_start - 0.02, y = mid, label = name),
            hjust = 1, size = 3.4, colour = col_text, family = font) +
  geom_text(data = filter(arrivals, is.na(arrivals)),
            aes(x = x_bars + 0.16, y = mid, label = name),
            hjust = 1, size = 3.4, colour = col_sub, fontface = "italic",
            family = font) +
  geom_text(data = filter(arrivals, is.na(arrivals)),
            aes(x = -1.33, y = mid - 0.05),
            label = arrivals_note,
            hjust = 0, size = 3.1, colour = col_sub, family = font) +
  geom_text(data = arrivals_bars,
            aes(x = xmax + 0.012, y = mid, label = value),
            hjust = 0, size = 3.2, colour = col_text, family = font) +
  geom_path(data = brackets, aes(x, y, group = country),
            colour = "#b9b3ab", linewidth = 0.4) +
  # ribbons and nodes
  geom_ribbon(data = ribbons,
              aes(x = xr, ymin = ymin, ymax = ymax, group = group,
                  fill = rent_class),
              alpha = ribbon_alpha, show.legend = FALSE) +
  geom_rect(data = nodes,
            aes(xmin = x - node_w / 2, xmax = x + node_w / 2,
                ymin = y0, ymax = pmax(y1, y0 + 0.003), fill = rent_class)) +
  # value labels
  geom_text(data = lab_left, aes(x = -node_w, y = y, label = label),
            hjust = 1, size = 3.6, colour = col_text, family = font) +
  geom_label(data = lab_mid, aes(x = x + node_w * 0.8, y = y, label = label),
             hjust = 0, size = 2.7, colour = col_sub, fill = col_bg,
             label.size = 0, label.padding = unit(0.08, "lines"), family = font) +
    # last column as a small table: names on the left, values right-aligned
  geom_text(data = lab_right,
            aes(x = x_stage[4] + node_w, y = y, label = city,
                colour = ifelse(is_pt, "pt", "text")),
            hjust = 0, size = 3.6, family = font) +
  geom_text(data = lab_right,
            aes(x = x_stage[4] + 0.50, y = y, label = value_lab,
                colour = ifelse(is_pt, "pt", ifelse(negative, "neg", "text"))),
            hjust = 1, size = 3.9, fontface = "bold", family = font) +
  geom_segment(data = no_expenses,
               aes(x = x_stage[3] + node_w / 2, y = y,
                   xend = x_stage[3] + 0.3, yend = y - 0.25),
               colour = "#b9b3ab", linewidth = 0.3) +
  geom_text(data = no_expenses,
            aes(x = x_stage[3] + 0.31, y = y - 0.25,
                label = paste0(city, ": no Eurostat\nexpenses data")),
            hjust = 0, vjust = 1, size = 2.8, colour = col_sub,
            lineheight = 0.95, family = font) +
  # column headers
  geom_text(data = headers, aes(x, 1.10, label = title),
            size = 4.5, fontface = "bold", colour = col_text, family = font) +
  geom_text(data = headers, aes(x, 1.055, label = sub),
            size = 3.8, colour = col_sub, family = font) +
  scale_fill_manual(values = col_class,
                    name = "Share of the net wage spent on rent") +
  scale_colour_manual(values = c(text = col_text, sub = col_sub,
                                 neg = col_class[["Beyond 40%"]],
                                 pt = col_pt),
                      guide = "none") +
  coord_cartesian(xlim = c(-1.45, 3.75), ylim = c(-0.02, 1.11),
                  expand = FALSE, clip = "off") +
  labs(
    title    = "Where the Portuguese go, and what is left at the end of the month",
    subtitle = paste("Each band is one city and narrows as taxes, rent and other",
                     "expenses are taken (width = \u20ac per person per month).",
                     "Colour = share of the net wage spent on rent"),
    caption  = trimws(paste(caption_notes, caption_sources))
  ) +
  theme_void(base_family = font) +
  theme(
    plot.background       = element_rect(fill = col_bg, colour = NA),
    plot.margin           = margin(18, 24, 12, 18),
    plot.title            = element_text(size = 19, face = "bold",
                                         colour = col_text,
                                         margin = margin(b = 4)),
    plot.subtitle         = element_text(size = 15.5, colour = col_sub,
                                         margin = margin(b = 10)),
    plot.caption          = element_text(size = 8, colour = col_sub, hjust = 0,
                                         lineheight = 1.2,
                                         margin = margin(t = 10)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "bottom",
    legend.title          = element_text(size = 9, colour = col_text),
    legend.text           = element_text(size = 9, colour = col_text),
    legend.key.size       = unit(0.45, "cm")
  )


# ---- 7. Save -----------------------------------------------------------------

# Skipped when the final composition script sets save_individual <- FALSE
if (!isFALSE(get0("save_individual"))) {
  ggsave(out_file, p_sankey, width = 16, height = 9, dpi = 300,
         device = ragg::agg_png, bg = col_bg)
}
