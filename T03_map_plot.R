# VPD project 2026/2027 - Team T03
# Leonor Carvalho, Mariana Pinho, Seyed Mirheydari
#
# Map panel: Europe coloured by net hourly wage, with one card per city
# showing, as a waffle chart, how many workdays a month go to rent.
#
# Input:  T03_project_data.xlsx (output of T03_data_preparation.R)
# Output: p_map, also saved as a PNG when run on its own
#
# Packages: ggplot2 (>= 3.5.0), dplyr, tidyr, readxl, sf, rnaturalearth,
#           rnaturalearthdata, ragg

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(rnaturalearth)


# ---- Parameters --------------------------------------------------------------

FILE      <- "T03_project_data.xlsx"
OUT_FILE  <- "T03_map_plot.png"
THRESHOLD <- 0.40   # rent burden limit, share of net pay
HOURS_DAY <- 8      # one square = one 8-hour workday
N_COLS    <- 5      # one row of squares = one workweek
CRS_MAP   <- 3035   # ETRS89 / LAEA Europe

# Text sizes in mm (geom_text units), tuned for the final composition
CARD_WIDTH_FRAC    <- 0.195
LEGEND_HEADER_SIZE <- 5.0
LEGEND_TEXT_SIZE   <- 5.5
LEGEND_WAGE_SIZE   <- 4.8
LEGEND_TICK_SIZE   <- 4.2
LEGEND_NOTE_SIZE   <- 4.6

pal <- list(
  paper   = "#faf7f2",
  card    = "#fffdf9",
  card_bd = "#d8d0c2",
  sea     = "#eef2f3",
  land    = "#e6e6e3",
  ink     = "#2b2b2b",
  muted   = "#7a766e",
  #within  = "#9fbdb9",
  within  = "#A2C597",
  above   = "#c0503a",
  left    = "#efe9de",
  accent  = "#c0452e",
  teal_lo = "#e9e2d3",
  teal_hi = "#7a6750"
)


# ---- 1. Data -----------------------------------------------------------------

cities    <- read_excel(FILE, sheet = "cities")
countries <- read_excel(FILE, sheet = "countries")

# English labels and coordinates (accents as Unicode escapes)
city_meta <- tribble(
  ~city,               ~label,       ~lon,     ~lat,
  "Bruxelles/Brussel", "Brussels",    4.352,   50.847,
  "Berlin",            "Berlin",     13.405,   52.520,
  "M\u00fcnchen",      "Munich",     11.575,   48.137,
  "Madrid",            "Madrid",     -3.703,   40.417,
  "Paris",             "Paris",       2.352,   48.857,
  "Luxembourg",        "Luxembourg",  6.130,   49.612,
  "Den Haag",          "The Hague",   4.300,   52.078,
  "Lisboa",            "Lisbon",     -9.139,   38.722,
  "Bern (Berne)",      "Bern",        7.447,   46.948,
  "Gen\u00e8ve",       "Geneva",      6.143,   46.204,
  "London",            "London",     -0.128,   51.507
)

# Rent and the 40% limit expressed in workdays.
# days_month is the number of paid workdays in a month (about 21.7).
df <- cities |>
  inner_join(city_meta, by = "city") |>
  mutate(
    days_rent   = hours_for_rent / HOURS_DAY,
    days_month  = net_month_eur / net_hour_eur / HOURS_DAY,
    days_thr    = THRESHOLD * days_month,
    days_rent_c = pmin(days_rent, days_month),
    is_pt       = country == "Portugal"
  )

stopifnot(nrow(df) == nrow(cities))


# ---- 2. Base map -------------------------------------------------------------

world <- ne_countries(scale = 50, returnclass = "sf") |>
  st_transform(CRS_MAP)

# Wide, shallow window covering the 11 cities
win <- st_as_sf(data.frame(lon = c(-18, 20.5, -18, 20.5),
                           lat = c(36.5, 36.5, 55.5, 55.5)),
                coords = c("lon", "lat"), crs = 4326) |>
  st_transform(CRS_MAP) |>
  st_bbox()

xmin <- win[["xmin"]]; xmax <- win[["xmax"]]
ymin <- win[["ymin"]]; ymax <- win[["ymax"]]
W <- xmax - xmin
H <- ymax - ymin

# Centre horizontally on the cities and widen by 20% on each side
# to leave room for the cards
city_xy_for_center <- city_meta |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(CRS_MAP) |>
  st_coordinates()
map_shift_x <- mean(city_xy_for_center[, 1]) - (xmin + xmax) / 2
map_xmin <- xmin - 0.20 * W + map_shift_x
map_xmax <- xmax + 0.20 * W + map_shift_x
map_W    <- map_xmax - map_xmin

world <- world |>
  left_join(countries |> select(admin = country, net_hour_eur), by = "admin")
world_crop <- suppressWarnings(st_crop(
  world, c(xmin = map_xmin, ymin = ymin, xmax = map_xmax, ymax = ymax)
))

pts <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(CRS_MAP)
xy <- st_coordinates(pts)
df$px <- xy[, 1]
df$py <- xy[, 2]


# ---- 3. City cards -----------------------------------------------------------

# Card centres as fractions of the map window, placed by hand over sea or
# sparse areas
card_pos <- tribble(
  ~label,       ~fx,    ~fy,
  "London",     0.133,  0.865,
  "Paris",      0.133,  0.640,
  "Madrid",     0.133,  0.415,
  "Lisbon",     0.133,  0.190,
  "The Hague",  0.390,  0.910,
  "Berlin",     0.865,  0.875,
  "Brussels",   0.865,  0.665,
  "Luxembourg", 0.865,  0.505,
  "Munich",     0.865,  0.345,
  "Bern",       0.865,  0.135,
  "Geneva",     0.505,  0.105
)

cw  <- CARD_WIDTH_FRAC * map_W
ch  <- min(0.105 * W, 0.147 * H)
pad <- 0.065 * ch
s   <- (ch - 2 * pad) / N_COLS   # side of one waffle square

# (ax, ay) is the point of the card closest to the city, used for the leader line
df <- df |>
  left_join(card_pos, by = "label") |>
  mutate(
    yc = ymin + fy * H,
    xl = map_xmin + fx * map_W - cw / 2,
    xr = xl + cw,
    yb = yc - ch / 2,
    yt = yc + ch / 2,
    ax = pmin(pmax(px, xl), xr),
    ay = pmin(pmax(py, yb), yt),
    wf_x = xl + pad,
    wf_y = yc + (N_COLS * s) / 2,
    tx   = wf_x + N_COLS * s + 0.040 * ch
  )

if (anyNA(df$fx) || anyNA(df$fy)) {
  stop("Some cities have no card position in card_pos.")
}

if (any(df$xl < map_xmin | df$xr > map_xmax | df$yb < ymin | df$yt > ymax)) {
  warning("A card falls outside the map window; adjust fx/fy in card_pos.")
}
for (i in seq_len(nrow(df) - 1)) {
  for (j in seq.int(i + 1, nrow(df))) {
    if (df$xl[i] < df$xr[j] && df$xr[i] > df$xl[j] &&
        df$yb[i] < df$yt[j] && df$yt[i] > df$yb[j]) {
      warning("Overlapping cards: ", df$label[i], " / ", df$label[j])
    }
  }
}


# ---- 4. Waffle geometry ------------------------------------------------------

# Splits the month into three intervals (in days): rent up to the limit,
# rent above the limit and days left
build_segments <- function(d) {
  d |>
    transmute(city, wf_x, wf_y, d = days_rent_c, thr = days_thr, M = days_month) |>
    rowwise() |>
    reframe(
      city, wf_x, wf_y,
      type = c("within", "above", "left"),
      a    = c(0, thr, d),
      b    = c(min(d, thr), max(d, thr), M)
    ) |>
    filter(b > a)
}

sq_gap <- 0.07   # gap between squares, as a fraction of the square side

# Fills squares day by day; a partial day becomes a partially filled square
build_waffle <- function(segs, sq) {
  segs |>
    crossing(i = seq_len(N_COLS * 5)) |>
    mutate(lo = pmax(a, i - 1), hi = pmin(b, i)) |>
    filter(hi > lo) |>
    mutate(
      fa  = lo - (i - 1),
      fb  = hi - (i - 1),
      col = (i - 1) %% N_COLS,
      row = (i - 1) %/% N_COLS,
      x1  = wf_x + (col + sq_gap + fa * (1 - 2 * sq_gap)) * sq,
      x2  = wf_x + (col + sq_gap + fb * (1 - 2 * sq_gap)) * sq,
      y1  = wf_y - (row + 1 - sq_gap) * sq,
      y2  = wf_y - (row + sq_gap) * sq,
      fill_col = case_when(type == "within" ~ pal$within,
                           type == "above"  ~ pal$above,
                           TRUE             ~ pal$left)
    )
}

# Stepped dashed line marking the 40% limit inside the grid
build_thr_path <- function(d, sq) {
  d |>
    select(city, wf_x, wf_y, thr = days_thr) |>
    rowwise() |>
    reframe(
      city,
      pts = {
        k   <- floor(thr / N_COLS)
        rem <- thr - N_COLS * k
        if (rem < 1e-9) {
          list(data.frame(u = c(0, N_COLS), v = c(k, k)))
        } else {
          list(data.frame(u = c(0, rem, rem, N_COLS), v = c(k + 1, k + 1, k, k)))
        }
      },
      wf_x, wf_y
    ) |>
    unnest(pts) |>
    mutate(x = wf_x + u * sq, y = wf_y - v * sq)
}

waffle   <- build_waffle(build_segments(df), s)
thr_path <- build_thr_path(df, s)

# Card text and colours; Lisbon is highlighted as the reference city
cards <- df |>
  mutate(
    over      = pmax(days_rent - days_thr, 0),
    t_title   = label,
    t_days    = sprintf("%.1f workdays", days_rent),
    t_pct     = sprintf("%.0f%% of net pay", rent_pct_wage),
    t_over    = if_else(
      over > 0,
      sprintf("+%.1f d above %.0f%%", over, THRESHOLD * 100),
      sprintf("within %.0f%% limit", THRESHOLD * 100)
    ),
    col_over  = if_else(over > 0, pal$above, pal$muted),
    border    = if_else(is_pt, pal$accent, pal$card_bd),
    bd_w      = if_else(is_pt, 1.0, 0.4),
    title_col = if_else(is_pt, pal$accent, pal$ink),
    days_col  = if_else(is_pt, pal$accent, pal$ink),
    pct_col   = if_else(over > 0, pal$above, pal$ink)
  )


# ---- 5. Legend strip ---------------------------------------------------------

# Both legends are drawn by hand in a strip below the map, so they share
# the same layout and font sizes. old_strip keeps the swatch size fixed.
old_strip   <- 0.108 * H
strip       <- 0.218 * H
canvas_xmin <- map_xmin
canvas_xmax <- map_xmax
canvas_ymin <- ymin - 0.82 * strip
canvas_ymax <- ymax
canvas_W    <- canvas_xmax - canvas_xmin

# Waffle legend (left)
legend_heading <- tibble(
  x = canvas_xmin + 0.030 * canvas_W,
  y = ymin - 0.140 * strip,
  label = "HOW TO READ  |  1 square = 8 h of work  |  5 squares = 1 workweek"
)

legend_items <- tibble(
  label = c(sprintf("Rent within %.0f%% of net pay", THRESHOLD * 100),
            sprintf("Rent beyond %.0f%% of net pay", THRESHOLD * 100),
            "Workdays left after rent"),
  col  = c(pal$within, pal$above, pal$left),
  frac = c(0.030, 0.280, 0.030),
  line = c(0.410, 0.410, 0.675)
) |>
  mutate(
    swatch_xmin = canvas_xmin + frac * canvas_W,
    swatch_xmax = swatch_xmin + 0.012 * canvas_W,
    text_x      = swatch_xmax + 0.007 * canvas_W,
    text_y      = ymin - line * strip,
    swatch_ymin = text_y - 0.050 * old_strip,
    swatch_ymax = text_y + 0.050 * old_strip
  )

legend_limit <- tibble(
  x    = canvas_xmin + 0.280 * canvas_W,
  xend = canvas_xmin + 0.302 * canvas_W,
  y    = ymin - 0.675 * strip,
  tx   = canvas_xmin + 0.311 * canvas_W,
  label = sprintf("Eurostat's housing-cost overburden threshold (%.0f%%)", THRESHOLD * 100)
)

# Wage legend (right), same gradient as scale_fill_gradient below
wage_legend_title <- tibble(
  x = canvas_xmin + 0.705 * canvas_W,
  y = ymin - 0.140 * strip,
  label = "NET HOURLY WAGE  (\u20ac/h, by country)"
)

wage_x0 <- canvas_xmin + 0.705 * canvas_W
wage_x1 <- canvas_xmin + 0.955 * canvas_W
wage_y0 <- ymin - 0.410 * strip - 0.0475 * old_strip
wage_y1 <- ymin - 0.410 * strip + 0.0475 * old_strip
wage_colors <- grDevices::colorRampPalette(
  c(pal$teal_lo, pal$teal_hi), space = "Lab"
)(120)

wage_gradient <- tibble(i = seq_len(120)) |>
  mutate(
    x1 = wage_x0 + (i - 1) / 120 * (wage_x1 - wage_x0),
    x2 = wage_x0 + i / 120 * (wage_x1 - wage_x0),
    y1 = wage_y0,
    y2 = wage_y1,
    shade = wage_colors[i]
  )

wage_ticks <- tibble(value = c(5, 15, 25, 35, 45)) |>
  mutate(
    x = wage_x0 + (value - 5) / (45 - 5) * (wage_x1 - wage_x0),
    y = ymin - 0.510 * strip,
    label = as.character(value)
  )
# ---- 6. Plot -----------------------------------------------------------------

p_map <- ggplot() +
  # map
  annotate("rect", xmin = map_xmin, xmax = map_xmax, ymin = ymin, ymax = ymax,
           fill = pal$sea) +
  geom_sf(data = world_crop, aes(fill = net_hour_eur),
          colour = pal$paper, linewidth = 0.25) +
  scale_fill_gradient(
    low = pal$teal_lo, high = pal$teal_hi, na.value = pal$land,
    limits = c(5, 45), guide = "none"
  ) +
  # cards and waffles
  geom_segment(data = cards, aes(x = ax, y = ay, xend = px, yend = py),
               colour = pal$ink, linewidth = 0.35, alpha = 0.55) +
  geom_rect(data = cards,
            aes(xmin = xl, xmax = xr, ymin = yb, ymax = yt,
                colour = I(border), linewidth = I(bd_w)),
            fill = pal$card) +
  geom_rect(data = waffle,
            aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2,
                fill = I(fill_col))) +
  geom_path(data = thr_path, aes(x = x, y = y, group = city),
            colour = pal$ink, linewidth = 0.4, linetype = "22") +
  # card text: city, workdays, % of net pay, distance to the limit
  geom_text(data = cards,
            aes(x = tx, y = yc + 0.33 * ch, label = t_title,
                colour = I(title_col)),
            hjust = 0, vjust = 0.5, fontface = "bold", size = 4.5) +
  geom_text(data = cards,
            aes(x = tx, y = yc + 0.095 * ch, label = t_days,
                colour = I(days_col)),
            hjust = 0, vjust = 0.5, fontface = "bold", size = 6.3) +
  geom_text(data = cards,
            aes(x = tx, y = yc - 0.145 * ch, label = t_pct,
                colour = I(pct_col)),
            hjust = 0, vjust = 0.5, fontface = "bold", size = 4.6) +
  geom_text(data = cards,
            aes(x = tx, y = yc - 0.355 * ch, label = t_over,
                colour = I(col_over)),
            hjust = 0, vjust = 0.5, size = 3.8) +
  geom_point(data = df,
             aes(px, py, colour = I(ifelse(is_pt, pal$accent, pal$ink))),
             size = 3.2, stroke = 1, shape = 21, fill = "white") +
  # legend strip
  annotate("rect", xmin = canvas_xmin, xmax = canvas_xmax,
           ymin = canvas_ymin, ymax = ymin, fill = pal$paper) +
  geom_text(data = legend_heading,
            aes(x = x, y = y, label = label), hjust = 0,
            vjust = 0.5, size = LEGEND_HEADER_SIZE,
            fontface = "bold", colour = pal$ink) +
  geom_rect(data = legend_items,
            aes(xmin = swatch_xmin, xmax = swatch_xmax,
                ymin = swatch_ymin, ymax = swatch_ymax, fill = I(col)),
            inherit.aes = FALSE) +
  geom_text(data = legend_items,
            aes(x = text_x, y = text_y, label = label),
            hjust = 0, vjust = 0.5, size = LEGEND_TEXT_SIZE, colour = pal$ink) +
  geom_segment(data = legend_limit,
               aes(x = x, xend = xend, y = y, yend = y),
               colour = pal$ink, linewidth = 0.6, linetype = "22") +
  geom_text(data = legend_limit,
            aes(x = tx, y = y, label = label),
            hjust = 0, vjust = 0.5, size = LEGEND_TEXT_SIZE, colour = pal$ink) +
  geom_text(data = wage_legend_title,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 0.5, size = LEGEND_WAGE_SIZE,
            fontface = "bold", colour = pal$ink) +
  geom_rect(data = wage_gradient,
            aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2,
                fill = I(shade)), inherit.aes = FALSE, colour = NA) +
  geom_text(data = wage_ticks,
            aes(x = x, y = y, label = label),
            hjust = 0.5, vjust = 1, size = LEGEND_TICK_SIZE,
            colour = pal$muted) +
  coord_sf(crs = CRS_MAP, datum = NA, expand = FALSE,
           xlim = c(canvas_xmin, canvas_xmax),
           ylim = c(canvas_ymin, canvas_ymax)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )


# ---- 7. Save -----------------------------------------------------------------

# Skipped when the final composition script sets save_individual <- FALSE
if (!isFALSE(get0("save_individual"))) {
  map_w <- 16
  map_h <- map_w * (canvas_ymax - canvas_ymin) / (canvas_xmax - canvas_xmin)
  ggsave(OUT_FILE, p_map, width = map_w, height = map_h, dpi = 300,
         device = ragg::agg_png, bg = pal$paper)
}
