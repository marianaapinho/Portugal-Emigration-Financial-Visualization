# VPD Project 2026/2027 - Team T03
# Leonor Carvalho, Mariana Pinho, Seyed Mirheydari
#
# Data preparation for the visualisation.
# Builds a city-level table combining the net hourly wage, the monthly rent of
# a 2-bedroom flat, other monthly expenses and the yearly number of Portuguese
# arrivals in each destination country.
#
# Sources (reference year 2025 unless stated otherwise):
#   OECD     - Average annual wages; Taxing Wages (income tax + employee SSC)
#   Eurostat - ert_bil_eur_a (exchange rates), prc_colc_rents (rents),
#              prc_ppp_ind (expenditure per inhabitant)
#   Observatorio da Emigracao - entries of Portuguese nationals (2024/2025)
#
# Input:  T03_intermediate_Datasets/*.xlsx
# Output: T03_project_data.xlsx (sheets: cities, countries, expenses_by_category)
#
# The script assumes the working directory is the project folder.

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

data_dir <- "."
input_dir <- file.path(data_dir, "T03_intermediate_Datasets")

f_wages      <- file.path(input_dir, "T03_annualSalary.xlsx")
f_taxes      <- file.path(input_dir, "T03_labourTaxation.xlsx")
f_exchange   <- file.path(input_dir, "T03_euroConversion.xlsx")
f_rents      <- file.path(input_dir, "T03_MonthRents.xlsx")
f_emigration <- file.path(input_dir, "T03_emigration.xlsx")
f_expenses   <- file.path(input_dir, "T03_expenses.xlsx")


# ---- Assumptions -------------------------------------------------------------

# Full-time employee working 40 h a week all year round (2080 h)
hours_per_week <- 40
weeks_per_year <- 52
hours_per_year <- hours_per_week * weeks_per_year

# The flat is shared by two people who split the rent equally
people_per_flat <- 2

# Some files store numbers as text with spaces or tabs ("6 585\t").
# Everything except digits is removed before conversion.
to_number <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(x))))
}


# ---- 1. Gross annual wage (OECD) ---------------------------------------------

# Countries are the numbered rows; the relevant columns are the 
# country (2), the currency (4) and 2025 (6).
wages <- read_excel(f_wages, sheet = "Table", col_names = FALSE,
                    .name_repair = "unique_quiet") |>
  select(n = 1, country = 2, currency = 4, gross_wage_nat = 6) |>
  filter(!is.na(n)) |>
  mutate(gross_wage_nat = as.numeric(gross_wage_nat)) |>
  select(-n)


# ---- 2. Income tax and employee social security (OECD Taxing Wages) ----------

# Average rate as % of gross earnings, for a single person without children
# earning 100% of the average wage.
# Excel drops empty columns on export, so their position is not stable.
# The country column is the one containing "Reference area" and the rate is
# the first numeric column.
taxes_raw <- read_excel(f_taxes, sheet = "Table", col_names = FALSE,
                        .name_repair = "unique_quiet")

country_col <- unname(which(sapply(taxes_raw, \(x)
  any(trimws(x) == "Reference area", na.rm = TRUE)))[1])
rate_col <- unname(which(sapply(taxes_raw, is.numeric))[1])
stopifnot(!is.na(country_col), !is.na(rate_col))

# Note: tax_wedge_pct holds the rate on the gross wage (income tax plus
# employee contributions), not the full OECD tax wedge.
taxes <- taxes_raw |>
  select(country = all_of(country_col), tax_wedge_pct = all_of(rate_col)) |>
  mutate(country = trimws(country)) |>
  filter(!is.na(tax_wedge_pct))


# ---- 3. Exchange rates (Eurostat) --------------------------------------------

# National currency units per euro. Euro area countries get a rate of 1.
exchange <- read_excel(f_exchange, sheet = "Sheet 1", col_names = FALSE,
                       .name_repair = "unique_quiet") |>
  select(currency = 1, exchange_rate = 2) |>
  mutate(exchange_rate = suppressWarnings(as.numeric(exchange_rate))) |>
  filter(!is.na(exchange_rate)) |>
  bind_rows(tibble(currency = "Euro", exchange_rate = 1))


# ---- 4. Portuguese arrivals by destination (Observatorio da Emigracao) -------

# The first row holds the years. Each country has a single value: 2025 where
# available, 2024 for Spain and France.
# Country names are in Portuguese. Accented characters are written as Unicode
# escapes so the script behaves the same on any system encoding.
emigration_names <- tribble(
  ~country_pt,           ~country,
  "Alemanha",            "Germany",
  "B\u00e9lgica",        "Belgium",
  "Espanha",             "Spain",
  "Fran\u00e7a",         "France",
  "Luxemburgo",          "Luxembourg",
  "Pa\u00edses Baixos",  "Netherlands",
  "Su\u00ed\u00e7a",     "Switzerland",
  "Reino Unido",         "United Kingdom"
)

emigration_raw <- read_excel(f_emigration, col_names = FALSE,
                             .name_repair = "unique_quiet")
emigration_years <- as.character(unlist(emigration_raw[1, -1]))

emigration <- emigration_raw[-1, ] |>
  setNames(c("country_pt", emigration_years)) |>
  mutate(country_pt = trimws(country_pt, whitespace = "[\\h\\v]"),
         across(-country_pt, to_number)) |>
  pivot_longer(-country_pt, names_to = "arrivals_year",
               values_to = "pt_arrivals") |>
  filter(!is.na(pt_arrivals)) |>
  mutate(arrivals_year = as.integer(arrivals_year)) |>
  group_by(country_pt) |>
  slice_max(arrivals_year, n = 1) |>
  ungroup() |>
  inner_join(emigration_names, by = "country_pt") |>
  select(country, pt_arrivals, arrivals_year)

# Every destination must have been matched
stopifnot(nrow(emigration) == nrow(emigration_names))


# ---- 5. Other monthly expenses (Eurostat prc_ppp_ind) ------------------------

# One sheet per consumption category, indicator "Nominal expenditure per
# inhabitant (in euro)". Values are yearly, so the sum of the categories is
# divided by 12 to get a monthly amount per person.
# Price level indices (EU27 = 100) are not amounts and cannot be summed;
# the indicator is checked below for that reason.
countries_used <- c("Belgium", "Germany", "Spain", "France", "Luxembourg",
                    "Netherlands", "Portugal", "Switzerland", "United Kingdom")

read_expense_sheet <- function(sheet) {
  raw <- read_excel(f_expenses, sheet = sheet, col_names = FALSE,
                    .name_repair = "unique_quiet")
  # category and indicator names sit in the sheet header, column 3
  header_value <- function(text) raw[[3]][which(grepl(text, raw[[1]]))[1]]

  raw |>
    select(country = 1, value = 2) |>
    filter(country %in% countries_used) |>
    mutate(value     = suppressWarnings(as.numeric(value)),  # ":" -> NA
           category  = header_value("^Analytical categories"),
           indicator = header_value("^National accounts indicator"))
}

expense_sheets <- setdiff(excel_sheets(f_expenses), "Summary")
expenses_long  <- bind_rows(lapply(expense_sheets, read_expense_sheet))

# Clothing is an occasional expense and is left out of the monthly budget
excluded_categories <- c("Clothing and footwear")
expenses_long <- filter(expenses_long, !category %in% excluded_categories)

indicators_found <- unique(expenses_long$indicator)
if (!all(grepl("inhabitant", indicators_found, ignore.case = TRUE) &
         grepl("euro", indicators_found, ignore.case = TRUE))) {
  stop("Unexpected indicator in T03_expenses.xlsx: ",
       paste(indicators_found, collapse = "; "),
       ". Expected 'Nominal expenditure per inhabitant (in euro)'.")
}

# Eurostat does not publish some categories for any country (e.g.
# "Electricity, gas and other fuels"). Keeping them would make every total NA.
empty_categories <- expenses_long |>
  group_by(category) |>
  summarise(all_missing = all(is.na(value))) |>
  filter(all_missing) |>
  pull(category)

if (length(empty_categories) > 0) {
  warning("Categories without data, excluded from the total: ",
          paste(empty_categories, collapse = ", "))
  expenses_long <- filter(expenses_long, !category %in% empty_categories)
}

# sum() without na.rm on purpose: a country missing one category gets NA
# instead of an underestimated total
expenses <- expenses_long |>
  group_by(country) |>
  summarise(other_exp_year_eur = sum(value),
            n_categories       = n()) |>
  mutate(other_exp_month_eur = other_exp_year_eur / 12)


# ---- 6. Country-level table --------------------------------------------------

country_data <- wages |>
  left_join(taxes,      by = "country") |>
  left_join(exchange,   by = "currency") |>
  left_join(emigration, by = "country") |>
  left_join(expenses,   by = "country") |>
  mutate(
    gross_wage_eur = gross_wage_nat / exchange_rate,
    net_wage_eur   = gross_wage_eur * (1 - tax_wedge_pct / 100),
    net_month_eur  = net_wage_eur / 12,
    net_hour_eur   = net_wage_eur / hours_per_year
  )


# ---- 7. Monthly rents (Eurostat prc_colc_rents) ------------------------------

# Rents of 2-bedroom flats, already in euros. Each city is linked to its
# country (Unicode escapes used for the same reason as in section 4).
city_country <- tribble(
  ~city,               ~country,
  "Bruxelles/Brussel", "Belgium",
  "Berlin",            "Germany",
  "M\u00fcnchen",      "Germany",
  "Madrid",            "Spain",
  "Paris",             "France",
  "Luxembourg",        "Luxembourg",
  "Den Haag",          "Netherlands",
  "Lisboa",            "Portugal",
  "Bern (Berne)",      "Switzerland",
  "Gen\u00e8ve",       "Switzerland",
  "London",            "United Kingdom"
)

rents <- read_excel(f_rents, sheet = "Sheet 1", col_names = FALSE,
                    .name_repair = "unique_quiet") |>
  select(city = 1, rent_eur = 2) |>
  mutate(rent_eur = suppressWarnings(as.numeric(rent_eur))) |>
  filter(!is.na(rent_eur)) |>
  inner_join(city_country, by = "city")


# ---- 8. City-level table -----------------------------------------------------

# Derived variables, all per person (half of the rent):
#   hours_for_rent          net working hours needed to pay one month's share
#   rent_pct_wage           share of the monthly net wage spent on rent (%)
#   left_after_rent_eur     monthly net wage minus the rent share
#   left_after_expenses_eur what remains after rent and other expenses
final_data <- rents |>
  left_join(country_data, by = "country") |>
  mutate(
    rent_person_eur         = rent_eur / people_per_flat,
    hours_for_rent          = rent_person_eur / net_hour_eur,
    rent_pct_wage           = 100 * rent_person_eur / net_month_eur,
    left_after_rent_eur     = net_month_eur - rent_person_eur,
    left_after_expenses_eur = left_after_rent_eur - other_exp_month_eur
  ) |>
  select(city, country, currency, exchange_rate,
         gross_wage_nat, gross_wage_eur, tax_wedge_pct,
         net_wage_eur, net_month_eur, net_hour_eur,
         rent_eur, rent_person_eur, hours_for_rent, rent_pct_wage,
         other_exp_month_eur, left_after_rent_eur, left_after_expenses_eur,
         pt_arrivals, arrivals_year) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

country_data <- country_data |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

expenses_long <- expenses_long |>
  select(country, category, value_year_eur = value) |>
  mutate(value_month_eur = round(value_year_eur / 12, 2))


# ---- 9. Consistency checks and export ----------------------------------------

# All joins must be complete, with two exceptions: Portugal has no arrivals
# (it is the origin country) and Eurostat may lack expenditure data for a
# country (":" in the source).
optional_cols <- c("other_exp_month_eur", "left_after_expenses_eur",
                   "pt_arrivals", "arrivals_year")

stopifnot(
  !anyNA(select(final_data, -all_of(optional_cols))),
  nrow(final_data) == nrow(city_country),
  all(!is.na(final_data$pt_arrivals[final_data$country != "Portugal"]))
)

missing_exp <- unique(final_data$country[is.na(final_data$other_exp_month_eur)])
if (length(missing_exp) > 0) {
  warning("No expenditure data for: ", paste(missing_exp, collapse = ", "))
}

print(final_data, width = Inf)

write_xlsx(
  list(cities               = final_data,
       countries            = country_data,
       expenses_by_category = expenses_long),
  path = file.path(data_dir, "T03_project_data.xlsx")
)
