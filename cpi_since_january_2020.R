library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(svglite)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ons_time_series <- function(dataset_id, timeseries_id) {
  api_endpoint <- "https://api.ons.gov.uk/"

  url <- paste(
    sep = "/",
    api_endpoint,
    "timeseries", timeseries_id,
    "dataset", dataset_id,
    "data"
  )

  return(jsonlite::fromJSON(url))
}


cpi <- ons_time_series("MM23", "D7BT") %>%
  purrr::pluck("months")

cpi <- cpi |>
  as_tibble()

start_date <- "2020-01-01"

cpi_data <- cpi |>
  mutate(date = ym(date), value = as.numeric(value)) |>
  mutate(value = value / value[date == start_date] * 100) |>
  mutate(trend = 1) %>%
  mutate(trend = c(NA[date < start_date], 1, cumprod(trend[date > start_date] * 1.001652)) * 100)

above_trend <- cpi_data |>
  filter(date == max(date)) |>
  transmute(value - trend) |>
  pluck(1) |>
  round(2)


time_diff <- cpi_data |>
  transmute(v = value / 100, time = as.numeric(difftime(max(date), start_date, units = "days")) / 365) %>%
  slice(1) |>
  pluck("time")

actual_growth <- cpi_data |>
  filter(date == max(date)) |>
  transmute(v = value / 100, time = as.numeric(difftime(max(date), start_date, units = "days")) / 365) %>%
  mutate(root = exp(log(v) / time) - 1) |>
  pluck("root")

actual_monthly_growth <- pracma::nthroot(actual_growth + 1, 12)

final_target <- cpi_data |>
  filter(date == max(date))

plot <- cpi_data |>
  filter(date >= "2018-01-01") |>
  mutate(actual = 1) |>
  mutate(actual = c(1[date < start_date], 1, cumprod(actual[date > start_date] * actual_monthly_growth))) |>
  mutate(actual = actual * 100) |>
  rename(
    Target = trend,
    Trend = actual,
    Actual = value
  ) |>
  tidyr::pivot_longer(cols = c("Target", "Trend", "Actual")) |>
  ggplot() +
  geom_line(aes(x = date, y = value, color = name), linewidth = 1.5) +
  annotate(
    "text",
    x = date("2023-01-01"),
    y = 103.5,
    label = "2% Target",
    size = 9,
    color = cbbPalette[2]
  ) +
  annotate(
    "text",
    x = date("2020-01-01"),
    y = 111,
    label = paste0("Average since 2020: ", round(actual_growth * 100, 2), "%"),
    size = 9,
    color = cbbPalette[3]
  ) +
  annotate(
    "segment",
    x = max(cpi_data$date),
    y = final_target$trend,
    yend = final_target$value,
    linewidth = 0.5,
    linetype = "dotted"
  ) +
  annotate(
    "text",
    x = max(cpi_data$date),
    y = (final_target$trend + final_target$value) / 2,
    label = paste0(round(above_trend, 1), "% gap"),
    size = 10
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_date(limits = c(
    ymd("2018-01-01"),
    max(cpi_data$date) + 300
  )) +
  labs(
    subtitle = "CPI Index (2020 Jan = 100)",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(40) +
  scale_color_manual(values = cbbPalette) +
  theme(
    plot.title.position = "plot",
    legend.position.inside = c(0.2, 0.9),
    legend.position = "inside",
  )

ggsave(
  "cpi_overshoot.svg",
  bg = "white",
  width = 10,
  height = 10
)

# TODO: instantenous measure. See here
# https://www.janeeckhout.com/wp-content/uploads/Instantaneous_Inflation.pdf

# cpi_data %>%
#   mutate(change = (value - lag(value)) / lag(value)) %>%
#   mutate(change = (1 + change)^12 - 1) %>%
#   # filter(date >= "2020-01-01") %>%
#   ggplot() +
#   geom_line(aes(x = date, y = change), linewidth = 1.5) +
#   scale_y_continuous(labels = scales::percent) +
#   geom_text(aes(
#     x = date,
#     y = change,
#     label = round(change, 3) * 100,
#   ),
#   size = 6,
#   check_overlap = T
#   )
