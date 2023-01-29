library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)


custom_theme <- function(plot_title_size = 30, colour = "black",
                         size = 20, angle = 0) {
  theme_minimal() +
    theme(
      plot.title = element_text(
        family = "sans",
        # face = "bold",
        colour = colour,
        size = plot_title_size
      ),
      axis.title.x = element_text(
        family = "sans",
        colour = colour,
        size = size
      ),
      strip.text.x = element_text(
        family = "sans",
        colour = colour,
        size = size
      ),
      axis.title.y = element_text(
        family = "sans",
        colour = colour,
        size = size
      ),
      plot.caption = element_text(
        size = 10,
        color = "black"
      ),
      axis.text.x = element_text(size = 10, angle = angle, vjust = 0.5)
    )
}

theme_set(custom_theme())

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


cpi_data |>
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
  geom_text(aes(
    x = date("2021-01-01"),
    y = 102 + 0.3,
    label = "2%"
  ),
  size = 7,
  check_overlap = T
  ) +
  geom_text(aes(
    x = date("2019-09-01"),
    y = 104 + 0.3,
    label = paste0("Average since 2020: ", round(actual_growth * 100, 2), "%")
  ),
  size = 7,
  check_overlap = T
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "CPI vs Target",
    x = "",
    y = "CPI Index (2020 Jan = 100)",
    color = ""
  ) +
  custom_theme() +
  scale_color_manual(values = cbbPalette)

ggsave("cpi_overshoot.svg",
  bg = "white",
  width = 10,
  height = 10
)

# TODO: some instantenous measure. See here
# https://www.janeeckhout.com/wp-content/uploads/Instantaneous_Inflation.pdf

cpi_data %>%
  mutate(change = (value - lag(value)) / lag(value)) %>%
  mutate(change = (1 + change)^12 - 1) %>%
  # filter(date >= "2020-01-01") %>%
  ggplot() +
  geom_line(aes(x = date, y = change), linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(
    x = date,
    y = change,
    label = round(change, 3) * 100,
  ),
  size = 6,
  check_overlap = T
  )
