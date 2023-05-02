library(sf)
library(sfhotspot)
library(tidyverse)

crime_data_raw <- read_rds(here::here("analysis_data/met_crime_data.rds"))

crime_data <- crime_data_raw |>
  # Calculate weight given to each row (since a row can involve multiple 
  # offences, e.g. if there is an assault against multiple victims)
  mutate(weight = offences * cchi_score) |>
  # Drop and rows with missing co-ordinates, count of offences or CCHI score
  drop_na(x, y, offences, cchi_score) |>
  # Remove police-generated crime
  filter(!new_major_text %in% c("Drug Offences", "Possession of Weapons")) |> 
  st_as_sf(coords = c("x", "y"), crs = 27700)

lsoa_data <- read_sf(here::here("analysis_data/london_lsoa_boundaries.gpkg"))

borough_data <- here::here("analysis_data/london_borough_boundaries.gpkg") |>
  read_sf() |>
  st_transform(27700)

imd_data <- read_csv(here::here("analysis_data/london_deprivation_data.csv"))

crime_counts_lsoa <- crime_data |> 
  mutate(row_number = row_number()) |> 
  st_join(lsoa_data) |> 
  st_drop_geometry() |> 
  group_by(row_number) |> 
  summarise(across(everything(), first)) |> 
  select(-row_number) |>
  group_by(lsoa_code) |>
  summarise(crime_count = sum(offences), crime_harm = sum(weight))

crime_counts_lsoa |>
  ggplot(aes(x = crime_count, y = crime_harm)) + 
  geom_smooth() + 
  geom_point(shape = ".") + 
  # scale_x_continuous(trans = scales::log_trans()) + 
  # scale_y_continuous(trans = scales::log_trans()) + 
  theme_minimal()

crime_counts_lsoa |> 
  mutate(
    rank_count = min_rank(crime_count) / n(), 
    rank_harm = min_rank(crime_harm) / n(), 
    rank_diff = rank_count - rank_harm,
    greater_harm = rank_diff < 0
  ) |>
  right_join(lsoa_data, by = "lsoa_code") |>
  st_as_sf() |>
  ggplot(aes(fill = rank_diff)) +
  geom_sf(colour = NA) +
  geom_sf(data = borough_data, inherit.aes = FALSE, fill = NA) +
  scale_fill_distiller(type = "div", palette = "PuOr", limits = c(-0.5, 0.5)) +
  facet_wrap(vars(greater_harm)) +
  labs(fill = "difference\nin ranks") +
  theme_void()

crime_counts_lsoa |> 
  mutate(
    rank_count = min_rank(crime_count) / n(), 
    rank_harm = min_rank(crime_harm) / n(), 
    rank_diff = rank_count - rank_harm,
    greater_harm = rank_diff < 0
  ) |>
  left_join(imd_data, by = "lsoa_code") |>
  ggplot(aes(x = rank_diff, y = imd_score)) +
  geom_smooth() + 
  geom_point(shape = ".") + 
  theme_minimal()

crime_kde_grid <- hotspot_grid(crime_data, cell_size = 750)
crime_kde_count <- hotspot_kde(crime_data, bandwidth_adjust = 0.75, grid = crime_kde_grid)
crime_kde_harm <- hotspot_kde(crime_data, bandwidth_adjust = 0.75, grid = crime_kde_grid, weights = weight)

crime_kde <- crime_kde_count |>
  mutate(
    harm = crime_kde_harm$sum,
    harm_kde = crime_kde_harm$kde
  ) |>
  select(
    count = n, 
    harm, 
    count_kde = kde, 
    harm_kde, 
    geometry
  ) |> 
  st_intersection(st_union(borough_data)) |>
  mutate(
    count_rank = n() - min_rank(count) + 1,
    harm_rank = n() - min_rank(harm) + 1,
    count_kde_rank = n() - min_rank(count_kde) + 1,
    harm_kde_rank = n() - min_rank(harm_kde) + 1,
    count_top = count_kde_rank < n() / 100,
    harm_top = harm_kde_rank < n() / 100,
    rank_diff = count_kde_rank - harm_kde_rank,
    harm_greater = rank_diff > 0
  ) |> 
  relocate(geometry, .after = last_col())

write_sf(crime_kde, here::here("analysis_data/crime_harm_kde_750m.gpkg"))

crime_kde |> 
  mutate(
    label_harm = if_else(
      count_rank <= 20 & harm_rank > 50, 
      str_glue("Count: {scales::ordinal(count_rank)} / Harm: {scales::ordinal(harm_rank)}"), 
      NA_character_
    )
  ) |>
  ggplot(aes(x = count, y = harm)) +
  geom_smooth() +
  ggrepel::geom_label_repel(aes(label = label_harm), na.rm = TRUE, xlim = c(7500, Inf), label.size = NA, lineheight = 1) +
  geom_point(alpha = 0.33) +
  scale_x_continuous(labels = scales::label_comma(scale = 1/1000, suffix = "k"), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(labels = scales::label_comma(scale = 1/1000, suffix = "k"), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "crime count",
    y = "crime harm"
  ) +
  theme_minimal()

crime_kde |> 
  mutate(
    count = scales::rescale(count, to = 0:1), 
    harm = scales::rescale(harm, to = 0:1)
  ) |> 
  ggplot(aes(x = "crime\ncount", y = count, xend = "crime\nharm", yend = harm)) + 
  geom_segment(alpha = 0.1) + 
  scale_x_discrete(expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = NULL,
    y = "proportion of maximum value"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

