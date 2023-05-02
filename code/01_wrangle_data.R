# This script loads and wrangles the raw data needed for this project

# SETUP ------------------------------------------------------------------------

# Load packages
library(sf)
library(tidyverse)



# CRIME DATA -------------------------------------------------------------------

# Load Met crime data
# This data is held in several CSV files. Column types need to be specified 
# manually because `col_guess()` produces some inconsistent results across files
# and that causes the `bind_rows()` within `map_dfr()` to fail.
crimes_raw <- here::here("original_data") |>
  dir(pattern = "UCL_Data_", full.names = TRUE) |> 
  set_names() |>
  map_dfr(
    read_csv, 
    col_types = cols(
      Offences = col_integer(),
      DateCommittedFrom = col_datetime(),
      DateCommittedTo = col_datetime(),
      DateCrRecorded = col_datetime(),
      CrNo = col_integer(),
      X = col_integer(),
      Y = col_integer(),
      .default = col_character()
    ),
    # Some values of `DateCommittedTo` have the text value "NULL" in the place
    # missing values, i.e. when the start and end times of an offence were the
    # same
    na = c("", "NA", "NULL"),
    .id = "file"
  ) |> 
  janitor::clean_names() |> 
  select(-method) |>
  rename(original_classification = classification) |>
  arrange(date_cr_recorded) |>
  mutate(
    classification = case_when(
      original_classification == "004/02" ~ "004/01",
      original_classification == "004/18" ~ "004/08",
      original_classification == "005/06" ~ "005/07",
      original_classification == "005/15" ~ "005/14",
      original_classification == "005/17" ~ "005/14", 
      original_classification == "005/19" ~ "005/16",
      original_classification %in% c(
        "005/81", "005/82", "005/83", "005/84", "005/85", "005/86"
      ) ~ "005/01",
      original_classification == "008/03" ~ "008/02",
      original_classification == "008/05" ~ "008/06",
      original_classification == c("008/14", "008/15") ~ "008/13",
      original_classification == "008/37" ~ "008/56",
      original_classification == "008/39" ~ "008/31",
      original_classification == "008/82" ~ "008/06",
      original_classification == "008/83" ~ "008/73",
      original_classification == "008/84" ~ "005/01",
      original_classification == "008/86" ~ "008/06",
      original_classification == "012/00" ~ "011/03",
      original_classification == "014/02" ~ "014/01",
      original_classification %in% c(
        "019/03", "019/06", "019/27", "019/28", "019/29", "019/30", "019/32", 
        "019/34", "019/36", "019/37"
      ) ~ "019/08",
      original_classification == "023/05" ~ "023/04",
      original_classification %in% c("023/08", "023/09") ~ "023/10",
      original_classification %in% c("023/14", "023/18") ~ "023/15",
      original_classification %in% c("023/23", "023/26", "023/27") ~ "023/22",
      original_classification %in% c("023/31", "023/36") ~ "023/16",
      original_classification %in% c(
        "028/89", "028/90", "028/91", "028/92"
      ) ~ "028/03",
      original_classification == "029/81" ~ "029/00",
      original_classification %in% c("030/83", "030/84") ~ "030/02",
      original_classification == "031/81" ~ "031/00",
      original_classification %in% c(
        "034/11", "034/12", "034/21", "034/22", "034/91", "034/92"
      ) ~ "034/01",
      original_classification %in% c("038/05", "038/06") ~ "038/04",
      original_classification == "039/11" ~ "039/00",
      original_classification %in% c("046/90", "046/91") ~ "046/00",
      original_classification %in% c(
        "056/11", "056/12", "056/13", "056/14"
      ) ~ "056/01",
      original_classification %in% c(
        "056/21", "056/22", "056/23", "056/24", "056/31", "056/32", "056/34", 
        "056/41", "056/42", "056/43", "056/44"
      ) ~ "056/02",
      original_classification %in% c(
        "057/10", "057/11", "057/20", "057/30", "057/40", "057/41"
      ) ~ "057/00",
      original_classification %in% c(
        "058/10", "058/11", "058/20", "058/21", "058/30", "058/31"
      ) ~ "058/00",
      original_classification %in% c("058/40", "058/41") ~ "058/04",
      original_classification == "059/12" ~ "059/13",
      original_classification %in% c("060/21", "060/22") ~ "061/21",
      original_classification == "061/27" ~ "061/26",
      original_classification == "066/08" ~ "066/17",
      original_classification == "066/15" ~ "066/03",
      original_classification %in% c("066/27", "066/51") ~ "066/45",
      original_classification == "066/52" ~ "066/28",
      original_classification %in% c("066/54", "066/56", "066/58") ~ "066/53",
      original_classification %in% c("070/02", "070/05", "070/06") ~ "070/01",
      original_classification == "070/09" ~ "070/10",
      original_classification %in% c("070/22", "070/23") ~ "070/20",
      original_classification == "071/09" ~ "071/08",
      original_classification == "071/12" ~ "071/11",
      original_classification == "071/14" ~ "071/13",
      original_classification == "073/12" ~ "073/11",
      original_classification == "073/13" ~ "073/14",
      original_classification == "080/02" ~ "080/00",
      original_classification == "081/07" ~ "081/03",
      original_classification == "081/08" ~ "081/04",
      original_classification %in% c("081/10", "081/42", "081/44") ~ "081/09",
      original_classification == "083/06" ~ "080/00",
      original_classification == "092/02" ~ "092/01",
      original_classification == "092/06" ~ "092/03",
      original_classification == "092/07" ~ "092/04",
      original_classification == "092/08" ~ "092/05",
      original_classification == "092/13" ~ "092/10",
      original_classification %in% c("092/17", "092/18") ~ "092/05",
      original_classification == "092/32" ~ "092/33",
      original_classification == "092/37" ~ "092/04",
      original_classification == "092/67" ~ "092/68",
      original_classification == "092/86" ~ "092/85",
      original_classification == "093/25" ~ "093/21",
      original_classification == "093/42" ~ "093/43",
      original_classification == "093/46" ~ "093/45",
      original_classification %in% c("093/69", "093/70") ~ "093/71",
      original_classification %in% c("093/87", "093/90") ~ "093/25",
      original_classification %in% c("149/70", "149/71", "149/74", "149/75", "149/78", "149/79", "149/82", "149/83") ~ "149/00",
      original_classification == "149/72" ~ "058/04",
      original_classification == "810/02" ~ "809/02",
      TRUE ~ original_classification
    ),
    changed_classification = classification != original_classification
  )





# HARM SCORES ------------------------------------------------------------------

# Download file
# Source: https://www.cambridge-ebp.co.uk/the-chi
download.file(
  url = "https://www.cambridge-ebp.co.uk/s/Cambridge-Crime-Harm-Index-updated-2021.xlsx",
  destfile = here::here("original_data/chi.xlsx"),
  mode = "wb"
)

# Load crime harm index scores
chi_scores <- here::here("original_data/chi.xlsx") |> 
  readxl::read_excel(
    sheet = "CCHI 2021 values sheet",
    range = cellranger::cell_cols(1:20)
  ) |>
  janitor::clean_names() |>
  select(
    classification = home_office_classification, 
    full_offence_title, 
    cchi_score,
    notes = if_no_sentencing_data_available_what_similar_offence_was_used
  )



# CRIME CATEGORIES -------------------------------------------------------------

# Download file
download.file(
  url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/987321/offence_group_classification2020.xls",
  destfile = here::here("original_data/crime_categories.xls"),
  mode = "wb"
)

# Load file
categories <- readxl::read_excel(
  path = here::here("original_data/crime_categories.xls"), 
  sheet = "2020"
) |>
  janitor::clean_names() |>
  select(offence_code, detailed_offence)



# JOIN DATA --------------------------------------------------------------------

crime_data <- crimes_raw |>
  mutate(
    code_without_slash = str_remove(classification, "/"),
    row = row_number()
  ) |>
  left_join(
    chi_scores, 
    by = c("classification" = "classification"), 
    na_matches = "never"
  ) |>
  left_join(
    categories, 
    by = c("code_without_slash" = "offence_code"), 
    na_matches = "never"
  ) |>
  group_by(row) |>
  summarise(across(everything(), first)) |>
  ungroup() |>
  select(
    original_classification, 
    classification, 
    changed_classification,
    new_major_text, 
    new_minor_text, 
    offence_description = detailed_offence, 
    date_committed_from,
    date_committed_to,
    date_cr_recorded,
    offences,
    cchi_score,
    flag_code,
    x,
    y
  ) |>
  arrange(date_cr_recorded) |>
  write_rds(here::here("analysis_data/met_crime_data.rds"), compress = "gz")



# BOUNDARY DATA ----------------------------------------------------------------

# Load London boundary file
london_boundary <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2020_EN_BFE_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  janitor::clean_names() |>
  st_transform(27700) |>
  filter(rgn20nm == "London") |>
  select(geometry)

# Download London boroughs file
download.file(
  url = "https://opendata.arcgis.com/api/v3/datasets/5c71e19f21024007b6ce54ef47bb51b1_0/downloads/data?format=geojson&spatialRefId=4326",
  destfile = here::here("original_data/districts.geojson"),
  mode = "wb"
)

# Open boroughs file
here::here("original_data/districts.geojson") |>
  read_sf() |>
  janitor::clean_names() |>
  filter(str_detect(lad20cd, "E09")) |>
  select(lad20cd, lad20nm, geometry) |>
  write_sf(here::here("analysis_data/london_borough_boundaries.gpkg"))

# Download LSOA file
# Source: https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2011-boundaries-full-extent-bfe-ew-v3/about
download.file(
  url = "https://opendata.arcgis.com/api/v3/datasets/e0b761d78e51491d84a3df33dff044c7_0/downloads/data?format=shp&spatialRefId=27700",
  destfile = here::here("original_data/lsoa_boundaries.zip"),
  mode = "wb"
)

# Open LSOA file
lsoa_temp_dir <- tempdir()
unzip(
  zipfile = here::here("original_data/lsoa_boundaries.zip"), 
  exdir = lsoa_temp_dir
)
lsoa_data_raw <- read_sf(str_glue("{lsoa_temp_dir}/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp"))

# Wrangle LSOA data
lsoa_data_raw |>
  janitor::clean_names() |>
  st_intersection(london_boundary) |>
  select(lsoa_code = lsoa11cd, lsoa_name = lsoa11nm, geometry) |>
  arrange(lsoa_code) |>
  write_sf(here::here("analysis_data/london_lsoa_boundaries.gpkg"))

# Central Activities Zone
# Source: https://data.london.gov.uk/dataset/central-activities-zone-boundary-london-plan-consultation-2009
download.file(
  url = "https://data.london.gov.uk/download/central-activities-zone-boundary-london-plan-consultation-2009/7bb0d660-f763-481d-b9e5-acabc661ec50/lp-consultation-oct-2009-central-activities-zone-shp.zip",
  destfile = here::here("original_data/london_caz_boundary.zip")
)
unzip(
  here::here("original_data/london_caz_boundary.zip"), 
  exdir = str_glue("{tempdir()}/caz")
)
str_glue("{tempdir()}/caz") |> 
  dir(pattern = "shp$", full.names = TRUE) |> 
  read_sf() |> 
  write_sf(here::here("analysis_data/london_caz_boundary.gpkg"))


# DEPRIVATION DATA -------------------------------------------------------------

# Source: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv") |>
  janitor::clean_names() |>
  filter(str_detect(local_authority_district_code_2019, "^E09")) |>
  rename_with(.fn = ~ str_remove(., "index_of_multiple_deprivation_")) |>
  rename_with(.fn = ~ str_remove(., "_where_1_is_most_deprived")) |>
  rename_with(.fn = ~ str_remove(., "_10_percent_of_lso_as")) |>
  rename(
    lsoa_code = lsoa_code_2011, 
    lsoa_name = lsoa_name_2011, 
    district_code = local_authority_district_code_2019, 
    district_name = local_authority_district_name_2019
  ) |>
  write_csv(here::here("analysis_data/london_deprivation_data.csv"))


