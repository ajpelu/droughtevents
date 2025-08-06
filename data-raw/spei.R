# Go to https://spei.csic.es/spei_database/#map_name=spei06#map_position=1475
# Download a time series of SPEI6, SPEI12, SPEI24, and SPEI48 for Granada

library(dplyr)
library(purrr)
library(readr)

all <- list.files("data-raw", pattern = "\\.csv$", full.names = FALSE)

spei <- all |>
  purrr::map(~ readr::read_delim(file.path("data-raw", .x), delim = ";", show_col_types = FALSE)) |>
  purrr::reduce(dplyr::full_join, by = "dates") |>
  dplyr::rename(spei6 = spei06) |>
  dplyr::relocate(spei6, .after = dates) |>
  dplyr::filter(!is.na(spei6))

usethis::use_data(spei, overwrite = TRUE)
