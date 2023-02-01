## code to prepare `kanton_data` dataset goes here

usethis::use_data(kanton_data, overwrite = TRUE)

# Bundesamt für Statistik
# Structure of the permanent resident population by canton, 2010-2021
# https://www.bfs.admin.ch/bfs/en/home/statistics/population.assetdetail.23064706.html
# https://dam-api.bfs.admin.ch/hub/api/dam/assets/23064706/master

kanton_pop_data <- readxl::read_xlsx("/Users/verne/Downloads/je-d-01.02.03.04.xlsx",
                  sheet = "2021",
                  skip = 4) %>%
  janitor::clean_names() %>%
  dplyr::select(kanton1 = x1, pop_21 = x2) %>%
  dplyr::filter(!is.na(pop_21))

kanton_names <- tibble::tribble(
  ~kanton, ~kanton1,
  "Aargau", "Aargau",
  "Appenzell Ausserrhoden", "Appenzell A. Rh.",
  "Appenzell Innerrhoden", "Appenzell I. Rh.",
  "Basel-Landschaft", "Basel-Landschaft",
  "Basel-Stadt", "Basel-Stadt",
  "Bern / Berne", "Bern",
  "Fribourg / Freiburg", "Freiburg",
  "Genève", "Genf",
  "Glarus", "Glarus",
  "Graubünden / Grigioni / Grischun", "Graubünden",
  "Jura", "Jura",
  "Luzern", "Luzern",
  "Neuchâtel", "Neuenburg",
  "Nidwalden", "Nidwalden",
  "Obwalden", "Obwalden",
  "Schaffhausen", "Schaffhausen",
  "Schwyz", "Schwyz",
  "Solothurn", "Solothurn",
  "St. Gallen", "St. Gallen",
  "Ticino", "Tessin",
  "Thurgau", "Thurgau",
  "Uri", "Uri",
  "Vaud", "Waadt",
  "Valais / Wallis", "Wallis",
  "Zug", "Zug",
  "Zürich", "Zürich"
)

kanton_pop_data <- kanton_pop_data %>%
  dplyr::left_join(kanton_names, by = "kanton1") %>%
  dplyr::filter(!is.na(kanton1),
                !is.na(kanton))

save(kanton_pop_data, file = "data/kanton_pop_data.rda")
