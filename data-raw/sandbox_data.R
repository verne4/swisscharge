## code to prepare `city_pop_data` dataset goes here

usethis::use_data(city_pop_data, overwrite = TRUE)

city_pop_data <- readxl::read_xlsx("/Users/verne/Downloads/su-d-01.02.03.06.xlsx", skip = 1) %>%
  janitor::clean_names() %>%
  dplyr::select(region, total) %>%
  dplyr::filter(!is.na(total)) %>%
  dplyr::mutate(
    kanton = dplyr::if_else(stringr::str_starts(region, "-"), region, NULL),
    kanton = stringr::str_remove(kanton, "- "),
    bezirk = dplyr::if_else(stringr::str_starts(region, ">"), region, NULL),
    bezirk = stringr::str_remove(bezirk, ">> "),
    dorf = dplyr::if_else(stringr::str_starts(region, "\\."), region, NULL),
    dorf = stringr::str_remove(dorf, "......"),
    bfs_nr = as.numeric(stringr::str_extract(dorf, "\\d\\d\\d\\d")),
    dorf = stringr::str_remove(dorf, "\\d\\d\\d\\d\\s")
  ) %>%
  tidyr::fill(c(kanton, bezirk), .direction = "down")	%>%
  dplyr::filter(!is.na(bfs_nr)) %>%
  dplyr::select(-region)

plz_bfs_data <- readr::read_delim("/Users/verne/Downloads/PLZO_CSV_WGS84/PLZO_CSV_WGS84.csv",
                  delim = ";") %>%
  janitor::clean_names() # %>%
# #   dplyr::count(plz)
# #   # dplyr::filter(gemeindename == ortschaftsname) %>% tibble::view()

# ga_hta_data <- readr::read_delim("/Users/verne/Downloads/generalabo-halbtax-mit-bevolkerungsdaten.csv",
#                   delim = ";") %>%
#   janitor::clean_names()

bevolkerung_data <- readr::read_delim("/Users/verne/Downloads/bevoelkerung_proplz.csv",
                                      delim = ";") %>%
  janitor::clean_names() %>%
  dplyr::filter(typ != "f") %>%
  dplyr::group_by(plz) %>%
  dplyr::summarise(total_pop = sum(anzahl)) %>%
  dplyr::ungroup()

save(bevolkerung_data, file = "data/bevolkerung_data.rda")

# city_pop_data %>%
#   dplyr::left_join(bevolkerung_data, by = c("plz")) %>%
#   tibble::view()

bevolkerung_kanton_data <- bevolkerung_data %>%
  dplyr::mutate(postal_code = as.character(plz)) %>%
  dplyr::inner_join(plz_all, by = "postal_code") %>%
  dplyr::select(-plz)
  # dplyr::filter(is.na(plz) |
  #                 is.na(total_pop) |
  #                 is.na(postal_code)) %>%
  # tibble::view()

# bevolkerung_data %>%
#   dplyr::distinct(plz)
#
# plz_all %>%
#   dplyr::distinct(postal_code)
#
# elektro_data_lists %>%
#   dplyr::full_join(bevolkerung_kanton_data, by = c("PostalCode" = "postal_code")) %>%
#   # dplyr::filter(!is.na(total_pop) |
#   #                 !is.na(PostalCode)) %>%
#   tibble::view()



# kanton_long <- read_csv("uglyVersion/data/kanton_short_long.csv")
#
plz_all <- readr::read_delim("/Users/verne/Downloads/plz_verzeichnis_v2.csv",
                      delim = ";") %>%
  janitor::clean_names() %>%
  mutate(postal_code = as.character(postleitzahl)) %>%
  select(postal_code, kanton) %>%
  group_by(postal_code) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(kanton_long)
