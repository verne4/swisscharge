## code to prepare `bevolkerung_data` dataset goes here

usethis::use_data(bevolkerung_data, overwrite = TRUE)

load("data/plz_all.rda")

bevolkerung_data <- readr::read_delim("/Users/verne/Downloads/bevoelkerung_proplz.csv",
                                      delim = ";") %>%
  janitor::clean_names() %>%
  dplyr::filter(typ != "f") %>% # remove companies
  dplyr::group_by(plz) %>%
  dplyr::summarise(pop_plz = sum(anzahl)) %>%
  dplyr::ungroup()

bevolkerung_data <- bevolkerung_data %>%
  dplyr::mutate(postal_code = as.character(plz)) %>%
  dplyr::select(-plz) %>%
  dplyr::left_join(plz_all, by = "postal_code") %>%
  dplyr::left_join(kanton_pop_data, by = "kanton") %>%
  dplyr::filter(!is.na(kanton))

save(bevolkerung_data, file = "data/bevolkerung_data.rda")
