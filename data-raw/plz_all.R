## code to prepare `plz_all` dataset goes here

usethis::use_data(plz_all, overwrite = TRUE)


# Kanton short and long name
# Bundesamt für Statistik
# Ständige und nichtständige Wohnbevölkerung nach Kanton, Anwesenheitsbewilligung, Staatsangehörigkeit, Geschlecht und Alter, 2021
#
# URL to download builder
# https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102010000_102/px-x-0102010000_102/px-x-0102010000_102.px/table/tableViewLayout2/
#
# URL to download
# https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102010000_102/px-x-0102010000_102/px-x-0102010000_102.px/table/tableViewLayout2/?downloadfile=FileTypeExcelXDoubleColumn

kanton_short_long <- readxl::read_xlsx("px-x-0102010000_102_20230116-065303_v3.xlsx", skip = 1) %>%
  janitor::clean_names() %>% # dplyr::glimpse()
  dplyr::select(kanton_kz = x8100, kanton = schweiz)


# PLZ and Kanton: PLZ_Verzeichnis - Die Schweizerische Post
#
# URL to export builder
# https://swisspost.opendatasoft.com/explore/dataset/plz_verzeichnis_v2/export/?disjunctive.postleitzahl
#
# URL to download
# https://swisspost.opendatasoft.com/explore/dataset/plz_verzeichnis_v2/download/?format=csv&timezone=Europe/Berlin&lang=de&use_labels_for_header=true&csv_separator=%3B

plz_all <- readr::read_delim("plz_verzeichnis_v2.csv",
                             delim = ";") %>%
  janitor::clean_names() %>%
  dplyr::mutate(postal_code = as.character(postleitzahl)) %>%
  dplyr::select(postal_code,
                ortbez18,
                ortbez27,
                kanton_kz = kanton) %>%
  dplyr::group_by(postal_code) %>%
  dplyr::slice_head() %>%
  dplyr::ungroup() %>%
  dplyr::left_join(kanton_short_long, by = "kanton_kz")

save(plz_all, file = "data/plz_all.rda")

