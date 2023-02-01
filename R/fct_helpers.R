#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

load_data <- function(){

  swiss_charge_spin <- tagList(
    img(src = "www/swiss_charge.gif", height = "400px"),
    h3("Charging...")
  )

  # waiter::waiter_set_theme(html = swiss_charge_spin, color = "white") # waiter:: spin_hexdots()
  wait_screen <- waiter::Waiter$new(html = swiss_charge_spin, color = "white")
  wait_screen$show()

  url_json <- "https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/oicp/ch.bfe.ladestellen-elektromobilitaet.json"

  elektro_data_json <- httr::GET(
    url_json,
    httr::content_type_json()
  ) %>%
    httr::content()

  elektro_data_lists <- elektro_data_json %>%
    purrr::pluck('EVSEData') %>%
    tibble::enframe() %>%
    tidyr::unnest_wider(value) %>%
    dplyr::filter(EVSEDataRecord != "NULL") %>%
    tidyr::unnest_longer(EVSEDataRecord) %>%
    tidyr::unnest_wider(EVSEDataRecord) %>%
    tidyr::unnest_wider(Address) %>%
    tidyr::unnest_wider(GeoCoordinates)

  elektro_data_lists <- elektro_data_lists %>%
    dplyr::mutate(
      lat = stringr::str_extract(Google, "/*.* "),
      lat = stringr::str_remove(lat, " "),
      lat = as.numeric(lat),
      lon = stringr::str_extract(Google, " /*.*"),
      lon = stringr::str_remove(lon, " "),
      lon = as.numeric(lon)
    ) %>%
    dplyr::filter(dplyr::between(lon, 5.956303, 10.491944),
                  dplyr::between(lat, 45.818031, 47.808264),
                  Country %in% c("CH", "CHE"))

  elektro_select <- elektro_data_lists %>%
    dplyr::select(
      "Country",
      "City",
      "HouseNum",
      "PostalCode",
      "Street",
      "Plugs",
      "Accessibility",
      "EvseID",
      "OperatorID",
      "OperatorName",
      "lat",
      "lon"
    ) %>%
    janitor::clean_names()

  ### Adding color palette

  op_color_pal <- c(
    "evpass" = "#ed556f",
    "eCarUp" = "#ffac57",
    "Swisscharge" = "#f7fb2f",
    "Move" = "#71ec2d",
    "PLUG’N ROLL" = "#62ffaa",
    "Other" = "#44ddf9"
  )

  elek_kanton <- elektro_select %>%
    dplyr::left_join(bevolkerung_data, by = "postal_code") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(op_color = forcats::fct_infreq(operator_name),
                  op_color = forcats::fct_lump_n(op_color, n = 5),
                  color = op_color_pal[as.character(op_color)])

  ### Adding URL for Kanton Wappen

  url_wiki <- "https://de.wikipedia.org/wiki/Liste_der_Wappen_und_Fahnen_der_Schweizer_Kantone"

  html_wappen <- rvest::read_html(url_wiki)

  url_wappen <- html_wappen %>%
    rvest::html_elements("img") %>%
    rvest::html_attr("src")

  wappen <- url_wappen %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, c("Wappen_|of_arms"))) %>%
    dplyr::mutate(kanton = stringr::str_extract_all(value, "/82px*.*$"),
                  kanton = stringr::str_remove_all(kanton,
                  stringr::str_glue("/82px-|Wappen_\\
                                        |_alt.svg.png|_matt.svg.png|.svg.png\\
                                        |Coat_of_arms_of_|Kanton_|canton_of_")),
                  kanton = stringr::str_replace_all(kanton, "_", " "),
                  kanton = stringr::str_replace_all(kanton, "%C3%BC", "ü"),
                  value = stringr::str_replace_all(value, "//", "https://")) %>%
    dplyr::filter(stringr::str_detect(kanton, "(0)",negate = TRUE)) %>%
    dplyr::rename(wappen_url = value) %>%
    dplyr::left_join(kanton_kz, by = "kanton") %>%
    dplyr::select(-kanton)

  elek_kanton <- elek_kanton %>%
    dplyr::left_join(wappen, by = "kanton_kz") %>%
    dplyr::mutate(kanton = stringr::str_replace_na(kanton, "None Listed"),
                  city = stringr::str_replace_na(city, "None Listed"))

  wait_screen$hide()
  # waiter::waiter_hide()

  return(elek_kanton)
}
