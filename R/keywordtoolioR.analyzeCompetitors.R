#' Get Keywordtool.io raw analyze competitors request as data frame
#'
#' The function gets raw data frame output for single analyze competitors request.
#'
#' @param competitorsWebsite Url of the website you want to get keywords for.
#' @param country A country code identifying the country. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @param location A location code identyfiying the location. Complete list can be found here: https://docs.keywordtool.io/reference#analyze-competitors-google-metrics-location.
#' @param language A language code identifying the results language. Complete list can be found here: https://docs.keywordtool.io/reference#analyze-competitors-google-metrics-language.
#' @param currency A currency code identifying the results currency. Complete list can be found here: https://docs.keywordtool.io/reference#analyze-competitors-google-metrics-currency.
#' @param network "googlesearch" or "googlesearchnetwork" for only search or search and partners respectively.
#' @param complete true or false if results should be complete.
#' @export
#' @example kwrAnalyzeCompetitors.raw("onet.pl", "PL", "2616", "pl", "PLN", "googlesearchnetwork")

keywordtoolioR.analyzeCompetitors.raw <- function(competitorsWebsite, country, location, language, currency, network, complete) {

  devtools::use_package("urltools")
  devtools::use_package("tidyverse")

  request <- paste("https://api.keywordtool.io/v2/search/analyze-competitors/google",
                   "?apikey=", keywortoolioR_credentials$apikey,
                   "&keyword=", urltools::url_encode(competitorsWebsite),
                   "&metics_location=", location,
                   "&metrics_language=", language,
                   "&metrics_network=", network,
                   "&metrics_currency=", currency,
                   "&complete=", complete,
                   "&output=json",
                   sep = "")

  try({
    output <- jsonlite::fromJSON(request)
  })

  output <- output$results %>%
    dplyr::bind_rows()

    output$competitorsWebsite <- competitorsWebsite
    output$currency <- currency
    output$source <- "google"
    output$market <- country

    return(output)

}

#' Get Keywordtool.io analyze-competitors request as data tidy data frame
#'
#' The function gets tidy data frame output for single analyze competitors request.
#' @param competitorsWebsite Url of the website you want to get keywords for.
#' @param country A country code identifying the country. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @param location A location code identyfiying the location. Complete list can be found here: https://docs.keywordtool.io/reference#analyze-competitors-google-metrics-location.
#' @param language A language code identifying the results language. Complete list can be found here: https://docs.keywordtool.io/reference#analyze-competitors-google-metrics-language.
#' @param currency A currency code identifying the results currency. Complete list can be found here: https://docs.keywordtool.io/reference#analyze-competitors-google-metrics-currency.
#' @param network "googlesearch" or "googlesearchnetwork" for only search or search and partners respectively.
#' @param complete true or false if results should be complete.
#' @export
#' @example kwrAnalyzeCompetitors.tidy("onet.pl", "PL", "2616", "pl", "PLN", "googlesearchnetwork")

keywordtoolioR.analyzeCompetitors.tidy <- function(competitorsWebsite, country, location, language, currency, network, complete = "false") {

  devtools::use_package("urltools")
  devtools::use_package("tidyverse")

  request <- paste("https://api.keywordtool.io/v2/search/analyze-competitors/google",
                   "?apikey=", apikey,
                   "&keyword=", urltools::url_encode(competitorsWebsite),
                   "&metrics_location=", location,
                   "&metrics_language=", language,
                   "&metrics_network=", network,
                   "&metrics_currency=", currency,
                   "&complete=", complete,
                   "&output=json",
                   sep = "")

  try({
    output <- jsonlite::fromJSON(request)
  })

  output <- output$results %>%
    dplyr::bind_rows()

  output$competitorsWebsite <- competitorsWebsite
  output$currency <- currency
  output$source <- "google"
  output$market <- country

  output_trended <- output %>%
    tidyr::gather(key = monthNum, value = monthlyVolume, "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12")

  output_months <- output_trended %>%
    dplyr::select(3:26) %>%
    dplyr::filter(row_number() == 1) %>%
    tidyr::gather(key = monthNum, value = month, "m1_month", "m2_month", "m3_month", "m4_month", "m5_month", "m6_month", "m7_month", "m8_month", "m9_month", "m10_month", "m11_month", "m12_month") %>%
    dplyr::mutate(monthNum = str_remove(monthNum, "_month")) %>%
    dplyr::select(13:14)

  output_years <- output_trended %>%
    dplyr::select(3:26) %>%
    dplyr::filter(row_number() == 1) %>%
    tidyr::gather(key = monthNum, value = year, "m1_year", "m2_year", "m3_year", "m4_year", "m5_year", "m6_year", "m7_year", "m8_year", "m9_year", "m10_year", "m11_year", "m12_year") %>%
    dplyr::mutate(monthNum = str_remove(monthNum, "_year")) %>%
    dplyr::select(13:14)

  output_ym <- output_months %>%
    dplyr::full_join(output_years)

  output_trended_ym <- output_trended %>%
    dplyr::select(-c(3:26)) %>%
    dplyr::left_join(output_ym, by = "monthNum")

  return(output_trended_ym)

}
