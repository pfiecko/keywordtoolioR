#' Get Kewordtool.io suggestions raw single request as data frame
#'
#' The function gets raw data.frame output for single suggestions request.
#'
#' @param source One of services ids from the list: google, youtube, bing, amazon, ebay, instagram
#' @param keyword A seed keyword you want to check related keywords to.
#' @param country A country code identifying the country. Different depending on the source. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @param language A language code identyfiying the results language. Different depending on the source. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @param currency A currency code identyfiying the results currency. Different depending on the source. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @export
#' @example kwrSuggestions.raw("google", "apple iphone", "US", "en", "USD").

keywordtoolioR.getSuggestions.raw <- function(source, keyword, country, language, currency) {

  devtools::use_package("urltools")
  devtools::use_package("jsonlite")
  devtools::use_package("tidyverse")

  request <- paste("https://api.keywordtool.io/v2/search/suggestions/", ap_source,
                   "?apikey=", keywortoolioR_credentials,
                   "&keyword=", urltools::url_encode(keyword),
                   "&country=", country,
                   "&language=", language,
                   "&metrics=true",
                   "&metrics_currency=", currency,
                   "&output=json",
                   sep = "")

  try({
    output <- jsonlite::fromJSON(request)
  })

  output <- output$results %>%
    dplyr::bind_rows()

  output$seedKw <- keyword
  output$currency <- currency
  output$source <- source
  output$market <- country

  return(output)

}

#' Get Kewordtool.io suggestions single request as tidy data frame
#'
#' The function gets raw data.frame output for single suggestions request.
#'
#' @param source One of services ids from the list: google, youtube, bing, amazon, ebay, instagram
#' @param keyword A seed keyword you want to check related keywords to.
#' @param country A country code identifying the country. Different depending on the source. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @param language A language code identyfiying the results language. Different depending on the source. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @param currency A currency code identyfiying the results currency. Different depending on the source. Complete list can be found here: https://docs.keywordtool.io/reference#keyword-suggestions-google.
#' @export
#' @example kwrSuggestions.tidy("google", "apple iphone", "US", "en", "USD").

keywordtoolioR.getSuggestions.tidy <- function(source, keyword, country, language, currency) {

  devtools::use_package("urltools")
  devtools::use_package("jsonlite")
  devtools::use_package("tidyverse")

  request <- paste("https://api.keywordtool.io/v2/search/suggestions/", ap_source,
                   "?apikey=", keywortoolioR_credentials$apikey,
                   "&keyword=", urltools::url_encode(keyword),
                   "&country=", country,
                   "&language=", language,
                   "&metrics=true",
                   "&metrics_currency=", currency,
                   "&output=json",
                   sep = "")

  try({
    output <- jsonlite::fromJSON(request)
  })

  output <- output$results %>%
    dplyr::bind_rows()

  output$seedKw <- keyword
  output$currency <- currency
  output$source <- source
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
