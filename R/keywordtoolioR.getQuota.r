#' Get Keywordtool.io remaining quota
#'
#' The function fetches remained keywordtool.io quotas as data frame

keywordtoolioR.getQuota <- function() {

  devtools::use_package("urltools")
  devtools::use_package("jsonlite")
  devtools::use_package("tidyverse")

  request <- paste("https://api.keywordtool.io/v2/quota",
                   "?apikey=", keywordtoolioR_credentials$apikey,
                   sep = "")

  output <- jsonlite::fromJSON(request)

  return(as.data.frame(output$limits))
}
