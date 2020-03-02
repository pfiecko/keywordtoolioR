# keywordtoolioR.utils.storeCredentials.r
#
#' Safely store your keywordtoolio credentials to your global environement.

keywordtoolioR.utils.storeCredentials <- function() {

  print("Welcome to keywordtoolioR, the simple R wrapper for keywordtool.io keyword retrieval tool. To conveniently use this package please follow the instructions below.")

  print("Please enter your keywordtool.io API code and press Enter.")
  apikey <- readlines()

  keywortoolioR_credentials <- list(apikey = apikey)
  assign("keywordtoolioR_credentials", keywordtoolioR_credentials, envir = globalenv())

  return(
    print("Your keywordtool.io credentials were successfully stored in your global environment.")
  )

}
