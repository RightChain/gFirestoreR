#' Authenticate with Google Cloud Firestore API
#'
#' @param json_file Authentication json file you have downloaded from your Google Project
#' @param token An existing auth token you may have by other means
#' @param email The email to default authenticate through
#'
#' @details
#'
#' The best way to authenticate is to use an environment argument pointing at your authentication file.
#'
#' Set the file location of your download Google Project JSON file in a \code{GFIRE_AUTH_FILE} argument
#'
#' Then, when you load the library you should auto-authenticate
#'
#' However, you can authenticate directly using this function pointing at your JSON auth file.
#'
#' @examples
#'
#' \dontrun{
#' library(gFirestoreR)
#' gFire_auth("location_of_json_file.json")
#'
#' #' # to use your own Google Cloud Project credentials
#' # go to GCP console and download client credentials JSON
#' # ideally set this in .Renviron file, not here but just for demonstration
#' Sys.setenv("GAR_CLIENT_JSON" = "location/of/file.json")
#' library(gFirestoreR)
#' # should now be able to log in via your own GCP project
#' gFire_auth()
#'
#' # reauthentication
#' # Once you have authenticated, set email to skip the interactive message
#' gFire_auth(email = "my@email.com")
#'
#' # or leave unset to bring up menu on which email to auth with
#' gFire_auth()
#' # The gFirestoreR package is requesting access to your Google account.
#' # Select a pre-authorised account or enter '0' to obtain a new token.
#' # Press Esc/Ctrl + C to abort.
#' #1: my@email.com
#' #2: work@mybusiness.com

#' # you can set authentication for many emails, then switch between them e.g.
#' gFire_auth(email = "my@email.com")
#' gFire_auth(email = "work@mybusiness.com")
#'
#' }
#' @export
gFire_auth <- function(json_file = NULL,
                     token = NULL,
                     email = Sys.getenv("GARGLE_EMAIL")){

  set_scopes()

  if(is.null(json_file)){
    googleAuthR::gar_auth(token = token,
             email = email,
             package = "gFirestoreR")
  } else {
    googleAuthR::gar_auth_service(json_file = json_file)
  }
}

set_scopes <- function(){
  required_scopes <- c("https://www.googleapis.com/auth/datastore")

  op <- getOption("gFirestoreR.scopes.selected")
  if(is.null(op)){
    options(gFirestoreR.scopes.selected = "https://www.googleapis.com/auth/datastore")
  } else if(!any(op %in% required_scopes)){
    myMessage("Adding https://www.googleapis.com/auth/datastore", level = 3)
    options(gFirestoreR.scopes.selected = c(op, "https://www.googleapis.com/auth/datastore"))
  }
}
