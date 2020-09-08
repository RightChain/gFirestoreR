.gFire_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  op <- options()
  op.gFirestoreR <- list(
    gFirestoreR.scopes.selected = c("https://www.googleapis.com/auth/datastore")
  )

  toset <- !(names(op.gFirestoreR) %in% names(op))

  if(any(toset)) options(op.gFirestoreR[toset])

  invisible()

}

.onAttach <- function(libname, pkgname){

  googleAuthR::gar_attach_auto_auth("https://www.googleapis.com/auth/datastore",
                                    environment_var = "GFIRE_AUTH_FILE")

  if(Sys.getenv("GFIRE_DEFAULT_PROJECT") != ""){
    .gFire_env$project <- Sys.getenv("GFIRE_DEFAULT_PROJECT")
    packageStartupMessage("Set default project name to '", Sys.getenv("GFIRE_DEFAULT_PROJECT"),"'")
  }

  if(Sys.getenv("GFIRE_DEFAULT_DB") != ""){
    .gFire_env$db <- Sys.getenv("GFIRE_DEFAULT_DB")
    packageStartupMessage("Set default database to '", Sys.getenv("GFIRE_DEFAULT_DB"),"'")
  } else {
    .gFire_env$db <- '(default)'
    packageStartupMessage("Set default database to (default)")
  }

  invisible()

}
