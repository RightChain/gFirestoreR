#global settings

gFire_get_global_db <- function() {
  if(!exists("db", envir = .gFire_env)){
    stop("DB is NULL and couldn't find global db name.
         Set it via gFire_set_global_db")
  }

  .gFire_env$db
}

gFire_get_global_project <- function() {
  if(!exists("project", envir = .gFire_env)){
    stop("Project is NULL and couldn't find global project name.
         Set it via gFire_set_global_project")
  }

  .gFire_env$project
}
