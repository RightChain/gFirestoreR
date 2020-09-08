#Collection groups fields

#' @export
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
gFire_get_cG_fields <- function(fields, cG_id,
                                db = gFire_get_global_db(),
                                project = gFire_get_global_project()){

  assert_that(
    is.string(fields),
    is.string(cG_id),
    is.string(db),
    is.string(project)
  )

  url <- sprintf(paste0(base_url, '{name=projects/%s/databases/%s/collectionGroups/%s/fields/%s}'),
                 project, db, cG_id, fields)

  f <- gar_api_generator(url, "GET")
  req <- f()

  structure(req, class='firestore_cG_field')
}


#' @export
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
gFire_list_cG_fields <- function(cG_id,
                                db = gFire_get_global_db(),
                                project = gFire_get_global_project()){

  assert_that(
    is.string(cG_id),
    is.string(db),
    is.string(project)
  )

  url <- sprintf(paste0(base_url, '{parent=projects/%s/databases/%s/collectionGroups/%s}/fields'),
                 project, db, cG_id)

  f <- gar_api_generator(url, "GET")
  req <- f()

  structure(req, class='firestore_cG_field')
}


# #' @export
# #' @import assertthat
# #' @importFrom googleAuthR gar_api_generator
# gFire_patch_cG_fields <- function(fields, cG_id,
#                                  db = gFire_get_global_db(),
#                                  project = gFire_get_global_project()){
#
#   assert_that(
#     is.string(fields),
#     is.string(cG_id),
#     is.string(db),
#     is.string(project)
#   )
#
#   url <- sprintf(paste0(base_url, '{parent=projects/%s/databases/%s/collectionGroups/%s}/fields'),
#                  project, db, cG_id)
#
#   f <- gar_api_generator(url, "PATCH")
#   req <- f()
#
#   structure(req, class='firestore_cG_field')
# }
