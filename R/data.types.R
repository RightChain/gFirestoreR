#data.types

#' @export
gFire.generate.name <- function(id, parent=NULL, partial.path=F,
                                db = gFire_get_global_db(),
                                project = gFire_get_global_project()){
  assertthat::assert_that(is.character(name))
  root_path <- paste('projects',project,'databases',db,'documents',sep='/')
}

#' @export
is.gFire.fieldPath <- function(a) {return(class(a)=='gFire.fieldPath')}

#' @export
gFire.fieldPath <- function(field) {
  assertthat::assert_that(is.character(field))
  a <- field
  class(a) <- 'gFire.fieldPath'
  return(a)
}

#'@export
as.data.frame.gFire.fieldPath <- function(a,...) {class(a)<-NULL; return(as.data.frame(a,...))}

#' @export
is.gFire.doc <- function(a) {return(class(a)=='gFire.doc')}

#' @export
as.data.frame.gFire.doc <- function(a,...) {
  b <- a$fields
  b$gFire_id <- basename(a$name)
  b$gFire_name <- a$name
  return(as.data.frame(b,...))
}

#' @export
gFire.doc <- function(name=NULL, fields=list()) {
  a <- list()
  if (!is.null(name)) {
    assertthat::assert_that(is.character(name))
    a$name <- name
  }
  a$fields <- gFire.fields(fields)
  class(a) <- 'gFire.doc'
  return(a)
}

#' @export
is.gFire.fields <- function(a) {return(class(a)=='gFire.fields')}

#' @export
as.data.frame.gFire.fields <- function(a,...) {class(a)<-NULL; return(as.data.frame(a,...))}

#' @export
gFire.fields <- function (data=list()) {
  assertthat::assert_that(!is.NullOb(data))
  if (!is.list(data)) {data<-list(data)}
  class(data) <- 'gFire.fields'
  return(data)
}

#' @export
is.gFire.docMask <- function(a) {return(class(a)=='gFire.docMask')}

#' @export
gFire.docMask <- function (data=NULL, names=NULL) {
  if (!is.NullOb(data)) {
    names <- gFire.names(data)
  } else {
    assertthat::assert_that(is.character(names))
  }
  a <- list(fieldPaths=names)
  class(a) <- 'gFire.docMask'
  return(a)
}

#' @export
is.gFire.writeObj <- function(a) {return(class(a)=='gFire.writeObj')}

#' @export
gFire.writeObj <- function (doc=NULL, method=c('update','delete'), updateMask=NULL, overwrite=F) {
  method=match.arg(method, c('update','delete'))
  assertthat::assert_that(is.gFire.doc(doc), method %in% c('update','delete'))

  if (method == 'delete') {
    a <- list('delete'=doc$name)
  } else {
    a <- list('update'=doc)
    if (!overwrite) {
      if (!is.null(updateMask)) {
        assertthat::assert_that(is.gFire.docMask(updateMask))
        a$updateMask <- updateMask
      } else {
        a$updateMask <- gFire.docMask(doc)
      }
    }
  }
  class(a) <- 'gFire.writeObj'
  return(a)
}


##### Filters
# https://firebase.google.com/docs/firestore/reference/rest/v1/StructuredQuery#FieldFilter
#' @export
is.gFire.filter <- function(a) {return(class(a)%in% c('gFire.compositeFilter','gFire.fieldFilter','gFire.unaryFilter'))}

#' @export
is.gFire.compositeFilter <- function(a) {return(class(a)=='gFire.compositeFilter')}

#' @export
gFire.compositeFilter <- function(filters, op=c('OPERATOR_UNSPECIFIED','AND')) {
  ops <- c('OPERATOR_UNSPECIFIED','AND')
  method=match.arg(op, ops)
  assertthat::assert_that(method %in% ops, all)
  a <- list(op=method,filters=filters)
  class(a) <- 'gFire.compositeFilter'
  return(a)
}

#' @export
is.gFire.fieldFilter <- function(a) {return(class(a)=='gFire.fieldFilter')}

#' @export
gFire.fieldFilter <- function(
  field,
  op=c('OPERATOR_UNSPECIFIED','LESS_THAN','LESS_THAN_OR_EQUAL','GREATER_THAN','GREATER_THAN_OR_EQUAL','EQUAL','ARRAY_CONTAINS','IN','ARRAY_CONTAINS_ANY'),
  value) {
  ops <- c('OPERATOR_UNSPECIFIED','LESS_THAN','LESS_THAN_OR_EQUAL','GREATER_THAN','GREATER_THAN_OR_EQUAL','EQUAL','ARRAY_CONTAINS','IN','ARRAY_CONTAINS_ANY')
  method=match.arg(op, ops)
  assertthat::assert_that(method %in% ops)
  a <- list(
    field=gFire.fieldPath(field),
    op=method,
    value=value
  )
  class(a) <- 'gFire.fieldFilter'
  return(a)
}


#Get names of fields recursively: does not work on encoded data
rec_names <- function(x) {
  sapply(names(x), function(y) {
    if (is.list(x[[y]])) {
      return(paste(y, rec_names, sep='.'))
    } else {return(y)}
  })
}
gFire.names <- function(a) {UseMethod('gFire.names')}
gFire.names.gFire.doc <- function(a) {rec_names(a$fields)}
gFire.names.list <- function(a) {rec_names(a)}



