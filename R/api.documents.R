#Interact with the firebase document api
# https://firebase.google.com/docs/firestore/reference/rest/

#' @export
getDocument <- function(doc=NULL, document_path=NULL, document_name=NULL, parent_path=NULL,
                        db = gFire_get_global_db(),
                        project = gFire_get_global_project(), partial.path=F){
  #Check args
  if (!is.null(doc)) {
    assertthat::assert_that(is.gFire.doc(doc))
    full_path <- doc$name
  } else if (!is.null(document_path)) {
    assertthat::assert_that(is.character(document_path))
    if (partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',document_path, sep='/')
    } else {
      full_path <- document_path
    }

  } else if (!is.null(document_name)) {
    assertthat::assert_that(is.character(document_name), is.character(parent_path))
    if (partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',parent_path,document_path, sep='/')
    } else {
      full_path <- paste(parent_path,document_path,sep='/')
    }
  }

  f <- googleAuthR::gar_api_generator(
    paste0(base_url, full_path), "GET",
    data_parse_function=gFire.decode.doc
  )
  return(f())
}








#' @export
listDocuments <- function(cG, parent=NULL, partial.path=F,
                          orderBy=NULL, showMissing=F,
                          db = gFire_get_global_db(),
                          project = gFire_get_global_project()){
  #Check args
  if (class(cG)=='gFire.cG') {
    assertthat::assert_that(is.character(cG$name))
    full_path <- cG$name
  } else {
    assertthat::assert_that(is.character(cG))
    if (is.null(parent) & !partial.path) {
      full_path <- cG
    } else if (is.null(parent) & partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',cG, sep='/')
    } else if (class(parent) %in% c('gFire.doc','gFire.cG')) {
      full_path <- paste(parent$name, cG, sep='/')
    } else {
      assertthat::assert_that(is.character(db), is.character(project), is.character(parent))
      full_path <- paste('projects',project,'databases',db,'documents',parent, cG, sep='/')
    }
  }

  pars <- list(orderBy = orderBy,
               showMissing = showMissing,
               pageToken = "")
  pars <- rmNullObs(pars)

  parse_f <- function(a) {
    nextPageToken <- a$nextPageToken
    if (length(a$documents)==0) {
      #No more objects
      return(list())
    }
    x <- lapply(seq_len(nrow(a$documents)), function(x) {
      gFire.decode.doc(a$documents[x,])
    })
    attr(x, 'nextPageToken') <- nextPageToken
    return(x)
  }

  f <- googleAuthR::gar_api_generator(
    paste0(base_url, full_path), "GET",
    pars_args=pars,
    data_parse_function = parse_f
  )

  req <- unlist(googleAuthR::gar_api_page(
    f,
    page_f = function(x) attr(x, "nextPageToken"),
    page_method = "param",
    page_arg = "pageToken"),
    recursive=F)

  return(req)
}








#' @export
createDocument <- function(data=list(), cG, parent=NULL, partial.path=F,
                           documentId=NULL,
                           db = gFire_get_global_db(),
                           project = gFire_get_global_project()){

  assertthat::assert_that(!is.NullOb(data))
  #Check args
  if (class(cG)=='gFire.cG') {
    assertthat::assert_that(is.character(cG$name))
    full_path <- cG$name
  } else {
    assertthat::assert_that(is.character(cG))
    if (is.null(parent) & !partial.path) {
      full_path <- cG
    } else if (is.null(parent) & partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',cG, sep='/')
    } else if (class(parent) %in% c('gFire.doc','gFire.cG')) {
      full_path <- paste(parent$name, cG, sep='/')
    } else {
      assertthat::assert_that(is.character(db), is.character(project), is.character(parent))
      full_path <- paste('projects',project,'databases',db,'documents',parent, cG, sep='/')
    }
  }

  pars <- list(documentId=documentId)
  pars <- rmNullObs(pars)

  body <- jsonlite::toJSON(gFire.encode(gFire.doc(fields=data)), auto_unbox=T, json_verbatim=T)

  f <- googleAuthR::gar_api_generator(
    paste0(base_url, full_path),
    "POST",
    pars_args=pars,
    data_parse_function = gFire.decode.doc
  )
  return(f(the_body = body))
}







#' @export
deleteDocument <- function(doc=NULL, document_path=NULL, document_name=NULL, parent_path=NULL,
                           db = gFire_get_global_db(),
                           project = gFire_get_global_project(), partial.path=F){

  #Check args
  if (!is.null(doc)) {
    assertthat::assert_that(class(doc)=='gFire.doc')
    full_path <- doc$name
  } else if (!is.null(document_path)) {
    assertthat::assert_that(is.character(document_path))
    if (partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',document_path, sep='/')
    } else {
      full_path <- document_path
    }

  } else if (!is.null(document_name)) {
    assertthat::assert_that(is.character(document_name), is.character(parent_path))
    if (partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',parent_path,document_path, sep='/')
    } else {
      full_path <- paste(parent_path,document_path,sep='/')
    }
  }

  f <- googleAuthR::gar_api_generator(
    paste0(base_url, full_path),
    "DELETE"
  )
  req <- f()
  return(length(req$content)==0)
}







#' @export
patchDocument <- function(doc=NULL, newData=NULL, overwrite=F,
                           document_path=NULL, document_name=NULL, parent_path=NULL,
                           db = gFire_get_global_db(),
                           project = gFire_get_global_project()){

  #Check args
  if (!is.null(doc)) {
    assertthat::assert_that(is.gFire.doc(doc))
    full_path <- doc$name
  } else if (!is.null(document_path)) {
    assertthat::assert_that(is.character(document_path))
    if (partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',document_path, sep='/')
    } else {
      full_path <- document_path
    }

  } else if (!is.null(document_name)) {
    assertthat::assert_that(is.character(document_name), is.character(parent_path))
    if (partial.path) {
      assertthat::assert_that(is.character(db), is.character(project))
      full_path <- paste('projects',project,'databases',db,'documents',parent_path,document_path, sep='/')
    } else {
      full_path <- paste(parent_path,document_path,sep='/')
    }
  }

  #Add newData
  if (!is.null(newData)) {
    if (is.gFire.doc(doc)) {doc$fields <- gFire.fields(newData)}
    else {doc <- gFire.doc(fields=newData)}
  }

  #Create the updateMask to not delete all the non-updated fields
  if (overwrite) {pars <- list()}
  else {
    updateMask <- gFire.docMask(doc)
    pars <- c(
      setNames(as.list(updateMask$fieldPaths), rep('updateMask.fieldPaths',length(updateMask$fieldPaths)))
    )
  }

  pars <- rmNullObs(pars)
  body <- jsonlite::toJSON(gFire.encode(doc), auto_unbox=T, json_verbatim=T)

  f <- googleAuthR::gar_api_generator(
    paste0(base_url, full_path),
    "PATCH",
    pars_args=pars,
    data_parse_function = gFire.decode.doc
  )
  return(f(the_body = body))
}








#' @export
batchGetDocuments <- function(docs, partial.path=F,
                             db = gFire_get_global_db(),
                             project = gFire_get_global_project()){

  assertthat::assert_that(
    is.character(db),
    is.character(project)
  )
  root_path <- paste('projects',project,'databases',db,'documents',sep='/')

  if (is.gFire.doc(docs)) { #Single document
    full_path <- docs$name
  } else if (is.list(docs)) {
    full_path <- sapply(docs, function(x) {
      if (is.character(x)) {
        if (partial.path) {return(paste(root_path,x,sep='/'))}
        return(x)
      } else {
        assertthat::assert_that(is.gFire.doc(x))
        return(x$name)
      }
    })
  } else if (is.character(docs)) {
    full_path <- docs
    if (partial.path) {full_path <- paste(root_path,full_path,sep='/')}
  }

  body <- list(documents=full_path)
  f <- googleAuthR::gar_api_generator(
    paste0(base_url, root_path, ':batchGet'),
    "POST",
    data_parse_function = function(a){ lapply(seq_len(nrow(a$documents)), function(x) {
      gFire.decode.doc(a$documents[x,])
    })}
  )
  return(f(the_body = jsonlite::toJSON(body, auto_unbox=T, json_verbatim=T)))
}








#' @export
batchWriteDocuments <- function(writes,
                                db = gFire_get_global_db(),
                                project = gFire_get_global_project()){

  assertthat::assert_that(
    all(sapply(writes, is.gFire.writeObj)),
    is.character(db),
    is.character(project)
  )
  root_path <- paste('projects',project,'databases',db,'documents',sep='/')
  body <- paste0('{writes:[',paste0(sapply(writes, function(x) {
    jsonlite::toJSON(gFire.encode.gFire.writeObj(x), auto_unbox=T, json_verbatim=T)
  }), collapse=','),']}')
  bOut <<- body

  f <- googleAuthR::gar_api_generator(
    paste0(base_url, root_path, ':batchWrite'),
    "POST"
    # data_parse_function = function(a){ lapply(seq_len(nrow(a$documents)), function(x) {
    #   gFire.decode.doc(a$documents[x,])
    # })}
  )
  return(f(the_body = body)$content)
}



