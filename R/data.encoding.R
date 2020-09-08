#Encode and decode from API format
gFire.types <- c('stringValue',
                 'integerValue',
                 'doubleValue',
                 'nullValue',
                 'arrayValue',
                 'mapValue',
                 'booleanValue',
                 'timestampValue',
                 'referenceValue',
                 'geoPointValue') #not supported, ignoring for now

#basic data.type conversions
gFire.encode.basic <- function(a, type) {
  n <- length(a)
  if (n<=0) {
    return(gFire.encode.null())
  } else if (n==1) {
    return(setNames(as.list(a), type))
  } else {
    return(gFire.encode.array(a, type))
  }
}
gFire.encode.array <- function(a, type) {
  b <- list(
    'arrayValue'=list(
      #jsonlite does not format this appropriately, so some custom json in here
      'values'=paste0(
        '[',
        paste0(sapply(seq_len(length(a)), function(x) {
          jsonlite::toJSON(setNames(list(a[x]), type), auto_unbox=T)
        }), collapse=','),
        ']')
    )
  )
  class(b$arrayValue$values) <- 'json'
  return(b)
}


#' gFire.encode
#' @export
gFire.encode <- function(a) {UseMethod('gFire.encode')}
gFire.encode.NULL<- function(a=NULL) {return(list('nullValue'=NULL))}
gFire.encode.logical <- function(a) {gFire.encode.basic(a, 'booleanValue')}
gFire.encode.integer <- function(a) {gFire.encode.basic(as.character(a), 'integerValue')}
gFire.encode.numeric <- function(a) {gFire.encode.basic(a, 'doubleValue')}
gFire.encode.POSIXct <- function(a) {gFire.encode.basic(format(a, '%Y-%m-%dT%H:%M:%SZ'), 'timestampValue')}
gFire.encode.character <- function(a) {gFire.encode.basic(a, 'stringValue')}
gFire.encode.factor <- function(a) {gFire.encode.basic(as.character(a), 'stringValue')}
gFire.encode.list <- function(a) {list('mapValue'=list('fields'=setNames(lapply(a, gFire.encode),names(a))))}
gFire.encode.data.frame <- function(a) {list('mapValue'=list('fields'=setNames(lapply(a, gFire.encode),names(a))))}
# bytes_2_gFire <- function(a) {gFire.encode.basic(a, 'bytesValue')}
# reference_2_gFire <- function(a) {gFire.encode.basic(a, 'referenceValue')}
# latlon_2_gFire <- function(a) {gFire.encode.basic(a, 'geoPointValue')}
gFire.encode.gFire.fields <- function(a) {
  assertthat::assert_that(class(a) == 'gFire.fields')
  return(setNames(lapply(a, gFire.encode),names(a)))
}
gFire.encode.gFire.map <- function(a) {
  assertthat::assert_that(class(a) == 'gFire.map')
  return(gFire.encode.list(a))
}

#' @import assertthat
gFire.encode.gFire.doc <- function(a, rm.name=F) {
  assertthat::assert_that(class(a) == 'gFire.doc')
  a$fields <- gFire.encode.gFire.fields(a$fields)
  a$updateTime <- NULL
  a$createTime <- NULL
  if (rm.name) {a$name <- NULL}
  return(rmNullObs(a))
}

#' @import assertthat
gFire.encode.gFire.docMask <- function(a) {
  assertthat::assert_that(class(a) == 'gFire.docMask')
  names(a$fieldPaths) <- NULL
  return(a)
}

#' @import assertthat
gFire.encode.gFire.writeObj <- function(a) {
  assertthat::assert_that(class(a) == 'gFire.writeObj')
  if ('update' %in% names(a)) {a$update <- gFire.encode.gFire.doc(a$update)}
  # if ('delete' %in% names(a)) {a$delete <- gFire.encode(a$delete)} #Do nothing
  if ('updateMask' %in% names(a)) {a$updateMask <- gFire.encode(a$updateMask)}
  return(rmNullObs(a))
}


#####################################################
#####################################################


gFire.decode.basic <- function(a, type, ignore.na=T) {
  stopifnot(is.list(a), length(a)==1, type %in% gFire.types, type %in% names(a))
  if (is.null(type)) {return(a[[1]])}
  if (all(is.na(a[[type]])) & ignore.na) {return(NULL)}
  switch(
    type,
    'nullValue'={return(NULL)},
    'timestampValue'={return(as.POSIXct(a[[type]], format = "%Y-%m-%dT%H:%M:%S"))},
    # 'geoPointValue'={}, #Not supported
    {return(a[[type]])}
  )
}

gFire.decode.array <- function(a, ignore.na=T) {
  stopifnot(
    is.list(a),
    'arrayValue' == names(a), is.list(a$arrayValue),
    'values' == names(a$arrayValue), is.list(a$arrayValue$values),
    names(a$arrayValue$values) %in% gFire.types
  )
  b <- a$arrayValue$values
  if (is.null(b[[1]])) {
    return(NULL)

  } else if (is.data.frame(b[[1]])) {
    nmes <- names(b[[1]])
    if (nrow(b[[1]])==0) {if (ignore.na) {return(NULL)} else {return(NULL)}}
    vals <- lapply(seq_len(length(nmes)), function(x) {
      gFire.decode.basic(b[[1]][x], nmes[x], ignore.na=F)
    })

  } else { #its a list
    nmes <- names(b)
    if (ignore.na & length(b)==0) {return(NULL)}
    vals <- lapply(seq_len(length(nmes)), function(x) {
      gFire.decode.basic(b[x], nmes[x], ignore.na=F)
    })
  }

  z <- vals[[1]]
  if (length(nmes)>1) {for (y in seq_len(length(nmes)-1)) {
    z <- ifelse(is.na(z), vals[[y+1]], z)
  }}
  if (ignore.na & all(is.na(z))) {return(NULL)}
  return(z)
}

gFire.decode.fields <- function(a, ignore.na=T, simplify.2.df=T) {
  stopifnot(is.list(a), unlist(sapply(a, names)) %in% gFire.types)
  nmes <- names(a)
  if (is.null(nmes)) {return(unlist(a))}

  vals <- sapply(nmes, function(x) {
    nmes2 <- names(a[[x]])
    z <- lapply(seq_len(length(nmes2)), function(y) {
      switch(
        nmes2[y],
        'arrayValue'={gFire.decode.array(a[[x]][y], ignore.na)},
        'mapValue'={gFire.decode.map(a[[x]][y], ignore.na, simplify.2.df)},
        {gFire.decode.basic(a[[x]][y], nmes2[y], ignore.na)}
      )
    })
    z <- unlist(z[!sapply(z, is.null)], recursive=F)
    if (ignore.na & is.null(z)) {return(list(NULL))}
    return(list(z))
  })
  vals <- vals[!sapply(vals, is.null)]
  if (ignore.na & length(vals)==0) {return(NULL)}
  # if (simplify.2.df & length(vals)>=2 & all(length(vals[[1]]) == sapply(vals, length))) {
  #   return(as.data.frame(vals, stringsAsFactors=F, check.names=F))
  # }
  class(vals) <- 'gFire.fields'
  return(vals)
}

gFire.decode.map <- function(a, ignore.na=T, simplify.2.df=T) {
  stopifnot(
    is.list(a),
    'mapValue' == names(a), is.list(a$mapValue),
    'fields' == names(a$mapValue), is.list(a$mapValue$fields)
  )
  b <- gFire.decode.fields(a$mapValue$fields, ignore.na, simplify.2.df)
  class(b) <- 'gFire.map'
  return(b)
}

gFire.decode.doc <- function(a, ignore.na=T, simplify.2.df=T) {
  stopifnot(
    is.list(a), c('name','fields','createTime','updateTime') %in% names(a)
  )
  b <- list(
    name=a$name,
    fields=gFire.decode.fields(a$fields, ignore.na, simplify.2.df),
    createTime=as.POSIXct(a$createTime, format = "%Y-%m-%dT%H:%M:%S"),
    updateTime=as.POSIXct(a$updateTime, format = "%Y-%m-%dT%H:%M:%S")
  )
  class(b) <- 'gFire.doc'
  return(b)
}

gFire.decode.writeObj <- function(a, ignore.na=T, simplify.2.df=T) {
  assertthat::assert_that(class(a) == 'gFire.writeObj')
  if ('update' %in% names(a)) {a$update <- gFire.decode.doc(a$update, ignore.na=T, simplify.2.df=T)}
  # if ('delete' %in% names(a)) {a$delete <- gFire.decode.basic(a$delete)} #Do nothing
  if ('updateMask' %in% names(a)) {a$updateMask <- gFire.decode(a$updateMask)}
  return(a)
}
