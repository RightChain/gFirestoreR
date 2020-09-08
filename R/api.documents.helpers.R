

#Convert to a document datatype for uploading
#' @export
asDocument <- function(name, fields=list()) {
  assertthat::assert_that(is.string(name))
  val <- tibble('name'=name, 'fields'=fields)
}


#Parse returned fields
parse_fields <- function(a, ignore.na=T) {
  nmes <- names(a)

  if (length(nmes) == 1 & all(nmes %in% gFire.types)) {
    x <- nmes
    switch(
      x,
      'nullValue'={return(NULL)},
      'arrayValue'={
        if (is.data.frame(a$arrayValue$values[[1]])) {
          return(parse_fields(a$arrayValue$values[[1]]))
        } else {
          return(unlist(lapply(seq_len(length(a$arrayValue$values)), function(x) {
            parse_fields(a$arrayValue$values[x])
          })))
        }
      },
      'mapValue'={return(parse_fields(a$mapValue$fields))},
      'geoPointValue'={return(data.frame('latitude'=a[[x]]$latitude, 'longitude'=a[[x]]$longitude))},
      'timestampValue'={return(timestamp_to_r(a$timestampValue))},
      {return(a[[x]])} #integerValue, doubleValue, stringValue, booleanValue, referenceValue
    )
  } else {
    return(setNames(lapply(nmes, function(x) {
      return(parse_fields(fields[[x]]))
    }), nmes))
  }
}
