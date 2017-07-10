library(jsonlite)


DATE.FORMAT <- "%Y%m%d"

setup <- function(context) {
  if (is.null(context$base.date)) {
    context$base.date = format(x = Sys.Date(), format = DATE.FORMAT)
  }
  
  context$base.date <- as.Date(x = context$base.date, format = DATE.FORMAT)
}


T.STAR <- 1:60
reduce <- function(k, v, context) {

  est <- list()
  l <- as.list(diff(Reduce(`+`, v), lag = 1, differences = 1))
  names(l) <- format(x = context$base.date + T.STAR - 1, format = DATE.FORMAT)
    
  est$cbgcnbd <- l

  list(k, as.character(toJSON(est, auto_unbox = TRUE)))
}
