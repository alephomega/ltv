map <- function(k, v, context) {
  x <- unlist(strsplit(k, split = "\001"))
  list(x[seq_along(x) %% 2 == 1], v)
}
