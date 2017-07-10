map <- function(k, v, context) {
  x <- unlist(strsplit(k, split = "\001"))
  r <- sample(1:10, length(x), replace = TRUE)

  list(paste(x[seq_along(x) %% 2 == 1], r, sep = '\001'), v)
}
