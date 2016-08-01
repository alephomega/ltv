library(gsl)
library(BTYD)
library(BTYDplus)
library(hdfs)
library(jsonlite)


h2f1 <- function(a, b, c, z) {
  lenz <- length(z)
  j = 0
  uj <- 1:lenz
  uj <- uj/uj
  y <- uj
  lteps <- 0
  while (lteps < lenz) {
    lasty <- y
    j <- j + 1
    k <- j - 1
    
    uj <- uj * (a + k) * (b + k) / (c + k) * z / j
    y <- y + uj
    lteps <- sum(y == lasty)
  }
  return(y)
}


conditionalExpectedTransactions <- function (params, T.star, x, t.x, T.cal) {
  max.length <- max(length(T.star), length(x), length(t.x), 
                    length(T.cal))
  if (max.length%%length(T.star)) 
    warning("Maximum vector length not a multiple of the length of T.star")
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  dc.check.model.params(c("k", "r", "alpha", "a", "b"), params, 
                        "cbgcnbd.ConditionalExpectedTransactions")
  if (params[1] != floor(params[1]) | params[1] < 1) 
    stop("k must be integer being greater or equal to 1.")
  if (params[1] > 1) 
    message("Results for k>1 are approximative")
  if (any(T.star < 0) || !is.numeric(T.star)) 
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  T.star <- rep(T.star, length.out = max.length)
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  k <- params[1]
  r <- params[2]
  alpha <- params[3]
  a <- params[4]
  b <- params[5]
  alpha <- alpha * k
  P1 <- ((a + b + x)/(a - 1))
  P2 <- 1 - ((alpha + T.cal)/(alpha + T.cal + T.star))^(r + x) * h2f1(r + x, b + x + 1, b + x + a, T.star/(alpha + 
                                                                                                             T.cal + T.star))
  P3 <- cbgcnbd.PAlive(params, x, t.x, T.cal)
  return(P1 * P2 * P3)
}



filter <- function(path) {
  f <- character(0)
  
  l <- list.files(path, pattern = "^[^_.]", full.names = TRUE, include.dirs = TRUE)
  repeat {
    isdir <- file.info(l)$isdir
    
    f <- c(f, l[which(!isdir)])
    if (all(!isdir)) {
      break
    }
    
    l <- list.files(l[which(isdir)], pattern = "^[^_.]", full.names = TRUE, include.dirs = TRUE)
  }
  
  f
}


DATE.FORMAT <- "%Y%m%d"

setup <- function(context) {
  if (is.null(context$base.date)) {
    context$base.date = format(x = Sys.Date(), format = DATE.FORMAT)
  }
  
  context$base.date <- as.Date(x = context$base.date, format = DATE.FORMAT)
  
  dst <- tempfile(pattern = "clv-params.")
  dir.create(dst)
  
  fs <- init.fs(default.fs = context$default.fs)
  fs.get(fs, src = context$params.dir, dst = dst)
  d <- do.call("rbind", 
               lapply(filter(dst), 
                      function(f) {
                        if (file.info(f)$size > 0) {
                          read.table(file = f, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
                        } else {
                          NULL
                        }
                      }))
  
  unlink(x = dst, recursive = TRUE, force = TRUE)
  
  context$models <- list(client = d[, 1], 
                         params = lapply(d[, 2], 
                                         function(p) {
                                           fromJSON(p)
                                         }))
}

T.STAR <- 1:60
reduce <- function(k, v, context) {
  i <- which(context$models$client == k)
  if (length(i) == 0) {
    return(NULL)  
  }
  
  params <- context$models$params[[i]]
  if (is.null(params$gg)) {
    return(NULL)
  }
  
  m <- matrix(unlist(strsplit(v, split = "\001")), 
              ncol = 4,
              byrow = TRUE)
  
  x = as.integer(m[, 1])
  t.x = as.numeric(m[, 2])
  T.cal = as.numeric(m[, 3])
  zbar = as.numeric(m[, 4])
  rm(m)
  
  if (max(T.cal) < 30) {
    return(NULL)
  }
  
  mv <- gg.ConditionalExpectedMeanTransactionValue(params = params$gg, 
                                                   x = x,
                                                   zbar = zbar)
  mv[mv < 0] <- 0
  
  est <- list()
  if (!is.null(params$pnbd)) {
    e <- c()
    for (t in T.STAR) {
      tx <- pnbd.ConditionalExpectedTransactions(params = params$pnbd, 
                                                 T.star = t, 
                                                 x = x,
                                                 t.x = t.x,
                                                 T.cal = T.cal)
      
      
      tx[tx < 0] <- 0
      e[t] <- sum(tx * mv, na.rm = TRUE)
    }
    
    l <- as.list(diff(c(0, e), lag = 1, differences = 1))
    names(l) <- format(x = context$base.date + T.STAR - 1, format = DATE.FORMAT)
    
    est$pnbd <- l
  }
  
  if (!is.null(params$bgnbd)) {
    e <- c()
    for (t in T.STAR) {
      tx <- bgnbd.ConditionalExpectedTransactions(params = params$bgnbd, 
                                                  T.star = t, 
                                                  x = x,
                                                  t.x = t.x,
                                                  T.cal = T.cal)
      
      tx[tx < 0] <- 0
      e[t] <- sum(tx * mv, na.rm = TRUE)
    }
    
    l <- as.list(diff(c(0, e), lag = 1, differences = 1))
    names(l) <- format(x = context$base.date + T.STAR - 1, format = DATE.FORMAT)
    
    est$bgnbd <- l
  }
  
  if (!is.null(params$cbgcnbd)) {
    e <- c()
    n <- length(x)
    
    #     std.x <- (x- ) / 
    #     std.t.x <- (t.x - ) / 
    #     std.v <- (x*zbar - ) / 
    #     w <- exp((std.x + std.t.x + std.v) / 3)
    
    for (t in T.STAR) {
      tx <- cbgcnbd.ConditionalExpectedTransactions(params = params$cbgcnbd, 
                                                    T.star = t, 
                                                    x = x,
                                                    t.x = t.x,
                                                    T.cal = T.cal)
      
      tx[tx < 0] <- 0
      e[t] <- sum(tx * mv, na.rm = TRUE)
    }
    
    l <- as.list(diff(c(0, e), lag = 1, differences = 1))
    names(l) <- format(x = context$base.date + T.STAR - 1, format = DATE.FORMAT)
    
    est$cbgcnbd <- l
  }
  
  list(k, as.character(toJSON(est, auto_unbox = TRUE)))
}
