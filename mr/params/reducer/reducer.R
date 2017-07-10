library(BTYD)
library(BTYDplus)
library(jsonlite)


gg.LL <- function(params, x, zbar) {
  p <- params[1]
  q <- params[2]
  r <- params[3]
  
  lgamma(p*x + q) - lgamma(p*x) - lgamma(q) + (p*x - 1)*log(zbar) + p*x*log(x) + q*log(r) - (p*x + q)*log(r + x*zbar)
}

gg.cbs.LL <- function(params, cal.cbs) {
  cal.cbs <- subset(cal.cbs, zbar > 0)
  sum(gg.LL(params, cal.cbs$x, cal.cbs$zbar))
}

gg.EstimateParameters <- function(cal.cbs, par.start = c(1, 1, 1), max.param.value = 10000, trace = 0) {
  tryCatch(x <- cal.cbs[, "x"], 
           error = function(e) {
             stop("Error in gg.EstimateParameters: cal.cbs must have a frequency column labelled \"x\"")
           }
  )
  
  tryCatch(zbar <- cal.cbs[, "zbar"], 
           error = function(e) {
             stop("Error in gg.EstimateParameters: cal.cbs must have a monetary value column labelled \"zbar\"")
           }
  )
  
  count <- 0
  gg.eLL <- function(params, cal.cbs, max.param.value) {
    params <- exp(params)
    params[params > max.param.value] <- max.param.value
    
    ll <- gg.cbs.LL(params, cal.cbs)
    count <<- count + 1
    if (trace > 0 & count%%trace == 0) {
      cat("gg.EstimateParameters - iter", count, ":", sprintf("%12.2f", ll), ":", sprintf("%10.6f", params), "\n")
    }
    
    return(-1 * ll)
  }
  
  logparams <- log(par.start)
  res <- optim(logparams, 
               gg.eLL,
               cal.cbs = cal.cbs,
               max.param.value = max.param.value,
               method = "L-BFGS-B")
  
  estimated.params <- exp(res$par)
  estimated.params[estimated.params > max.param.value] <- max.param.value
  return(estimated.params)
}

gg.ConditionalExpectedMeanTransactionValue <- function(params, x, zbar) {
  p <- params[1]
  q <- params[2]
  r <- params[3]
  
  w <- (q - 1) / (p*x + q - 1)
  ez <- p*r / (q - 1)
  
  return(w*ez + (1 - w)*zbar)
}

gg.pdf.zeta <- function(params, zeta) {
  p <- params[1]
  q <- params[2]
  r <- params[3]
  
  return((p*r)^q * zeta^(-q - 1) * exp(-p*r/zeta) / gamma(q))
}

gg.pdf.zbar <- function(params, x) {
  p <- params[1]
  q <- params[2]
  r <- params[3]
  
  return((p*r)^q * zeta^(-q - 1) * exp(-p*r/zeta) / gamma(q))
}


cbgcnbd.estimateParameters <- function(cal.cbs, k = NULL, par.start = c(1, 1, 1, 1), max.param.value = 10000, trace = 0) {
  dc.check.model.params(c("r", "alpha", "a", "b"), par.start, "cbgcnbd.estimateParameters")
  
  if (is.null(k) & !"litt" %in% colnames(cal.cbs)) {
    stop("Either regularity parameter k need to be specified, or a column with logarithmic interpurchase times litt need to be present in cal.cbs")
  }
  
  if (is.null(k)) {
    params <- list()
    LL <- c()
    for (k in 1:12) {
      params[[k]] <- tryCatch(
        cbgcnbd.estimateParameters(cal.cbs, 
                                   par.start, 
                                   max.param.value, 
                                   k = k,
                                   trace = trace), 
        
        error = function(e) {
          e
        }
      )
      
      if (inherits(params[[k]], "error")) {
        params[[k]] <- NULL
        break
      }
      
      LL[k] <- cbgcnbd.cbs.LL(params[[k]], cal.cbs)
      if (k > 4 && LL[k] < LL[k - 1] && LL[k - 1] < LL[k - 2]) {
        break
      }
    }
    
    k <- which.max(LL)
    return(params[[k]])
  }
  
  if (!"litt" %in% colnames(cal.cbs)) {
    cal.cbs[, "litt"] <- 0
  }
  
  count <- 0
  cbgcnbd.eLL <- function(params, k, cal.cbs, max.param.value) {
    params <- exp(params)
    params[params > max.param.value] <- max.param.value
    params <- c(k, params)
    
    loglik <- cbgcnbd.cbs.LL(params, cal.cbs)
    
    count <<- count + 1
    if (trace > 0 & count%%trace == 0) {
      cat("cbgcnbd.estimateParameters - iter", count, ":", sprintf("%10.6f", loglik), ":", sprintf("%10.6f", params), "\n")
    }
    
    return(-1 * loglik)
  }
  
  results <- optim(log(par.start), cbgcnbd.eLL, cal.cbs = cal.cbs, k = k, max.param.value = max.param.value, method = "L-BFGS-B")
  
  estimated.params <- exp(results$par)
  estimated.params[estimated.params > max.param.value] <- max.param.value
  estimated.params <- c(k, estimated.params)
  
  names(estimated.params) <- c("k", "r", "alpha", "a", "b")
  return(estimated.params)
}

cbgcnbd.cbs.LL <- function (params, cal.cbs) {
  dc.check.model.params(c("k", "r", "alpha", "a", "b"), params, "cbgcnbd.cbs.LL")
  
  tryCatch(x <- cal.cbs$x, error = function(e) {
    stop("Error in cbgcnbd.cbs.LL: cal.cbs must have a frequency column labelled \"x\"")
  })
  
  tryCatch(t.x <- cal.cbs$t.x, error = function(e) {
    stop("Error in cbgcnbd.cbs.LL: cal.cbs must have a recency column labelled \"t.x\"")
  })
  
  tryCatch(T.cal <- cal.cbs$T.cal, error = function(e) {
    stop("Error in cbgcnbd.cbs.LL: cal.cbs must have a column for length of time observed labelled \"T.cal\"")
  })
  
  tryCatch(litt <- cal.cbs$litt, error = function(e) {
    stop("Error in cbgcnbd.cbs.LL: cal.cbs must have a column for sum over logarithmic inter-transaction-times labelled \"litt\"")
  })
  
  if ("custs" %in% colnames(cal.cbs)) {
    custs <- cal.cbs$custs
  } else {
    custs <- rep(1, length(x))
  }
  
  return(sum(custs * cbgcnbd.LL(params, x, t.x, T.cal, litt)))
}

cbgcnbd.LL <- function (params, x, t.x, T.cal, litt) {
  max.length <- max(length(x), length(t.x), length(T.cal))
  
  if (max.length%%length(x)) {
    warning("Maximum vector length not a multiple of the length of x")
  }
  
  if (max.length%%length(t.x)) {
    warning("Maximum vector length not a multiple of the length of t.x")
  }
  
  if (max.length%%length(T.cal)) {
    warning("Maximum vector length not a multiple of the length of T.cal")
  }
  
  if (max.length%%length(litt)) {
    warning("Maximum vector length not a multiple of the length of litt")
  }
  
  dc.check.model.params(c("k", "r", "alpha", "a", "b"), params, "cbgcnbd.LL")
  
  if (params[1] != floor(params[1]) | params[1] < 1) {
    stop("k must be integer being greater or equal to 1.")
  }
  
  if (any(x < 0) || !is.numeric(x)) {
    stop("x must be numeric and may not contain negative numbers.")
  }
  
  if (any(t.x < 0) || !is.numeric(t.x)) {
    stop("t.x must be numeric and may not contain negative numbers.")
  }
  
  if (any(T.cal < 0) || !is.numeric(T.cal)) {
    stop("T.cal must be numeric and may not contain negative numbers.")
  }
  
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  litt <- rep(litt, length.out = max.length)
  
  k <- params[1]
  r <- params[2]
  alpha <- params[3]
  a <- params[4]
  b <- params[5]
  
  P0 <- (k - 1) * litt - x * log(factorial(k - 1))
  P1 <- lgamma(a + b) + lgamma(b + x + 1) - lgamma(b) - lgamma(a + b + x + 1)
  P2 <- lgamma(r + k * x) + r * log(alpha) - lgamma(r)
  P3 <- -1 * (r + k * x) * log(alpha + T.cal)
  P4 <- (a/(b + x)) * ((alpha + T.cal)/(alpha + t.x))^(r + k * x)
  
  ifelse(
    is.infinite(P4), 
    P0 + P1 + P2 + P3 + log(a/(b+x)) + (r + k*x)*log((alpha + T.cal)/(alpha + t.x)), 
    P0 + P1 + P2 + P3 + log(P4 + 1)
  )
}


estimateParameters <- function(cal.cbs, par.start, max.param.value = 10^9, trace = 0) {
  params <- list()
  LL <- c()
  for (i in seq_along(par.start)) {
    params[[i]] <- tryCatch(
      cbgcnbd.estimateParameters(cal.cbs = cal.cbs,
                                 k = 1,
                                 par.start = par.start[[i]], 
                                 max.param.value = max.param.value,
                                 trace = trace), 
      error = function(e) { 
        NA
      })
    
    if (identical(params[[i]], NA)) {
      LL[i] <- NA
    } else {
      LL[i] <- cbgcnbd.cbs.LL(params = params[[i]], cal.cbs = cal.cbs)
      cat("params: ", params[[i]], "\nLL: ", sprintf("%10.6f", LL[i]), "\n\n")
    }
  }
  
  i.max <- which.max(LL)
  if (length(i.max) == 0) {
    NA
  } else {
    params[[i.max]]
  }
}


outliers <- function(x, na.rm = TRUE, ...) {
  x <- x[x > 0]
  q <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  d <- 1.5 * IQR(x, na.rm = na.rm)
  
  o <- rep(FALSE, length(x))
  o[x < (q[1] - d)] <- TRUE
  o[x > (q[2] + d)] <- TRUE
  o
}


gg.estimateParameters <- function(cal.cbs, par.start, max.param.value) {
  params <- list()
  LL <- c()
  for (i in seq_along(par.start)) {
    params[[i]] <- tryCatch(
      gg.EstimateParameters(cal.cbs = cal.cbs, 
                            par.start = par.start[[i]],
                            max.param.value = max.param.value), 
      error = function(e) { 
        NA
      })
    
    if (identical(params[[i]], NA)) {
      LL[i] <- NA
    } else {
      LL[i] <- gg.cbs.LL(params = params[[i]], cal.cbs = cal.cbs)
    }
  }
  
  i.max <- which.max(LL)
  if (length(i.max) == 0) {
    NA
  } else {
    params[[i.max]]
  }
}



pnbd.estimateParameters <- function(cal.cbs, par.start, max.param.value) {
  params <- list()
  LL <- c()
  for (i in seq_along(par.start)) {
    params[[i]] <- tryCatch(
      pnbd.EstimateParameters(cal.cbs = cal.cbs, 
                              par.start = par.start[[i]], 
                              max.param.value = max.param.value),
      error = function(e) { 
        NA
      })
    
    if (identical(params[[i]], NA)) {
      LL[i] <- NA
    } else {
      LL[i] <- pnbd.cbs.LL(params = params[[i]], cal.cbs = cal.cbs)
    }
  }
  
  i.max <- which.max(LL)
  if (length(i.max) == 0) {
    NA
  } else {
    params[[i.max]]
  }
}

bgnbd.estimateParameters <- function(cal.cbs, par.start, max.param.value) {
  params <- list()
  LL <- c()
  for (i in seq_along(par.start)) {
    params[[i]] <- tryCatch(
      bgnbd.EstimateParameters(cal.cbs = cal.cbs, 
                               par.start = par.start[[i]], 
                               max.param.value = max.param.value),
      error = function(e) { 
        NA
      })
    
    if (identical(params[[i]], NA)) {
      LL[i] <- NA
    } else {
      LL[i] <- bgnbd.cbs.LL(params = params[[i]], cal.cbs = cal.cbs)
    }
  }
  
  i.max <- which.max(LL)
  if (length(i.max) == 0) {
    NA
  } else {
    params[[i.max]]
  }
}


setup <- function(context) {
  if (is.null(context$rm.outliers)) {
    context$rm.outliers <- FALSE
  }
  
  if (is.null(context$pnbd.max.param.value)) {
    context$pnbd.max.param.value <- 10^9
  }
  
  if (is.null(context$bgnbd.max.param.value)) {
    context$bgnbd.max.param.value <- 10^9
  }
  
  if (is.null(context$cbgcnbd.max.param.value)) {
    context$cbgcnbd.max.param.value <- 10^9
  }
  
  if (is.null(context$gg.max.param.value)) {
    context$gg.max.param.value <- 10^9
  }
}

reduce <- function(k, v, context) {
  gc()

  m <- matrix(unlist(strsplit(v, split = "\001")), 
              ncol = 4,
              byrow = TRUE)
  
  
  cbs <- data.frame(x = as.integer(m[, 1]), 
                    t.x = as.numeric(m[, 2]),
                    T.cal = as.numeric(m[, 3]),
                    custs = 1,
                    stringsAsFactors = FALSE)

 
  cbs <- aggregate(custs ~ x + t.x + T.cal, data = cbs, sum) 
  
  cbs$litt <- 0
  
  if (max(cbs$T.cal) < 30) {
    return(NULL)
  }

  gc()
  
  par.start <- list(
    c(1.0, 1.0, 1.0, 1.0),
    c(0.5, 1.0, 0.5, 1.0),
    c(2.0, 2.0, 2.0, 2.0),
    c(1.5, 1.0, 2.0, 0.5)
  )
  
#   params.pnbd <- pnbd.estimateParameters(cal.cbs = cbs,
#                                          par.start = par.start,
#                                          max.param.value = context$pnbd.max.param.value)
  
#   params.bgnbd <- bgnbd.estimateParameters(cal.cbs = cbs, 
#                                            par.start = par.start,
#                                            max.param.value = context$bgnbd.max.param.value)
  
  params.cbgcnbd <- estimateParameters(cal.cbs = cbs,
                                       par.start = par.start,
                                       max.param.value = context$cbgcnbd.max.param.value)
  
  par.start <- list(
    c(1.0, 1.0, 1.0),
    c(0.5, 0.5, 1.0),
    c(2.0, 2.0, 2.0),
    c(1.5, 2.0, 0.5)
  )

  cbs <- data.frame(x = as.integer(m[, 1]), 
                    t.x = as.numeric(m[, 2]),
                    T.cal = as.numeric(m[, 3]),
                    zbar = as.numeric(m[, 4]),
                    stringsAsFactors = FALSE)


  params.gg <- gg.estimateParameters(cal.cbs = cbs, 
                                     par.start = par.start,
                                     max.param.value = context$gg.max.param.value)

  

  
  params <- list()
#   if (!identical(params.pnbd, NA)) {
#     params$pnbd <- params.pnbd
#   }
#   
#   if (!identical(params.bgnbd, NA)) {
#     params$bgnbd <- params.bgnbd
#   }
  
  if (!identical(params.cbgcnbd, NA)) {
    params$cbgcnbd <- params.cbgcnbd
  }
  
  if (!identical(params.gg, NA)) {
    params$gg <- params.gg
  }

  list(k, as.character(toJSON(params, auto_unbox = FALSE)))
}
