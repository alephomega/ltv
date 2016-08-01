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

cbgcnbd.estimateParameters <- function(cal.cbs, par.start, max.param.value) {
  params <- list()
  LL <- c()
  for (i in seq_along(par.start)) {
    params[[i]] <- tryCatch(
      cbgcnbd.EstimateParameters(cal.cbs = cal.cbs,
                                 k = 1,
                                 par.start = par.start[[i]], 
                                 max.param.value = max.param.value), 
      error = function(e) { 
        NA
      })
    
    if (identical(params[[i]], NA)) {
      LL[i] <- NA
    } else {
      LL[i] <- cbgcnbd.cbs.LL(params = params[[i]], cal.cbs = cal.cbs)
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
  
  params.cbgcnbd <- cbgcnbd.estimateParameters(cal.cbs = cbs,
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
