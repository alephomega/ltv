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

gg.pdf.zbar <- function(params, zbar, x) {
  p <- params[1]
  q <- params[2]
  r <- params[3]
  
  1 / zbar / beta(p*x, q) * (r / (r + x*zbar))^q * (x * zbar / (r + x*zbar))^(p*x)
}

gg.var.z <- function(params) {
  p <- params[1]
  q <- params[2]
  r <- params[3]
  
  p^2 * r^2 / (q - 1)^2 / (q - 2)
}
