#!/usr/bin/Rscript --vanilla

source("common.R")
source("config.R")

library(RPostgreSQL)
library(jsonlite)

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

basedate <- args.basedate()
offset <- args.offset()

conf <- config()
tz.basedir <- job.tz.basedir(conf, basedate, offset)

options(digits=10)

src <- sprintf("%s/CLV/estimation", tz.basedir)
dst <- tempfile(pattern = "estimation.")
dir.create(dst)

dfs.get(conf$fs, src = src, dst = dst, src.del = FALSE)
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


conn <- dbConnect(dbDriver(conf$db$driver),
                 user = conf$db$user,
                 password = conf$db$password,
                 dbname = conf$db$dbname,
                 host = conf$db$host,
                 port = conf$db$port)


client <- d$V1
est <- lapply(d$V2, function(j) {
  v <- fromJSON(j)$cbgcnbd
  toJSON(v, auto_unbox = T)
})

dbGetQuery(conn, "BEGIN TRANSACTION")
for(i in seq_along(est)) {
  l <- fromJSON(est[[i]])
  if (length(l) == 0) {
    next
  }
  
  est.dt <- names(l)
  est.value <- round(as.numeric(unlist(l)), digits=3)
  
  d <- data.frame(est_dt = est.dt, est_value = est.value, stringsAsFactors = FALSE)
  d$client_id <- client[i]
  d$stat_dt <- basedate
  
  apply(d,
        1,
        function(r) {
          sql <- sprintf("INSERT INTO ltvs (stat_dt, client_id, est_dt, est_value) VALUES ('%s', '%s', '%s', %f)",
                         r[4], r[3], r[1], as.numeric(r[2]))
          
          dbSendQuery(conn, sql)
        })
}

dbCommit(conn)
dbDisconnect(conn)
