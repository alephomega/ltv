#!/usr/bin/Rscript --vanilla


basedir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  m <- grep(needle, args)
  if (length(m) > 0) {
    f <- normalizePath(sub(needle, "", args[m]))
  } else {
    f <- normalizePath(sys.frames()[[1]]$ofile)
  }
  
  dirname(f)
}

setwd(basedir())
source("common.R")
source("config.R")

basedate <- args.basedate()
offset <- args.offset()


run.task <- function(command, args, wait = TRUE) {
  status <- system2(command, args, wait = wait)
  if (status != 0) {
    stop(sprintf("%s failed.", command))
  }
}


cat(print.timestamp(), "Reading configurations.\n")

conf <- config()
print(conf)


tz.basedir <- job.tz.basedir(conf, basedate, offset)

cat(print.timestamp(), "** Running ltv.\n")

z <- FALSE
z <- fs.exists(conf$fs, sprintf("%s/attributes/{NEW,ACTIVE,WINBACK}-*", tz.basedir))
if (!z) {
  cat(print.timestamp(), "No input to process.\n")
} else {
  args <- c(
    "--base-date", 
    basedate, 
    "--offset",
    offset  
  )

  command <- file.path(getwd(), "rfm.R")
  run.task(command, args)

  command <- file.path(getwd(), "params.R")
  run.task(command, args)

  command <- file.path(getwd(), "estimation.R")
  run.task(command, args)
  
  command <- file.path(getwd(), "pg.R")
  run.task(command, args)
}
