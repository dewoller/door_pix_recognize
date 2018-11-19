#!/usr/bin/Rscript

library(plumber)

r <- plumb("/code/pivideo/door_pix_recognize/lib/plumber.R")
r$run(port = 8000)
