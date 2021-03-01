
# clear
rm(list=ls())

# load packages
library(rdrobust)
library(ggplot2)
library(shiny)

# change wd
setwd("/Users/ChristopherSimard/Desktop/Economics/Simulations/rd_simulation/")

# source helper functions
source("rd_data_gen.R")
source("rd_regress.R")
source("rd_clean.R")

# parameters
discontinuity = 0.5
order = 2
variance = 0.2
n = 300
bw_l = 1
bw_r = 0.5
kernel = "triangular"

# draw data
draws = rd_data_gen(discontinuity, order, variance, n)

# find optimal bandwidths
bws = rdbwselect(draws$y, draws$x, p=as.numeric(order), q=as.numeric(order)+1, kernel=kernel, bwselect="msetwo")
bw_l = bws[["bws"]][1]
bw_r = bws[["bws"]][2]
bw_l = 1
bw_r = 0.5

# run RD regression
results = rd_regress(draws, order, bw_l, bw_r, kernel, discontinuity)

# clean RD data for plotting
temp = rd_clean(draws, results, order, bw_l, bw_r)

