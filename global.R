#global

#packages
library(shiny)
library(dani)
library(ggplot2)
library(dplyr)

#total sample size calculation
total_sample_size <- function(p0, p1, p1tol, sig.level, power, r) {
  n <- sample.size.NI(
    p0.expected  = p0,
    p1.expected  = p1,
    p1.tolerable = p1tol,
    sig.level    = sig.level,
    power        = power,
    r            = r,
    scale        = "RD"
  )
  sum(n)  
}