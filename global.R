#global

#packages

library(shiny)
library(dani)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(bslib)
library(DT)
library(thematic)


#themes for app
default_mode <- bs_theme( bootswatch = "flatly")
dark_mode <- bs_theme(bootswatch = "darkly")

#plot theme definition
plot_theme_large <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x  = element_text(size = 14),
  axis.text.y  = element_text(size = 14)
)



thematic_shiny() #to make plot backgrounds dark when dark mode enabled

  ###PROPORTIONS
#total sample size calculation for proportions
total_sample_size_prop <- function(p0, p1, p1tol, sig.level, power, r) {
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

  ###CONTINUOUS
#total sample size calculation for continuous outcomes
total_sample_size_mean <- function(mu0, mu1, sd, delta, sigma, power, r = 1){
  
  #mu0 = control mean
  #mu1 = expected mean
  #std = standard deviations (assumption is that both groups have the same std)
  #delta = NI margin on mean difference scale (+)
  #sigma = one sided alpha
  #power = desired power
  #r = allocation ratio (n in treatment/ n in control)
  
  if (is.na(sigma) || sigma <= 0 || sigma >= 1) return(Inf)
  if (is.na(power) || power <= 0 || power >= 1) return(Inf)
  
  z_alpha <- stats::qnorm(1 - sigma)
  z_beta  <- stats::qnorm(power)
  diff_exp <- mu1 - mu0
  eff <- diff_exp + delta
  
  if (eff <= 0) {
    return(Inf)
  }
  
  n_con <- ((1 + 1 / r) * (sd^2) * (z_alpha + z_beta)^2) / (eff^2)
  n_treat   <- r * n_con
  n_con <- ceiling(n_con)
  n_treat   <- ceiling(n_treat)
  total_n <- n_con + n_treat
  return(total_n)

}

