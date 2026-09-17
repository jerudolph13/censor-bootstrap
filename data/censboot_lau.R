
###########################################################################
#
# Project: Bootstrapping censored data
#
# Purpose: Apply different bootstrap approaches to survival data
#
# Author: Jacqueline Rudolph
#
# Last Update: 20 Jul 2026
#
###########################################################################

library(tidyverse)
library(boot)
library(survival)


# Read in data ------------------------------------------------------------

dat <- read_csv("./wihs_public_lau2009.csv") %>% 
  mutate(delta = as.numeric(eventtype==2)) %>% 
  select(t, delta, BASEIDU) 

# The below doesn't seem to be necessary for conditional; maybe package handles that
#dat <- bind_rows(dat, data.frame(t=10.81451+2, delta=0, BASEIDU=1))


# Run analyses ------------------------------------------------------------

# GOAL: Estimate risk at 10 years for AIDS or death (censor at treatment) by baseline
#       injection drug use and estimate standard errors

# Greenwood's variance
surv <- survfit(Surv(t, delta) ~ BASEIDU, data=dat)

res <- data.frame(idu = c(rep(0, surv$strata[[1]]), rep(1, surv$strata[[2]])),
                  time = surv$time,
                  risk = 1 - surv$surv,
                  std.error = surv$std.err)
res2 <- res[res$time<=10, ]
  res2[!duplicated(res2$idu, fromLast=T), ]

# Classic bootstrap
set.seed(123)
surv.fun <- function(data, d) {
  
  boot <- data[d, ]
  
  surv <- survfit(Surv(t, delta) ~ BASEIDU, data=boot)
  
  res <- data.frame(idu = c(rep(0, surv$strata[[1]]), rep(1, surv$strata[[2]])),
                    time = surv$time,
                    risk = 1 - surv$surv)

  res2 <- res[res$time<=10, ]
  out <- res2[!duplicated(res2$idu, fromLast=T), "risk"]

  return(out)
  
}
classic <- boot(dat, surv.fun, R=1000)
  classic
  
# Case-based bootstrap
set.seed(123)
surv.fun2 <- function(data) {
  
  surv <- survfit(Surv(t, delta) ~ BASEIDU, data=data)
  
  res <- data.frame(idu = c(rep(0, surv$strata[[1]]), rep(1, surv$strata[[2]])),
                    time = surv$time,
                    risk = 1 - surv$surv)
  
  res2 <- res[res$time<=10, ]
  out <- res2[res2$time==max(res2$time), "risk"]
  
  out <- res2[!duplicated(res2$idu, fromLast=T), "risk"]
  
  return(out)
  
}
case <- censboot(dat, surv.fun2, R=1000)
  case

# Conditional bootstrap (NOTE: this does not play nicely with tibbles)
  # Not sure what bias is measuring, but bias is large if you don't do stratified bootstrap
set.seed(123)
event.surv <- survfit(Surv(t, delta) ~ BASEIDU, data=dat)
censor.surv <- survfit(Surv(t-0.0001*delta, 1-delta) ~ 1, data=dat)
cond <- censboot(data.frame(dat), surv.fun2, R=1000, strata=dat$BASEIDU,
                 F.surv=event.surv, G.surv=censor.surv, sim="cond")
  cond

# Weird bootstrap
set.seed(123)
surv.fun3 <- function(data, str) {
  
  surv <- survfit(Surv(data[, 1], data[, 2]) ~ str)
  
  res <- data.frame(idu = c(rep(0, surv$strata[[1]]), rep(1, surv$strata[[2]])),
    time = surv$time,
    risk = 1 - surv$surv)
  
  res2 <- res[res$time<=10, ]
  out <- res2[!duplicated(res2$idu, fromLast=T), "risk"]
  
  return(out)
  
}
weird <- censboot(cbind(dat$t, dat$delta), surv.fun3, R=1000,
                  strata=dat$BASEIDU, F.surv=event.surv, sim="weird")
  weird
