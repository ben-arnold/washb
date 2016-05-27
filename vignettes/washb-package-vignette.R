## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

## ----set-options, echo=FALSE, cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)

## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results = "hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
#No need without SL analysis
#library(tmle)
#library(SuperLearner)

## ----washb_bd_enrol-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(washb)
#load("data/washb_bd_enrol.rda")
#bd <- washb_bd_enrol
#data(washb_bd_diar)
#head(washb_bd_enrol)
#head(bd)
  # drop svydate and month because they are superceded in the child level diarrhea data
  #bd$svydate <- NULL
  #bd$month <- NULL

#load("data/washb_bd_diar.rda")
#d <- washb_bd_diar


