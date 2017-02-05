#LOAD PACKAGES
library(tidyverse)
library(haven)
library(readr)

#LOAD DATA
WimmerFeinsteinReplication <- read_dta("~/Google Drive/R Projects/decolonization/WimmerFeinsteinReplication.dta")
navco <- read_csv("~/Google Drive/R Projects/decolonization/navco.csv")
