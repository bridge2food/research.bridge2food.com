library(haven)
library(tidyverse)
library(plotly)
library(here)

path <- paste0(here(),"/healthferm/wp4survey/")
setwd(path)

source("colors.R")
source("prep.R")
source("demographics.R")
source("diet.R")
source("attitudes.R")
source("organoleptics.R")
