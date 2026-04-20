/#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-20
# Description: Clean survey data on expense shocks
#############################################################

# Set WD ------------------------------------------------------------
#run once per session - this will be the root directory for all file paths in this project
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(readxl)

# Load data ------------------------------------------------------------
df <- read_excel("data/raw/Expense Shock Final Results.xlsx")

setwd("/Users/kamilajanmohamed/Library/CloudStorage/Dropbox/Research/ExpenseShocks/code")

getwd()
