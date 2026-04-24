#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-24
# Description: Plot exhibit set C:
        # C1. Shock Size Distribution as Share of Income
        # C2. Shock Size Distribution as Share of Liquid Savings 
        # C3. Shock Size by Income Group/Cash Buffer/Shock Type
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(pal)

# Load data ------------------------------------------------------------
df <- read_csv("data/clean/surveyData.csv")

# Plot C1: Shock Size Distribution as Share of Income ------------------------------------------------------------
df$lshock_size
df$lshock_hh_inc

ggplot(df, aes(x = lshock_hh_inc, y = lshock_size)) + 
geom_point()
