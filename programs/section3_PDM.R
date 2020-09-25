# Install wbstats package
install.packages("wbstats")

# Load libraries
library(tidverse)
library(stargazer)
library(wbstats)

# Load world bank data
wb_indicators <- wb_indicators()
