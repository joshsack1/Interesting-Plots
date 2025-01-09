using Pkg
Pkg.activate(".")
#%% Set up RCall with tidycensus and sf
using RCall
R"""
library(tidyverse)
library(tidycensus)
library(sf)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
"""
#%% Set up the rest of the data
using DataFrames
#%%
R"""
michigan_tsv <- read_tsv("data/michigan-2016.tsv")
"""
michigan = rcopy(R"michigan_tsv")
