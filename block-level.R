#%% Set up packages
library(tidyverse)
library(tidycensus)
library(sf)
#%%
census_api_key(Sys.getenv("CENSUS_API_KEY"))
#%% Get block level data on the number of hispanics and population for Union County, NC 
union_2020 <- get_decennial(
  geography = "block",
  variables = c(total = "P1_001N", hispanic = "P2_002N"),
  year = 2020,
  state = "NC",
  county = "Union",
  geometry = TRUE
)
#%% pivot the data so that total and hispanic are in their own columns
union_plottable <- pivot_wider(union_2020, names_from = variable, values_from = value)
#%% add a variable for the hispanic share, with na if there are no people on the block
union_plottable <- union_plottable |>
    mutate(hispanic_share = ifelse(total ==0, NA, hispanic / total))
#%% create a map of the data. Color missing data grey
union_block <- ggplot(union_plottable) +
    geom_sf(aes(fill = hispanic_share)) +
    scale_fill_gradient2(
        low = "red", mid = "white", high = "purple",
        labels = scales::percent, midpoint = 0.0,
        na.value = "dimgray"
    ) + labs(
        fill = "Hispanic Population Share"
    ) +
    theme_void()
#%% Save the map 
ggsave("plots/union_nc.png", union_block, width = 10, height = 10)
