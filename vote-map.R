# Call the necessary libraries
library(tidyverse)
library(tidycensus)
library(sf)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
#%% Load the necessary data
vote_data <- read_tsv("data/michigan-2016.tsv")
vote_data <- vote_data |>
  mutate(two_party = Trump + Clinton) |>
  mutate(GEOID = paste0(FIPS))
#%% Get the data on each county's population
mi_geo <- get_acs(
  geography = "county",
  variables = c(
    total_pop = "B03003_001" # Total population
  ),
  state = "MI",
  year = 2017,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
)
#%%
voter_geo <- left_join(mi_geo, vote_data, by = "GEOID")
#%% Create a generic map of the margin in each county
county_results <- ggplot(voter_geo) +
  geom_sf(aes(fill = Margin)) +
  scale_fill_gradient2(
    low = "blue", mid = "mistyrose", high = "red",
    midpoint = 0, labels = scales::percent
  ) +
  labs(
    fill = "Two Party %"
  ) +
  theme_void()
#%% Save county_results as county-results.png in the plots directory
ggsave("plots/county-results.png", county_results, width = 10, height = 10)
#%% Create a second plot that maps the votes cast divided by the population
turnout_proxy <- ggplot(voter_geo) +
  geom_sf(aes(fill = two_party / total_popE)) +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "green4",
    midpoint = 0.50, labels = scales::percent
  ) +
  labs(
    fill = "Two Party Vote / Estimated Population"
  ) +
  theme_void()
#%% Save the turnout proxy as turnout.png in the plots directory 
ggsave("plots/turnout.png", turnout_proxy, width = 10, height = 10)
