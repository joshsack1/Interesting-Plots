#%% Set up packages
library(tidyverse)
library(tidycensus)
library(sf)
#%%
census_api_key(Sys.getenv("CENSUS_API_KEY"))
#%% Create a function that takes a state's two letter abbreviation
# and a year the ACS was run and returns a data.frame with estimated population and geometry
county_pops <- function(state, acs) {
  dat <- get_acs(
    geography = "county",
    variables = c(
      total_pop = "B03003_001" # Total population
    ),
    state = state,
    year = acs,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
  )
  return(dat)
}
#%% Gather the data
pa_2017 <- county_pops("PA", 2017)
pa_2017 <- select(pa_2017, GEOID, pop_2017 = total_popE)
pa_2022 <- county_pops("PA", 2022)
pa_2022 <- select(pa_2022, GEOID, NAME, pop_2022 = total_popE, geometry)

mi_2017 <- county_pops("MI", 2017)
mi_2017 <- select(mi_2017, GEOID, pop_2017 = total_popE)
mi_2022 <- county_pops("MI", 2022)
mi_2022 <- select(mi_2022, GEOID, NAME, pop_2022 = total_popE, geometry)

wi_2017 <- county_pops("WI", 2017)
wi_2017 <- select(wi_2017, GEOID, pop_2017 = total_popE)
wi_2022 <- county_pops("WI", 2022)
wi_2022 <- select(wi_2022, GEOID, NAME, pop_2022 = total_popE, geometry)
#%% Join the data by state and calculate the growth rate of each
pa <- left_join(st_set_geometry(pa_2017, NULL), pa_2022, by = "GEOID") |>
  mutate(growth_rate = (pop_2022 - pop_2017) / pop_2017) |>
  st_as_sf()

mi <- left_join(st_set_geometry(mi_2017, NULL), mi_2022, by = "GEOID") |>
  mutate(growth_rate = (pop_2022 - pop_2017) / pop_2017) |>
  st_as_sf()

wi <- left_join(st_set_geometry(wi_2017, NULL), wi_2022, by = "GEOID") |>
  mutate(growth_rate = (pop_2022 - pop_2017) / pop_2017) |>
  st_as_sf()

# Create maps for each state individually, and then combine them all
#%% Pennslyvania first
pa_pop_growth <- ggplot(pa) +
  geom_sf(aes(fill = growth_rate)) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green4",
    midpoint = 0.0, labels = scales::percent
  ) +
  labs(
    fill = "Population Growth (2013 - 2017) - (2018 - 2022)"
  ) +
  theme_void()

mi_pop_growth <- ggplot(mi) +
  geom_sf(aes(fill = growth_rate)) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green4",
    midpoint = 0.0, labels = scales::percent
  ) +
  labs(
    fill = "Population Growth (2013 - 2017) - (2018 - 2022)"
  ) +
  theme_void()

wi_pop_growth <- ggplot(wi) +
  geom_sf(aes(fill = growth_rate)) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green4",
    midpoint = 0.0, labels = scales::percent
  ) +
  labs(
    fill = "Population Growth (2013 - 2017) - (2018 - 2022)"
  ) +
  theme_void()

#%% Save the individual maps
ggsave("plots/pa-growth.png", pa_pop_growth, width = 10, height = 10)
ggsave("plots/mi-growth.png", mi_pop_growth, width = 10, height = 10)
ggsave("plots/wi-growth.png", wi_pop_growth, width = 10, height = 10)

#%% Combine the data from pa, mi, and wi
blue_wall <- rbind(pa, mi, wi)
#%% Create a map of the blue wall states and their county-level population growth
bw_pop_growth <- ggplot(blue_wall) +
  geom_sf(aes(fill = growth_rate)) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green4",
    midpoint = 0.0, labels = scales::percent
  ) +
  labs(
    fill = "Population Growth (2013 - 2017) - (2018 - 2022)"
  ) +
  theme_void()
#%% Save the map
ggsave("plots/blue-wall-growth.png", bw_pop_growth, width = 10, height = 10)
