library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(leaflet)
library(fredr)
#%% Connect to the Census API
census_api_key(Sys.getenv("CENSUS_API_KEY"))
fredr_set_key(Sys.getenv("FRED_API_KEY"))
#%% Retrieve TIGER shapefiles for ZCTAs in Atlanta
zip_code_files <- zctas(cb = TRUE, year = 2020) # 2020 is the latest year for which there are these files
#%% Filter these zip codes by the boundaries of the counties of Metro Atlanta
metro_atlanta_boundaries <- counties(state = "GA", cb = TRUE, year = 2020) |>
  filter(NAME %in% c("Fulton", "DeKalb", "Cobb", "Gwinnett", "Clayton"))
#%% Transform and intersect the boundaries with the zip code data
metro_atlanta_boundaries <- st_transform(metro_atlanta_boundaries, st_crs(zip_code_files))
metro_atlanta_zips <- st_intersection(zip_code_files, metro_atlanta_boundaries)
#%% Read in the single family home price data
sfr_prices <- read_csv("data/zip-sf-price.csv")
#%% rename the RegionName to ZCTA5CE10
sfr_prices <- sfr_prices |>
  rename(ZCTA5CE20 = RegionName) |>
  select(ZCTA5CE20, State, City, Metro, CountyName, "2024-10-31", "2024-12-31")
#%% Join the price data with the zip code data
# The result should be a data frame with the geometry of the zip codes and the price data
atlanta_price_data <- left_join(metro_atlanta_zips, sfr_prices, by = "ZCTA5CE20")
#%%
monthly_payment <- function(home_price, mortgage_rate) {
  principal <- home_price * 0.8
  monthly_rate <- mortgage_rate / 12
  denominator <- (1 + monthly_rate)^360 - 1
  numberator <- monthly_rate * (1 + monthly_rate)^360
  payment <- principal * (numberator / denominator)
  return(payment)
}
#%% Get mortgage rates from the FRED API
mortgage_rates <- fredr("MORTGAGE30US") |>
  filter(date > "2024-10-24") |>
  select(date, value) |>
  rename(mortgage_rate = value)
#%% Get values for the mortgage rate the closest to the end of the months of October 2024 and December 2024
october_end_rate <- mortgage_rates |>
  filter(date == "2024-10-31") |>
  pull(mortgage_rate)
october_end_rate <- october_end_rate / 100
# December did not end on a thursday, and the closest the data comes is January 2nd
december_end_rate <- mortgage_rates |>
  filter(date == "2025-01-02") |>
  pull(mortgage_rate)
december_end_rate <- december_end_rate / 100
#%% For each zip code, calculate the monthly payment for a home at the end of October and the end of December
atlanta_price_data <- atlanta_price_data |>
  mutate(
    october_payment = monthly_payment(`2024-10-31`, october_end_rate),
    december_payment = monthly_payment(`2024-12-31`, december_end_rate),
    payment_change = 12 * (december_payment - october_payment),
    percent_increase = 100 * (december_payment - october_payment) / october_payment,
    annual_percent_increase = 100 * ((1 + (percent_increase / 100))^6 - 1)
  )
#%% Create an interactive map
pal <- colorNumeric(
  palette = "inferno",
  domain = atlanta_price_data$payment_change,
  na.color = "grey"
)

example_map <- leaflet(atlanta_price_data) |>
  addTiles() |>
  addPolygons(
    fillColor = ~ pal(payment_change),
    weight = 1,
    opacity = 1,
    color = "black",
    fillOpacity = 0.7,
    popup = ~ paste0(
      "<strong>ZIP:</strong> ", ZCTA5CE20,
      "<br><strong>Payment Change:</strong> ",
      ifelse(is.na(payment_change), "No Data", paste0("$", round(payment_change, 2))),
      "<br><strong>Mortgage Inflation Rate:</strong> ", paste0(round(annual_percent_increase, 2), "%")
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~payment_change,
    title = "Change in Monthly Payments since Trump's Election",
    position = "bottomright",
    na.label = "No Data"
  )
#%% Save the map for sharing
saveWidget(example_map, file = "plots/atlanta_trump_home_prices.html", selfcontained = TRUE)
