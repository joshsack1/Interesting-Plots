using Pkg
Pkg.activate(".")
#%%
using RCall 
R"""
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(tigris)
library(htmlwidgets)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
"""
#%%
using DataFrames, CSV, FixedEffectModels, Turing, Random, Plots, PlotThemes
#%%
raw = DataFrame(CSV.File("data/wi-medicaid.csv"))
@rput raw
#%%
R"""
county_pop <- get_acs(
    geography = "county",
    variables = "B01003_001E",
    state = "WI",
    year = 2022,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
"""
#%%
R"""
raw <- mutate(raw, GEOID = paste0(FIPS))
"""
#%%
R"""
full_ds <- left_join(raw, county_pop, by = "GEOID")
"""
#%%
R"""
clean_df <- full_ds |>
    select(GEOID, County, trumpMargin, pop_2022 = B01003_001E, Enrollment = Medicaid_12_24, geometry) |>
    mutate(enrollment = Enrollment/pop_2022)
"""
#%% Create an interactive map colored by the population medicaid enrollment rate what includes the 2024 margin data
R"""
clean_df <- st_as_sf(clean_df)
pal <- colorNumeric(
    palette = "inferno",
    domain = clean_df$enrollment,
    na.color = "grey"
)
medicaid_map <- leaflet(clean_df) |>
    addTiles() |>
    addPolygons(
        fillColor = ~ pal(enrollment),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        popup = ~ paste0(
            "<strong>County: </strong>", County, "<br>",
            "<strong> Medicaid Enrollment: </strong>", Enrollment, "<br>",
            "<strong> Trump Margin 2024: </strong>", trumpMargin, "<br>",
            "<strong> Enrollment / Population: </strong>", enrollment
        )
        )|>
        addLegend(
            pal = pal,
            values = ~enrollment,
            title = "Population on Medicaid",
            position = "topright"
        )
"""
#%%
R"""
saveWidget(medicaid_map, file = "plots/wisconsin-medicaid.html", selfcontained = TRUE)
"""
