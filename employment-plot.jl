include("michigan-votes.jl")
using FredData, DataFrames, FixedEffectModels, StatsPlots, PlotThemes
#%% Set up Fred
key = ENV["FRED_API_KEY"]
f = Fred(key)
#%%
R"""
mi_geo <- get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "MI",
    year = 2022,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
"""
#%%
mi_fips = rcopy(R"mi_geo").GEOID
#%% Use the fips data to get employment change
# Create a function that will use the FIPS string data to get the employment data
function employment_ref(fips::String)
    return "LAUCN" * fips * "0000000005"
end
#%% create a function to pull fred data from january 2017 to february 2020
function get_pre_covid_trump_percent_change(f, series::String)
    pull = get_data(f, series; observation_start="2017-01-01", observation_end="2020-02-01")
    data = pull.data
    return (data.value[end] - data.value[1]) / data.value[1]
end
#%% Calculate the percentage change for all counties
mi_refs = employment_ref.(mi_fips)
#%%
mi_percent_change = get_pre_covid_trump_percent_change.(Ref(f), mi_refs)
#%% Get the national employment percent change
national_emp = get_pre_covid_trump_percent_change(f, "PAYEMS") # PAYEMS is the national series
#%% Create a full data frame to return to R to create the map
emp_df = DataFrame(;
    GEOID=mi_fips,
    employment_change=mi_percent_change,
    relative_change=mi_percent_change .- national_emp,
)
#%% Pull it back into R to create a map of the change in employment
@rput emp_df
@rput national_emp
R"""
mi_data <- left_join(emp_df, mi_geo, by = "GEOID")
"""
#%%
R"""
st_as_sf(mi_data)
raw_employment_plot <- ggplot(mi_data) +
    geom_sf(aes(fill = employment_change)) +
    scale_fill_gradient2(
        low = "red", mid = "yellow", high = "green4",
        midpoint = 0.0, labels = scales::percent
    ) +
    labs(
        fill = "Percent Change, Employment",
    ) +
    theme_void()
"""
#%% Save the data
R"""
ggsave("plots/raw-employment-plot.png", raw_employment_plot, width = 10, height = 10)
"""
#%% Create a plot of the relative change in employment over the same period
R"""
relative_employment_plot <- ggplot(mi_data) +
    geom_sf(aes(fill = relative_change)) +
    scale_fill_gradient2(
        low = "red", mid = "yellow", high = "green4",
        midpoint = 0.0, labels = scales::percent
    ) +
    labs(
        fill = "Relative Change, Employment",
    ) +
    theme_void()
"""
#%% Save the plot
R"""
ggsave("plots/relative-employment-plot.png", relative_employment_plot, width = 10, height = 10)
"""
