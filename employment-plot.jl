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
#=mi_percent_change = get_pre_covid_trump_percent_change.(Ref(f), mi_refs)=#
mi_percent_change1 = get_pre_covid_trump_percent_change.(Ref(f), mi_refs[1:41])
#%% Wait two minutes
sleep(120)
#%%
mi_percent_change2 = get_pre_covid_trump_percent_change.(Ref(f), mi_refs[42:end])
#%% Combine the data with a vertical concatenation
mi_percent_change = vcat(mi_percent_change1, mi_percent_change2)
#%% Get the national employment percent change
national_emp = get_pre_covid_trump_percent_change(f, "PAYEMS") # PAYEMS is the national series
#%% Create a full data frame to return to R to create the map
mi_emp_df = DataFrame(;
    GEOID=mi_fips,
    employment_change=mi_percent_change,
    relative_change=mi_percent_change .- national_emp,
)
#%% Pull it back into R to create a map of the change in employment
@rput mi_emp_df
@rput national_emp
R"""
mi_data <- left_join(mi_emp_df, mi_geo, by = "GEOID")
"""
#%%
R"""
mi_data <- st_as_sf(mi_data)
raw_employment_plot <- ggplot(mi_data) +
    geom_sf(aes(fill = employment_change)) +
    scale_fill_gradient2(
        low = "red", mid = "white", high = "green4",
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
        low = "red", mid = "white", high = "green4",
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
#%% Wisconsin Section
R"""
wi_geo <- get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "WI",
    year = 2022,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
"""
#%%
wi_fips = rcopy(R"wi_geo").GEOID
wi_refs = employment_ref.(wi_fips)
#%% due to rate limits, we start with the first 36 counties
wi_data1 = get_pre_covid_trump_percent_change.(Ref(f), wi_refs[1:36])
#%% then wait two minutes
sleep(120)
#%%
wi_data2 = get_pre_covid_trump_percent_change.(Ref(f), wi_refs[37:end])
#%% Combine the data with a vertical concatenation
wi_percent_change = vcat(wi_data1, wi_data2)
#%%
wi_emp_df = DataFrame(;
    GEOID=wi_fips,
    employment_change=wi_percent_change,
    relative_change=wi_percent_change .- national_emp,
)
#%%
@rput wi_emp_df
R"""
wi_data <- left_join(wi_emp_df, wi_geo, by = "GEOID")
"""
#%%
R"""
wi_data <- st_as_sf(wi_data)
raw_employment_plot <- ggplot(wi_data) +
    geom_sf(aes(fill = employment_change)) +
    scale_fill_gradient2(
        low = "red", mid = "white", high = "green4",
        midpoint = 0.0, labels = scales::percent
    ) +
    labs(
        fill = "Percent Change, Employment",
    ) +
    theme_void()
"""
#%% Save the data
R"""
ggsave("plots/raw-employment-plot-wi.png", raw_employment_plot, width = 10, height = 10)
"""
#%% Create a plot of the relative change in employment over the same period
R"""
relative_employment_plot <- ggplot(wi_data) +
    geom_sf(aes(fill = relative_change)) +
    scale_fill_gradient2(
        low = "red", mid = "white", high = "green4",
        midpoint = 0.0, labels = scales::percent
    ) +
    labs(
        fill = "Relative Change, Employment",
    ) +
    theme_void()
"""
#%% Save the plot
R"""
ggsave("plots/relative-employment-plot-wi.png", relative_employment_plot, width = 10, height = 10)
"""
#%% Pennsylvania Section
R"""
pa_geo <- get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "PA",
    year = 2022,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
"""
#%%
pa_fips = rcopy(R"pa_geo").GEOID
pa_refs = employment_ref.(pa_fips)
#%%
pa_data1 = get_pre_covid_trump_percent_change.(Ref(f), pa_refs[1:33])
#%%
sleep(120)
#%%
pa_data2 = get_pre_covid_trump_percent_change.(Ref(f), pa_refs[34:end])
#%%
pa_percent_change = vcat(pa_data1, pa_data2)
#%%
pa_emp_df = DataFrame(;
    GEOID=pa_fips,
    employment_change=pa_percent_change,
    relative_change=pa_percent_change .- national_emp,
)
#%%
@rput pa_emp_df
R"""
pa_data <- left_join(pa_emp_df, pa_geo, by = "GEOID")
"""
#%%
R"""
pa_data <- st_as_sf(pa_data)
raw_employment_plot <- ggplot(pa_data) +
    geom_sf(aes(fill = employment_change)) +
    scale_fill_gradient2(
        low = "red", mid = "white", high = "green4",
        midpoint = 0.0, labels = scales::percent
    ) +
    labs(
        fill = "Percent Change, Employment",
    ) +
    theme_void()
"""
#%% Save the map
R"""
ggsave("plots/raw-employment-plot-pa.png", raw_employment_plot, width = 10, height = 10)
"""
#%% Create a plot of the relative change in employment over the same period
R"""
relative_employment_plot <- ggplot(pa_data) +
    geom_sf(aes(fill = relative_change)) +
    scale_fill_gradient2(
        low = "red", mid = "white", high = "green4",
        midpoint = 0.0, labels = scales::percent
    ) +
    labs(
        fill = "Relative Change, Employment",
    ) +
    theme_void()
"""
#%% Save the map
R"""
ggsave("plots/relative-employment-plot-pa.png", relative_employment_plot, width = 10, height = 10)
"""
