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
#%% Create a function that will get the age adjusted premature death rate for a given county
function create_ref(fips::String)
    return "CDC20N2UAA0" * fips
end
# This data is only annual, so it will just be 2017 to 2019 (at first)
function trump_pre_covid(f, series::String)
    pull = get_data(f, series; observation_start="2017-01-01", observation_end="2019-01-01")
    data = pull.data
    return (data.value[end] - data.value[1]) / data.value[1]
end
#%%
mi_refs = create_ref.(mi_fips)
#%%
#=pre_covid = trump_pre_covid.(Ref(f), mi_refs)=#
#%%
pre_covid_1 = [trump_pre_covid(f, mi_refs[i]) for i in 1:9]
# There are currently issues with Keweenaw County, as the data only exists up through 2008
# Therefore, it will have to be excluded
