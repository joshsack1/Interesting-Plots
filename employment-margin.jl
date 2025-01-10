# Set up Envirnoment
using Pkg
Pkg.activate(".")
#%% # Set up R environment
using RCall
R"""
library(tidyverse)
library(tidycensus)
library(sf)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
"""
#%% Call the necessary julia packages
using DataFrames,
    FixedEffectModels,
    Plots,
    PlotThemes,
    FredData,
    PlotThemes,
    StatsPlots,
    Distributions,
    Turing,
    Random,
    LinearAlgebra,
    RegressionTables,
    CategoricalArrays
#%% Read in the election result data
R"""
michigan <- read_tsv("data/michigan-2016.tsv")
wisconsin <- read_csv("data/wisconsin-2016.csv")
pennsylvania <- read_csv("data/pennsylvania-2016.csv")
"""
michigan = rcopy(R"michigan")
wisconsin = rcopy(R"wisconsin")
pennsylvania = rcopy(R"pennsylvania")
states = [michigan, wisconsin, pennsylvania]
#%% Set up FRED
key = ENV["FRED_API_KEY"]
f = Fred(key)
#%% Convert the FIPS data to integers and then to strings
# NOTE: Figure out how to vectorize this later
for state in states
    state.FIPS = convert.(Int, state.FIPS)
    state.FIPS = string.(state.FIPS)
end
#%%
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
#%%
national_emp = get_pre_covid_trump_percent_change(f, "PAYEMS") # PAYEMS is the national series
#%% Because of FRED's rate limiting, it will be necessary to break up each state's counties before getting all of the employment stuff together
mi_refs = employment_ref.(michigan.FIPS)
wi_refs = employment_ref.(wisconsin.FIPS)
pa_refs = employment_ref.(pennsylvania.FIPS)
#%%
mi_chg1 = get_pre_covid_trump_percent_change.(Ref(f), mi_refs[1:41])
#%%
sleep(120)
#%%
mi_chg2 = get_pre_covid_trump_percent_change.(Ref(f), mi_refs[42:end])
#%%
mi_chg = vcat(mi_chg1, mi_chg2)
michigan.employment_change = mi_chg
michigan.relative_change = mi_chg .- national_emp
#%%
wi_chg1 = get_pre_covid_trump_percent_change.(Ref(f), wi_refs[1:41])
#%%
sleep(120)
#%%
wi_chg2 = get_pre_covid_trump_percent_change.(Ref(f), wi_refs[42:end])
#%%
wi_chg = vcat(wi_chg1, wi_chg2)
wisconsin.employment_change = wi_chg
wisconsin.relative_change = wi_chg .- national_emp
#%%
pa_chg1 = get_pre_covid_trump_percent_change.(Ref(f), pa_refs[1:33])
#%%
sleep(120)
#%%
pa_chg2 = get_pre_covid_trump_percent_change.(Ref(f), pa_refs[34:end])
#%%
pa_chg = vcat(pa_chg1, pa_chg2)
pennsylvania.employment_change = pa_chg
pennsylvania.relative_change = pa_chg .- national_emp
#%% us R to get population and geographic data
@rput michigan wisconsin pennsylvania
R"""
mi_pop <- get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "MI",
    year = 2017,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
wi_pop <- get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "WI",
    year = 2017,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
pa_pop <- get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "PA",
    year = 2017,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
)
"""
#%% Use R to combine all of the interesting data to export before regression
R"""
michigan <- michigan |>
    left_join(mi_pop, by = c("FIPS" = "GEOID")) |>
    select(Trump, Clinton, Margin, County = Count, FIPS, employment_change, relative_change, pop_2017 = B01003_001E)

wisconsin <- wisconsin |>
    left_join(wi_pop, by = c("FIPS" = "GEOID")) |>
    select(Trump, Clinton, Margin, County, FIPS, employment_change, relative_change, pop_2017 = B01003_001E)

pennsylvania <- pennsylvania |>
left_join(pa_pop, by = c("FIPS" = "GEOID")) |>
select(Trump, Clinton, Margin, County, FIPS, employment_change, relative_change, pop_2017 = B01003_001E)
"""
#%%
michigan = rcopy(R"michigan")
wisconsin = rcopy(R"wisconsin")
pennsylvania = rcopy(R"pennsylvania")
#%% Add state lables to the data
michigan.state = fill("MI", nrow(michigan))
wisconsin.state = fill("WI", nrow(wisconsin))
pennsylvania.state = fill("PA", nrow(pennsylvania))
#%% Combine the data
df = vcat(michigan, wisconsin, pennsylvania)

#%% Run the frequentest regression
corr_model = reg(df, @formula(relative_change ~ Margin), Vcov.robust())
#%%
pop_control = reg(df, @formula(relative_change ~ Margin + pop_2017), Vcov.robust())
#%%
fe_model_corr = reg(df, @formula(relative_change ~ Margin + fe(state)), Vcov.robust())
#%%
fe_model_pop = reg(
    df, @formula(relative_change ~ Margin + pop_2017 + fe(state)), Vcov.robust()
)
#%% Export a regression table
regtable(
    corr_model,
    pop_control,
    fe_model_corr,
    fe_model_pop;
    digits=4,
    regression_statistics=[R2, AdjR2],
    render=LatexTable(),
    file="frequantists.tex",
)
#%% Make some distribution plots for the coefficients
mi_corr = reg(michigan, @formula(relative_change ~ Margin), Vcov.robust())
wi_corr = reg(wisconsin, @formula(relative_change ~ Margin), Vcov.robust())
pa_corr = reg(pennsylvania, @formula(relative_change ~ Margin), Vcov.robust())
#%% State by state controlling for population
mi_pop = reg(michigan, @formula(relative_change ~ Margin + pop_2017), Vcov.robust())
wi_pop = reg(wisconsin, @formula(relative_change ~ Margin + pop_2017), Vcov.robust())
pa_pop = reg(pennsylvania, @formula(relative_change ~ Margin + pop_2017), Vcov.robust())
#%%
theme(:mute)
freq_coefs = plot(Normal(coef(corr_model)[2], stderror(corr_model)[2]), fill=(0, 0.3, :grey), color = :black, label="Blue Wall", title = "Coefficent")
plot!(Normal(coef(mi_corr)[2], stderror(mi_corr)[2]), fill=(0, 0.3, :gold), color = :gold, label="Michigan")
plot!(Normal(coef(wi_corr)[2], stderror(wi_corr)[2]), fill=(0, 0.3, :red), color = :red, label="Wisconsin")
plot!(Normal(coef(pa_corr)[2], stderror(pa_corr)[2]), fill=(0, 0.3, :navy), color = :navy, label="Pennsylvania")
plot!(Normal(coef(fe_model_corr)[1], stderror(fe_model_corr)[1]), fill=(0, 0.3, :green), color = :green, label="State FE")
vline!([0], color = :black, label = "Zero", linestyle = :dash)
#%% Save the figure
savefig(freq_coefs, "plots/frequentist-coefs.png")
#%% Do the same for when population is controlled for
pop_coefs = plot(Normal(coef(pop_control)[2], stderror(pop_control)[2]), fill=(0, 0.3, :grey), color = :black, label="Blue Wall", title = "Coefficent controlling for County Pop")
plot!(Normal(coef(mi_pop)[2], stderror(mi_pop)[2]), fill=(0, 0.3, :gold), color = :gold, label="Michigan")
plot!(Normal(coef(wi_pop)[2], stderror(wi_pop)[2]), fill=(0, 0.3, :red), color = :red, label="Wisconsin")
plot!(Normal(coef(pa_pop)[2], stderror(pa_pop)[2]), fill=(0, 0.3, :navy), color = :navy, label="Pennsylvania")
plot!(Normal(coef(fe_model_pop)[1], stderror(fe_model_pop)[1]), fill=(0, 0.3, :green), color = :green, label="State FE")
vline!([0], color = :black, label = "Zero", linestyle = :dash)
#%% Save the figure
savefig(pop_coefs, "plots/population-coefs.png")
#%% Bayesian versions of the models
Random.seed!(42)
# ε is the wrong 
@model function single_corr(y, X, β, ε)
    # Set the coefficient priors
    α̂ ~ Uniform(β[1] - 3 * ε[1], β[1] + 3 * ε[1])
    β̂ ~ Uniform(β[2] - 3 * ε[2], β[2] + 3 * ε[2])
    # Set up a prior for the error
    σ² ~ InverseGamma()
    # Set up the liklihood
    pred = α̂ .+ X * β̂
    y ~ MvNormal(pred, σ² * I)
end
#%%
corr_bayesian = single_corr(df.relative_change, df.Margin, coef(corr_model), stderror(corr_model))
b_corr_2 = sample(corr_bayesian, NUTS(), 2_000_000)
#%%
mi_corr_bayesian = single_corr(michigan.relative_change, michigan.Margin, coef(mi_corr), stderror(mi_corr))
b_mi_corr = sample(mi_corr_bayesian, NUTS(), 2_000_000)
#%%
wi_corr_bayesian = single_corr(wisconsin.relative_change, wisconsin.Margin, coef(wi_corr), stderror(wi_corr))
b_wi_corr = sample(wi_corr_bayesian, NUTS(), 2_000_000)
#%%
pa_corr_bayesian = single_corr(pennsylvania.relative_change, pennsylvania.Margin, coef(pa_corr), stderror(pa_corr))
b_pa_corr = sample(pa_corr_bayesian, NUTS(), 2_000_000)
#%%
assess(state, target) = state == target ? 1 : 0
#%%
df.wisc = assess.(df.state, "WI")
df.penn = assess.(df.state, "PA")
#%%
effective_fe_corr = reg(df, @formula(relative_change ~ Margin + wisc + penn), Vcov.robust())
#%%
@model function fe_proxy_single(y, X, β, s)
    α̂ ~ Uniform(β[1] - 3 * s[1], β[1] + 3 * s[1])
    β̂ ~ Uniform(β[2] - 3 * s[2], β[2] + 3 * s[2])
    wi ~ Uniform(β[3] - 3 * s[3], β[3] + 3 * s[3])
    pa ~ Uniform(β[4] - 3 * s[4], β[4] + 3 * s[4])
    σ² ~ InverseGamma()
    pred = α̂ .+ X[:, 1] * β̂ .+ X[:, 2] * wi .+ X[:, 3] * pa
    y ~ MvNormal(pred, σ² * I)
end
#%%
X_corr = Matrix(df[!, [:Margin, :wisc, :penn]])
efe_corr_bayesian = fe_proxy_single(df.relative_change, X_corr, coef(effective_fe_corr), stderror(effective_fe_corr))
efe_corr = sample(efe_corr_bayesian, NUTS(), 2_000_000)
#%%
b_corr_coefs = density(b_corr_2[:β̂].data, color = :black, fill = (0, 0.3, :grey), label = "Blue Wall", title = "Bayesian Coefficients")
density!(b_mi_corr[:β̂].data, color = :gold, fill = (0, 0.3, :gold), label = "Michigan")
density!(b_wi_corr[:β̂].data, color = :red, fill = (0, 0.3, :red), label = "Wisconsin")
density!(b_pa_corr[:β̂].data, color = :navy, fill = (0, 0.3, :navy), label = "Pennsylvania")
density!(efe_corr[:β̂].data, color = :green, fill = (0, 0.3, :green), label = "State FE")
vline!([0], color = :black, label = "Zero", linestyle = :dash)
#%%
savefig(b_corr_coefs, "plots/bayesian-corr-coefs.png")
#%% Now do the same for a population controlled model
@model function single_pop_corr(y, X, β, s)
    α̂ ~ Uniform(β[1] - 3 * s[1], β[1] + 3 * s[1])
    β̂₁ ~ Uniform(β[2] - 3 * s[2], β[2] + 3 * s[2])
    β̂₂ ~ Uniform(β[3] - 3 * s[3], β[3] + 3 * s[3])
    σ² ~ InverseGamma()
    pred = α̂ .+ X[:, 1] * β̂₁ .+ X[:, 2] * β̂₂
    y ~ MvNormal(pred, σ² * I)
end
#%%
X_pop = Matrix(df[!, [:Margin, :pop_2017]])
#%%
pop_corr_bayesian = single_pop_corr(df.relative_change, X_pop, coef(pop_control), stderror(pop_control))
b_pop_corr = sample(pop_corr_bayesian, NUTS(), 2_000_000)
#%%
X_mi_pop = Matrix(michigan[!, [:Margin, :pop_2017]])
#%%
pop_corr_mi = single_pop_corr(michigan.relative_change, X_mi_pop, coef(mi_pop), stderror(mi_pop))
b_pop_corr_mi = sample(pop_corr_mi, NUTS(), 2_000_000)
#%%
X_wi_pop = Matrix(wisconsin[!, [:Margin, :pop_2017]])
#%%
pop_corr_wi = single_pop_corr(wisconsin.relative_change, X_wi_pop, coef(wi_pop), stderror(wi_pop))
b_pop_corr_wi = sample(pop_corr_wi, NUTS(), 2_000_000)
#%%
X_pa_pop = Matrix(pennsylvania[!, [:Margin, :pop_2017]])
#%%
pop_corr_pa = single_pop_corr(pennsylvania.relative_change, X_pa_pop, coef(pa_pop), stderror(pa_pop))
b_pop_corr_pa = sample(pop_corr_pa, NUTS(), 2_000_000)
#%% Non-positive semidefine matrix meant that GLS wasn't really an option
effective_fe_pop = reg(df, @formula(relative_change ~ Margin + pop_2017 + wisc + penn))
#%%
@model function fe_proxy_pop(y, X, β, s)
    α̂ ~ Uniform(β[1] - 3 * s[1], β[1] + 3 * s[1])
    β̂₁ ~ Uniform(β[2] - 3 * s[2], β[2] + 3 * s[2])
    β̂₂ ~ Uniform(β[3] - 3 * s[3], β[3] + 3 * s[3])
    wi ~ Uniform(β[4] - 3 * s[4], β[4] + 3 * s[4])
    pa ~ Uniform(β[5] - 3 * s[5], β[5] + 3 * s[5])
    σ² ~ InverseGamma()
    pred = α̂ .+ X[:, 1] * β̂₁ .+ X[:, 2] * β̂₂ .+ X[:, 3] * wi .+ X[:, 4] * pa
    y ~ MvNormal(pred, σ² * I)
end
#%%
X_pop_corr = Matrix(df[!, [:Margin, :pop_2017, :wisc, :penn]])
#%%
efe_pop_corr_bayesian = fe_proxy_pop(df.relative_change, X_pop_corr, coef(effective_fe_pop), stderror(effective_fe_pop))
b_efe_pop_corr = sample(efe_pop_corr_bayesian, NUTS(), 2_000_000)
#%% Create a figure of the distributions
b_pop_coefs = density(b_pop_corr[:β̂₁].data, color = :black, fill = (0, 0.3, :grey), label = "Blue Wall", title = "Bayesian Coefficients controlling for County Pop")
density!(b_pop_corr_mi[:β̂₁].data, color = :gold, fill = (0, 0.3, :gold), label = "Michigan")
density!(b_pop_corr_wi[:β̂₁].data, color = :red, fill = (0, 0.3, :red), label = "Wisconsin")
density!(b_pop_corr_pa[:β̂₁].data, color = :navy, fill = (0, 0.3, :navy), label = "Pennsylvania")
density!(b_efe_pop_corr[:β̂₁].data, color = :green, fill = (0, 0.3, :green), label = "State FE")
vline!([0], color = :black, label = "Zero", linestyle = :dash)
#%%
savefig(b_pop_coefs, "plots/bayesian-pop-coefs.png")
