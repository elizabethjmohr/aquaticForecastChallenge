using Turing
using Distributions
using StatsPlots
using CSV
using DataFrames
using MCMCChains
using ReverseDiff

@model function TempOx(times, waterTempData, airTempData, oxygenData, ::Type{T} = Float64) where {T}
    missingAirIndices = findall(ismissing, airTempData)
    
    # Priors
    s ~ MvNormal(3, 10.0)
    c ~ MvNormal(3, 10.0)
    sₐ ~ MvNormal(3, 10.0)
    cₐ ~ MvNormal(3, 10.0)
    μ ~ Normal(15.0, 10.0)
    μₐ ~ Normal(10.0, 10.0)
    β ~ MvNormal(2, 10.0)
    βₒ ~ MvNormal(2, 10.0)
    α ~ truncated(Normal(1,1), 0, Inf)
    σ ~ truncated(Cauchy(0,100), 0, Inf)
    σₒ ~ truncated(Cauchy(0,100), 0, Inf)

    # Seasonal model
    Sw_hat = μ .+ 
        s[1].*sin.(2*pi*times./365.25) + c[1].* cos.(2*pi*times./365.25) +
        s[2].*sin.(4*pi*times./365.25) + c[2].* cos.(4*pi*times./365.25) +
        s[3].*sin.(6*pi*times./365.25) + c[3].* cos.(6*pi*times./365.25)
    Sa_hat = μₐ .+ 
        sₐ[1].*sin.(2*pi*times./365.25) + cₐ[1].* cos.(2*pi*times./365.25) +
        sₐ[2].*sin.(4*pi*times./365.25) + cₐ[2].* cos.(4*pi*times./365.25) +
        sₐ[3].*sin.(6*pi*times./365.25) + cₐ[3].* cos.(6*pi*times./365.25)

    missingAirTemps ~ MvNormal(repeat([10.0], length(missingAirIndices)), 10.0)
    imputedAirTempData = Vector{T}(undef, length(airTempData))

    for i in eachindex(airTempData)
        if in(missingAirIndices)(i) 
            imputedAirTempData[i] = missingAirTemps[findall(x-> x == i, missingAirIndices)][1] 
        else 
            imputedAirTempData[i]= airTempData[i]
        end
    end

    # Water temperature vs. air temperature residuals
    Ra = Sa_hat - imputedAirTempData 
    Rw_hat = β[1].+ β[2].*Ra

    # Autocorrelated residuals
    rw_hat = Vector{T}(undef, length(waterTempData))
    rw_hat[1] = Sw_hat[1] - waterTempData[1] - Rw_hat[1]
    for i in 1:(length(waterTempData) - 1)
        if ismissing(waterTempData[i])
            rw_hat[i+1] = rw_hat[i]
        else
            rw_hat[i+1] = α * (Sw_hat[i] - waterTempData[i] - Rw_hat[i])
        end
    end

    # Temperature data model
    Tw_hat = Sw_hat + Rw_hat + rw_hat
    
    # Dissolved oxygen vs. water temperature 
    DO_hat = βₒ[1].+ βₒ[2].*Tw_hat

    for i in eachindex(waterTempData)
        waterTempData[i] ~ Normal(Tw_hat[i], σ)
        oxygenData[i] ~ Normal(DO_hat[i], σₒ)
    end
end

data = CSV.read("/Users/elizabethmohr/Documents/MSU/RProjects/aquaticForecastChallenge/data/POSE.csv", 
    DataFrame,
    types = [String, Float64, Float64, Float64, Float64, Float64, Int64],
    missingstring = "NA")
  
model = TempOx(data.time, Array(data.temperature), Array(data.airTemp), Array(data.oxygen))
setadbackend(:reversediff)
chains = sample(model, NUTS(), MCMCThreads(), 1000, 3)
chainsDF = DataFrame(chains)
@save "chains.jld2" chainsDF

# Get names of sections in chains
missingAirTempData = chainsDF[:,r"missingAirTemps"]
plot(Array(data.time), Array(data.airTemp))
plot!(Array(data.time)[findall(ismissing, Array(data.airTemp))], Array(missingAirTempData[400,:]))

means = mean(chains)
# Plot seasonal water temperature trend using mean of parameters
s = means[1:3,2]
c = means[4:6,2]
μ = means[13:14,2][1]
times = data.time
Sw_hat = μ .+ 
    s[1].*sin.(2*pi*times./365.25) + c[1].* cos.(2*pi*times./365.25) +
    s[2].*sin.(4*pi*times./365.25) + c[2].* cos.(4*pi*times./365.25) +
    s[3].*sin.(6*pi*times./365.25) + c[3].* cos.(6*pi*times./365.25)
plot(times, Sw_hat)

# Plot seasonal air temperature trend using mean of parameters
sₐ = means[7:9,2]
cₐ = means[10:12,2]
μₐ = means[13:14,2][2]
Sa_hat = μₐ .+ 
        sₐ[1].*sin.(2*pi*times./365.25) + cₐ[1].* cos.(2*pi*times./365.25) +
        sₐ[2].*sin.(4*pi*times./365.25) + cₐ[2].* cos.(4*pi*times./365.25) +
        sₐ[3].*sin.(6*pi*times./365.25) + cₐ[3].* cos.(6*pi*times./365.25)
plot(times, Sa_hat)

N = 1000 # number of parameter combinations
indices = rand(1:3000, N)
M = 31 # number of noaa air temperature forecast ensembles
h = 7
ensemble = Array{Float64}(undef, N, M, (h+1))
for i in 1:N
    for j in 1:M
        ensemble[i,j,1] = rand(Normal(last(Array(data.temperature)), params.σ[indices[i]]))
    end
end
for i in 1:N
    for j in 1:M
        for k in 2:(h+1)
            mu = airTempForecast[j,k]
            ensemble[i,j,k] 
        end
    end
end