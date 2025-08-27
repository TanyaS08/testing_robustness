using CSV
using DataFrames
using SpeciesInteractionNetworks
using StatsBase

include("lib/robustness.jl")

# set seed
import Random
Random.seed!(66)

# Create a graph from edgelist
edgelist = DataFrame(CSV.File("data/edgelist.csv"))
extinction_order = DataFrame(CSV.File("data/extinction_order.csv"))

# build network
S = unique(vcat(edgelist.resource, edgelist.consumer))
nodes = Unipartite(Symbol.(S))
edgs = Binary(zeros(Bool, (length(S), length(S))))

N = SpeciesInteractionNetwork(nodes, edgs)

for i = 1:nrow(edgelist)
    interaction = (Symbol(edgelist.consumer[i]), Symbol(edgelist.resource[i]))
    N[interaction...] = true
end

# empty dataframe for storing results
robustness_vals = DataFrame(
    threshold = Any[],
    robustness = Any[],
    rep = Any[],
);

# represents the % of species that have gone extinct (primary and secondary)
spread = collect(1:5:99)

for j in eachindex(spread)
    
    for i in 1:ncol(extinction_order)
        
        # get extinction order
        spp = Symbol.(extinction_order[!, i])
        
        rob = robustness_gradient(N, spp;
                    threshold = spread[j])
    
        D = DataFrame(
            threshold = spread[j],
            robustness = rob,
            rep = i,
            )
    
            # send to results
            append!(robustness_vals, D)
    end
end

# write the results
CSV.write(
    "data/robustness_julia.csv",
    robustness_vals,
)
