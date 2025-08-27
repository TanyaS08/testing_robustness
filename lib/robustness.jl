"""
    Calculates the robustness for a series of networks. That is the proportion of primary extinction that result in
    the user specified % of species going extinct. This threshold is specified by `threshold`
"""
function robustness_gradient(
    N::SpeciesInteractionNetwork{<:Partiteness,<:Binary};
    extinction_order::Union{Nothing,Vector{Symbol}} = nothing, 
    threshold::Int = 50)

    initial_rich = richness(N)
    percent_loss = initial_rich*(threshold/100)

    # for recording the number of primary extinctions
    num_prim = 1

    global K = N

    if extinction_order == nothing
        extinction_order = StatsBase.shuffle(species(K))
    end

    # keep removing species until richness drops below threshold
    for (i, sp_primary) in enumerate(extinction_order)
            # check if sp in network
            if sp_primary ∈ SpeciesInteractionNetworks.species(K)
                
                # find predators of that sp
                preds = collect(predecessors(K, sp_primary))

                # find preys of the preds
                spp_remove = Symbol[]
                for j in eachindex(preds)
                    # if the predator only has one prey (i.e. spp to remove then we add to list)
                    if length(successors(N, preds[j])) == 1
                        push!(spp_remove, preds[j])
                    end
                end

                # add primary and secondary extinction list together
                push!(spp_remove, sp_primary)

                # remove those species from the network
                spp_keep = filter(sp -> sp ∉ spp_remove, SpeciesInteractionNetworks.species(K))
            
                # nth extinction
                K = subgraph(K, spp_keep)

                # update at global scale
                global K

                if richness(K) / initial_rich >= (threshold/100)
                    # keep record of the number of primary extinctions
                    num_prim += 1
                    continue
                else
                    break
                end
            end
        end

    # return prop of primary extinctions as total number of initial species
    return num_prim/initial_rich
end
