"""
    Calculates the robustness for a series of networks. That is the proportion of primary extinction that result in
    the user specified % of species going extinct. This threshold is specified by `threshold`
"""
function robustness_gradient(Ns::Vector{T}; threshold::Int = 50) where {T<:SpeciesInteractionNetwork}

    # get initial richness
    init_rich = richness(Ns[1])

    # threshold richness
    # threshold = X% of original size of network
    thresh_rich = floor(Int, init_rich * threshold / 100)

    # get first index in network series that is equal to or less than threshold richness
    # this is effectively the number of primary extinctions since each element = 1 primary extinction
    net_in = findfirst(x -> x <= thresh_rich, richness.(Ns))

    # we subtract 1 since 1st network in series is the OG network (no extinctions)
    primary = net_in - 1

    # return prop of primary extinctions as total number of initial species
    return primary/init_rich
end
