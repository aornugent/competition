#' Pre-processing for biomass dataset
#'
#' Seedlings were grown for up to 82 days before removing seed, leaf samples
#' and harvesting for biomass. Biomass samples were dried and weighed. Seed
#' and leaf samples were weighed separately, then added together for total
#' above ground biomass.
#'

seedlings <- read_csv("data-raw/seedling_list.csv")
biomass <- read_csv("data-raw/biomass/biomass_weights.csv")

# Biomass dataframe has only entries for surviving seedlings.
nrow(biomass) == nrow(filter(seedlings, !grepl("died|not planted", planting_notes)))
