#' Pre-processing for seed weight dataset
#'
#' Seeds were harvested at the end of growing period, dried and weighed (g).
#' A number of A. fatua seed had already dispersed, however we also counted
#' the number of seeds and number of seed husks.
#'

seedlings <- read_csv("data-raw/seedling_list.csv")

seeds <- read_csv("data-raw/seed/seed_weight_count.csv")

# Test samples/seedling IDs match
nrow(anti_join(seeds, seedlings)) == 0

seed_weights <- left_join(seedlings, seeds)


ggplot(seed_weights, aes(y = seed_weight_g, x = block)) +
    geom_boxplot() +
    facet_grid(~species_code)

ggplot(seed_weights, aes(seed_number, seed_weight_g)) +
  geom_point()

ggplot(seed_weights, aes(husk_number, seed_weight_g)) +
  geom_point()

