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


# Some seeds have been counted but not weighed.
filter(seed_weights, is.na(seed_weight_g) & !is.na(seed_number))

# Some seeds have been weighed but not counted
filter(seed_weights, !is.na(seed_weight_g) & is.na(seed_number), species_code == "ABAR")

# More still haven't had their husks counted
filter(seed_weights, !is.na(seed_weight_g),
       !is.na(seed_number),
       is.na(husk_number))


ggplot(seed_weights, aes(y = seed_weight_g, x = block)) +
    geom_boxplot() +
    facet_grid(~species_code)

ggplot(seed_weights, aes(seed_number, seed_weight_g)) +
  geom_point()

ggplot(seed_weights, aes(husk_number, seed_weight_g)) +
  geom_point()

ABAR <- filter(seed_weights, species_code == "ABAR") %>%
  group_by(pot) %>%
  mutate(mean_weight_per_seed = mean(seed_weight_g / seed_number, na.rm = T),
         est_weight = (ceiling(husk_number) /  2) * mean_weight_per_seed)

ggplot(ABAR, aes(y = est_weight, x = seed_weight_g, colour = block)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

group_by(ABAR, block) %>%
  summarise(mean = mean(est_weight - seed_weight_g, na.rm = T))
