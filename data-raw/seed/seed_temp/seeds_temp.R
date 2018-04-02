#' Pre-processing for seed weight dataset
#'
#' Seeds were harvested at the end of growing period, dried and weighed (g).
#' A number of A. fatua seed had already dispersed, however we also counted
#' the number of seeds and number of seed husks.
#'

seedlings <- read_csv("data-raw/seedling_list.csv")
seeds <- read_csv("data-raw/seed/seed_temp/seed_weights.csv") %>%
  select(seedling_id, sample_id, seed_weight_g, seed_number, seed_notes)

# Test samples/seedling IDs match
nrow(anti_join(seeds, seedlings)) == 0

husks_AB <- read_csv("data-raw/seed/seed_temp/seed_husks_AB.csv")

b <- full_join(seeds, husks_AB, by = c("seedling_id", "sample_id")) %>%
  mutate(seed_number = ifelse(is.na(seed_number.x) & !is.na(seed_number.y),
                               seed_number.y, seed_number.x)) %>%
  select(-seed_number.x, -seed_number.y)


counts_husks <- read_csv("data-raw/seed/seed_temp/seed_weight_husks_counts.csv")

a <- full_join(b, counts_husks, by = c("seedling_id", "sample_id")) %>%
  mutate(seed_weight_g = ifelse(is.na(seed_weight_g) & !is.na(seed_weight2),
                              seed_weight2, seed_weight_g),
         seed_number = ifelse(is.na(seed_number) & !is.na(seed_number2),
                              seed_number2, seed_number),
         husk_number = ifelse(is.na(husk_number) & !is.na(husk_number2),
                         husk_number2, husk_number)) %>%
  select(-seed_weight2, -seed_number2, -husk_number2)


counts_2017 <- read_csv("data-raw/seed/seed_temp/seed_weights_2017.csv")

d <- full_join(a, counts_2017) %>%
  mutate(seed_weight_g = ifelse(is.na(seed_weight_g) & !is.na(seed_weight2),
                                seed_weight2, seed_weight_g),
         seed_number = ifelse(is.na(seed_number) & !is.na(seed_number2),
                              seed_number2, seed_number),
         husk_number = ifelse(is.na(husk_number) & !is.na(husk_number2),
                              husk_number2, husk_number)) %>%
  select(-seed_weight2, -seed_number2, -husk_number2) %>%
  filter(!is.na(seed_weight_g) | !is.na(seed_number) | !is.na(husk_number))

write_csv(d, "data-raw/seed/seed_weight_count.csv")
