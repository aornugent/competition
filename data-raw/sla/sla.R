#' Pre-processing for SLA dataset
#'
#' Leaf samples were collected from select individuals in a subset of treatments.
#' Area of whole leaves (cm^2), flattened and photographed immediately after
#' sampling, was measured using ImageJ, then dried and weighed (g). Sample
#' SLA (cm2.g) is calculated as total area of all leaves, divided by total weight
#' of all leaves (cm2.g). Mean SLA and LDMC is calculated as sample SLA and total
#' weight divided by number of leaves, respectively.

area <- read_csv("data-raw/sla/leaf_area.csv") %>%
  group_by(sample_id, seedling_id) %>%
  summarise(total_area = sum(leaf_area_cm2))

weight <- read_csv("data-raw/sla/leaf_weights.csv")

samples <- full_join(area, weight, by = c("seedling_id", "sample_id")) %>%
  mutate(total_SLA = total_area / total_leaf_weight_g)


write_csv(samples, "data-raw/sla/sla.csv")

# Check for possible mistakes
missing <- filter(samples, is.na(leaf_area_cm2) | is.na(total_leaf_weight_g))

unique(missing$sample_id)

# Average in sample
total_SLA <- group_by(samples, sample_id) %>%
  summarise(total_SLA = sum(leaf_area_cm2) / max(total_leaf_weight_g)) %>%
  left_join(seedlings)

ggplot(total_SLA, aes(x = block, y = total_SLA)) +
  geom_boxplot() +
  facet_grid(~species_code)
