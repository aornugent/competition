# Check full dataset for obvious mistakes

seedlings <- read_csv("data-raw/seedling_list.csv")

biomass <- read_csv("data-raw/biomass/biomass_weights.csv")

seed <- read_csv("data-raw/seed/seed_weight_count.csv")

leaf <- read_csv("data-raw/sla/sla.csv")

pots <- read_csv("data-raw/study_design.csv")



# Relabel A. bigeniculata as B. hordaeceus, relevel factors
samples <- full_join(seedlings, biomass) %>%
  full_join(seed) %>%
  full_join(leaf) %>%
  left_join(pots) %>%
  filter(!grepl("died|not planted", planting_notes)) %>%
  mutate(fertility = fct_relevel(fertility, "LOW", "MED", "HIGH"),
         species_code = fct_recode(species_code, BHOR = "ABIG"),
         species_code = fct_relevel(species_code, "ABAR", "BDIA", "ECUR")) %>%
  group_by(pot) %>%
  mutate(total_density = n()) %>%
  select(-native_dens, -exotic_dens, -total_dens,
         -weight_notes, -seed_notes, -leaf_weight_notes)

usethis::use_data(samples)

# Pot weight
total_weight <- group_by(samples, block, pot, fertility, total_density) %>%
  summarise(total_weight = sum(biomass_weight_g, na.rm = T) + sum(seed_weight_g, na.rm = T))

pot_weight <- glmer(total_weight ~ 1 + (1|block) + (1|fertility) + (1|total_density),
                      data = total_weight)

summary(pot_weight)
data.frame(summary(pot_weight)$varcor) %>%
  mutate(proportion_variation= vcov / sum(vcov))


# Individual biomass variation
indiv_weight <- glmer(biomass_weight_g ~ 1 + (1|species_code) + (1|block) + (1|fertility) + (1|total_density),
            data = samples)

summary(indiv_weight)
data.frame(summary(indiv_weight)$varcor) %>%
  mutate(proportion_variation= vcov / sum(vcov))


# Biomass by fertility
ggplot(samples, aes(x = species_code, y = biomass_weight_g)) +
  geom_boxplot() +
  facet_grid(~ fertility) +
  theme_bw() +
  labs(title = "Vegetative biomass")


# Seed by biomass
ggplot(samples, aes(x = biomass_weight_g, seed_weight_g, color = species_code)) +
  geom_point(size = 2, alpha = .5) +
  geom_abline(aes(intercept = 0, slope = 1), size = 1, alpha = 0.2) +
  facet_wrap(~ fertility) +
  theme_bw() +
  labs(title = "SLA (cm2.g) per vegetative biomass") +
  theme(aspect.ratio = 1)

# SLA by biomass
ggplot(samples, aes(x = biomass_weight_g, total_SLA, color = species_code)) +
  geom_point(size = 1.5, alpha = .5) +
  geom_abline(aes(intercept = 0, slope = 1), size = 1, alpha = 0.2) +
  facet_wrap(~ species_code) +
  theme_bw() +
  guides(color = F)+
  labs(title = "SLA (cm2.g) per vegetative biomass") +
  theme(aspect.ratio = 1)
