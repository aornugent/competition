# Check full dataset for obvious mistakes

seedlings <- read_csv("data-raw/seedling_list.csv")

biomass <- read_csv("data-raw/biomass/biomass_weights.csv")
pretransplant <- read_csv("data-raw/biomass/pretransplant_weights.csv")

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
  ungroup() %>%
  select(-native_dens, -exotic_dens, -total_dens,
         -weight_notes, -seed_notes, -leaf_weight_notes)


# Impute missing ABAR seed weight
abar <- filter(samples, species_code == "ABAR") %>%
  select(seedling_id, sample_id, biomass_weight_g,
         seed_weight_g, seed_number, husk_number,
         fertility, total_density, block, pot) %>%
  mutate(single_seed = seed_weight_g / seed_number)

# Choose 1:1 husk-seed ratio
ggplot(abar, aes(x = seed_number, y = husk_number)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_abline(aes(intercept = 0, slope = 2), color = "red") +
  labs(title= "Husk to seed ratio in ABAR",
       subtitle = "black = 1:1, red = 2:1")

# Regress avg. seed weight against biomass
lm(single_seed ~ biomass_weight_g, data = abar)

# Impute using individaul avg. weight and biomass regression methods
imp <-  group_by(abar, pot) %>%
  mutate(single_seed_pot =
           ifelse(is.na(seed_weight_g),
                  mean(single_seed, na.rm = T),
                  single_seed)) %>%
  ungroup() %>%
  mutate(est_weight_pot = husk_number * single_seed_pot,
         est_weight_reg = husk_number * (0.0078 + 0.0065 * biomass_weight_g))

# Calculate RMSE
mutate(imp, pot_err = est_weight_pot - seed_weight_g,
            reg_err = est_weight_reg - seed_weight_g) %>%
  summarise(pot_rmse = sqrt(mean(pot_err ^ 2, na.rm = T)),
            reg_rmse = sqrt(mean(reg_err ^ 2, na.rm = T)))

# Plot methods
gather(imp, method, value, est_weight_pot:est_weight_reg) %>%
  ggplot(., aes(seed_weight_g, value)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  facet_grid(~method) +
  theme(aspect.ratio = 1) +
  labs(x = "Observed seed weight (g)",
       y = "Imputed total seed weight (g)",
       title = "Comparison of imputation methods against observed data",
       subtitle = "Black line describes 1:1 correspondence")


# Compare against other species
corrected_samples <-
  select(imp, seedling_id, est_weight_pot, est_weight_reg) %>%
  left_join(samples, .) %>%
  mutate(est_weight_pot = ifelse(is.na(est_weight_pot), seed_weight_g, est_weight_pot),
         est_weight_reg = ifelse(is.na(est_weight_reg), seed_weight_g, est_weight_reg))


gather(corrected_samples,
       method, val,
       est_weight_pot:est_weight_reg) %>%
  filter(!is.na(val)) %>%
    ggplot(., aes(x = biomass_weight_g, y = val)) +
  geom_point(aes(color = species_code)) +
  geom_abline(aes(intercept=0, slope = 1)) +
  facet_grid(method ~ fertility) +
  theme(aspect.ratio = 1) +
  labs(x = "Vegetative biomass (g)",
       y = "Total seed weight (g)",
       title = "Biomass, seed output comparison of four species",
       subtitle = "Total ABAR biomass imputed using two different methods, black line = 1:1 correspondence")

# Use regression method for corrected seed weight
samples <- group_by(corrected_samples, seedling_id) %>%
  mutate(aboveground_biomass_g =
           sum(biomass_weight_g,
           est_weight_reg,
           total_leaf_weight_g, na.rm = T)) %>%
  ungroup() %>%
  select(seedling_id,
         block,
         pot,
         fertility,
         mixture = competition,
         pot_density = total_density,
         position,
         species_code,
         species_id,
         date_planted,
         age_days,
         native,
         exotic,
         aboveground_biomass_g,
         vegetative_biomass_g = biomass_weight_g,
         corrected_seed_weight_g = est_weight_reg,
         total_SLA)

usethis::use_data(samples, overwrite = T)



# Summarise pre-transplant seedling biomass (n ~ 40/sp)
biomass_t0 <- mutate(pretransplant,
                     species_code = fct_recode(species_code, BHOR = "ABIG"),
                     species_code = fct_relevel(species_code, "ABAR", "BDIA", "ECUR")) %>%
  select(-n)

usethis::use_data(biomass_t0, overwrite = T)
