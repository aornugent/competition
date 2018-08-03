#' Format data for analysis
#'
#' Takes the preloaded Pinnacle vegetation dataset and prepares it for
#' analysis in Stan. The Stan models are written such that the are able
#' to take largely the same data structure, with the exception of M0.
#'
#' @param n_sp  Treat residents as separate species (n=6), or single community (n=4).
#' @param ts    Number of timesteps for iteration method.
#'
#' @importFrom forcats fct_collapse
#' @importFrom tidyr complete
#' @export

format_data <- function(model,
                        n_sp = 6,
                        ts = 12,
                        ...) {

  # Load dataset
  data(samples)
  data(biomass_t0)

  # Group resident species
  if(n_sp == 4){
    samples <- mutate(samples,
             species_code = fct_collapse(species_code,
                                         NAT = c("BHOR", "RCAES", "PLAB")))

    biomass_t0 <- mutate(biomass_t0,
             species_code = fct_collapse(species_code,
                                         NAT = c("BHOR", "RCAES", "PLAB")))
  }

  species_code = levels(samples$species_code)
  fertility_code = levels(samples$fertility)

  # Convert factors to numeric
  dat <- select(samples,
           seedling_id, species_code, block, pot,
           aboveground_biomass_g, vegetative_biomass_g, fertility, total_density) %>%
    mutate_at(vars(seedling_id, block, pot,
                   species_code, fertility),
      funs(as.numeric(factor(.)))) %>%
    mutate(sp_by_fert = as.numeric(factor(paste(fertility, species_code))),
      block_by_sp = as.numeric(factor(paste(block, species_code)))) %>%
    arrange(seedling_id)

  # Calculate initial size distribution
  log_t0 <- group_by(biomass_t0, species_code) %>%
    summarise(aboveground_mean = mean(log(aboveground_sample_mean)),
              aboveground_sd = sd(log(aboveground_sample_mean)))

  # Create indices for interacting individuals.
  # This includes self-interactions (within an individual)
  species <- select(dat, seedling_id, species_code)

  interactions <-data.frame(pot = dat$pot,
                            ind_i = dat$seedling_id,
                            ind_j = dat$seedling_id,
                            fertility = dat$fertility) %>%
    group_by(pot, fertility) %>%
    complete(ind_i, ind_j) %>%
    #filter(ind_i != ind_j) %>%
    right_join(species,
               by = c("ind_i" = "seedling_id")) %>%
    right_join(species,
               by = c("ind_j" = "seedling_id"),
               suffix = c("_i", "_j")) %>%
    ungroup() %>%
    mutate(alpha_ij = paste(fertility, species_code_i, species_code_j),
           alpha_ij_id = as.numeric(as.factor(alpha_ij))) %>%
    arrange(ind_i)

  # Create indicies for fitness components between interacting species.
  fitness_components <- select(interactions, fertility, species_code_i, species_code_j) %>%
    distinct() %>%
    mutate(l_i = paste(fertility, species_code_i),
           l_j = paste(fertility, species_code_j),
           a_ii = paste(fertility, species_code_i, species_code_i),
           a_jj = paste(fertility, species_code_j, species_code_j),
           a_ij = paste(fertility, species_code_i, species_code_j),
           a_ji = paste(fertility, species_code_j, species_code_i)) %>%
    mutate_all(funs(factor)) %>%
    arrange(a_ij) %>%
    mutate_at(vars(a_ii, a_jj), funs(factor(., levels = a_ij))) %>%
    mutate_all(funs(as.numeric))

  # Package data as list
  data_list = append(
    list(S = max(dat$species_code),
         K = max(dat$fertility),
         N_alpha = max(interactions$alpha_ij_id),
         N_pots = max(dat$pot),
         N_block_effect = max(dat$block_by_sp),
         N_individuals = nrow(dat),
         N_interactions = nrow(interactions),
         ts = ts,
         biomass = dat$aboveground_biomass_g,
         dens_m1 = dat$total_density - 1,
         species = dat$species_code,
         sp_by_fert = dat$sp_by_fert,
         alpha_ij = interactions$alpha_ij_id,
         species_i = interactions$species_code_i,
         species_j = interactions$species_code_j,
         individual_j = interactions$ind_j,
         block_by_sp = dat$block_by_sp,
         log_mu_t0 = log_t0$aboveground_mean,
         log_sigma_t0 = log_t0$aboveground_sd),
    as.list(fitness_components[, 4:9]))

  data_list$species_code = species_code
  data_list$fertility_code = fertility_code
  data_list$interactions = interactions
  data_list$fitness_components = fitness_components

  return(data_list)
}
