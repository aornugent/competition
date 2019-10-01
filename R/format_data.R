#' Format data for analysis
#'
#' Loads and prepares the experimental data for analysis in Stan.
#'
#' @return
#'  \itemize{
#'   \item N    - the number of individuals
#'   \item N_jk - the number of observed interactions
#'   \item S    - the number of species
#'   \item A    - the number of distinct interactions
#'   \item P    - the number of blocks
#'   \item y    - aboveground biomass measurements
#'   \item jl   - IDs for species j in fertility treatment l
#'   \item jkl  - IDs for interaction between species k on species j in treatment l
#'   \item jp   - IDs for random effect of block p on species j
#'   \item n_i  - number of competitors
#'  }
#'
#' @examples
#' # load data
#' data_list <- format_data()
#' str(data_list)
#'
#' @export

format_data <- function(...) {

  # Load dataset
  data(samples)

  # Create indices, calculate the number of competitors in a pot.
  d <- samples %>%
    mutate(sp = as.numeric(species_code),
           f = as.numeric(fertility),
           sp_fert = as.numeric(factor(paste(f, sp))),
           blck = as.numeric(factor(block)),
           sp_blck = as.numeric(factor(paste(sp, blck)))) %>%
    arrange(seedling_id) %>%
    group_by(pot) %>%
    mutate(n_i = n() - 1) %>%
    ungroup()


  # Generate interaction indices for each individual, summed by pot in model
  interactions <- data.frame(pot = d$pot,
                             ind_j = d$seedling_id,
                             ind_k = d$seedling_id) %>%
    group_by(pot) %>%
    complete(ind_j, ind_k) %>%
    filter(ind_j != ind_k) %>%
    ungroup() %>%
    arrange(ind_j) %>%
    left_join(select(d, ind_j = seedling_id, f, sp_j = sp)) %>%
    left_join(select(d, ind_k = seedling_id, sp_k = sp)) %>%
    # mutate_at(vars(sp_j, sp_k), ~if_else(. > 3, 4, .)) %>%
    mutate(a_jk = as.numeric(factor(paste0(f, sp_j, sp_k))))

  # Recode species for post-processing
  sp <- select(d, j = sp_fert, fr = f, sp, species_code, fertility) %>%
    unique() %>%
    mutate(species = factor(species_code,
                            labels = c("A. fatua", "B. diandrus",
                                       "E. curvula", "B. hordeaceus",
                                       "P. labillardieri", "R. caespitosum")))

  a_jk <- select(interactions, jk = a_jk, fertility = f,
                 species_j = sp_j, species_k = sp_k) %>%
    unique() %>%
    mutate_at(vars(species_j, species_k),
                   ~ factor(., labels = c("A. fatua", "B. diandrus",
                                          "E. curvula", "B. hordeaceus",
                                          "P. labillardieri", "R. caespitosum"))) %>%
    mutate_at(vars(fertility), ~ factor(., labels = c("LOW", "MED", "HIGH")))

  blck <- select(d, sp_blck, blck, species_code, block) %>%
    unique() %>%
    mutate(species = factor(species_code,
                            labels = c("A. fatua", "B. diandrus",
                                       "E. curvula", "B. hordeaceus",
                                       "P. labillardieri", "R. caespitosum")))


  data_list <- list(
    N = nrow(d),
    N_jk = nrow(interactions),
    JK = max(d$sp_fert),
    JKL = max(interactions$a_jk),
    B = max(d$blck),
    y = d$aboveground_biomass_g,
    jk = d$sp_fert,
    jkl = interactions$a_jk,
    b = d$blck,
    n_l = d$n_i,
    species = sp,
    interactions = a_jk,
    block = blck
  )

  return(data_list)
}
