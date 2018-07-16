#' Create figures
#'
#' Creates a series of figures to aide the interpretation of model outputs.
#' The figures are model dependent and should only be used if the models have
#' converged correctly.
#'
#' Figures include:
#' \enumerate{
#'  \item{Multiplicative growth rates}
#'  \item{Interaction coefficients}
#'  \item{Fitness differences}
#'  \item{Niche overlap}
#'  \item{Competitive abilities}
#'  \item{Posterior predictions}
#' }
#'
#'
#' @usage create_figures_models(fig = 1)
#' @export

create_figures <-  function(figs = 1:6, ...) {

  # Load model
  model_output <- load_model(...)

    # Lambda
    growth_rates(model_output, ...)

    # A_ij
    interaction_coef(model_output, ...)

    # Fitness
    fitness_differences(model_output, ...)

    # Niche
    niche_overlap(model_output, ...)

    # Comp
    competitive_ability(model_output, ...)

    # Pred
    posterior_prediction(model_output, ...)
}

#' Growth rates
growth_rates <- function(m, n_sp = 4, ts = 12){

  lambda <- extract_pars(m$model_summary, "^lambda", "id") %>%
    left_join(m$data_list$fitness_components, by = c("id" = "l_i")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code))

  p <- ggplot(lambda,
              aes(x = factor(fertility))) +
    geom_hline(aes(yintercept = 1),
               colour = "red", size = 0.6) +
    geom_point(aes(y = mean), size = 3) +
    geom_errorbar(aes(ymin = conf_low,
                      ymax = conf_high),
                  size = 1, width = 0) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    facet_grid(~ species_code_i) +
    labs(x = "Fertility",
         y = "lambda\n(multiplicative growth rate)")

  filename <- paste0("figures/growth_rates_", n_sp, "sp", ts, "ts.png")

  n = n_sp * 2

  ggsave(filename = filename, plot = p, device = "png",
         height = 5, width = n, dpi = 400, units = "in")
  print(p)
}


#' Interaction coefficients
interaction_coef <- function(m, n_sp = 4, ts = 12){

  a_ij <- extract_pars(m$model_summary, "^alpha", "id") %>%
    left_join(m$data_list$fitness_components, by = c("id" = "a_ij")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code),
           species_code_j = factor(species_code_j, labels = m$data_list$species_code))

  p <- ggplot(a_ij,
              aes(x = factor(fertility))) +
    geom_hline(aes(yintercept = 1),
               colour = "red", size = 0.6) +
    geom_point(aes(y = mean), size = 3) +
    geom_errorbar(aes(ymin = conf_low,
                      ymax = conf_high),
                  size = 1, width = 0) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    facet_grid(species_code_j ~ species_code_i) +
    labs(x = "Fertility",
         y = "a_ij (effect of row on column)")

  filename <- paste0("figures/interaction_coefficients_", n_sp, "sp", ts, "ts.png")

  n = n_sp * 2 + 1

  ggsave(filename = filename, plot = p, device = "png",
         height = n, width = n, dpi = 400, units = "in")
  print(p)
}

#' Interaction coefficients
interaction_coef <- function(m, n_sp = 4, ts = 12){

  a_ij <- extract_pars(m$model_summary, "^alpha", "id") %>%
    left_join(m$data_list$fitness_components, by = c("id" = "a_ij")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code),
           species_code_j = factor(species_code_j, labels = m$data_list$species_code))

  p <- ggplot(a_ij,
              aes(x = factor(fertility))) +
    geom_hline(aes(yintercept = 1),
               colour = "red", size = 0.6) +
    geom_point(aes(y = mean), size = 3) +
    geom_errorbar(aes(ymin = conf_low,
                      ymax = conf_high),
                  size = 1, width = 0) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    facet_grid(species_code_j ~ species_code_i) +
    labs(x = "Fertility",
         y = "a_ij (effect of row on column)")

  filename <- paste0("figures/interaction_coefficients_", n_sp, "sp", ts, "ts.png")

  n = n_sp * 2 + 1

  ggsave(filename = filename, plot = p, device = "png",
         height = n, width = n, dpi = 400, units = "in")
  print(p)
}

#' Fitness differences
fitness_differences <- function(m, n_sp = 4, ts = 12){

  rho <- extract_pars(m$model_summary, "^fitness_difference", "id") %>%
    left_join(m$data_list$fitness_components, by = c("id" = "a_ij")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code),
           species_code_j = factor(species_code_j, labels = m$data_list$species_code)) %>%
    filter(as.numeric(species_code_j) %in% 1:3,
           as.numeric(species_code_i) %in% 4:6)

  p <- ggplot(rho,
              aes(x = factor(fertility),
                  y = mean,
                  color = species_code_j)) +
    geom_hline(aes(yintercept = 1),
               colour = "black", size = 0.6) +
    geom_point(size = 4) +
    geom_line(aes(group = species_code_j),
              size = 1.2) +
    # geom_errorbar(aes(ymin = conf_low,
    #                   ymax = conf_high),
    #               size = 1, width = 0) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    facet_grid( ~ species_code_i) +
    labs(x = "Fertility",
         y = "Fitness difference",
         color = "Invader")

  filename <- paste0("figures/invader_fitness_differences_", n_sp, "sp", ts, "ts.png")

  n = n_sp * 2

  ggsave(filename = filename, plot = p, device = "png",
         height = 8, width = n, dpi = 400, units = "in")
  print(p)
}

#' Niche overlap
niche_overlap <- function(m, n_sp = 4, ts = 12){

  gamma <- extract_pars(m$model_summary, "^niche_overlap", "id") %>%
    left_join(m$data_list$fitness_components, by = c("id" = "a_ij")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code),
           species_code_j = factor(species_code_j, labels = m$data_list$species_code)) %>%
    filter(as.numeric(species_code_j) %in% 1:3,
           as.numeric(species_code_i) %in% 4:6)

  p <- ggplot(gamma,
              aes(x = factor(fertility),
                  y = mean,
                  color = species_code_j)) +
    geom_hline(aes(yintercept = 1),
               colour = "black", size = 0.6) +
    geom_point(size = 4) +
    geom_line(aes(group = species_code_j),
              size = 1.2) +
    # geom_errorbar(aes(ymin = conf_low,
    #                   ymax = conf_high),
    #               size = 1, width = 0) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    facet_grid( ~ species_code_i) +
    labs(x = "Fertility",
         y = "Niche overlap",
         color = "Invader")

  filename <- paste0("figures/invader_niche_overlap_", n_sp, "sp", ts, "ts.png")

  n = n_sp * 2

  ggsave(filename = filename, plot = p, device = "png",
         height = 8, width = n, dpi = 400, units = "in")
  print(p)
}

# Competitive ability
competitive_ability <- function(m, n_sp = 4, ts = 12){

  gamma <- extract_pars(m$model_summary, "^competitive_ability", "id") %>%
    left_join(m$data_list$fitness_components, by = c("id" = "a_ij")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code),
           species_code_j = factor(species_code_j, labels = m$data_list$species_code))

  p <- ggplot(gamma,
              aes(x = factor(fertility))) +
    geom_hline(aes(yintercept = 1),
               colour = "red", size = 0.6) +
    geom_point(aes(y = mean), size = 3) +
    geom_errorbar(aes(ymin = conf_low,
                      ymax = conf_high),
                  size = 1, width = 0) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    facet_grid(species_code_j ~ species_code_i) +
    labs(x = "Fertility",
         y = "Competitive ability")

  filename <- paste0("figures/competitive_ability_", n_sp, "sp", ts, "ts.png")

  n = n_sp * 2 + 1

  ggsave(filename = filename, plot = p, device = "png",
         height = n, width = n, dpi = 400, units = "in")
  print(p)
}

posterior_prediction <- function(m, n_sp = 4, ts = 12){

  dat <- data.frame(obs = m$data_list$biomass,
                    pred = extract_pars(m$model_summary, "^pred_biomass", "ind")$mean,
                    sp_by_fert = m$data_list$sp_by_fert) %>%
    left_join(m$data_list$fitness_components, by = c("sp_by_fert" = "l_i")) %>%
    mutate(fertility = factor(fertility, labels = m$data_list$fertility_code),
           species_code_i = factor(species_code_i, labels = m$data_list$species_code))

  p <- ggplot(dat, aes(x = obs,
                       y = pred,
                       color = species_code_i)) +
    geom_abline(aes(intercept = 0, slope = 1),
                linetype = "dashed", size = 1) +
    geom_point(alpha = 0.8, size = 1.5) +
    coord_cartesian(expand = F) +
    facet_wrap(~ fertility) +
    annotate("segment", x = -Inf, xend = Inf,
             y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,
             y = -Inf, yend = Inf) +
    labs(x = "Measured aboveground biomass",
         y = "Predicted aboveground biomass",
         color = "Species") +
    theme(legend.position = "bottom")

  filename <- paste0("figures/posterior_predictions", n_sp, "sp", ts, "ts.png")

  ggsave(filename = filename, plot = p, device = "png",
         height = 6, width = 10, dpi = 400, units = "in")
  print(p)
}
