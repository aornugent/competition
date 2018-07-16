#' Run models
#'
#' Models are coded in Stan and can be found in the `/models` subdirectory.
#' Model output is saved to .Rdata files for later inspection. These are
#' are not distributed due to their size. Previously saved analysis can be
#' inspected using \code{check_models()} or loaded with \code{load_models()}.
#'
#' Models include:
#' \enumerate{
#'  \item{BV: Beverton-Holt biomass model}
#' }
#'
#' @param model BV (~2hrs).
#' @param n_sp  Treat residents as separate species (n=6), or single community (n=4).
#' @param ts    Number of timesteps for iteration method.
#' @param path  Directory to save model outputs, defaults to /models.
#' @param check auto-run model checks, boolean (default = T).
#'
#' @usage run_model(model = "m1")
#'
#' @export

run_models <- function(
  models = c("BV"),
  # n_sp = 4,
  # ts = 12,
  path = "models/",
  check = T, ...) {

  for(model in models) {
    model_output <- run_stan_model(model, path, ...)

    if(check == T)
      check_models(model, path, model_output, ...)
  }
}


#' Run Stan model
#'
#' Pre-processes data and wraps \link{rstan} to run analyses using
#' adaptive Hamiltonian Monte Carlo. Sensible default settings are provided,
#' but can be tweaked as necessary.
#'
#' @param model one of c("BV")
#' @param ... tuning parameters for Stan sampler
#'
#' @export

run_stan_model <- function(model, path, ...) {

  # Set parallel options
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() - 1)

  # Do data prep
  data_list <- format_data(model, ...)

  # Get appropriate file
  model_file <- switch(
    model,
    BV = "models/iterative_beaverton_holt.stan"
  )

  # Pre-compile model
  printf("\nCompiling model... (ignore non-fatal warnings)")

  suppressWarnings(
    testfit <- rstan::stan(
      file = model_file,
      data = data_list,
      iter = 1,
      chains = 1,
      init_r = 0.5)
    )

  # Run model
  printf("Running model...")

  mod <- rstan::stan(
    fit = testfit,
    data = data_list,
    iter = 1000,
    chains = 3,
    init_r = 0.5,
    save_warmup = F,
    control = list(max_treedepth = 15,
                   adapt_delta = 0.8)
  )

  summary <- rstan::summary(mod)$summary %>%
    as.data.frame()

  # Append data_list to model output
  model_output = list(model_summary = summary,
                      stan_output = mod,
                      data_list = data_list)

  # Save model output
  filename = paste0(path, model, "_", n_sp, "sp", ts, "ts","_output.Rdata")
  printf(c("Saving", filename))
  save(model_output, file = filename)

  return(model_output)
}
