#' Check models
#'
#' Having used \code{run_models()}, we can now evaluate the convergence of
#' the model and a variety of posterior predictive checks. These results
#' are output to the console.
#'
#' @param model BV
#' @param path  Directory to save model outputs, defaults to /models.
#'
#' @usage check_models(model = "BV")
#'
#' @export

check_models <- function(model_output = NA, ...) {

    # Load model if need be
    if(is.na(model_output[1]))
      model_output <- load_model(...)

    # Check for convergence
    convergence_check(model_output)

    # Run posterior check
    posterior_check(model_output)

}


#' Convergence checks
#'
#' Returns the model run time, the number of parameters with Rubin-Gelman
#' statistics greater than 1.1 (indicating that they have not converged),
#' and the number of effective samples of the worst sampled parameter.
#'
#' @usage convergence_check(model_output)
#'
#' @export

convergence_check <- function(model_output) {

  # Extract model fit, summary and time
  fit <- model_output$stan_output
  summary <- model_output$model_summary
  time <- max(rowSums(rstan::get_elapsed_time(fit)))

  # Print results
  printf("Time elapsed: %.1f hr", time / 3600)

  Rhat = sum(summary$Rhat > 1.1, na.rm = T)
  n_eff = floor(min(summary$n_eff))

  if (Rhat > 0) {
    printf("Model has **not** converged")
  }
  else {
    printf("Model has converged")
  }

  printf("Rhats > 1.1 =  %d", Rhat)
  printf("Minimum number of effective samples = %d, (%.2f/sec)",
         n_eff, n_eff/time)
}


#' Posterior check
#'
#' Runs posterior checks on model output by comparing the observed data to
#' posterior predictions under the same conditions. Presently, these are only
#' defined for tobit models (m1-m6) and include the root square mean error
#' (RMSE) of predictions after censoring, the total Euclidean distance between
#' all censored predictions and the data, the accuracy of predicted presences
#' and absences given as proportion of correct predictions, and the R-squared
#' statistic of predicted positive abundances against data.
#'
#' @usage posterior_check(model_output)
#'
#' @export

posterior_check <- function(model_output) {

  # Extract observed data
  y_obs <- model_output$data_list$biomass

  # Extract posterior mean predictions
  y_pred <- extract_pars(model_output$model_summary,
                         pars = "^pred_biomass",
                         index = "ind")$mean

  # Root square mean error of censored predictions
  error <- y_obs - y_pred
  rmse_censored <- round(sqrt(mean(error^2)), 2)
  printf("Root square mean error of predicted biomass = %.3f", rmse_censored)

  # R-squared of predicted abundances
  Rsq <- cor(y_obs, y_pred)^2

  printf("R-squared of observed and predicted biomass = %.2f", Rsq)
}
