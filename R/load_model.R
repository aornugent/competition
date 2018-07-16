#' Load model
#'
#' Loads model output from .Rdata file.
#'
#' @param model m0-m3, defaults to all.
#' @param path  Directory to save model outputs, defaults to /models.
#'
#' @usage model_output <- load_models(model = "m1", path = "models/")
#'
#' @export

load_model <- function(model = "BV",
                       path = "models/",
                       n_sp = 4,
                       ts = 12, ...) {

  # Format filename
  filename = paste0(path, model, "_", n_sp, "sp", ts, "ts","_output.Rdata")

  printf(paste("Loading:", filename))

  # Check that this fails nicely
  tryCatch(
    model_output <- get(load(filename)),
    error = function(e) {
      printf(paste(filename, "does not exist"))
    }
  )

  # Check model type
  if(class(model_output$stan_output) != "stanfit"){
    printf(paste(filename, "does not contain the output of a Stan model"))
    return(NULL)
  }

  return(model_output)
}
