#' Function to write to console
#'
#' @name printf
#' @rdname printf
#' @usage printf("Hello world")

printf <- function(...) cat(sprintf(...), "\n")

#' Load dplyr
#'
#' dplyr is used extensively in this package
#'
#' @name dplyr
#' @import dplyr
NULL

#' Basic ggplot theme
#'
#' Set as default on package load.
#'
#' @import ggplot2

.onLoad <- function(libname, pkgname) {

  theme_impact <-
    theme_minimal() +
      theme(
        text = element_text(size = 16),
        panel.grid = element_blank(),
        axis.line.x = element_line(
          color="black",
          size = 0.5),
        axis.line.y = element_line(
          color="black",
          size = 0.5),
        axis.ticks = element_line(),
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(
          margin = margin(t = 10, b = 20),
          hjust = -0.1, vjust = -8),
        legend.position = "bottom",
        aspect.ratio = 1)

  theme_set(theme_impact)
}

#' expand.grid for dataframes
#'
#' @param ... a dataframe
#' @usage df_long <- expand.grid.df(df)

expand_grid <- function(...) {
  Reduce(function(...) merge(..., by=NULL), list(...))
}


#' Back-transform from logit scale
#'
#' @param continuous value between 0 and 1.

unlogit <- function(x){
  exp(x) / (1 + exp(x))
}

#' Add corner label to plots
#'
#' @param ggplot ggplot object
#' @param label string to label in top left corner
#'
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid textGrob gpar

label <- function(ggplot, label, x = 0.05) {
  ggplot <- arrangeGrob(ggplot,
                        top = textGrob(label,
                                       x = x,
                                       y = 0.6,
                                       just= c(0, 1),
                                       gp = gpar(fontsize = 20)))
}

#' Extract parameter estimates
#'
#' Stan output provides summaries of parameter estimates, this can be faster than working directly with samples
#'
#' @param summary output of a Stan model summary
#' @param pars vector of parameter names
#' @param index vector of labels for parameter indexes
#'
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate
#'
#' @examples
#' pars <- extract_par(fit = model_output$model_summary,
#'                           pars = c("B"),
#'                           index = c("species", "covariate", "treatment"))


extract_pars <- function(summary, pars, index) {

  # Grep for desired parameter estimates
  suppressWarnings(estimates <- summary %>%
                     rownames_to_column(var = "id") %>%
                     filter(grepl(paste0("^", pars, collapse = "|"), id)
                            & !grepl("raw", id)) %>%
                     separate(id,
                              into = c("parameter", index),
                              sep = "\\[|,|\\]", extra = "drop") %>%
                     mutate_at(vars(one_of(index)), as.numeric) %>%
                     select(parameter, index, mean,
                            conf_low = `2.5%`, conf_high = `97.5%`))

  return(estimates)
}


#' Index nested factors
#'
#' Used for double-indexing in Stan models. Assigns numerical id of top-level
#' factors to each entry of lower-level factors.
#'
#' @usage nested(plot, site)


nested <- function(x, y){
  nestbl <- table(factor(x), factor(y))
  idx <- unname(apply(nestbl, 1, function(x) which(x > 0)))
  return(idx)
}

