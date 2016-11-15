#' balancedSurveyR: Examine sample/population characteristics
#'
#' @author Brock Ferguson, \email{brock.ferguson@gmail.com}
#'

#' Examine sample characteristics before/after responses
#'
#' After generating a new sample, you can examine the performance
#' of this tool by examining the characteristics of your samples.
#'
#' When called with state='sampled', you'll view the characteristics
#' of your sample in its entirety. This should over-represent segments
#' of your population that are unlikely to respond, because we need to
#' ask more of them to complete the survey than your typical person.
#'
#' When called with state='responded', you'll view the expected characteristics
#' of the portion of your sample that responds to the survey. Note: this is based
#' off their *expected* response rates: if people don't respond in the future
#' like they have to this point, then this may be inaccurate.
#'
#' @param sample          (dataframe) Your sample returned by sampler()
#' @param population      (dataframe) Your population dataframe sent to sampler()
#' @param model           (balancedSurveyR model object) the output of a historical_model() fit your historical data.
#' @param state           (character) Either 'sampled' or 'responded' (default: 'sampled')
#' @param plot_type       (character) Either 'density' or 'ecdf' (default: density)
#'
#' @return ggplot2 object
#' @export
characteristics <- function(sample,
                             population,
                             model,
                             state = 'sampled',
                             plot_type = 'density') {
  # argument validation
  if (!state %in% c('sampled','responded')) {
    stop("state must be either 'sampled' or 'responded'")
  }

  if (!is.data.frame(sample) && !is.tbl(sample)) {
    stop("sample must be a dataframe/tbl.")
  }

  if (!is.data.frame(population) && !is.tbl(population)) {
    stop("population must be a dataframe/tbl.")
  }

  if (!inherits(model, 'balancedSurveyR_model')) {
    stop("model must be a balancedSurveyR model -- i.e., the output from historical_model()")
  }

  # isolate numeric (plottable) from non-numeric (non-plottable) columns
  numeric_columns <- colnames(sample)[sapply(sample[, model$attribute_columns], function(x) { return(is.numeric(x)) })]
  non_numeric_columns <- colnames(sample)[!colnames(sample) %in% numeric_columns]

  # if we are only looking at responders, let's trim down sample()
  if (state == 'responded') {
    sample <- sample %>%
      mutate(
        .responded = rbinom(nrow(.), size = 1, prob=response_prob)
      ) %>%
      filter(
        .responded == 1
      ) %>%
      select(-starts_with('.'))
  }

  # generate compiled dataframe of the true clientbase, random sample, and model sample
  comparisons <- bind_rows(
    sample %>%
      dplyr::select_(.dots = model$attribute_columns) %>%
      tidyr::gather_('Attribute','Value', numeric_columns) %>%
      mutate(Group='Model Sample'),

    population %>%
      mutate(random_code = sample(1:nrow(.))) %>%
      arrange(random_code) %>%
      dplyr::slice(1:nrow(sample)) %>%
      do({
        if (state == 'responded') {
          # remove people who we expect not to respond
          model_matrix <- build_model_matrix(model$model_formula, .)
          .$response_prob <- xgboost::predict(model$model, model_matrix)

          # transform probabilities with spline/linear
          if (length(model$spline_fit) > 1) {
            .$response_prob <- predict(model$spline_fit, .$response_prob)$y
          } else {
            .$response_prob <- predict(model$linear_fit, .)
          }

          .$responder = rbinom(nrow(.), size=1, prob = .$response_prob)

          return(.[.$responder == 1, ])
        } else {
          return(.)
        }
      }) %>%
      dplyr::select_(.dots = model$attribute_columns) %>%
      tidyr::gather_('Attribute','Value', numeric_columns) %>%
      mutate(Group='Random Sample'),

    population %>%
      dplyr::select_(.dots = model$attribute_columns) %>%
      tidyr::gather_('Attribute','Value', numeric_columns) %>%
      mutate(Group='Population')
  )

  p <- ggplot2::ggplot(comparisons, ggplot2::aes(x=Value, fill=Group, color=Group))

  if (plot_type == 'ecdf') {
    p <- p + ggplot2::stat_ecdf(geom='step', size=1, alpha=.5)
  } else {
    p <- p + ggplot2::geom_density(alpha=.3)
  }

  if (length(non_numeric_columns) == 0) {
    p <- p + ggplot2::facet_wrap(~Attribute, scales='free', nrow=4) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="top")
  } else {
    p <- p + ggplot2::facet_wrap(c('Attribute',non_numeric_columns), scales='free', nrow=4) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="top")
  }

  if (state == 'responded') {
    p <- p + ggplot2::ggtitle('Responder Characteristics')
  } else {
    p <- p + ggplot2::ggtitle('Sample Characteristics')
  }

  return(p)
}
