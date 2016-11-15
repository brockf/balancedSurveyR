#' balancedSurveyR: Gather new sample
#'
#' @author Brock Ferguson, \email{brock.ferguson@gmail.com}
#'

#' Gather a new, balanced sample
#'
#' This should be the second function you call when using balancedSampler.
#' After you generate your historical_model(), you will feed in a new dataframe
#' to this function. It will return to you a dataframe representing unique
#' sample of people that, when surveyed, should generate a representative
#' sample of your population approximately the size of your desired N(umber)
#' of responses.
#'
#' This balanced sample is gathered through the following process:
#'
#' 1. Fit the historical model to the new population
#' 2. Weight people in the new sample as the inverse of their response probability
#'    (w = 1 / prob).
#' 3. Repeatedly draw weighted samples of clients until the sum of their response probabilities
#'    is greater or equal to the desired N.
#' 4. Return the compiled sample as the suggested sample.
#'
#' @param population           (dataframe/tbl) new people to consider surveying (i.e., your population).
#' @param model                (balancedSurveyR model object) the output of a historical_model() fit your historical data.
#' @param desired_n            (integer) how many people do you want to *respond* to your survey (your desired N)?
#'
#' @return dataframe of sampled clients
#' @export
sampler <- function(population,
                    model,
                    desired_n
) {
  # argument validation
  if (!is.data.frame(population) && !is.tbl(population)) {
    stop("population must be a dataframe/tbl.")
  }

  if (nrow(population) <= desired_n) {
    stop("population is <= the desired_n... you'll have to just sample everyone")
  }

  if (!inherits(model, 'balancedSurveyR_model')) {
    stop("model must be a balancedSurveyR model -- i.e., the output from historical_model()")
  }

  if (!is.numeric(desired_n)) {
    stop("desired_n must be an integer representing the desired number of responses for the survey")
  }

  if (model$model_type == 'xgbTree') {
    model_matrix <- build_model_matrix(model$model_formula, population)
    population$response_prob <- xgboost::predict(model$model, model_matrix)

    # transform probabilities with spline/linear
    if (length(model$spline_fit) > 1) {
      population$response_prob <- predict(model$spline_fit, population$response_prob)$y
    } else {
      population$response_prob <- predict(model$linear_fit, population)
    }

    # splines can push outside of boundaries; truncate extreme values
    population$response_prob <- ifelse(population$response_prob <= 0, 0.005, population$response_prob)
    population$response_prob <- ifelse(population$response_prob >= 1, 0.995, population$response_prob)

    # calculate sample weights as w = 1 / prob
    population$sample_weight = 1 / population$response_prob

    # generate a sample by sampling batches until sum(response_prob) >= desired_n
    expected_n <- 0 # start at 0, increment with each batch
    batch_size <- round(desired_n / 2)

    # give an ID to each person in the population so we can track who has been sampled
    population$.person_id <- 1:nrow(population)

    # create empty dataframe to hold for sampled clients
    sampled_pop <- population %>% filter(1 == 2)

    while (expected_n < desired_n) {
      sampled_pop <- bind_rows(
        # existing sample:
        sampled_pop,

        # new sample:
        population %>%
          do({
            new_sample <- base::sample(.$.person_id[!.$.person_id %in% sampled_pop$.person_id],
                                       size = batch_size,
                                       prob = .$sample_weight[!.$.person_id %in% sampled_pop$.person_id])

            .[new_sample, ]
          })
      )

      # update expected N
      expected_n <- sum(sampled_pop$response_prob)
    }
  }

  # give a final summary
  cat(paste0("Returned sample of ", nrow(sampled_pop), " people, ", round(expected_n), " of which are expected to respond to the survey."))

  # drop any hidden columns
  sampled_pop <- sampled_pop %>% select(-starts_with('.'))

  return(sampled_pop)
}
