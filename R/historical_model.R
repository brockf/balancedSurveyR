#' balancedSurveyR: Historical model
#'
#' @author Brock Ferguson, \email{brock.ferguson@gmail.com}
#'

#' Fit a model to historical response data
#'
#' This should be the first function you call when using balancedSurveyR.
#' You will feed it a dataframe of existing survey records. Each record
#' should have 1+ columns of "attributes" (things you think might matter
#' in responding to a survey) and a column indicating whether they
#' responded or not (1/0).
#'
#' This function will then model these data using the model indicated
#' (default: 'xgbTree') and then use a smoothing spline to recover
#' best possible estimates of response rates for each prediction.
#'
#' It will return a balancedSurveyR model object which can be passed
#' to the sample() function.
#'
#' @examples
#' library('dplyr')
#' library('ggplot2')
#'
#' # generate fake historical data
#' historical_responses <- expand.grid(
#'   person=1:100,
#'   age = 20:50
#' ) %>%
#'   mutate(
#'     responded = rbinom(nrow(.), size=1, prob=(age / 100))
#'   ) %>%
#'   select(-starts_with('.')) %>%
#'   select(-person)
#'
#' # fit historical_model()
#' model <- historical_model(data = historical_responses,
#'                           attribute_columns = c('age'),
#'                           response_column = 'responded',
#'                           model = 'xgbTree')
#'
#' # verify predictions
#' historical_responses$predictions <- model$predictions
#' ggplot(historical_responses, aes(x=predictions, y=responded)) +
#'   stat_summary(fun.y='mean', geom='point') +
#'   stat_smooth(method="glm", method.args=list(family="binomial"))
#'
#' # generate new population data
#' survey_population <- expand.grid(
#'   person=1:1000,
#'   age = 20:50
#' ) %>%
#'   select(-person)
#'
#' # generate sample
#' sampled_population <- sampler(population = survey_population,
#'                               model = model,
#'                               desired_n = 1000)
#'
#' # examine characteristics
#' # sampled characteristics should be biased towards under-responders
#' sampled_characteristics <- characteristics(sample = sampled_population,
#'                                            population = survey_population,
#'                                            model = model,
#'                                            state = 'sampled')
#'
#' sampled_characteristics
#'
#' # responder characteristics should be representative of population
#' responder_characteristics <- characteristics(sample = sampled_population,
#'                                              population = survey_population,
#'                                              model = model,
#'                                              state = 'responded')
#'
#' responder_characteristics
#'
#' @param data                 (dataframe/tbl) of historical response data.
#' @param attribute_columns    (vector) column names indicating possibly-predictive attributes
#' @param response_column      (character) column name for response (1/0) column
#' @param model                (character) type of model to use on the data
#'
#' @return balancedSurveyR model object, a list with keys: model, fit, predictions
#' @export
historical_model <- function(data,
                             attribute_columns,
                             response_column,
                             model = 'xgbTree') {

  # argument validation
  if (!is.data.frame(data) && !is.tbl(data)) {
    stop("data must be a dataframe/tbl.")
  }

  if (!nrow(data) > 0) {
    stop("data do not exist")
  }

  if (!is.character(attribute_columns)) {
    stop("attribute_columns must be a character vector of column names located in data")
  }

  if (!length(attribute_columns) > 0) {
    stop("attribute_columns is empty")
  }

  if (!is.character(response_column)) {
    stop("response_column must be a character string pointing to a column in the dataframe")
  }

  if (sum(is.na(as.logical(data[[response_column]]))) > 0) {
    stop("response_column should contain numeric (1, 0) or logical (TRUE, FALSE) data indicating
          response/no response for each record. No missing data is allowed")
  }

  if (!model %in% c('xgbTree')) {
    stop("invalid model selected. Maybe use 'xgbTree'?")
  }

  # prepare response variable
  data[, response_column] <- as.logical(data[[response_column]])

  # xgbTree method
  if (model == 'xgbTree') {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("xgboost package needed for this function to work. Please install it.", call. = FALSE)
    }

    model_formula <- as.formula(paste0(' ~ ', paste0(attribute_columns, collapse = ' + ')))
    model_matrix <- build_model_matrix(model_formula, data)

    cat("Fitting cross-validated xgBoost model...")

    # cross validate to determine best number of iterations
    model_cv <- xgboost::xgb.cv(data = model_matrix,
                       nfold = 10,
                       early.stop.round = 3,
                       maximize=FALSE,
                       nrounds = 5000,
                       verbose = 0,
                       label = as.numeric(data[[response_column]]),
                       params = list(
                         booster = 'gbtree',
                         objective = 'reg:logistic',

                         # shrinkage:
                         eta = .01
                       ))

    # get best iteration for optimal nrounds
    model_cv$iteration <- 1:nrow(model_cv)

    # sometimes there are ties, so take the first (min) round with minimum error
    optimal_nrounds <- min(model_cv[which(model_cv$test.rmse.mean == min(model_cv$test.rmse.mean)), ]$iteration)

    # fit final model
    model <- xgboost::xgboost(data = model_matrix,
                     nrounds = optimal_nrounds,
                     label = as.numeric(data[[response_column]]),
                     verbose = 0,
                     params = list(
                       booster = 'gbtree',
                       objective = 'reg:logistic',

                       # shrinkage:
                       eta = .01
                     ))

    cat(".\n   Cross-validated RMSE: ", min(model_cv$test.rmse.mean))

    # get predictions and map smoothing spline to approximate empirical response rate
    # why do we do this? because some models are biased, and we actually care about
    # the *actual* response rate (not relative response rate)
    model_predictions <- xgboost::predict(model, model_matrix)
    empirical_response_rate <- as.numeric(data[[response_column]])

    # splines only work if we have 4+ unique predicted values
    if (length(unique(model_predictions)) >= 4) {
      cat("\nCorrecting to empirical response rates using smoothing spline.\n")
      spline_fit <- smooth.spline(model_predictions, empirical_response_rate, nknots = 20)
      predictions <- predict(spline_fit, model_predictions)$y

      linear_fit <- NA
    } else{
      cat("\nCorrecting to empirical response rates using linear model.\n")
      # create dataframe so we can reference by column name
      # (this allows us to use predict later, without warnings)
      temp_df <- data.frame(
                  empirical_response_rate = empirical_response_rate,
                  response_prob = as.numeric(model_predictions))

      # use a simple linear fit
      linear_fit <- lm(empirical_response_rate ~ response_prob,
                                data = temp_df)
      predictions <- fitted(linear_fit)

      spline_fit <- NA
    }

    object <- list(
                model_type = 'xgbTree',
                model = model,
                predictions = predictions,
                attribute_columns = attribute_columns,
                response_column = response_column,
                model_formula = model_formula,
                spline_fit = spline_fit,
                linear_fit = linear_fit
            )
  }

  # tag object as a balancedSurveyR object
  class(object) <- c('balancedSurveyR_model','list')

  return(object)
}

