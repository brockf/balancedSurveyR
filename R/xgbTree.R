# xgbTree.R helper functions

build_model_matrix <- function(formula, dataframe) {
  # there shouldn't be any NA's, but just in case...
  prev_setting <- getOption('na.action')
  options(na.action='na.pass')

  train_model_matrix <- Matrix::sparse.model.matrix(formula, data = dataframe)

  options(na.action=prev_setting)

  return(train_model_matrix)
}
