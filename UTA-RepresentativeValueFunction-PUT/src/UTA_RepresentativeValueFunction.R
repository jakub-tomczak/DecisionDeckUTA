UTA_RepresentativeValueFunction <- function(model)
{
  library(Rglpk)
  result <- solveProblem(model, allowInconsistency = TRUE)
  # add performances to access alternatives and criteria names in outputsHandler
  result$performances <- model$performances
  result
}

