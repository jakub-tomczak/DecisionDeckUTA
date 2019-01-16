UTA <- function(inputs)
{
  library(Rglpk)
  model <- buildModel(inputs)
  solveProblem(model, allowInconsistency = TRUE)
}
