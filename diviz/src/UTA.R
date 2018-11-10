UTA <- function(inputs)
{
  library(Rglpk)
  model <- buildModel(inputs)
  solve(model, allowInconsistency = TRUE)
}
