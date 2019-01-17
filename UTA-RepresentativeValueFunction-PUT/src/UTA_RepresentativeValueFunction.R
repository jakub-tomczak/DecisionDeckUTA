UTA_RepresentativeValueFunction <- function(model)
{
  library(Rglpk)
  solveProblem(model, allowInconsistency = TRUE)
}

