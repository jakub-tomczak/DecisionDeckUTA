RORUTA_ExtremeRankingAnalysis <- function(inputs)
{
  library(Rglpk)
  result <- necessaryAndPossiblePreferencesRelationAnalysis(model)
  # add performances to access alternatives and criteria names in outputsHandler
  result$performances <- model$performances
  result
}
