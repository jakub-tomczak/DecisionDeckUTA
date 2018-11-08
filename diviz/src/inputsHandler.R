checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  if(xmcdaData$alternatives.isEmpty())
  {
    stop("There should be at least one alternative.")
  }
  nrAlternatives <-length(xmcdaData$alternatives)
  
  if(xmcdaData$criteria.isEmpty())
  {
    stop("There should be at least one criterion.")
  }
  nrCriteria <- length(xmcdaData$critera)
  
  #create preferences matrix
  preferences <- matrix(nrow = nrAlternatives, ncol = nrCriteria)
  
  #process criteria.xml
  criteriaTypes <- sapply(xmcdaData$criteria, function(x){
    type <- x$scale$quantitative$preferenceDirection
    if(type == "min")
    {
      'c'
    }
    else if(type == "max")
    {
      'g'
    }
    else
    {
      stop(paste("Unknown criterion type:", type))
    }
  })
  
  #process characteristicPoints.xml
  characteristicPoints <- rep(2, nrCriteria)
  if(!is.null(xmcdaData$characteristicPoints))
  {
    if(length(xmcdaData$characteristicPoints) != nrCriteria)
    {
     stop("If list with number of characteristic points is defined, its length should be equal to the number of criteria.")
    }
    characteristicPoints <- sapply(xmcdaData$characteristicPoints, function(x){
      x$criterionValue$value$integer
    })
  }
  
  #process preferences files
  strongPreferences <- NULL
  weakPreferences <- NULL
  indifference <- NULL
  if(!is.null(xmcdaData$strongPreferences) && !xmcdaData$strongPreferences.isEmpty())
  {
    strongPreferences <- sapply(xmcdaData$strongPreferences$pairs, function(x){
      sapply(x.pair, function(y){
        c(y$initial$alternativeID, y$terminal$alternativeID)
      })
    })
  }
  if(!is.null(xmcdaData$weakPreferences) && !xmcdaData$weakPreferences.isEmpty())
  {
    weakPreferences <- sapply(xmcdaData$weakPreferences$pairs, function(x){
      sapply(x.pair, function(y){
        c(y$initial$alternativeID, y$terminal$alternativeID)
      })
    })
  }
  if(!is.null(xmcdaData$indifference) && !xmcdaData$indifference.isEmpty())
  {
    indifference <- sapply(xmcdaData$indifference$pairs, function(x){
      sapply(x.pair, function(y){
        c(y$initial$alternativeID, y$terminal$alternativeID)
      })
    })
  }
  
  #parse methodParameters.xml
  method <- "utag"
  if(!is.null(xmcdaData$methodParameters))
  {
    rawMethod <- xmcdaData$methodParameters$methodName
    if(!is.null(rawMethod))
    {
      if(rawMethod == 1)
      {
        method = "utag"
      } 
      else if (rawMethod == 2)
      {
        method = "utamp1"
      } 
      else if (rawMethod == 3)
      {
        method = "utamp2"
      }
      else
      {
        stop("Unknown method")
      }
    }
    
  }
  
  source("calculations.R")  
  #validate data and build problem
  problem <- buildProblem(preferences, criteriaTypes, characteristicPoints, 
                          strongPreference = strongPreference,
                          weakPreferences = weakPreferences,
                          indifference = indifference)
  return(problem)
}
