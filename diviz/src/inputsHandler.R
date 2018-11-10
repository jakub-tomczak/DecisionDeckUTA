library(purrr)
library(stringr)
alternativesToInt <- function(alternativesNames){
  #if a number has not been found NA is returned
  as.integer( str_extract(alternativesNames, "\\d") )
}

# Creates a list of strong, weak preferences, indifferences
# each element in a list is a list returned from processPreferences function
getPreferences <- function(xmcdaData, alternatives, programExecutionResult) {
  out <- list()
  for(i in seq(xmcdaData$alternativesMatricesList$size()) )
  {
    preference <- xmcdaData$alternativesMatricesList$get(as.integer(i-1))
    preferenceList <- processPreferences(preference, alternatives$alternatives)
    if(!(preferenceList$relationName %in% c("strong", "weak", "indifference")))
    {
      programExecutionResult(xmcdaMessages, errors = paste("Preference name", preferenceList$relationName , "is not valid"))
    }
    if(preferenceList$relationName == "strong"){
      out$strong <- preferenceList$relationPairs
    } else if(preferenceList$relationName == "weak"){
      out$weak <- preferenceList$relationPairs
    } else {
      out$indifference <- preferenceList$relationPairs
    }
  }
  out
}

# Creates a list consisting of two fields: relationName and relationPairs
# Each pair consists of two ids represented by two integers
processPreferences <- function(preferencesMatrix, alternatives) {
  #relationsMatrix list of characters on indices with a pair of alternatives defined in alternativesComparisons.xml 
  relationsMatrix <- outer(alternatives, alternatives, Vectorize(function(item1, item2) {
    #add a pair if relation exists
    if(!is.null(preferencesMatrix$get(item1, item2)))
      #create a vector from this pair and cast its values to integers, "a1" "a2" -> 1 2
      alternativesToInt(c(item1$id(), item2$id()))
  }))
  
  #filter out NULL values, return value is a vector
  relationsVec <- Filter(Negate(is.null), relationsMatrix)
  #create two column matrix with pairs in rows
  relationsVec <- matrix(unlist(relationsVec), ncol = 2)
  
  list(
    relationName = preferencesMatrix$id(),
    relationPairs = relationsVec
  )
}

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  #getMatrixValue(xmcdaData$alternativesMatricesList, as.integer(0), as.integer())
  if(xmcdaData$alternatives$isEmpty())
  {
    putProgramExecutionResult(xmcdaMessages, errors="There should be at least one alternative.")
  }
  alternatives <- getActiveAlternatives(xmcdaData)
  #cast alternatives names to ints
  alternatives$alternativesIDs <- alternativesToInt(alternatives$alternativesIDs)
  
  if(xmcdaData$criteria$isEmpty())
  {
    putProgramExecutionResult(xmcdaMessages, errors = "There should be at least one criterion.")
  }
  criteria <- getActiveCriteria(xmcdaData)
  
  
  #create performance matrix
  performanceMatrix <- getNumericPerformanceTableList(xmcdaData)
  if(is.list(performanceMatrix))
  {
    performanceMatrix <- performanceMatrix[[1]]
  }
  if(!is.matrix(performanceMatrix))
  {
    putProgramExecutionResult(xmcdaMessages, errors = "An error occured when processing preferences matrix. Check xml file with performanceMatrix." )
  }
  #assign integer ids to rownames
  row.names(performanceMatrix) <- alternativesToInt(row.names(performanceMatrix))
  
  
  #process criteria scales, after conversion to v3
  criteriaDirections <-  getCriteriaPreferenceDirectionsList(xmcdaData)
  if(is.null(criteriaDirections))
  {
    putProgramExecutionResult(xmcdaMessages, errors = "Criteria directions cannot be null.")
  }
  else if(length(criteriaDirections) == 0)
  {
    putProgramExecutionResult(xmcdaMessages, errors = "Criteria directions cannot be empty.")
  }
  criteriaDirections <-c(sapply(criteriaDirections, function(x){
    ifelse(x=="min", 'c', 'g')
  }))
  
  
  #process characteristicPoints.xml
  #set 2 characteristic points on all criteria by default
  characteristicPoints <- rep(2, criteria$numberOfCriteria)
  criteriaValuesList <- getNumericCriteriaValuesList(xmcdaData)
  if(!is.null(criteriaValuesList) && !is.null(criteriaValuesList$characteristicPoints))
  {
    if(length(criteriaValuesList$characteristicPoints) != criteria$numberOfCriteria)
    {
      putProgramExecutionResult(xmcdaMessages, errors = "If characteristic points are defined, number of them should match the number of criteria.")
    }
    characteristicPoints <- c(criteriaValuesList$characteristicPoints)
  }
  
  #parse preferences
  preferencesList <-getPreferences(xmcdaData, alternatives, programExecutionResult)

  #get program parameters
  programParameters <- getProgramParametersList(xmcdaData)
  
  #parse methodParameters.xml
  method <- ifelse(is.null(programParameters$programParameters$methodSettings$methodName), 
                   "uta-g",
                   programParameters$methodSettings$methodName)

  #validate data and build problem
  problem <- buildProblem(performanceTable = performanceMatrix,
                          criteria = criteriaDirections,
                          characteristicPoints = characteristicPoints, 
                          strongPreference = preferencesList$strong,
                          weakPreferences = preferencesList$weak,
                          indifference = preferencesList$indifference,
                          method = method)
  return(problem)
}
