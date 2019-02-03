library(purrr)
library(stringr)
library(hashmap)

#replaces text with the first number found in this text 
idToInt <- function(alternativesNames){
  # if a number has not been found NA is returned
  # str_extract(alternativesNames, "\\d") # extracts first number in the string
  as.integer( alternativesNames )
}

# Creates a list of strong, weak preferences, indifferences
# each element in a list is a list returned from processPreferences function
getPreferences <- function(xmcdaData, alternatives, alternativesIds2Names, programExecutionResult, xmcdaMessages) {
  out <- list()
  if(xmcdaData$alternativesMatricesList$size() == 0)
  {
    return(out)
  }
  for(i in seq(xmcdaData$alternativesMatricesList$size()) )
  {
    preference <- xmcdaData$alternativesMatricesList$get(as.integer(i-1))
    preferenceList <- processPreferences(preference, alternatives$alternatives)
    if(!(preferenceList$relationName %in% c("strong", "weak", "indifference")))
    {
      programExecutionResult(xmcdaMessages, errors = paste("Preference name", preferenceList$relationName , "is not valid"))
    }
    if( !any(preferenceList$relationPairs %in% alternativesIds2Names$keys()) )
    {
      programExecutionResult(xmcdaMessages, errors = 
                               paste("Error during parsing ", preferenceList$relationName, " preferences list - ", 
                                     preferenceList$relationName, ". All relations must be specified using alternatives ids", sep=""))
      # return parsed relations so far
      return(out)
    }
    
    #exchange ids with alternativesNames
    for(row in 1:nrow(preferenceList$relationPairs))
    {
      for(column in 1:ncol(preferenceList$relationPairs))
      {
        if(alternativesIds2Names$has_key(preferenceList$relationPairs[row, column]))
        {
          preferenceList$relationPairs[row, column] <- alternativesIds2Names$find(preferenceList$relationPairs[row, column])
        } else {
          programExecutionResult(xmcdaMessages, errors = 
                                   paste("Error during parsing alternative ", preferenceList$relationPairs[row, column], 
                                         " in the ", preferenceList$relationName ," preferences list. This id doesn't match ids from alternatives list.", sep=""))
          # return parsed relations so far
          return(out)
        }
      }
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
      #create a vector from this pair and take the value as they are
      c(item1$id(), item2$id())
  }))
  
  #filter out NULL values, return value is a vector
  relationsVec <- Filter(Negate(is.null), relationsMatrix)
  #create two column matrix with pairs in rows
  if(!is.null(relationsVec) && length(relationsVec) > 0){
    relationsVec <- matrix(unlist(relationsVec), ncol = 2, byrow = TRUE)
  }
  # return an empty list if there were no relations
  list(
    relationName = preferencesMatrix$id(),
    relationPairs = relationsVec
  )
}

# get names from ids or from name attribute if getNamesFromIDs == FALSE
getValuesNames <- function(xmcdaV3Matrix, getNamesFromIDs = TRUE)
{
  sapply(xmcdaV3Matrix, function(x){
    if(getNamesFromIDs)
      x$id()
    else
      x$name()
  })
}

# ids should be a vector
# names should be a list
# typeName is used for an error message
createID2NameMapping <- function(ids, namesList, typeName)
{
  if(length(unlist(namesList)) != length(ids))
  {
    putProgramExecutionResult(xmcdaMessages, errors = paste("One of the ", typeName, " has a NULL name. ",
                                                            "Check wheter ", typeName, " ids from xml files match those from ", typeName, " xml file.", sep=""))
    return(NULL)
  }
  hashmap(ids, namesList)
}

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  if(xmcdaData$alternatives$isEmpty())
  {
    putProgramExecutionResult(xmcdaMessages, errors="There should be at least one alternative.")
  }
  alternatives <- getActiveAlternatives(xmcdaData)
  
  if(xmcdaData$criteria$isEmpty())
  {
    putProgramExecutionResult(xmcdaMessages, errors = "There should be at least one criterion.")
  }
  criteria <- getActiveCriteria(xmcdaData)
  
  alternativesNames <- getValuesNames(alternatives$alternatives, getNamesFromIDs = FALSE)
  criteriaNames <- getValuesNames(criteria$criteria, getNamesFromIDs = FALSE)
  
  alternativesIds2Names <- createID2NameMapping(alternatives$alternativesIDs, alternativesNames, "alternatives")
  
  if(is.null(alternativesIds2Names))
  {
    return(NULL)
  }

  criteriaIds2Names <- createID2NameMapping(criteria$criteriaIDs, criteriaNames, "criteria")
  
  if(is.null(criteriaIds2Names))
  {
    return(NULL)
  }

  #parse preferences
  preferencesList <- getPreferences(xmcdaData, alternatives, alternativesIds2Names, programExecutionResult, xmcdaMessages)
  
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
  
  performanceMatrix.rownames <- sapply(rownames(performanceMatrix), function(x){
    if(alternativesIds2Names$has_key(x)){
      alternativesIds2Names$find(x)
    } else {
      putProgramExecutionResult(xmcdaMessages, 
                                errors = paste("Error when parsing performanceMatrix. Alternative", x, " from performanceMatrix doesn't exist."))
    }
  })
  rownames(performanceMatrix) <- performanceMatrix.rownames
  colnames(performanceMatrix) <- criteriaNames
  
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
    # convert names of criteria into numbers
    indices <- idToInt(names(criteriaValuesList$characteristicPoints))
    characteristicPoints[indices] <- c(criteriaValuesList$characteristicPoints)
  }
  
  #get program parameters
  programParameters <- getProgramParametersList(xmcdaData)
  
  #parse methodParameters.xml
  method <- ifelse(is.null(programParameters$methodSettings$methodName), 
                   "uta-g",
                   programParameters$methodSettings$methodName[[1]])

  #validate data and build problem
  problem <- buildProblem(performanceTable = performanceMatrix,
                          criteria = criteriaDirections,
                          characteristicPoints = characteristicPoints, 
                          strongPreferences = preferencesList$strong,
                          weakPreferences = preferencesList$weak,
                          indifferenceRelations = preferencesList$indifference)
  
  model <- buildModel(problem, method)
  model
}
