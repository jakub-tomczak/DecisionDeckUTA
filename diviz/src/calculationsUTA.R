#********************helpers.R********************

assert <- function(expression, message)
{
  if(!all(expression))
  {
    stop(if(is.null(message)) "Error" else message)
  }
}


#********************model.R********************

#### BUILDING MODEL

#' @export
buildModel <- function(problem, minEpsilon = 1e-4, method="utamp-1") { # includeEpsilonAsVariable,
  if(! (method %in% c("uta", "utamp-1", "utamp-2")))
  {
    stop("Method must be on of the following: `uta`, `utamp-1`, `utamp-2`")
  }
  nrAlternatives <- nrow(problem$performance)
  nrCriteria <- ncol(problem$performance)


  #preferences to model variables used in solution
  coefficientsMatrix <- calculateCoefficientsMatrix(problem)

  #when constructing problem we get into consideration only the number of characteristic points from value functions
  #we don't consider rho and k
  #here we add one variable that corresponds to rho value
  numberOfVariables <- problem$numberOfVariables + 2
  rhoIndex <- numberOfVariables - 1
  kIndex <- numberOfVariables
  #add rho column
  coefficientsMatrix <- cbind(coefficientsMatrix, 0)
  #add k column
  coefficientsMatrix <- cbind(coefficientsMatrix, 0)

  # constraints
  ## sum to 1
  lhs <- rep(0, numberOfVariables)

  for (j in seq_len(nrCriteria)) {
    if (problem$criteria[j] == 'g')
      lhs[problem$criteriaIndices[j] + problem$characteristicPoints[j] - 1] <- 1
    else
      lhs[problem$criteriaIndices[j]] <- 1
  }

  constraints <- list(lhs = lhs, dir = "==", rhs = 1)

  ## monotonicity of vf
  for (j in seq_len(nrCriteria)) {
    for (k in seq_len(problem$characteristicPoints[j] - 1)) {
      lhs <- rep(0, numberOfVariables)
      rhs <- 0

      if (problem$criteria[j] == "g") {
        lhs[problem$criteriaIndices[j] + k - 1] <- 1
        lhs[problem$criteriaIndices[j] + k] <- -1
      } else {
        lhs[problem$criteriaIndices[j] + k - 1] <- -1
        lhs[problem$criteriaIndices[j] + k] <- 1
      }

      if (problem$strictVF) {
        lhs[rhoIndex] <- 1
      }

      constraints <- combineConstraints(constraints,
                                        list(lhs = lhs, dir = "<=", rhs = rhs))
    }
  }

  constraints$variablesTypes <- rep("C", numberOfVariables) #continous criteria

  #remove least valuable characteristic points from coefficientMatri and criteria indices
  leastValuableCharacteristicPoints <- c()
  for(criterion in seq_len(nrCriteria)){
    if(problem$criteria[criterion] == 'g'){
      leastValuableCharacteristicPoints <- c(leastValuableCharacteristicPoints, problem$criteriaIndices[criterion])
    } else {
      leastValuableCharacteristicPoints <- c(leastValuableCharacteristicPoints, problem$criteriaIndices[criterion] + problem$characteristicPoints[criterion] - 1)
    }
  }
  problem$criteriaIndices <- createCriteriaIndices(problem, substractZeroCoefficients=TRUE)
  coefficientsMatrix <- coefficientsMatrix[,-leastValuableCharacteristicPoints]
  #remove as many variables as columns
  numberOfVariables <- numberOfVariables - length(leastValuableCharacteristicPoints)
  #update epsilion value
  rhoIndex <- numberOfVariables - 1
  kIndex <- numberOfVariables
  #update constraints - remove least valuable variables, update constraints types
  constraints$lhs <- constraints$lhs[, -leastValuableCharacteristicPoints]
  constraints$variablesTypes <- constraints$variablesTypes[-leastValuableCharacteristicPoints]


  ## building model
  model <- list(
    constraints = constraints,
    criteriaIndices = problem$criteriaIndices,
    rhoIndex = rhoIndex,
    kIndex = kIndex,
    chPoints = problem$characteristicPoints,
    preferencesToModelVariables = coefficientsMatrix,
    criterionPreferenceDirection = problem$criteria,
    generalVF = problem$generalVF,
    minEpsilon = minEpsilon,
    methodName = problem$methodName
  )

  # preference information
  #prefInfoIndex <- 1

  if (is.matrix(problem$strongPreference)) {
    for (k in seq_len(nrow(problem$strongPreference))) {
      alternative <- problem$strongPreference[k, 1]
      referenceAlternative <- problem$strongPreference[k, 2]
      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, preferenceType = "strong"))

      #model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      #prefInfoIndex <- prefInfoIndex + 1
    }
  }

  if (is.matrix(problem$weakPreference)) {
    for (k in seq_len(nrow(problem$weakPreference))) {
      alternative <- problem$weakPreference[k, 1]
      referenceAlternative <- problem$weakPreference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, preferenceType = "weak"))

      #model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      #prefInfoIndex <- prefInfoIndex + 1
    }
  }

  if (is.matrix(problem$indifference)) {
    for (k in seq_len(nrow(problem$indifference))) {
      alternative <- problem$indifference[k, 1]
      referenceAlternative <- problem$indifference[k, 2]

      model$constraints <- combineConstraints(model$constraints,
                                              buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                                model, preferenceType = "indifference"))

      #model$prefInfoToConstraints[[prefInfoIndex]] <- nrow(model$constraints$lhs)
      #prefInfoIndex <- prefInfoIndex + 1
    }
  }
  return(model)
}


#********************modelHelpers.R********************

#### HELPERS
calculateCoefficientsMatrix <- function(problem){
  numberOfColumns <- sum(problem$characteristicPoints)
  numberOfAlternatives <- nrow(problem$performance)
  numberOfCriteria <- ncol(problem$performance)

  characteristicPointsValues <- replicate(numberOfCriteria, c())
  for(j in seq_len(numberOfCriteria))
  {
    minV <- min(problem$performance[,j])
    maxV <- max(problem$performance[,j])
    characteristicPointsValues[[j]] <- seq(minV, maxV, length.out = problem$characteristicPoints[j])
  }

  coefficientsMatrix <- matrix(0, nrow=numberOfAlternatives, ncol=numberOfColumns)
  thresholds <- c("lower", "upper")

  for(criterion in seq_len(numberOfCriteria)){
    characteristicPoints <- characteristicPointsValues[[criterion]]
    interval <- characteristicPoints[2]-characteristicPoints[1]
    for(alternative in seq_len(numberOfAlternatives)){
      value <- problem$performance[alternative, criterion]
      for(threshold in thresholds){
        thresholdResult <- getCharacteristicPointValueAndIndex(value, threshold, characteristicPoints)
        alternativeCoefficient <- calculateCoefficient(value, thresholdResult$value, interval)
        criterionIndex <- problem$criteriaIndices[criterion] + (thresholdResult$index - 1 )

        coefficientsMatrix[alternative,  criterionIndex] <- alternativeCoefficient
      }
    }
  }
  coefficientsMatrix
}

getCharacteristicPointValueAndIndex <- function(value, typeOfBoundToFind, characteristicPoints){
  assert(typeOfBoundToFind %in% c("lower", "upper"), "Type to find should be one of the following: `lower`, `upper`.")
  assert(length(characteristicPoints) > 1, "There must be at least 2 characteristic points!")
  #lower and upper value must be lower than at least one element and greater than at least one element
  foundValue <- NULL

  if(typeOfBoundToFind == "upper" && value == max(characteristicPoints))
  {
    foundValue <- value
  } else if(typeOfBoundToFind == "lower" && value == min(characteristicPoints))
  {
    foundValue <- value
  } else {
    for(x in characteristicPoints){
      if(typeOfBoundToFind == "lower" && value > x) {
        foundValue <- x
      } else if(typeOfBoundToFind == "upper" && value < x){
        foundValue <- x
        break
      }
    }
  }

  if(is.null(foundValue))
    return(NULL)

  index <- match(foundValue, characteristicPoints)
  if(index > 0)
  {
    return(list(index=index, value=foundValue))
  }
  return(NULL)
}

calculateCoefficient <- function(value, thresholdValue, interval){
  distance <- thresholdValue - value
  #hack, avoid rounding numerical error that are lesser than the additionalEpsilon
  #they happen when alternative's value is at characteristic point
  additionalEpsilon <- 1e-9
  assert(abs(distance) <= interval + additionalEpsilon, "Distance between value and threshold cannot be higher then the interval.")
  1 - (abs(distance)/interval)
}

getLowerAndUpperValuesCoefficients <- function(value, minimalValue, intervalLength, direction, characteristicPointIndex){
  assert(direction %in% c("c", "g"), "Direction must be of type `c` or `g`.")

  #get the bounds of this chunk
  lowerValue = minimalValue + intervalLength * characteristicPointIndex
  upperValue = minimalValue + intervalLength * (characteristicPointIndex + 1)

  assert(value >= lowerValue && value <= upperValue, 'Value cannot be neither lower than lowerValue nor higher than upperValue')

  lowerValueCoeff <- if (value == lowerValue) 1.0 else 0.0
  upperValueCoeff <- if (value == upperValue) 1.0 else 0.0

  if(direction == "g") {
    #gain type
    #find the coefficients for the lower and upper bounds
    #U(value) = U(lowerValue) + (value-lowerValue)/(upperValue-lowerValue)*(U(upperValue)-U(lowerValue))=
    # = (value-lowerValue)/(upperValue-lowerValue)
    lowerValueCoeff = (lowerValue - value) / intervalLength + 1.0
    upperValueCoeff = (value - lowerValue) / intervalLength
  } else {
    #cost type
    lowerValueCoeff = (upperValue - value) / intervalLength
    upperValueCoeff = (value - upperValue) / intervalLength + 1.0
  }
  return(list(lowerValueCoeff=lowerValueCoeff, upperValueCoeff=upperValueCoeff))
}

getInterpolationCoefficients <- function(value, minimalValue, intervalLength){
  coeff <- (value - minimalValue)/intervalLength
  return(list(lowerCoefficient = 1-coeff, upperCoefficient = coeff))
}

#substractZeroCoefficients:
# TRUE == remove one column that responds for the characteristic point that has the worst value == 0
# FALSE == don't remve this column
createCriteriaIndices <- function(problem, substractZeroCoefficients){
  criteriaIndices <- c(1)
  zeroCoefficientsToSubstract <- if(substractZeroCoefficients) 1 else 0
  for(i in seq_len(ncol(problem$performance)-1))
  {
    criteriaIndices[i+1] <- criteriaIndices[i] + problem$characteristicPoints[i] - zeroCoefficientsToSubstract
  }
  criteriaIndices
}

buildPairwiseComparisonConstraint <- function(alternativeIndex, referenceAlternativeIndex, model, preferenceType) {
  stopifnot(preferenceType %in% c("weak", "strong", "indifference"))

  lhs <- model$preferencesToModelVariables[referenceAlternativeIndex,] - model$preferencesToModelVariables[alternativeIndex, ]
  dir <- "<="
  rhs <- 0

  if (preferenceType == "strong") {
    if (!is.null(model$kIndex)) {
      lhs[model$kIndex] <- 1
    } else {
      assert(!is.null(model$minEpsilon), "Model has not an epsilon and minEpsilon is not set.")
      rhs <- -model$minEpsilon
    }
  } else if (preferenceType == "indifference") {
    dir <- "=="
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs))
}

combineConstraints <- function(...) {
  allConst <- list(...)

  lhs <- c()
  dir <- c()
  rhs <- c()
  variablesTypes <- c()

  for (const in allConst) {
    if (!is.null(const)) {
      lhs <- rbind(lhs, const$lhs)
      dir <- c(dir, const$dir)
      rhs <- c(rhs, const$rhs)
      variablesTypes <- c(variablesTypes, const$variablesTypes)
    }
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs, variablesTypes = variablesTypes))
}

ua <- function(alternative, preferencesToModelVariables) {
  preferencesToModelVariables[alternative,]
}


#********************problem.R********************

#' @export
buildProblem <- function(performanceTable, criteria, characteristicPoints, strongPreferences = NULL,
                         weakPreferences = NULL , indifference = NULL, method = NULL)
{
  problem <- validateModel(performanceTable, criteria, strongPreferences,
                           weakPreferences, characteristicPoints, indifference, method)

  nrAlternatives <- nrow(problem$performance)
  #contains TRUE on indices corresponding to the criteria that has no characteristicPoints
  #keep this information in order to calculate the solution properly
  problem$generalVF <- problem$characteristicPoints == 0
  #if there is not stated how many characteristic points we've got on a criterion
  #assume that there are as many as alternatives (each alternative is a characteristic point)
  problem$characteristicPoints <- sapply(problem$characteristicPoints, function(x) {
    if(x == 0)
      nrAlternatives
    else
      x
  })
  #only the sum of characteristic points from criteria except the least valuable point from each criterion
  problem$numberOfVariables <- sum(problem$characteristicPoints)
  problem$criteriaIndices <- createCriteriaIndices(problem, substractZeroCoefficients=FALSE)

  return(problem)
}

validateModel <- function(performanceTable, criteria, strongPreferences,
                          weakPreferences, characteristicPoints, indifference, method)
{
  assert(is.matrix(performanceTable), "PerformanceTable must be a matrix.")

  numberOfPreferences <- nrow(performanceTable)
  numberOfCriterions <- ncol(performanceTable)
  assert(ncol(criteria) == numberOfCriterions, "Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  assert(all(criteria %in% c("c", "g")), "Criteria must be of type `c` or `g`.")
  #what if a,b,c,d
  #aPb, aPc, bPc, aPd, bPd, cPd
  #stopifnot(ncol(strongPreferences) <= numberOfPreferences) #stops execution if number of strong preference pairs is greater than number of performanceTable

  strongPreferences <- validatePreferenceRelation(strongPreferences, numberOfPreferences)
  weakPreferences <- validatePreferenceRelation(weakPreferences, numberOfPreferences)
  indifference <- validatePreferenceRelation(indifference, numberOfPreferences)

  assert(is.matrix(indifference), "Indifference must be a matrix")

  #validate method name
  if(!is.null(method))
  {
    assert(method %in% c('uta-g', 'utamp-1', 'utamp-2'), "Method must be one of the following: 'uta-g', 'utamp-1', 'utamp-2'")
  }

  return (list(
    performanceTable = performanceTable,
    criteria = criteria,
    characteristicPoints = characteristicPoints,
    strongPreferences = strongPreferences,
    weakPreferences = weakPreferences,
    indifference = indifference,
    strictVF = TRUE,
    methodName = method
  ))
}

validatePreferenceRelation <- function(preferenceRelation, numberOfPreferences)
{
  if(!is.matrix(preferenceRelation) || is.null(preferenceRelation))
  {
    return (matrix(nrow=0, ncol=2))
  }

  assert(ncol(preferenceRelation) == 2, "Preference relation must be given in pairs.")

  #check weather minimum and maximum index are in performanceTable indices bounds
  assert(min(preferenceRelation) >= 1, "There is no preference with index lower than 1")
  assert(max(preferenceRelation) <= numberOfPreferences, paste("There is no preference with index higher than", numberOfPreferences))

  return (preferenceRelation)
}


#********************solver.R********************

#Solver method used when model$methodName is set
#' @export
solveProblem <- function(model, allowInconsistency = FALSE)
{
  if(is.null(model$methodName)){
    stop("Method name is not set. Set problem$method or model$methodName.")
  }

  if(model$methodName == "uta-g")
  {
    utag(model, allowInconsistency)
  } else if(model$methodName == "utamp-1")
  {
    utamp1(model, allowInconsistency)
  } else if(model$methodName == "utamp-2")
  {
    utamp2(model, allowInconsistency)
  } else {
    stop(paste("Method", model$method, "is not available."))
  }
}
#UTA-G
#' @export
utag <- function(model, allowInconsistency = FALSE)
{
  nrCriteria <- length(model$criterionPreferenceDirection)
  nrAlternative <- nrow(model$preferencesToModelVariables)
  partialUtilityValues <- matrix(0, nrow = nrAlternative, ncol = nrCriteria)
  methodResult <- list()

  for (j in seq_len(nrCriteria))
  {
    if(model$criterionPreferenceDirection[j] == 'c') {
      extremizedCriterionIndex <- model$criteriaIndices[j]
    } else {
      extremizedCriterionIndex <- model$criteriaIndices[j] + model$chPoints[j] - 2
    }
    objective <- createObjective(model$constraints$lhs, extremizedCriterionIndex)
    solutionMin <- extremizeVariable(objective, model$constraints, maximize=FALSE)
    solutionMax <- extremizeVariable(objective, model$constraints, maximize=TRUE)
    #add appropriate from min and max solution
    partialUtilityValues[, j] <- partialUtilityValues[, j] + calculateUtilityValuesOnCriterion(model, solutionMin$solution, j)
    partialUtilityValues[, j] <- partialUtilityValues[, j] + calculateUtilityValuesOnCriterion(model, solutionMax$solution, j)
  }
  #divide each column by the number of criteria
  partialUtilityValues <- apply(partialUtilityValues, MARGIN = 2, function(x){
    x / 2*nrCriteria
  })

  #calculate global utility values = sum values by rows = sum partial utility values for each alternative
  utilityValues <- apply(partialUtilityValues, MARGIN = 1, function(x){
    sum(x)
  })
  methodResult$localUtilityValues <- partialUtilityValues
  methodResult$ranking <- generateRanking(utilityValues)
  methodResult
}

#UTAMP-1
#' @export
utamp1 <- function(model, allowInconsistency = FALSE) {
  objectiveIndex <- c(model$kIndex)
  if (is.null(objectiveIndex)) {
    stop("Use function buildModel with includeEpsilonAsVariable = TRUE.")
  }

  objectiveIndex <- c(model$kIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize=TRUE)
  methodResult <- list()

  if(validateSolution(solution, allowInconsistency)){
    methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
    globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
    methodResult$ranking <- generateRanking(globalUtilityValues)
    #method specific functionality

    methodResult$epsilon <- solution$solution[objectiveIndex]

  }
  methodResult
}

#UTAMP-2
#' @export
utamp2 <- function(model, allowInconsistency = FALSE) {
  objectiveIndex <- c(model$rhoIndex, model$kIndex)

  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize = TRUE)
  methodResult <- list()

  if(validateSolution(solution, allowInconsistency)){
    methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
    globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
    methodResult$ranking <- generateRanking(globalUtilityValues)
    #method specific functionality
    methodResult$k <- solution$solution[model$kIndex]
    methodResult$rho <- solution$solution[model$rhoIndex]
  }
  methodResult
}


#********************solverHelpers.R********************

#' @import Rglpk
extremizeVariable <- function(objective, constraints, maximize) {
  Rglpk_solve_LP(objective, constraints$lhs, constraints$dir, constraints$rhs, max = maximize,
                 types = constraints$variablesType)
}

createObjective <- function(numberOfConstraints, extremizedVariableIndex){
  assert(!is.null(extremizedVariableIndex), 'Variable to extremize is NULL')
  obj <- rep(0, ncol(numberOfConstraints))
  obj[extremizedVariableIndex] <- 1
  obj
}

#this function takes model and values variable that is a vector with
#LP problem solutions
#comprehensiveValue is a vector containing on each index a
#dot product of a marginal value function's coefficient and an alternative's coefficient
generateRanking <- function(utilityValues) {
  nrAlternatives <- length(utilityValues)
  ranking <- sort(utilityValues, decreasing = TRUE, index.return = TRUE)
  rankingMatrix <- matrix(utilityValues)

  alternativeToRanking <- rep(0, nrAlternatives)
  for(x in seq_len(nrAlternatives)){
    alternativeToRanking[ranking$ix[x]] <- x
  }
  rankingMatrix <- cbind(c(1:nrAlternatives), rankingMatrix, alternativeToRanking)
  colnames(rankingMatrix) <- c("alternativeNo.", "utilityValue", "ranking")
  rankingMatrix
}

#values attained from LP solution
#number of these values corresponds to the number of columns in the model$preferencesToModelVariables
calculateUtilityValues <- function(model, values){
  # v1 %*% v2 => dot product of v1 and v2
  # alternative are in rows, columns represents criteria, values are utility values of the alternative on that criterion
  utilityValues <- sapply(seq_len(length(model$criteriaIndices)), function(y){
    calculateUtilityValuesOnCriterion(model, values, y)
  })
}

#allows multiplying
calculateUtilityValuesOnCriterion <- function(model, value, criterionNumber){
  sapply(seq_len(nrow(model$preferencesToModelVariables)), function(alternative)
  {
    #from and to are used to index a criterion's marginal values coefficients on a preferences matrix
    from <- model$criteriaIndices[criterionNumber]
    #minus (1 - beacuse of the fact we ommit the least value characterisitc point, 1 - last index is included)
    to <- from + model$chPoints[criterionNumber]-2

    if(length(value) == 1){
      #multiplying by number
      sum(model$preferencesToModelVariables[alternative, from:to] * value)
    } else{
      assert(ncol(model$preferencesToModelVariables) == length(value),
             "If value is a vector it must represent the same number of coefficients as in the preferencesToModelVariables.")
      #multiplying by vector
      model$preferencesToModelVariables[alternative, from:to] %*% value[from:to]
    }
  })
}

validateSolution <- function(solution, allowInconsistency){
  if(is.null(solution))
  {
    stop("Solution object is empty.")
  }
  else if ((solution$status == 0 && solution$optimum >= model$minEpsilon) || allowInconsistency)
  {
    return(TRUE)
  }
  else if(solution$status != 0)
  {
    stop("Soultion hasn't been found.")
  }
  else if(solution$status == 0 && solution$optimum < model$minEpsilon)
  {
    stop("Solution has been found but optimum is lower than minEpsilon value.")
  }
  else if(!allowInconsistency)
  {
    stop("Inconsistency is not allowed.")
  }
  else
  {
    print("Unknown error.")
    return(FALSE)
  }
}


