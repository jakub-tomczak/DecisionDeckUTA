#********************helpers.R********************

getAvailableMethods <- function(){
  list(utag = "uta-g", utamp1 = "utamp-1", utamp2 = "utamp-2", roruta = "roruta")
}

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
buildModel <- function(problem, method, minK = 1e-4, minEpsilon = 1e-4, bigNumber = 1e9) { # includeEpsilonAsVariable,
  includeRho <- TRUE
  includeK <- TRUE

  availableMethods <- getAvailableMethods()
  assert(method %in% availableMethods, paste(availableMethods, " "))
  nrAlternatives <- nrow(problem$performance)
  nrCriteria <- ncol(problem$performance)


  #preferences to model variables used in solution
  coefficientsMatrix <- calculateCoefficientsMatrix(problem)

  #when constructing problem we get into consideration only the number of characteristic points from value functions
  #we don't consider rho and k
  #here we add one variable that corresponds to rho value
  numberOfVariables <- problem$numberOfVariables + ifelse(includeRho, 1, 0) + ifelse(includeK, 1, 0)
  rhoIndex <- NULL
  kIndex <- NULL
  if(includeRho)
    rhoIndex <- numberOfVariables-1
  if(includeK)
    kIndex <- numberOfVariables

  # constraints
  constraints <- list()
  ## sum to 1
  constraints <- combineConstraints(constraints,
                                    normalizationConstraint(problem, numberOfVariables, nrCriteria))

  ## least valuable characteristic points should be equal 0
  constraints <- combineConstraints(constraints,
                                    leastValuableChPointsEqualZero(problem, numberOfVariables, nrCriteria))

  ## monotonicity of vf
  constraints <- combineConstraints(constraints,
                                    monotonicityConstraints(problem, numberOfVariables, nrCriteria, rhoIndex))

  lhs.colnames <- colnames(coefficientsMatrix)
  if(includeRho)
  {
    lhs.colnames <- c(lhs.colnames, "rho")
  }
  if(includeK)
  {
    lhs.colnames <- c(lhs.colnames, "k")
  }

  colnames(constraints$lhs) <- lhs.colnames
  #criteria of a continous type
  constraints$variablesTypes <- rep("C", ncol(constraints$lhs))

  # update criteria indices, before removing the leastValuableCharacteristicPoints
  problem$criteriaIndices <- getCriteriaIndices(problem, substractZeroCoefficients=FALSE)

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
    methodName = method,
    performances = problem$performanceTable
  )

  # preference information
  #prefInfoIndex <- 1
  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "strong"))

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "weak"))

  model$constraints <- combineConstraints(model$constraints,
                                          pairwisePreferenceConstraints(problem, model, "indifference"))

  # method specific actions
  if(method == availableMethods$utag){
    # exchange k variable with a small positive constant
    # get indices of all constraints that use k
    constraintsWithKIndex <- which(model$constraints$lhs[, model$kIndex] %in% 1)
    model <- removeColumnsFromModelConstraints(model, model$kIndex)
    if(length(constraintsWithKIndex) > 0){
      model$constraints$rhs[constraintsWithKIndex] = minK
    }
  } else if(method == availableMethods$roruta){
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "strong"))

    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "weak"))
    model$constraints <- combineConstraints(model$constraints,
                                            intensitiesConstraints(problem, model, "indifference"))
    # remove rho
    model <- removeColumnsFromModelConstraints(model, model$rhoIndex)

    # rename k to epsilon
    model$epsilonIndex <- model$kIndex
    # remove k index
    model$kIndex <- NULL

    # rank requirements
    # add constraints for the desiredRank and desiredUtilityValue
    # desiredUtilityValue matters only when there is at least one row in the desiredRank
    if(!is.null(problem$desiredRank) && nrow(problem$desiredRank) > 0)
    {
      result <- createRankRelatedConstraints(problem, model, minEpsilon, bigNumber)
      desiredRankConstraints <- result$constraints
      model$rankConstraintsFirstBinaryVariableIndices <- result$rankConstraintsFirstBinaryVariableIndices

      if(is.null(colnames(model$constraints$lhs))){
        col.names <- c(rep("-", ncol(model$constraints$lhs)), desiredRankConstraints$variables.labels)
      } else {
        col.names <- c(colnames(model$constraints$lhs), desiredRankConstraints$variables.labels)
      }
      model$constraints$constraints.labels <- c(model$constraints$constraints.labels, desiredRankConstraints$constraints.labels)
      # augment model's lhs to fit desiredRankConstraints
      numberOfColumnsToAddToOldConstraints <- ncol(desiredRankConstraints$lhs) - ncol(model$constraints$lhs)
      matrixToAdd <- matrix(0, nrow = nrow(model$constraints$lhs), ncol = numberOfColumnsToAddToOldConstraints)
      model$constraints$lhs <- cbind(model$constraints$lhs, matrixToAdd)

      # now we can merge desiredRankConstraints with model's
      # LHS
      model$constraints$lhs <- rbind(model$constraints$lhs, desiredRankConstraints$lhs)
      colnames(model$constraints$lhs) <- col.names
      # RHS
      model$constraints$rhs <- c(model$constraints$rhs, desiredRankConstraints$rhs)
      # dir
      model$constraints$dir <- c(model$constraints$dir, desiredRankConstraints$dir)
      model$constraints$variablesTypes <- desiredRankConstraints$variablesTypes
    }
  } else if(method == availableMethods$utamp1){
    model$constraints <- splitVariable(model, model$kIndex)
  } else if(method == availableMethods$utamp2) {
    # split rho into rho_jk
    model$constraints <- splitVariable(model, model$rhoIndex)
    model$constraints <- splitVariable(model, model$kIndex)
  }

  rownames(model$constraints$lhs) <- model$constraints$constraints.labels

  return(model)
}


#********************modelHelpers.R********************

#### HELPERS
analysePositionInRanking <- function(model, alternative, rankType){
  assert(rankType %in% c("min", "max"), "rankType may be either min or max.")
  hasAlternativeRankRequirementsInModel <- !is.na(model$rankConstraintsFirstBinaryVariableIndices[alternative])
  nrAlternatives <- nrow(model$preferencesToModelVariables)

  rankingAnalysisConstraints <-
    createConstraintsForExtremeRankAnalysis(model, alternative, rankType, hasAlternativeRankRequirementsInModel)

  #merge new constraints with the model's ones
  constraints <- model$constraints
  if(!hasAlternativeRankRequirementsInModel){
    # add some columns to lhs
    numberOfAdditionalColumns <- ncol(rankingAnalysisConstraints$lhs) - ncol(constraints$lhs)
    constraints$lhs <- cbind(constraints$lhs, matrix(0, nrow=nrow(constraints$lhs), ncol=numberOfAdditionalColumns))
  }

  constraints <- combineConstraints(constraints, rankingAnalysisConstraints)

  objective <- rep(0, ncol(constraints$lhs))
  startIndex <- 0

  if(hasAlternativeRankRequirementsInModel){
    objectiveIndices <-
      seq(ifelse(rankType=="min", 0, 1), 2*nrAlternatives-1, 2) + model$rankConstraintsFirstBinaryVariableIndices[alternative]
    objective[objectiveIndices] <- 1
    # don't set 1 in lhs on the current alternative's indices
    objective[model$rankConstraintsFirstBinaryVariableIndices[alternative]+2*(alternative-1)+ifelse(rankType=="min", 0, 1)] <- 0
    startIndex <- model$rankConstraintsFirstBinaryVariableIndices[alternative]
    stopIndex <- startIndex + 2*nrAlternatives - 1
  } else {
    startIndex <- ncol(constraints$lhs)-(nrAlternatives-1)
    objective[startIndex:ncol(constraints$lhs)] <- 1
    objective[startIndex+alternative-1] <- 0
    stopIndex <- startIndex + nrAlternatives - 1
  }
  # solve
  solution <- extremizeVariable(objective, constraints, maximize=FALSE)
  if(validateSolution(solution, allowInconsistency = TRUE)){
    return(sum(solution$solution[startIndex:stopIndex]))
  }
  NULL
}

createConstraintsForExtremeRankAnalysis <- function(model, alternative, rankType, hasAlternativeRankRequirementsInModel){
  constraints <- list()
  nrAlternatives <- nrow(model$preferencesToModelVariables)
  modelsNumberOfVariables <- ncol(model$constraints$lhs)
  for(i in 1:nrAlternatives){
    if(i != alternative){

      lhs <- rep(0, modelsNumberOfVariables)
      bPositionInLHS <- 0

      if(rankType == "min"){
        # min
        utilityValuesDifference <- substractUtilityValuesOfAlternatives(alternativeIndex = alternative,
                                                                        referenceAlternativeIndex = i,
                                                                        model)

        if(hasAlternativeRankRequirementsInModel){
          bPositionInLHS <- model$rankConstraintsFirstBinaryVariableIndices[alternative] + (i-1)*2
        } else {
          # augment LHS to represent alternative's v_b
          lhs <- c(lhs, rep(0, nrAlternatives))
          bPositionInLHS <- modelsNumberOfVariables + i
        }

        constraints.labels <- paste("extremeRank_v_>", alternative, ",", i, sep="")
      } else {
        # max
        utilityValuesDifference <- substractUtilityValuesOfAlternatives(alternativeIndex = i,
                                                                        referenceAlternativeIndex = alternative,
                                                                        model)
        # put U(a) - U(b) into the LHS
        lhs[1:length(utilityValuesDifference)] <- utilityValuesDifference


        if(hasAlternativeRankRequirementsInModel){
          bPositionInLHS <- model$rankConstraintsFirstBinaryVariableIndices[alternative] + (i-1)*2 + 1
        } else {
          lhs <- c(lhs, rep(0, nrAlternatives))
          bPositionInLHS <- modelsNumberOfVariables + i
        }

        constraints.labels <- paste("extremeRank_v_<", alternative, ",", i, sep="")
      }

      lhs[1:length(utilityValuesDifference)] <- utilityValuesDifference
      lhs[bPositionInLHS] <- 1
      dir <- ">="
      rhs <- model$minEpsilon
      constraints <- combineConstraints(constraints, list(
        lhs = lhs,
        rhs = rhs,
        dir = dir,
        constraints.labels = constraints.labels
      ))
    }
  }

  variablesTypes <- c()
  if(hasAlternativeRankRequirementsInModel) {
    colnames(constraints$lhs) <- colnames(model$constraints$lhs)
  } else {
    newColnames <- paste("v_", rankType, "_", alternative, "_", 1:nrAlternatives, sep="")
    colnames(constraints$lhs) <- c(colnames(model$constraints$lhs), newColnames)
    variablesTypes <- rep("B", nrAlternatives)
  }

  constraints$variablesTypes <- variablesTypes
  rownames(constraints$lhs) <- constraints$constraints.labels

  constraints
}

checkPreferenceRelationFeasibility <- function(model, alternative, referenceAlternative, relationType){
  assert(relationType %in% c("possible", "necessary"),
         "Only necessary and weak preference relations are possible")
  assert(!is.null(model$epsilonIndex), "Epsilon index must be included in a model")

  objective <- createObjective(model$constraints$lhs, model$epsilonIndex)

  constraints <- createModelForNecessaryAndPossibleRelationAnalysis(model, alternative, referenceAlternative, relationType)

  solution <- extremizeVariable(objective, constraints, maximize=TRUE)

  if(relationType == "necessary"){
    return(solution$status != 0 || solution$optimum < model$minEpsilon)
  } else if(relationType == "possible") {
    return(solution$status == 0 && solution$optimum >=model$minEpsilon)
  }
}

createModelForNecessaryAndPossibleRelationAnalysis <- function(model, alternative, referenceAlternative, relationType){
  constraints <- model$constraints

  constraints <- combineConstraints(constraints,
                                    buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                      model, preferenceType = relationType))
  constraints
}

createRankRelatedConstraints <- function(problem, model, minEpsilon, bigNumber=1e9){
  nrAlternatives <- nrow(problem$performanceTable)
  numberOfCriteria <- length(problem$criteria)
  numberOfDesiredRankConstraints <- nrow(problem$desiredRank)
  # number of constraints generated by the rank requirements, this +numberOfDesiredRankConstraints
  # stands for binary variables normalization (v_> + v_< <= 1)
  numberOfConstraints <- numberOfDesiredRankConstraints*( 3*(nrAlternatives - 1) + 2)
  # here we include binary variables that represents relations between the same alternative (v_{x,x})
  # they will be removed afterwards
  numberOfBinaryVariables <- 2*nrAlternatives*numberOfDesiredRankConstraints
  # needed to determine the number of the columns in the new lhs
  currentLHSColumnsNumber <- ncol(model$constraints$lhs)

  totalNumberOfColumnsInLHS <- numberOfBinaryVariables + ncol(model$constraints$lhs)

  # here we store indices that corresponds to the first binary variable associated with a corresponding alternative
  # to get an index of the first binary variable associated with a n-th alternative
  # use rankConstraintsFirstBinaryVariableIndices[n]
  # if returned value is NA there are no rank requirements related to the n'th alternative
  rankConstraintsFirstBinaryVariableIndices <- matrix(nrow=4)

  desiredRankConstraintsLHS <- c()
  desiredRankConstraintsRHS <- c()

  #constraints labels
  constraints.labels <- c()
  #variables labels
  variables.labels <- c()
  variables.labels <- c()
  for(alternative in problem$desiredRank[, 1]){
    for(referenceAlternative in seq(nrAlternatives))
    {
      variables.labels <- c(variables.labels, paste("v_", c(">_", "<_"), alternative, "_", referenceAlternative, sep=""))
    }
  }


  for(i in seq(numberOfDesiredRankConstraints))
  {
    alternative <- problem$desiredRank[i, 1]
    lowerPlace <- problem$desiredRank[i, 2]
    higherPlace <- problem$desiredRank[i, 3]

    rankConstraintsFirstBinaryVariableIndices[alternative] <- (i - 1)*2*nrAlternatives + 1
    # constraints:
    # U(a*) - U(b) + M*v >= epsilon for each b from A, which is not a
    # and
    # U(b) - U(a*) + M*v >= epsilon for each b from A, which is not a

    for(referenceAlternative in seq(nrAlternatives)){
      if(referenceAlternative != alternative)
      {
        for(type in c("lower", "upper"))
        {
          binaryVariablePosition <- (i - 1)*2*nrAlternatives +  ifelse(type == "lower",  2*referenceAlternative-1, 2*referenceAlternative)
          utilityValueDifference <- rep(0, currentLHSColumnsNumber)
          difference <- substractUtilityValuesOfAlternatives(alternative, referenceAlternative, model)
          # create a constraint row, difference can be a subvector if there were variables representing no characteristic points variables
          utilityValueDifference[1:length(difference)] <- difference
          # add epsilon
          utilityValueDifference[model$epsilonIndex] <- 1

          binaryVariables <- rep(0, numberOfBinaryVariables)
          binaryVariables[binaryVariablePosition] <- -bigNumber

          LHS <- c(utilityValueDifference, binaryVariables)
          desiredRankConstraintsLHS <- rbind(desiredRankConstraintsLHS, LHS)

          # RHS
          desiredRankConstraintsRHS <- c(desiredRankConstraintsRHS, 0)

          if(type == "lower"){
            constraints.labels <- c(constraints.labels, paste(alternative, "-", referenceAlternative))
          } else {
            constraints.labels <- c(constraints.labels, paste(referenceAlternative, "-", alternative))
          }
        }
        # sum of binary variables v_> + v_< <= 1 constraint
        activeBinaryVariables <- rep(0, numberOfBinaryVariables)
        pos <- (i - 1)*2*nrAlternatives + c(2*(referenceAlternative-1)+1, 2*(referenceAlternative-1)+2)
        activeBinaryVariables[pos] <- 1
        LHS <- c(rep(0, currentLHSColumnsNumber), activeBinaryVariables)
        desiredRankConstraintsLHS <- rbind(desiredRankConstraintsLHS, LHS)
        desiredRankConstraintsRHS <- c(desiredRankConstraintsRHS, 1)
        constraints.labels <- c(constraints.labels, paste("v_", alternative, "_",referenceAlternative,sep=""))
      }
    }
    # sum of binary variables constraints
    for(type in c("lower", "upper"))
    {
      # LHS
      sign <- ifelse(type == "lower", ">", "<")
      binaryVariables <- rep(0, numberOfBinaryVariables)
      startIndex <- (i - 1)*2*nrAlternatives + ifelse(type=="lower", 1, 2)
      indicesForSum <- seq(startIndex, startIndex + 2*nrAlternatives - 1, 2)
      binaryVariables[indicesForSum] <- 1
      LHS <- c(rep(0, currentLHSColumnsNumber), binaryVariables)
      desiredRankConstraintsLHS <- rbind(desiredRankConstraintsLHS, LHS)

      # RHS
      RHS <- ifelse(type=="lower", lowerPlace - 1, nrAlternatives - higherPlace)
      desiredRankConstraintsRHS <- c(desiredRankConstraintsRHS, RHS)

      constraints.labels <- c(constraints.labels, paste("sum_v_", sign, "a,b", sep = ""))
    }
  }
  # shift indices of first binary variables by the number of variables in the current lhs
  rankConstraintsFirstBinaryVariableIndices <- rankConstraintsFirstBinaryVariableIndices + currentLHSColumnsNumber

  # here we should combine constraints
  dir <- rep("<=", numberOfConstraints)
  variablesTypes <- c(rep("C", currentLHSColumnsNumber), rep("B", numberOfBinaryVariables))

  rownames(desiredRankConstraintsLHS) <- constraints.labels
  colnames(desiredRankConstraintsLHS) <- c(rep("-", ncol(model$constraints$lhs)), variables.labels)

  list(
    rankConstraintsFirstBinaryVariableIndices = rankConstraintsFirstBinaryVariableIndices,
    constraints = list(
      lhs = desiredRankConstraintsLHS,
      rhs = desiredRankConstraintsRHS,
      dir = dir,
      variablesTypes = variablesTypes,
      variables.labels = variables.labels,
      constraints.labels = constraints.labels
    )
  )
}

normalizationConstraint <- function(problem, numberOfVariables, numberOfCriteria){
  ## sum to 1
  lhs <- rep(0, numberOfVariables)

  for (j in seq_len(numberOfCriteria)) {
    if (problem$criteria[j] == 'g')
      lhs[problem$criteriaIndices[j] + problem$characteristicPoints[j] - 1] <- 1
    else
      lhs[problem$criteriaIndices[j]] <- 1
  }

  list(lhs = lhs,
       dir = "==",
       rhs = 1,
       constraints.labels = "norm")
}

leastValuableChPointsEqualZero <- function(problem, numberOfVariables, numberOfCriteria){
  constraints <- list()

  for(criterion in seq_len(numberOfCriteria)){
    lhs <- rep(0, numberOfVariables)
    chPointIndex <- ifelse(problem$criteria[criterion] == 'g',
                           problem$criteriaIndices[criterion],
                           problem$criteriaIndices[criterion] + problem$characteristicPoints[criterion] - 1)

    lhs[chPointIndex] <- 1
    rhs <- 0
    dir <- "=="
    constraints.labels <- paste("worstChPoint@",criterion,"=0", sep="")

    constraints <- combineConstraints(constraints,
                                      list(lhs = lhs,
                                           dir = dir,
                                           rhs = rhs,
                                           constraints.labels = constraints.labels))
  }
  constraints
}

monotonicityConstraints <- function(problem, numberOfVariables, numberOfCriteria, rhoIndex){
  ## monotonicity of vf
  constraints <- list()
  for (j in seq_len(numberOfCriteria)) {
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
                                        list(lhs = lhs,
                                             dir = "<=",
                                             rhs = rhs,
                                             constraints.labels = paste("mono_", k, "_", k+1, sep="")))
    }
  }
  constraints
}

intensitiesConstraints <- function(problem, model, typeOfRelation){
  assert(typeOfRelation %in% c("strong", "weak", "indifference"),
         paste("typeOfRelation", typeOfRelation, "is not valid. Valid types of intensities are: strong, weak, indifference"))
  availableMethods <- getAvailableMethods()
  assert(model$methodName == availableMethods$roruta, paste("So far, only the `", availableMethods$roruta ,"` method supports intensities relation."))

  constraints <- list()
  relationsMatrix <- NULL
  if(typeOfRelation == "strong"){
    relationsMatrix <- problem$strongIntensitiesPreferences
  } else if(typeOfRelation == "weak") {
    relationsMatrix <- problem$weakIntensitiesPreferences
  } else if(typeOfRelation == "indifference") {
    relationsMatrix <-problem$indifferenceIntensitiesRelations
  }

  if (is.matrix(relationsMatrix)) {
    for (k in seq_len(nrow(relationsMatrix))) {
      alternatives <- relationsMatrix[k, c(1,2)]
      referenceAlternatives <- relationsMatrix[k, c(3,4)]
      constraints <- combineConstraints(constraints,
                                        buildPairwiseComparisonConstraint(alternatives, referenceAlternatives,
                                                                          model, preferenceType = typeOfRelation))

    }
  }
  constraints
}

pairwisePreferenceConstraints <- function(problem, model, typeOfPreference){
  assert(typeOfPreference %in% c("strong", "weak", "indifference"),
         paste("typeOfPreference", typeOfPreference, "is not valid. Valid types of pairwise preferences are: strong, weak, indifference"))

  preferenceMatrix <- matrix()
  if(typeOfPreference == "strong")
  {
    preferenceMatrix <- problem$strongPreferences
  } else if(typeOfPreference == "weak") {
    preferenceMatrix <- problem$weakPreferences
  } else if(typeOfPreference == "indifference") {
    preferenceMatrix <- problem$indifferenceRelations
  } else {
    return(list())
  }
  constraints <- list()

  if (is.matrix(preferenceMatrix)) {
    for (k in seq_len(nrow(preferenceMatrix))) {
      alternative <- preferenceMatrix[k, 1]
      referenceAlternative <- preferenceMatrix[k, 2]
      constraints <- combineConstraints(constraints,
                                        buildPairwiseComparisonConstraint(alternative, referenceAlternative,
                                                                          model, preferenceType = typeOfPreference))

    }
  }
  constraints
}

calculateCoefficientsMatrix <- function(problem){
  numberOfColumns <- sum(problem$characteristicPoints)
  numberOfAlternatives <- nrow(problem$performance)
  numberOfCriteria <- ncol(problem$performance)

  characteristicPointsValues <- replicate(numberOfCriteria, c())
  coefficientsMatrix.colnames <- c()
  for(j in seq_len(numberOfCriteria))
  {
    minV <- min(problem$performance[,j])
    maxV <- max(problem$performance[,j])
    characteristicPointsValues[[j]] <- seq(minV, maxV, length.out = problem$characteristicPoints[j])
    coefficientsMatrix.colnames <- c(coefficientsMatrix.colnames, paste("x_", j, "_", 1:problem$characteristicPoints[j], sep=""))
  }

  coefficientsMatrix <- matrix(0, nrow=numberOfAlternatives, ncol=numberOfColumns)
  colnames(coefficientsMatrix) <- coefficientsMatrix.colnames
  rownames(coefficientsMatrix) <- rownames(problem$performanceTable)
  thresholds <- c("lower", "upper")

  for(criterion in seq_len(numberOfCriteria)){
    characteristicPoints <- characteristicPointsValues[[criterion]]
    interval <- characteristicPoints[2] - characteristicPoints[1]
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
getCriteriaIndices <- function(problem, substractZeroCoefficients){
  criteriaIndices <- c(1)
  zeroCoefficientsToSubstract <- if(substractZeroCoefficients) 1 else 0
  for(i in seq_len(ncol(problem$performance)-1))
  {
    criteriaIndices[i+1] <- criteriaIndices[i] + problem$characteristicPoints[i] - zeroCoefficientsToSubstract
  }
  criteriaIndices
}

substractUtilityValuesOfAlternatives <- function(alternativeIndex, referenceAlternativeIndex, model){
  # it returns U(referenceAlternative) - U(alternative)
  # alternativeIndex P referenceAlternativeIndex
  model$preferencesToModelVariables[referenceAlternativeIndex,] - model$preferencesToModelVariables[alternativeIndex, ]
}

buildPairwiseComparisonConstraint <- function(alternativeIndex, referenceAlternativeIndex, model, preferenceType) {
  # TODO test for that assert
  assert(preferenceType %in% c("strong", "weak", "indifference", "necessary", "possible"),
         paste("preferenceType", preferenceType, "is not valid. Valid values are: strong, weak, indifference, necessary, possible"))
  # TODO test for that assert
  assert(length(alternativeIndex) == length(referenceAlternativeIndex),
         paste("alternativeIndex (size=",length(alternativeIndex),") must be a vector of the same size as refernceAlternativeIndex (size=",length(referenceAlternativeIndex),")", sep=""))
  # TODO test for that assert
  assert(length(alternativeIndex) == 1 || length(alternativeIndex) == 2,
         paste("Relations arity must be equal 1 or 2, got", length(alternativeIndex),".", sep=""))

  isIntensityRelation <- length(alternativeIndex) == 2

  marginalValuesVariables <- c()
  if(isIntensityRelation){
    # -a + b + (-d + c)
    marginalValuesVariables <- substractUtilityValuesOfAlternatives(alternativeIndex[1], alternativeIndex[2], model) +
      substractUtilityValuesOfAlternatives(referenceAlternativeIndex[1], referenceAlternativeIndex[2], model)
  } else {
    marginalValuesVariables <- substractUtilityValuesOfAlternatives(alternativeIndex, referenceAlternativeIndex, model)
  }
  # lhs holds a vector of a length equal to the number of marginal values
  # lhs should be a vector of the length of a number of columns in constraints matrix
  lhs <- rep(0, ncol(model$constraints$lhs))
  lhs[0:length(marginalValuesVariables)] <- marginalValuesVariables

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
  } else if (preferenceType == "necessary") {
    dir <- ">="
    lhs[model$epsilonIndex] <- -1
  } else if (preferenceType == "possible") {
    # U(a) - U(b) >= 0 <==> U(b) - U(a) <= 0
    # do nothing
  }

  # use collapse parameter in case of intensity type constraints
  constraints.labels.alternative <- paste(alternativeIndex, collapse = ",")
  constraints.labels.refAlternative <- paste(referenceAlternativeIndex, collapse = ",")

  # substr(preferenceType, 1, 3)
  constraints.labels <- paste(preferenceType, "_rel_", constraints.labels.alternative, "_", constraints.labels.refAlternative, sep="")

  return (list(lhs = lhs, dir = dir, rhs = rhs, constraints.labels = constraints.labels))
}

combineConstraints <- function(...) {
  allConst <- list(...)

  lhs <- c()
  dir <- c()
  rhs <- c()
  variablesTypes <- c()
  constraints.labels <-c()

  for (const in allConst) {
    if (!is.null(const)) {
      lhs <- rbind(lhs, const$lhs)
      dir <- c(dir, const$dir)
      rhs <- c(rhs, const$rhs)
      variablesTypes <- c(variablesTypes, const$variablesTypes)
      constraints.labels <- c(constraints.labels, const$constraints.labels)
    }
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs, variablesTypes = variablesTypes, constraints.labels = constraints.labels))
}

ua <- function(alternative, preferencesToModelVariables) {
  preferencesToModelVariables[alternative,]
}

removeColumnsFromModelConstraints <- function(model, columnsIndices){
  assert(ncol(model$constraints$lhs) >= max(columnsIndices),
         paste("Cannot remove a column with index", max(columnsIndices), ", LHS contains only", ncol(model$constraints$lhs)))
  if(length(columnsIndices) < 1){
    return(model)
  }
  sortedColumnsIndices <- sort(columnsIndices)
  lhs <- model$constraints$lhs[, -sortedColumnsIndices]
  variablesTypes <- model$constraints$variablesTypes[-sortedColumnsIndices]

  # remove columns from the model$preferencesToModel if appropriate columns were indicated
  preferencesIndicesToRemove <- sortedColumnsIndices[sortedColumnsIndices < ncol(model$preferencesToModelVariables)]
  preferencesToModelVariables <- model$preferencesToModelVariables
  if(length(preferencesIndicesToRemove) > 0){
    preferencesToModelVariables <- model$preferencesToModelVariables[, -preferencesIndicesToRemove]
  }

  # if the columnIndex is equal to the of the additional variables then remove it
  rhoIndex <- if(model$rhoIndex %in% sortedColumnsIndices) NULL else model$rhoIndex
  kIndex <- if(model$kIndex %in% sortedColumnsIndices) NULL else model$kIndex

  # otherwise move its position in the constraints matrix to the left (decrease its index)
  if(!is.null(rhoIndex)){
    positionToMove <- length(sortedColumnsIndices[sortedColumnsIndices < rhoIndex])
    rhoIndex <- rhoIndex - positionToMove
  }

  if(!is.null(kIndex)){
    positionToMove <- length(sortedColumnsIndices[sortedColumnsIndices < kIndex])
    kIndex <- kIndex - positionToMove
  }

  constraints <- list(lhs = lhs,
                     rhs = model$constraints$rhs,
                     dir = model$constraints$dir,
                     variablesTypes = variablesTypes,
                     constraints.labels = model$constraints$constraints.labels)

  # return a new model
  list(
    constraints = constraints,
    criteriaIndices = model$criteriaIndices,
    rhoIndex = rhoIndex,
    kIndex = kIndex,
    chPoints = model$chPoints,
    preferencesToModelVariables = preferencesToModelVariables,
    criterionPreferenceDirection = model$criterionPreferenceDirection,
    generalVF = model$generalVF,
    minEpsilon = model$minEpsilon,
    methodName = model$methodName,
    performances = model$performance
  )
}

splitVariable <- function(model, variableIndex){
  colsInConstraints <- ncol(model$constraints$lhs)

  # find all indices of constraints where variableIndex is enabled <==> 1
  constraintsRowsWithVariable <- which(model$constraints$lhs[, variableIndex] %in% 1)
  constraints <- model$constraints

  if(length(constraintsRowsWithVariable) > 0) {
    # there are some constraints with the variableIndex enabled

    # add as many columns as the number of indices found
    constraints$lhs <- cbind(constraints$lhs, matrix(0, nrow = nrow(constraints$lhs), ncol = length(constraintsRowsWithVariable)))

    # for all constraints where the variableIndex is enabled
    columnsIterator <- colsInConstraints + 1
    for(i in constraintsRowsWithVariable){
      # disable variableIndex index in constraint
      constraints$lhs[i, variableIndex] <- 0

      # add partial variable constraint
      constraints$lhs[i, columnsIterator] <- 1

      # add constraint row: partialVariable >= variable <=> variable - partialVariable <= 0
      lhs = rep(0, ncol(constraints$lhs))

      # variable
      lhs[variableIndex] <- 1
      # -partialVariable
      lhs[columnsIterator] <- -1

      constraint <- list(lhs = lhs, dir = "<=", rhs = 0, constraints.labels = "var_i<var")
      # add new constraint: partialVariable >= variable
      constraints <- combineConstraints(constraints, constraint)

      columnsIterator <- columnsIterator + 1
    }

    # all variables are of a continous type
    constraints$variablesTypes <- rep("C", ncol(constraints$lhs))
  }
  # return new constraints
  constraints
}


#********************problem.R********************

# TODO
# Check validation messages.
#' @export
buildProblem <- function(performanceTable, criteria, characteristicPoints,
                         strongPreferences = NULL, weakPreferences = NULL, indifferenceRelations = NULL,
                         strongIntensitiesPreferences = NULL, weakIntensitiesPreferences = NULL,
                         indifferenceIntensitiesRelations = NULL,
                         desiredRank = NULL)
{
  problem <- validateModel(performanceTable, criteria, characteristicPoints,
                           strongPreferences, weakPreferences, indifferenceRelations,
                           strongIntensitiesPreferences, weakIntensitiesPreferences,
                           indifferenceIntensitiesRelations,
                           desiredRank)

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
  problem$criteriaIndices <- getCriteriaIndices(problem, substractZeroCoefficients=FALSE)

  return(problem)
}

validateModel <- function(performanceTable, criteria, characteristicPoints,
                          strongPreferences, weakPreferences, indifferenceRelations,
                          strongIntensitiesPreferences, weakIntensitiesPreferences,
                          indifferenceIntensitiesRelations,
                          desiredRank)
{
  validate(is.matrix(performanceTable), "performanceTable", "performanceTable must be a matrix.")

  numberOfPreferences <- nrow(performanceTable)
  numberOfCriterions <- ncol(performanceTable)
  validate(ncol(criteria) == numberOfCriterions, "numberOfCriterions","Number of criteria given in performanceTable matrix is not equal to the number of criteria names.")
  validate(all(criteria %in% c("c", "g")), "criteria", "Criteria must be of type `c` or `g`.")

  validateRelations(strongPreferences, numberOfPreferences, relationName = "strong")
  validateRelations(weakPreferences, numberOfPreferences, relationName = "weak")
  validateRelations(indifferenceRelations, numberOfPreferences, relationName = "indifference")
  validateRelations(strongIntensitiesPreferences, numberOfPreferences, arity = 4, relationName = "strongIntensities")
  validateRelations(weakIntensitiesPreferences, numberOfPreferences, arity = 4, relationName = "weakIntensities")
  validateRelations(indifferenceIntensitiesRelations, numberOfPreferences, arity = 4, relationName = "indifferenceIntensities")

  validateDesiredRank(desiredRank, performanceTable, "desiredRank")

  return (list(
    performanceTable = performanceTable,
    criteria = criteria,
    characteristicPoints = characteristicPoints,
    strongPreferences = strongPreferences,
    weakPreferences = weakPreferences,
    indifferenceRelations = indifferenceRelations,
    strongIntensitiesPreferences = strongIntensitiesPreferences,
    weakIntensitiesPreferences = weakIntensitiesPreferences,
    indifferenceIntensities = indifferenceIntensitiesRelations,
    strictVF = TRUE,
    desiredRank = desiredRank
  ))
}

validateRelations <- function(relation, numberOfPreferences, arity = 2, relationName = "unknown")
{
  action <- "validateRelations"

  if(!is.null(relation))
  {
    validate(is.matrix(relation), action, paste("Relation", relationName, "must be repesented by a matrix"))
    validate(ncol(relation) == arity, action, paste("Relation", relationName, "has arity:", arity, ". Number of arguments typed:", ncol(relation), "."))

    #check weather minimum and maximum index are in performanceTable indices bounds
    validate(all(relation >= 1), action, "There is no preference with index lower than 1")
    validate(all(relation <= numberOfPreferences), action, paste("There is no preference with index higher than:", numberOfPreferences, ". Typed a preference with index:", max(relation)))
  }
}

validateDesiredRank <- function(desiredRank, performanceTable, action){
  if(is.null(desiredRank)){
    return(matrix(nrow=0, ncol=3))
  }
  error_text <- ifelse(action == "desiredRank",
                       "l = lower place in the ranking, u = upper place in the ranking.",
                        "l = lower value of the utility value, u = upper value of the utility value.")

  validate( is.matrix(desiredRank) && ncol(desiredRank) == 3, action,
          paste(action, " variable should be a matrix with 3 columns (a, l, u), where a = alternative index, ", error_text))

  if(action == "desiredRank"){
    validate(all(desiredRank >= 1), action, "There is no an alternative with index lower than 1")
    numberOfAlternatives <- nrow(performanceTable)
    validate(all(desiredRank <= numberOfAlternatives), action,
             paste("Both alternatives indices and ", action, " must be lower than", numberOfAlternatives))
    validate(all(desiredRank[,2] >= desiredRank[,3]), action,
             paste("All lower places in the desiredRank should be greater or equal to the upper places"))
  } else {
    validate(all(desiredRank[,1] >= 1), action, "There is no an alternative with index lower than 1")
    validate(all(desiredRank[,c(2,3)] >= 0) && all(desiredRank[,c(2,3)] <= 1), action, "Desired utility values must be in range <0, 1>")
    numberOfAlternatives <- nrow(performanceTable)
    validate(all(desiredRank <= numberOfAlternatives), action,
             paste("Both alternatives indices and ", action, " must be lower than", numberOfAlternatives))
    validate(all(desiredRank[,2] <= desiredRank[,3]), action,
             paste("Lower value of the utility value must be lower than the upper value of the utility value."))
  }
}

translateRelationsStringsIntoAlternativesIds <- function(relation, performances)
{
  action <- "Translating relations strings to ids"
  alternativesNames <- rownames(performances)
  if(is.null(alternativesNames))
  {
    validate(is.numeric(relation),
             action,
             "Relations must be defined as numerical ids of alternatives when alternatives names are not available")
  }

  if(is.numeric(relation))
  {
    return(relation)
  }

  validate(is.character(relation) && !is.null(alternativesNames), action,
           "Alternatives names must be specified when relation arguments are strings.")

  newRelationWithIds <- c()
  for(row in 1:nrow(relation))
  {
    newRow <- c()
    for(col in 1:ncol(relation))
    {
      id <- which(alternativesNames == relation[row, col])
      validate(length(id) > 0, action,
               paste("There is no alternative with a name corresponding to a relation's argument", relation[row, col]))
      newRow <- c(newRow, id)
    }
    newRelationWithIds <- rbind(newRow)
  }
  newRelationWithIds
}

validate <- function(condition, action, message){
  assert(condition, paste("Error while validating - ", action, ":", message))
}


#********************solver.R********************

#Solver method used when model$methodName is set
#' @export
solveProblem <- function(model, allowInconsistency = FALSE)
{
  if(is.null(model$methodName)){
    stop("Method name is not set. Set problem$method or model$methodName.")
  }

  availableMethods <- getAvailableMethods()
  if(model$methodName == availableMethods$utag)
  {
    utag(model, allowInconsistency)
  } else if(model$methodName == availableMethods$utamp1)
  {
    utamp1(model, allowInconsistency)
  } else if(model$methodName == availableMethods$utamp2)
  {
    utamp2(model, allowInconsistency)
  } else if(model$methodName == availableMethods$roruta){
    roruta(model, allowInconsistency)
  } else {
    stop(paste(availableMethods, " "))
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
  meanVFMarginalValues <- rep(0, ncol(model$constraints$lhs))

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

    if(!validateSolution(solutionMin, allowInconsistency) || !validateSolution(solutionMax, allowInconsistency))
    {
      return(NULL)
    }

    meanVFMarginalValues <- meanVFMarginalValues + solutionMin$solution + solutionMax$solution
    # add appropriate from min and max solution
    # to all criteria
    partialUtilityValues <- partialUtilityValues + calculateUtilityValues(model, solutionMin$solution) + calculateUtilityValues(model, solutionMax$solution)
  }
  # divide each column by the number of criteria
  partialUtilityValues <- apply(partialUtilityValues, MARGIN = 2, function(x){
    x/(2*nrCriteria)
  })

  VFMarginalValues <- sapply(meanVFMarginalValues, function(x){
    x/(2*nrCriteria)
  })
  # calculate global utility values = sum values by rows = sum partial utility values for each alternative
  utilityValues <- apply(partialUtilityValues, MARGIN = 1, function(x){
    sum(x)
  })
  methodResult$localUtilityValues <- partialUtilityValues
  methodResult$ranking <- generateRanking(utilityValues)
  methodResult$valueFunctionsMarginalValues <- getValueFunctionsMarginalValues(model, VFMarginalValues)
  methodResult
}

#UTAMP-1
#' @export
utamp1 <- function(model, allowInconsistency = FALSE) {
  assert(!is.null(model$kIndex),
         "k must be a variable in the model. Try building model again with a command `buildModel(problem, 'utamp-1')`.")

  objectiveIndex <- c(model$kIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize=TRUE)

  if(validateSolution(solution, allowInconsistency)){
    methodResult <- getMethodResult(model, solution)
    #method specific functionality
    methodResult$k <- solution$solution[model$kIndex]
    return(methodResult)
  }
  NULL
}

#UTAMP-2
#' @export
utamp2 <- function(model, allowInconsistency = FALSE) {
  assert(!is.null(model$kIndex) && !is.null(model$rhoIndex),
         "k or rho must be a variable in the model. Try building model again with a command `buildModel(problem, 'utamp-2')`.")

  objectiveIndex <- c(model$rhoIndex, model$kIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize = TRUE)

  if(validateSolution(solution, allowInconsistency)){
    methodResult <- getMethodResult(model, solution)
    #method specific functionality
    methodResult$k <- solution$solution[model$kIndex]
    methodResult$rho <- solution$solution[model$rhoIndex]
    return(methodResult)
  }
  NULL
}

# roruta
#' @export
roruta <- function(model, allowInconsistency){
  assert(!is.null(model$epsilonIndex),
         "epsilon must be a variable in the model. Try building model again with a command `buildModel(problem, 'roruta')`.")

  objectiveIndex <- c(model$epsilonIndex)
  objective <- createObjective(model$constraints$lhs, objectiveIndex)
  solution <- extremizeVariable(objective, model$constraints, maximize = TRUE)

  if(validateSolution(solution, allowInconsistency))
  {
    methodResult <- getMethodResult(model, solution)
    #method specific functionality
    methodResult$epsilon <- solution$solution[model$epsilonIndex]
    return(methodResult)
  }
  NULL
}


#********************solverHelpers.R********************

#' @import Rglpk
extremizeVariable <- function(objective, constraints, maximize) {
  Rglpk_solve_LP(objective, constraints$lhs, constraints$dir, constraints$rhs, max = maximize,
                 types = constraints$variablesType)
}

getMethodResult <- function(model, solution){
  methodResult <- list()
  methodResult$localUtilityValues <- calculateUtilityValues(model, solution$solution)
  globalUtilityValues <- utilityValues <- apply(methodResult$localUtilityValues, MARGIN = 1, function(x){ sum(x) })
  methodResult$ranking <- generateRanking(globalUtilityValues)
  methodResult$valueFunctionsMarginalValues <- getValueFunctionsMarginalValues(model, solution$solution)
  methodResult
}

createObjective <- function(lhsMatrix, extremizedVariableIndex){
  assert(!is.null(extremizedVariableIndex), 'Variable to extremize is NULL')
  obj <- rep(0, ncol(lhsMatrix))
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
    #minus (1 -> beacuse of the fact that we ommit the least valuable characterisitc point + 1 -> last index is included)
    to <- from + model$chPoints[criterionNumber]-1

    if(length(value) == 1){
      #multiplying by number
      sum(model$preferencesToModelVariables[alternative, from:to] * value)
    } else{
      # assert(ncol(model$preferencesToModelVariables) == length(value),
      #       "If value is a vector it must contain the same number of coefficients as the preferencesToModelVariables.")
      #multiplying by vector
      model$preferencesToModelVariables[alternative, from:to] %*% value[from:to]
    }
  })
}

getValueFunctionsMarginalValues <- function(model, solution){
  sapply(seq_len(length(model$criteriaIndices)), function(j){
    from <- model$criteriaIndices[j]
    to <- from + model$chPoints[j] - 1

    x <- seq(min(model$performances[,j]), max(model$performances[,j]), length.out = model$chPoints[j])
    y <- solution[from:to]

    list(criterionIndex = j,
         characteristicPointsX = x,
         characteristicPointsY = y,
         criterionType = model$criterionPreferenceDirection[j])
  })
}

#' @export
necessaryAndPossiblePreferencesRelationAnalysis <- function(model){
  assert(model$methodName == "roruta",
         "Necessary and possible preference relation analysis is available only in roruta method.")

  nrAlternatives <- nrow(model$preferencesToModelVariables)
  necessaryWeakRelations <- matrix(nrow=nrAlternatives, ncol=nrAlternatives)
  possibleWeakRelations <- matrix(nrow=nrAlternatives, ncol=nrAlternatives)

  objective <- createObjective(model$constraints$lhs, model$epsilonIndex)
  # check whether base model may be solved
  solution <- extremizeVariable(objective = objective, constraints = model$constraints, maximize = TRUE)
  if(!validateSolution(solution)){
    return(NULL)
  }

  for(i in 1:nrAlternatives)
  {
    for(j in 1:nrAlternatives)
    {
      if(i != j)
      {
        necessaryWeakRelations[i,j] <- checkPreferenceRelationFeasibility(model, i, j, "necessary")
        possibleWeakRelations[i,j] <- checkPreferenceRelationFeasibility(model, i, j, "possible")
      } else {
        necessaryWeakRelations[i, j] <- TRUE
        possibleWeakRelations[i, j] <- TRUE
      }

    }
  }
  list(
    necessaryWeakRelations = necessaryWeakRelations,
    possibleWeakRelations = possibleWeakRelations
  )
}

#' @export
extremeRankingAnalysis <- function(model){
  nrAlternatives <- nrow(model$preferencesToModelVariables)
  rankPositions <- matrix(nrow=nrAlternatives, ncol=2)
  colnames(rankPositions) <- c("min position", "max position")

  objective <- createObjective(model$constraints$lhs, model$epsilonIndex)
  # check whether base model may be solved
  solution <- extremizeVariable(objective = objective, constraints = model$constraints, maximize = TRUE)
  if(!validateSolution(solution)){
    return(NULL)
  }

  for(i in 1:nrAlternatives)
  {
    minPosition <- nrAlternatives - analysePositionInRanking(model, i, "min")
    maxPosition <- analysePositionInRanking(model, i, "max") + 1

    rankPositions[i, ] <- c(minPosition, maxPosition)
  }
  rankPositions
}

validateSolution <- function(solution, allowInconsistency){
  if(is.null(solution))
  {
    print("Solution object is empty.")
  }
  else if ((solution$status == 0 && solution$optimum >= model$minEpsilon) || allowInconsistency)
  {
    return(TRUE)
  }
  else if(solution$status != 0)
  {
    print("Soultion hasn't been found.")
  }
  else if(solution$status == 0 && solution$optimum < model$minEpsilon)
  {
    print("Solution has been found but optimum is lower than minEpsilon value.")
  }
  else if(!allowInconsistency)
  {
    print("Inconsistency is not allowed.")
  }
  else
  {
    print("Model is not feasible.")
  }
  return(FALSE)
}


