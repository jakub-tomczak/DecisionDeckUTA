# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  alternativesMarginalUtility = "alternativesValues",
  marginalValueFunctions = "criteria",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternativesMarginalUtility = "alternativesValues",
  marginalValueFunctions = "criteriaFunctions",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}

convertAlternatives <- function(alternativesMatrix){
  xmcdaResult <- .jnew("org/xmcda/XMCDA")
  
  alternativeValuesList <-.jnew("org/xmcda/AlternativesValues")
  for (i in seq(nrow(alternativesMatrix))){
    alternative <- .jnew("org/xmcda/v2/AlternativeValue")
    alternative$setAlternativeID(as.character(i))
    
    valueLables <- colnames(alternativesMatrix)[2:3]
    sapply(valueLables, function(label){
      value <- .jnew("org/xmcda/v2/Value")
      value$setId(label)
      value$setReal(.jnew("java/lang/Double",as.numeric(alternativesMatrix[[i, label]])))
      alternative$getValueOrValues()$add(value)
    })
    alternativeValuesList$put(alternative,.jnew("java/lang/Double",as.numeric(i)))
  }
  xmcdaResult$alternativesValuesList$add(alternativeValuesList)
  xmcdaResult
  #alternativeValuesList
}

convertUtilityValueInAlternatives <- function(alternatives){
  xmcda <- .jnew("org/xmcda/XMCDA")
  javaAlternativesValues <-.jnew("org/xmcda/AlternativesValues")
  for (i in 1:nrow(alternatives)){
    javaAlternativesValues$put(.jnew("org/xmcda/Alternative",paste("a", i, sep="")),.jnew("java/lang/Double",as.numeric(alternatives[[i, 2]])))
  }
  xmcda$alternativesValuesList$add(javaAlternativesValues)
  xmcda
}

convertValueFunctions <- function(results){
  
  valueFunctions <- results$valueFunctionsMarginalValues
  
  performancesColnames <- colnames(results$performances)
  if(is.null(performancesColnames))
  {
    criteriaNames <- paste("g", 1:ncol(valueFunctions), sep = "")
  } 
  else
  {
    criteriaNames <- performancesColnames
  }
  
  xmcda <- .jnew("org/xmcda/XMCDA")
  javaMarginalFunctions <- .jnew("org/xmcda/CriteriaFunctions")
  
  for(i in 1:ncol(valueFunctions)){
    
    valueFunction <- valueFunctions[, i]
    # for each criterion
    criterionFunctions <- .jnew("org/xmcda/CriterionFunctions")
    criterionFunction <- .jnew("org/xmcda/value/PiecewiseLinearFunction")
    criterion <- .jnew("org/xmcda/Criterion", criteriaNames[i], criteriaNames[i])
    
    for(pointIndex in length(valueFunction$characteristicPointsX)-1){
      segment <- .jnew("org/xmcda/value/Segment")
      
      head.x <- .jnew("java/lang/Double", as.numeric(valueFunction$characteristicPointsX[pointIndex]))
      head.y <- .jnew("java/lang/Double", as.numeric(valueFunction$characteristicPointsY[pointIndex]))
      head.point <- .jnew("org/xmcda/value/Point")
      head.qualifiedValueX <- .jnew("org/xmcda/QualifiedValue")
      head.qualifiedValueX$setValue(head.x)
      head.qualifiedValueY <- .jnew("org/xmcda/QualifiedValue")
      head.qualifiedValueY$setValue(head.y)
      head.point$setAbscissa(head.qualifiedValueX)
      head.point$setOrdinate(head.qualifiedValueY)
      
      head <- .jnew("org/xmcda/value/EndPoint", head.point)
      
      tail.x <- .jnew("java/lang/Double", as.numeric(valueFunction$characteristicPointsX[pointIndex+1]))
      tail.y <- .jnew("java/lang/Double", as.numeric(valueFunction$characteristicPointsY[pointIndex+1]))
      tail.point <- .jnew("org/xmcda/value/Point")
      tail.qualifiedValueX <- .jnew("org/xmcda/QualifiedValue")
      tail.qualifiedValueX$setValue(tail.x)
      tail.qualifiedValueY <- .jnew("org/xmcda/QualifiedValue")
      tail.qualifiedValueY$setValue(tail.y)
      tail.point$setAbscissa(tail.qualifiedValueX)
      tail.point$setOrdinate(tail.qualifiedValueY)
      
      tail <- .jnew("org/xmcda/value/EndPoint", tail.point)
      
      segment$setHead(head)
      segment$setTail(tail)
      
      criterionFunction$add(segment)
    }
    
    criterionFunctions$add(criterionFunction)
    
    # little hack, criteria must be included in the XMCDA object
    # otherwise converter_V2toV3 won't get CriteriaFunctions into consideration  
    xmcda$criteria$add(criterion)
    javaMarginalFunctions$put(criterion, criterionFunctions)
  }
  xmcda$criteriaFunctionsList$add(javaMarginalFunctions)
  xmcda
}

convert <- function(results, programExecutionResult) {

  # converts the outputs of the computation to XMCDA objects

  # translate the results into XMCDA v3
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  alternatives <- convertUtilityValueInAlternatives(results$ranking)
  functions <- convertValueFunctions(results)
  
  list(
    alternativesMarginalUtility = alternatives,
    marginalValueFunctions = functions
  )
}
