# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  alternatives = "alternativesValues",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternatives = "alternativesValues",
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
  javaAlternativesValues<-.jnew("org/xmcda/AlternativesValues")
  for (i in 1:nrow(alternatives)){
    javaAlternativesValues$put(.jnew("org/xmcda/Alternative",paste("a", i, sep="")),.jnew("java/lang/Double",as.numeric(alternatives[[i, 2]])))
  }
  xmcda$alternativesValuesList$add(javaAlternativesValues)
  return(xmcda)
}

convert <- function(results, programExecutionResult) {

  # converts the outputs of the computation to XMCDA objects

  # translate the results into XMCDA v3
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  
  #ranking
  list(alternativesValues = convertUtilityValueInAlternatives(results$ranking))
}
