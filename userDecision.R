# askRepeatedly ----------------------------------------------------------------
askRepeatedly <- function
(
  askFunction = function() {
    if ("" == (answer <- readline("Answer: "))) NULL else list(answer = answer)
  },
  askArgs = list(),
  runFunction = NULL, 
  runArgs = list()
)
{
  userArgs <- NULL
  finished <- FALSE
  
  # Repeat while the user did not confirm with Enter
  while (! finished) {
    
    # Call the given plot function with the given arguments
    if (! is.null(runFunction)) {
      callWith(runFunction, runArgs, userArgs)
    }
    
    # Call the function that asks the user for an iniput
    answer <- callWith(askFunction, askArgs)
    
    # Save the user input if it is not NULL
    if (is.null(answer)) {
      finished <- TRUE
    } else {
      userArgs <- answer
    }
    
  } # while (! finished)
  
  userArgs
}

# askForUserHeights ------------------------------------------------------------
askForUserHeights <- function()
{
  cat("Accept (RET) or select signals to mark (ESC = quit)...")
  
  answer <- readline("Your input: ")
  
  if (answer == "") {
    return (NULL)
  }
  
  keysValues <- lapply(toKeysAndValues(answer), hsTrim)
  
  valid <- lapply(keysValues, function(x) hsValidValue(hsTrim(x), "en"))
  
  if (! all(valid$keys) || ! all(valid$valus)) {
    message("Invalid input. Examples: '1,2', '1,2=1.0,3=0.5'")
    return (NULL)
  }
  
  list(
    bars = as.integer(keysValues$keys),
    heights = defaultIfNA(as.double(keysValues$values), 0.0)
  )
  
  ### list with elements \code{bars} (numbers of the bars for which the user
}
