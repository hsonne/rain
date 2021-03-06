# rainValidation ---------------------------------------------------------------
rainValidation <- function
(
  rainData,
  ### data frame with rain data
  corrData,
  ### data frame with rain correction data
  neighb = NULL,
  ### matrix of gauge neighbours
  gauges = NULL,
  ### names of gauges to be validated. Default: names of columns 3:ncol(rd)
  tolerance = 0.001,
  ### used for "almost equal" comparison of numeric values. Default: 0.001
  dbg = TRUE,
  ### if \code{TRUE} debug messages are shown
  ...
  ### further arguments passed to validateRainDay, such as neighb, devPdf, ask
)
{
  clearConsole()
  
  gauges <- defaultIfNULL(gauges, names(rainData)[- c(1, 2)])

  catIf(dbg, "\n** Gauges found/selected:\n  ", stringList(gauges), "\n   ")
  
  catIf(dbg, "\n** Exclude rows with sum of signals = 0...\n   ")
  rainSums <- rowSums(selectColumns(rainData, gauges))
  rainSignals <- rainData[defaultIfNA(rainSums, 1) > 0, ]
  catIf(dbg, nrow(rainSignals), "rows kept (-> attribute 'rainSignals').\n")
  
  catIf(dbg, "\n** Find the cases in which a correction is necessary...\n   ")
  cases.all <- kwb.rain::getCorrectionCases(corrData, rainSignals)
  catIf(dbg, nrow(cases.all), "cases found (-> attribute 'cases.all').\n")
  
  catIf(dbg, "\n** Prevalidate all cases... ")
  cases.pre <- prevalidate(cases.all, tolerance = 0.001)
  catIf(dbg, "ok. (-> attribute 'cases.pre')\n")
  frequency <- hsRenameColumns(as.data.frame(table(cases.pre$action)), list(
    Var1 = "Proposed_Action", Freq = "Frequency"
  ))
  printIf(dbg, frequency, "\nFrequency of Proposed Actions")
  undecided <- frequency$Proposed_Action %in% c("", "?")
  catIf(dbg, "\n   ->", sum(frequency$Frequency[undecided]), 
        "cases to be decided manually.\n")
  
  cases <- cases.all
 
  # From the undecided cases, look for cases in which the correction value
  # equals a sum of highest signals or the sum of the highest signal and its
  # left or right neighbours
  
  catIf(dbg, "\n** Add column 'day' to rainData and rainSignals... ")
  rainData$day <- hsDateStr(rainData[, 1])
  rainSignals$day <- hsDateStr(rainSignals[, 1])
  catIf(dbg, "ok.\n")
  
  catIf(dbg, "\n** Guess the wrong signals... \n   ")
  diffs.raw <- guessDifferences(cases.all, rainSignals, dbg = FALSE)
  diffs <- mergeDiffs(diffs)
  catIf(dbg, nrow(diffs$data), "signals guessed (-> attribute 'diffs')")

  # args.common <- list(cases = cases, rainData = rainData, diffs = diffs)
  # 
  # args.case <- list(
  #   list(plotperneighb = FALSE, args.pdf = list(landscape = TRUE)),
  #   list(plotperneighb = TRUE, args.pdf = list(landscape = FALSE))
  # )
  # 
  # callWith(plotCases, args.common, args.case[[1]])
  # callWith(plotCases, args.common, args.case[[2]])
  # 
  # cases <- cases.all[! isSolved(diffs, cases.all, method = 1), ]
  # 
  # # Loop through the remaining cases
  # results <- lapply(seq_len(nrow(cases)), function(i) {
  # 
  #   ## Validate rain data of this day and gauge
  #   if (FALSE) {
  #     case <- cases[1, ]
  #     neighb <- getNeighbourMatrix(gauges = names(rainData)[-(1:2)])
  #     num.neighb = 2
  #   }
  #   userValidation(
  #     case = cases[i, ], 
  #     rainData = rainData,
  #     neighb = neighb,
  #     diff.thresh = diff.thresh,
  #     rd.digits = rd.digits,
  #     dbg = dbg,
  #     ...
  #   )
  # })

  # Create the output structure
  # out <- list(
  #   rd.diff = rbindAll(lapply(results, "[[", "rd.diff")), 
  #   cd.diff = rbindAll(lapply(results, "[[", "cd.diff")), 
  #   dbgRain = rbindAll(lapply(results, "[[", "dbgRain"))
  # )
  
  out <- list()
  
  # Return all relevant intermediate variables as attributes
  structure(out, rainSignals = rainSignals, cases.all = cases.all, 
            cases.pre = cases.pre, diffs = diffs)
}

# mergeDiffs -------------------------------------------------------------------
mergeDiffs <- function(diffs)
{
  # Exclude NULL entries
  diffs <- diffs[! sapply(diffs, is.null)]
  
  out <- list(
    data = rbindAll(lapply(diffs, selectElements, "data")),
    corr = rbindAll(lapply(diffs, selectElements, "corr"))
  )
  
  out$corr$corr.new <- round(out$corr$corr.new, 1)
  
  out
}

# guessDifferences -------------------------------------------------------------
guessDifferences <- function(cases, rainData, dbg = FALSE)
{
  lapply(seq_len(nrow(cases)), function(i) {
    printIf(dbg, cases[i, ], "case")
    analyseCase(case = cases[i, ], rainData)
  })
}

# isSolved ---------------------------------------------------------------------
isSolved <- function(diffs, cases, method = 1, tolerance = 0.1)
{
  stopifnot(length(diffs) == nrow(cases))
  
  if (method == 1) {
    solved <- ! sapply(diffs, is.null)
  } else if (method == 2) {
    sums <- sapply(diffs, function(x) sum(x$data$value.old))
    stopifnot(length(sums) == nrow(cases))
    corr <- selectColumns(cases, "corr_mm")
    solved <- almostEqual(corr, sums, tolerance)
  } else {
    stop("Unknown method: ", method)
  }

  solved
}

# showOverviewMessages ---------------------------------------------------------
showOverviewMessages <- function(gauges, gauges.corr, cases)
{
  skipMessage <- function(x, y) sprintf("*** Skipping gauge '%s' (%s)" , x, y)
  
  gauges.skip <- setdiff(gauges, gauges.corr)
  
  if (length(gauges.skip) > 0) {
    catLines(skipMessage(gauges.skip, "no correction data available"))
    gauges <- setdiff(gauges, gauges.skip)
  }
  
  gauges.ok <- setdiff(gauges, unique(cases$gauge))
  
  if (length(gauges.ok) > 0) {
    catLines(skipMessage(gauges.ok, "no corrections required"))
    gauges <- setdiff(gauges, gauges.ok)
  }
  
  for (gauge in gauges) {
    cat(sprintf("*** Corrections to be done for gauge '%s':\n", gauge))
    print(cases[cases$gauge == gauge, 2:3]) 
  }
}

# prevalidate2 ...

# validateRainDay --------------------------------------------------------------
validateRainDay <- function
(
  rainData,
  corrData,
  case,
  # rdd,
  # ### data frame with rain data of one day for all gauges
  # cdd,
  # ### data frame with correction data of one day for all gauges (one row)
  # gauge,
  # ### gauge name
  neighb = NULL,
  ### neighbour matrix
  devPdf = 0,
  ### id of pdf device; 0 = no pdf device
  diff.thresh = 0.001,
  ### threshold for differences due to rounding
  rd.digits = 6,
  ### number of digits (?)
  ask = TRUE,
  ### if TRUE user is asked to select signals to remove/modify
  dbg = FALSE,
  ...
)
{
  day <- selectColumns(case, "day")
  
  rdd <- rainData[hsDateStr(rainData[[1]]) == day, ]
  cdd <- corrData[hsDateStr(corrData[[1]]) == day, ]
  
  heights <- selectColumns(rdd, selectColumns(case, "gauge"))
  
  ## The sum should not be less than the value to correct!
  digits <- 1
  rain_mm <- round(selectColumns(case, "rain_mm"), digits)
  corr_mm <- round(selectColumns(case, "corr_mm"), digits)
  
  ## Is the sum of rain equal to the correction value?
  if (case$analysis == "corr_mm == rain_mm") {
    
    selected <- (defaultIfNA(rain_mm, 0) != 0)
    
    diffs <- getDiffs(rdd, case, selected)
  }
  else if (case$analysis == "corr_mm == highest") {
    
    selected <- (almostEqual(rain_mm, case$highest))
    
    diffs <- getDiffs(rdd, case, selected)
  }
  else {
    
    diffs <- "user decision required"
  }
  
  rd.diff <- selectElements(diffs, "rd.diff") # "rain"
  cd.diff <- selectElements(diffs, "cd.diff") # "corr"
  #dbgInfo <- selectElements(res, "dbgInfo")
  
  ## Add original signals vs corrected signals to plot
  if (devPdf > 0) {
    plotRainForValidation(rd = rdd[, c(1, 3)], title = msg, #highlight = 1,
                          cor = case$corr_mm, dev = devPdf, dbg = dbg)
  }
  
  ## Return list of rain data differences and correction data differences
  list(
    rd.diff = rd.diff,
    cd.diff = cd.diff,
    dbgRain = hsAddMissingCols(dbgInfo, colNames = c(
      "gauge", "day", "corr_mm", "rain_mm", "numNA", "analysis", "action"
    ))
  )
}

# getDiffs ---------------------------------------------------------------------
getDiffs <- function
(
  rdd, case, selected, columns = c("tBeg_BWB", "tEnd_BWB", "tDate_BWB")
)
{
  gauge <- case[, "gauge"]
  
  rain_mm <- selectColumns(rdd, gauge)
  
  list(
    
    rain = data.frame(
      selectColumns(rdd[selected, ], columns[1:2]),
      gauge = case["gauge"],
      diff_mm = - rain_mm[selected],
      rain_mm = 0.0,
      stringsAsFactors = FALSE
    ),
    
    corr = data.frame(
      date = case["day"],
      gauge = case["gauge"],
      diff_mm = - case["corr_mm"],
      corr_mm = 0.0,
      stringsAsFactors = FALSE
    )
  )
}

# formatDebugInfo --------------------------------------------------------------
formatDebugInfo <- function(di, dbg = FALSE)
{
  printIf(dbg, di, "di")
  
  paste(
    sprintf("\nGauge '%s' on %s", di$gauge, di$day),
    sprintf(" * Correction: %8.2f mm", di$corr_mm),
    sprintf(" * Rain sum:   %8.2f mm (%d-times NA, treated as 0)",
            di$rain_mm, di$numNA),
    sprintf(" -> %s", di$analysis),
    sprintf(" -> %s\n", di$action),
    sep = "\n"
  )
}

# userValidation ---------------------------------------------------------------
userValidation <- function
(
  case, rainData,  
  neighb = NULL,
  ### neighbour matrix
  num.neighb = ncol(neighb),
  ### how many neighbours are to be shown for comparison?
  tolerance,
  rd.digits,
  ask = FALSE,
  ### if TRUE user is asked to select signals to remove/modify
  dbg = FALSE,
  dbgInfo = NULL,
  ...
  ### further arguments passed to plotRainForValidation
)
{
  gauge <- selectColumns(case, "gauge")
  
  rainDataDay <- selectCaseData(rainData, case, neighb, num.neighb, trim = TRUE)
  
  # dbg <- TRUE
  plotArgs <- list(NULL
    , rd = rainDataDay[, -(2:3)]
    , main = sprintf("to correct: %0.2f mm\n", selectColumns(case, "corr_mm"))
    #, rdiff = data.frame(decreasingOrder = prp, diff = hts - rdd[prp, gauge])
    , dbg = dbg
    , plotperneighb = TRUE
    #, ...
  )

  callWith(plotRainForValidation, plotArgs)

  userHeights <- askRepeatedly(
    askFunction = askForUserHeights,
    runFunction = plotRainForValidation, 
    runArgs = plotArgs
  )
  
  if (! is.null(userHeights)) {
    
    ## signals to be removed
    sig <- rdd[wrong, gauge] - valid # reduce signals by remaining heights
    diffs <- getDiffs(Data = rdd, selected)    
  }
  
  ## Return diff records for rain and correction data
  diffs
}

## Plot rain series with removed signal indicated
#     plotRainForValidation(
#       rd = rdd[, c("tBeg_BWB", gauge)],
#       title = sprintf("Removed: %0.2f mm (single signal)", sig),
#       #del,
#       dht, 
#       case$corr_mm, 
#       dbg = dbg
#     )
