# doRainValidation -------------------------------------------------------------
doRainValidation <- function
(
  rainData,
  ### rain data
  corrData,
  ### correction data
  ask = FALSE,
  ### passed to rainValidation
  to.pdf = FALSE
)
{
  ## Prepare pdf device but do not set current device to pdf device
  file.pdf <- preparePdfIf(to.pdf, makeCurrent = FALSE)
  
  neighb <- getNeighbourMatrix(gauges = names(rainData)[-(1:2)])
  
  ## Call the rain validation routine
  corr <- rainValidation(
    rainData,
    corrData,
    neighb = neighb, # passed to validateRainDay
    devPdf = kwb.base::hsPdfDev(), # passed to validateRainDay
    plotperneighb = FALSE,
    num.neighb = 2,
    cex = c(legend = 1.1, barid = 0.8),
    ask = ask, # passed to validateRainDay
    dbg = FALSE
  )
  
  finishAndShowPdfIf(to.pdf, file.pdf, which = kwb.base::hsPdfDev())
  
  corr
}

# getNeighbourMatrix -----------------------------------------------------------
getNeighbourMatrix <- function(gauges = NULL)
{
  if (file.exists(kwb.read::mdb_rain_meta())) {
    distanceToNeighbour(getGaugeDistances())
  } else {
    message("no neighbour data available, using random neighbours!")
    randomNeighbours(gauges)
  }
}

# randomNeighbours -------------------------------------------------------------
randomNeighbours <- function(gauges)
{
  structure(
    do.call(rbind, lapply(seq_along(gauges), function(i) sample(gauges[-i]))),
    dimnames = list(gauges, paste0("n", seq_len(length(gauges) - 1)))
  )
}

# rainValidation ---------------------------------------------------------------
rainValidation <- function
(
  rainData,
  ### data frame with rain data
  corrData,
  ### data frame with rain correction data
  gauges = names(rainData)[-(1:2)],
  ### names of gauges to be validated. Default: names of columns 3:ncol(rd)
  tolerance = 0.001,
  dbg = FALSE,
  ### if \code{TRUE} debug messages are shown
  ...
  ### further arguments passed to validateRainDay, such as neighb, devPdf, ask
)
{
  cases.all <- kwb.rain::getCorrectionCases(corrData, rainData)
  
  #gauges = names(rainData)[-(1:2)]
  showOverviewMessages(gauges, gauges.corr = names(corrData), cases.all)
  
  #cases <- prevalidate(cases.all, tolerance = 0.001)
  cases <- cases.all
  
  # From the undecided cases, look for cases in which the correction value
  # equals a sum of highest signals or the sum of the highest signal and its
  # left or right neighbours
  rainData$day <- hsDateStr(rainData[, 1])
  
  diffs <- guessDifferences(cases, rainData)

  cases <- cases.all[! isSolved(diffs, cases.all, method = 1), ]
  
  # Loop through the remaining cases
  results <- lapply(seq_len(nrow(cases)), function(i) {

    ## Validate rain data of this day and gauge
    if (FALSE) {
      case <- cases[1, ]
      neighb <- getNeighbourMatrix(gauges = names(rainData)[-(1:2)])
      num.neighb = 2
    }
    userValidation(
      case = cases[i, ], 
      rainData = rainData,
      neighb = neighb,
      diff.thresh = diff.thresh,
      rd.digits = rd.digits,
      dbg = dbg,
      ...
    )
  })
  
  list(
    rd.diff = rbindAll(lapply(results, "[[", "rd.diff")), 
    cd.diff = rbindAll(lapply(results, "[[", "cd.diff")), 
    dbgRain = rbindAll(lapply(results, "[[", "dbgRain"))
  )
}

# guessDifferences -------------------------------------------------------------
guessDifferences <- function(cases, rainData)
{
  lapply(seq_len(nrow(cases)), function(i) {
    printIf(TRUE, cases[i, ], "case")
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
  num.neighb = 2,
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
####for (i in seq_len(nrow(cases))) {case <- cases[i, ]
    
  gauge <- selectColumns(case, "gauge")
  
  rainDataDay <- selectCaseData(rainData, case, neighb, num.neighb, trim = TRUE)
  
  # dbg <- TRUE
  plotArgs <- list(NULL
    , rd = rainDataDay[, -(2:3)]
    , main = sprintf("to correct: %0.2f mm\n", selectColumns(case, "corr_mm"))
    #, rdiff = data.frame(decreasingOrder = prp, diff = hts - rdd[prp, gauge])
    , label = .barLabels(rainDataDay[, -(2:3)])
    , dbg = dbg
    , plotperneighb = TRUE
    #, ...
  )

  callWith(plotRainForValidation, plotArgs)
####}
  
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

# neighbourGauges --------------------------------------------------------------
neighbourGauges <- function
(
  gauge, neighb = NULL, num.neighb = 0
)
{
  if (! is.null(neighb)) {
    neighb[gauge, colnames(neighb)[seq_len(num.neighb)]]
  } else {
    NULL
  }
}

# .barLabels -------------------------------------------------------------------
.barLabels <- function(Data, digits = 1)
{
  values <- Data[, 2]
  isSignal <- values > 0
  
  indices <- which(isSignal)[order(- round(values[isSignal], digits))]
  
  getLabels(n = nrow(rainDataDay), indices = indices)
}

# getLabels --------------------------------------------------------------------
getLabels <- function(n, indices)
{
  labels <- rep(NA, n)
  labels[indices] <- seq_along(indices)
  labels
}
