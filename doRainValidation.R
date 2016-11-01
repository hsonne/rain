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
  cases <- kwb.rain::getCorrectionCases(corrData, rainData)
  
  #tolerance = 0.001
  cases <- prevalidate(cases, tolerance)
  
  # From the undecided cases, look for cases in which the correction value
  # equals a sum of highest signals
  cases.bak <- cases
  cases <- cases.bak
  
  # Try to find more cases (increase the tolerance within each loop)
  result <- list()
  
  tolerances <- c(tolerance, c(0.1, 0.2, 0.4))
  
  cases <- cases[cases$action == "", ]
  
  for (i in seq_along(tolerances)) {
    
    message("\ntolerance:", tolerances[i], "\n")
    
    #tolerance = tolerances[i]; do.plot = TRUE
    diffs <- prevalidate2(cases, rainData, tolerance = tolerances[i], do.plot = TRUE)
    
    head(cases); head(res)
    
    sums <- unlist(lapply(res, sum))

    cases <- cases[! almostEqual(cases$corr_mm, sums, tolerance), ]

    result[[i]] <- res
  }

  x <- resetRowNames(cases[cases$action == "", ])
  x

  out <- capture.output(
    showOverviewMessages(gauges, gauges.corr = names(corrData), cases)
  )
  
  catLines(out)
  head(out, 10)
  
  # Loop through the cases
  results <- lapply(seq_len(nrow(cases)), function(i) {
    
    ## Validate rain data of this day and gauge
    validateRainDay(rainData, corrData, case, dbg = dbg, ...)
  })
  
  list(
    rd.diff = rbindAll(lapply(results, "[[", "rd.diff")), 
    cd.diff = rbindAll(lapply(results, "[[", "cd.diff")), 
    dbgRain = rbindAll(lapply(results, "[[", "dbgRain"))
  )
}

# prevalidate ------------------------------------------------------------------
prevalidate <- function(cases, diff.digits = 4, tolerance = 0.0001)
{
  rain_mm <- selectColumns(cases, "rain_mm")
  corr_mm <- selectColumns(cases, "corr_mm")
  highest <- selectColumns(cases, "highest")
  
  cases$diff <- round(rain_mm - corr_mm, diff.digits)
  cases$absdiff <- round(abs(cases$diff), diff.digits)
  
  cases$analysis <- ""
  cases$action <- ""
  
  ## Is the sum of rain (almost) equal to the correction value?
  selected <- almostEqual(rain_mm, corr_mm, tolerance)
  
  cases <- checkAndSet(
    cases, selected, "corr_mm == rain_mm", "Remove all signals of day"
  )
  
  selected <- (! selected & (rain_mm < corr_mm))
  
  cases <- checkAndSet(
    cases, selected, "Less rain available than to correct!", "?"
  )

  selected <- (! selected & almostEqual(corr_mm, highest, tolerance))

  cases <- checkAndSet(
    cases, selected, "corr_mm == highest", "Remove highest signal of day"
  )

  cases
}

# almostEqual ------------------------------------------------------------------
almostEqual <- function(x, y, tolerance = 1e-8)
{
  # stopifnot(length(x) == length(y))
  
  abs(x - y) < tolerance
}

# checkAndSet ------------------------------------------------------------------
checkAndSet <- function(cases, selected, analysis, action)
{
  cases$analysis[selected] <- analysis
  cases$action[selected] <- action
  
  cases
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

# prevalidate2 -----------------------------------------------------------------
prevalidate2 <- function(cases, rainData, tolerance, do.plot = FALSE)
{
  rainData$day <- hsDateStr(rainData[, 1])
  
  lapply(seq_len(nrow(cases)), function(i) {
    
    printIf(TRUE, cases[i, ], "Analysing case")
    
    rdd <- rainData[rainData$day == cases[i, "day"], ]
    heights <- selectColumns(rdd, cases[i, "gauge"])
    heights <- defaultIfNA(heights, 0.0)
    isSignal <- heights > 0
    signals <- heights[isSignal]
    
    ord <- order(- signals)

    #barplots(heights, signals, ord)

    target <- cases[i, "corr_mm"]
    
    indices <- matchingCumsum(signals, ord, target, tolerance)
    
    if (is.null(indices)) {
      neword <- seq(ord[1], length(signals))
      indices <- matchingCumsum(signals, neword, target, tolerance, do.plot)
    }
    
    if (is.null(indices)) {
      neword <- seq(ord[1], 1, -1)
      indices <- matchingCumsum(signals, neword, target, tolerance, do.plot)
    }
    
    if (is.null(indices)) {
      NULL
    } else {
      selected <- which(isSignal)[indices]
      getDiffs(rdd, case = cases[i, ], selected)
    }
  })
}

# barplots ---------------------------------------------------------------------
barplots <- function(heights, signals, ord)
{
  barplot(heights)
  barplot(signals)
  barplot(signals[ord])
}

# matchingCumsum ---------------------------------------------------------------
matchingCumsum <- function(signals, ord, target, tolerance, do.plot = FALSE)
{
  isMet <- almostEqual(cumsum(signals[ord]), target, tolerance)

  col <- rep("blue", length(ord))
  
  if (any(isMet, na.rm = TRUE)) {
    indices <- seq_len(which(isMet)[1])
    result <- ord[indices]
    col[indices] <- "red"
  } else {
    result <- NULL
  }
  
  if (do.plot) {
    y <- signals[ord]
    xpos <- barplot(y, main = paste("target:", target, "tol:", tolerance), 
                    col = col)
    text(xpos, 0.25, round(y, 1), cex = 0.6)
    text(xpos, 0.5, round(cumsum(y), 1), cex = 0.6)
  }
  
  result
}

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
    
    diffs <- userValidation(
      rdd = rdd, 
      cdd = cdd,
      gauge = gauge,
      neighb = neighb,
      diff.thresh = diff.thresh, 
      rd.digits = rd.digits, 
      ask = ask,
      dbg = dbg, 
      dbgInfo = dbgInfo, 
      ...
    )
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
  rdd,
  ### rain data of one day
  cdd,
  ### correction data of one day
  case,
  ## one row data frame containing date, gauge, rain_mm, corr_mm
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
  ## init result variables
  result <- list(
    rd.diff = NULL, ## diff record for rain data
    cd.diff = NULL, ## diff record for correction data
    wrong = c(), # indices of wrong signals
    valid = c(), # new (valid) heights of wrong signals
    prp = c() # proposed indices of signals to delete
  )
  
  ## Get indices of rain signals, odered decreasingly by rain height
  signals <- selectColumns(rdd, case$gauge)
  decreasingOrder <- order(signals, decreasing = TRUE)
  
  ## reduce to indices at which rain > 0
  decreasingOrder <- decreasingOrder[defaultIfNA(signals[decreasingOrder], 0) > 0]
  
  ## Cumulate the highest values
  cumulativeSum <- cumsum(signals[decreasingOrder])
  
  ## Is the correction value met by the sum of n highest signals?
  isMet <- almostEqual(cumulativeSum, case$corr_mm, tolerance)
  
  if (any(isMet)) {
    
    case$analysis <- sprintf("corr_mm == sum(%d highest sigs)", which(isMet))
    case$action <- sprintf("set %d highest signals to zero", which(isMet))

    prp <- decreasingOrder[seq_len(sum(isMet))]
  }
  else  {
    case$analysis <- "corr_mm != sum(highest signals)"
    case$action <- "?"
  }

  ## Let the user decide if no signals have been marked for deletion so far
  if (length(wrong) == 0) {
    
    dbgInfo$action <- "User decision required ***"
    cat(formatDebugInfo(dbgInfo))
    #cat(" -> User decision required...\n")
    
    if (ask) {
      
      ## Columns of rain data to select
      cols <- c("tBeg_BWB", gauge)
      
      ## If a neighbour matrix is given, select the two nearest neighbours, too
      cols <- extendToNeighbours(cols, neighb, num.neighb, gauge)
      cols <- excludeMissing(cols, cols.available = names(rdd))
      
      plotArgs <- list(
        rd = rdd[, cols],
        title = sprintf("to correct: %0.2f mm\n", case$corr_mm),
        rdiff = data.frame(decreasingOrder = prp, diff = hts - rdd[prp, gauge]),
        label = getLabels(n = nrow(rdd), indices = decreasingOrder),
        dbg = dbg,
        ...
      )
      
      userHeights <- askRepeatedly(
        askFunction = askForUserHeights,
        runFunction = plotRainForValidation, 
        runArgs = plotArgs
      )
      
      if (! is.null(userHeights)) {
        
        # signal ids and new signal heights
        prp <- selectElements(idsAndHeights, "bars")
        hts <- selectElements(idsAndHeights, "heights")
      } 
      
    } # if (ask)
    else {
      answer <- ""
      prp <- c()
      hts <- c()
    }
    
    ## If proposal was accepted with return, mark proposed signals for deletion
    if (answer == "") {
      wrong <- prp
      valid <- hts
    }
  }
  else {
    valid <- rep(0, length(wrong))
  }
  
  if (length(wrong) > 0) {
    
    ## signals to be removed
    sig <- rdd[wrong, gauge] - valid # reduce signals by remaining heights
    diffs <- getDiffs(selected = wrong)    
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

# extendToNeighbours -----------------------------------------------------------
extendToNeighbours <- function(cols, neighb = NULL, num.neighb = 0, gauge = "")
{
  if (! is.null(neighb)) {
    
    cols.neighb <- names(neighb)[seq_len(num.neighb)]
    
    cols <- c(cols, selectColumns(neighb[gauge, ], cols.neighb))
  }
  
  cols
}

# excludeMissing ---------------------------------------------------------------
excludeMissing <- function(cols, cols.available = names(rdd))
{
  miscols <- setdiff(cols, cols.available)
  
  if (length(miscols) > 0) {
    
    warning(
      "Missing column(s) in rain data: ", stringList(miscols)
    )
    
    cols <- setdiff(cols, miscols)
  }
  
  cols
}

# getLabels --------------------------------------------------------------------
getLabels <- function(n, indices)
{
  labels <- rep(NA, n)
  labels[indices] <- seq_along(indices)
  labels
}
