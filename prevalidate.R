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

# analyseCase ------------------------------------------------------------------
analyseCase <- function
(
  case, rainData, tolerance.range = c(0.001, 1), 
  nextTolerance = function(x) 10 * x, do.plot = FALSE
)
{
  #case <- cases[1, ]
  Data <- selectCaseData(rainData, case)

  values <- defaultIfNA(Data[, ncol(Data)], 0.0)

  nonZero <- (values > 0)
  
  tolerance <- tolerance.range[1]
  
  indices <- NULL
  
  while (is.null(indices) & tolerance < tolerance.range[2]) {
    
    indices <- findIndicesWithSum(
      signals = values[nonZero], 
      target = case$corr_mm, 
      tolerance = tolerance,
      do.plot = do.plot
    )
    
    if (is.null(indices)) {
      tolerance <- nextTolerance(tolerance)
    }
  }
  
  if (is.null(indices)) {
    NULL
  } else {
    getDiffs(Data, case, selected = which(nonZero)[indices])
  }
}

# selectCaseData ---------------------------------------------------------------
selectCaseData <- function
(
  rainData, case, neighb = NULL, num.neighb = ncol(neighb), trim = FALSE, 
  n.context = 2
)
{
  gauge <- selectColumns(case, "gauge")
  
  # Which rows (belonging to the day of the current case) are to be selected?
  selected <- selectColumns(rainData, "day") == selectColumns(case, "day")
  
  # Which columns (belonging to the gauge of the current case and neighbour 
  # columns, if required) are to be selected?
  neighbours <- neighbourGauges(gauge, neighb)
  
  columns <- c(names(rainData)[1:2], "day", gauge, neighbours)
  
  rainDataDay <- selectColumns(rainData, columns, do.stop = FALSE)[selected, ]
  
  # Trim the data to the time interval between first and last signal
  if (trim) {
    values <- selectElements(rainDataDay, gauge)
    indexRange <- range(which(values > 0))
    indexRange[1] <- max(c(indexRange[1] - n.context, 1))
    indexRange[2] <- min(c(indexRange[2] + n.context, length(values)))
    rainDataDay <- rainDataDay[do.call(seq, as.list(indexRange)), ]
  }
  
  rainDataDay
}

# findIndicesWithSum -----------------------------------------------------------
findIndicesWithSum <- function
(
  signals, target, tolerance = 0.1, do.plot = FALSE
)
{
  ord <- order(signals, decreasing = TRUE)
  
  indices <- matchingCumsum(signals, ord, target, tolerance, do.plot)
  
  if (is.null(indices)) {
    neword <- seq(ord[1], length(signals))
    indices <- matchingCumsum(signals, neword, target, tolerance, do.plot)
  }
  
  if (is.null(indices)) {
    neword <- seq(ord[1], 1, -1)
    indices <- matchingCumsum(signals, neword, target, tolerance, do.plot)
  }
  
  indices
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
    xpos <- barplot(y, main = paste("target:", target, "tolerance:", tolerance), 
                    col = col)
    text(xpos, 0.25, round(y, 1), cex = 0.6)
    text(xpos, 0.5, round(cumsum(y), 1), cex = 0.6)
  }
  
  result
}

# getDiffs ---------------------------------------------------------------------
getDiffs <- function
(
  Data, case, selected, keycolumns = names(Data)[1:2]
)
{
  gauge <- selectColumns(case, "gauge")

  values.old <- selectColumns(Data[selected, ], gauge)
  values.new <- rep(0.0, length(values.old))
  
  corr.old <- selectColumns(case, "corr_mm")
  
  list(
    
    data = cbind(
      selectColumns(Data[selected, ], keycolumns),
      gauge = gauge,
      value.old = values.old,
      value.new = values.new,
      stringsAsFactors = FALSE
    ),
    
    corr = cbind(
      selectColumns(case, c("day", "gauge")),
      corr.old = corr.old,
      corr.new = corr.old - sum(values.old - values.new),
      stringsAsFactors = FALSE
    )
  )
}
