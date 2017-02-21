# plotRainForValidation --------------------------------------------------------
plotRainForValidation <- function
(
  rd,
  ### rain data with columns 1: timestamp, 2: gauge data, 3, ..., n: neighbour
  ### gauges' data
  main = "Title? Use argument 'main' to set a title",
  ### plot title
  barheights = NULL,
  ### data frame with the indices of the bars to be corrected in the first and
  ### the new heights of the bars to be corrected in the second column
  plotperneighb = FALSE,
  ### one plot per neighbour?
  nrowlab = 5,
  ### number of label rows
  dateFormat = "%H:%M",
  ### date/time format with placeholders %d (day), %m (month), %y, %Y (year),
  ### %H (hour), %M (minute), %S (second). Default: "%H:%M"
  cex = c(legend = 0.8, barid = 0.8),
  ### legend: scaling factor for legend, barid: scaling factor for bar-id labels
  dbg = FALSE
)
{
  ## prepare matrix plot and restore old graphical parameters on exit
  oldpar <- setMatrixPlot(plotperneighb, ncol(rd))
  on.exit(par(oldpar)) 

  # Plot the rain at the gauge to be validated
  args.plot <- plotRainAtGauge(
    rd = rd, 
    barheights = barheights, 
    main = main, 
    dateFormat = dateFormat, 
    cex = cex
  )

  ## Plot the rain at neighbour gauges (if any)
  if (ncol(rd) > 2) {
    FUN <- ifelse(plotperneighb, onePlotPerNeighbour, allNeighboursInOnePlot)
    callWith(FUN, args.plot, rd = rd)
  }
}

# setMatrixPlot ----------------------------------------------------------------
setMatrixPlot <- function(plotperneighb, n.cols)
{
  n.rows <- if (plotperneighb) {
    n.cols - 1 # one plot for each (non-time) column
  } else {
    1 + (n.cols > 2) # no neighbours: 1, any neighbours: 2
  }
  
  par(mfrow = c(n.rows, 1))
}

# plotRainAtGauge --------------------------------------------------------------
plotRainAtGauge <- function
(
  rd, 
  barheights = NULL, 
  main = "main?", 
  dateFormat = "%H:%M", 
  label = TRUE, 
  cex = c(legend = 1.0, barid = 1.0), 
  nrowlab = 5,
  case = case
)
{
  ## Define general plot arguments
  ## las = 3: axis labels always vertical to the axis
  args.plot <- arglist(
    constargs("barplot_2"), 
    cex.main = cex["legend"], 
    names.arg = .toTimeLabels(rd[, 1], dateFormat)
  )

  # Get the rain heights
  rain <- rd[, 2]
  
  # Get the indices of the signals (ordered decreasingly by their value)
  signal.order <- signalOrder(rain)
  
  ## Prepare a (2 x n)-matrix of bar heights with n = number of rows in rd. 
  ## - 1st row: new (corrected) signal heights,
  ## - 2nd row: heights by which the original signals were corrected.
  height <- prepareMatrix(rain, barheights, signal.order)
  
  ## Call the barplot function with general arguments, constant arguments and 
  ## specific arguments
  x <- callWith(
    barplot, args.plot, constargs("barplot_1")
    , height = height
    , ylim = .ylim(rain, step = 0.05, extra = 0.15)
    , main = main
  )
  
  ## label the bars if required
  if (isTRUE(label)) {
    
    kwb.plot::addLabels(
      x = x[signal.order], 
      labels = seq_along(signal.order),
      y0 = rain[signal.order], 
      cex = cex["barid"]
    )
  }
  
  # Return the general arguments so that they can be reused
  args.plot
}

# .toTimeLabels ----------------------------------------------------------------
.toTimeLabels <- function
(
  timestamps, dateFormat = "", max.labels = 20, 
  nicesteps = c(1, 2, 3, 4, 6, 12, 18, 24, 7*24)
)
{
  timelabels <- format(timestamps, dateFormat)

  labelstep <- length(timelabels) / max.labels
  labelstep <- nicesteps[which.min(abs(labelstep - nicesteps))]
  
  firsttimes <- timestamps[seq_len(labelstep)]
  
  index <- which(as.numeric(firsttimes) %% (labelstep * 60) == 0)
  
  stopifnot(length(index) == 1)
  
  kwb.plot::niceLabels(timelabels, labelstep = labelstep, offset = index -1)
}

# signalOrder ------------------------------------------------------------------
signalOrder <- function(x, digits = 1)
{
  x <- round(defaultIfNA(x, 0), digits)
  order(x, decreasing = TRUE)[seq_len(sum(x > 0))]
}

# prepareMatrix ----------------------------------------------------------------
prepareMatrix <- function(rain, barheights, signal.order, dbg = FALSE)
{
  rain.new <- if (is.null(barheights)) {
    rain
  } else {
    setAtIndices(rain, signal.order[barheights[, 1]], barheights[, 2])
  }
  
  printIf(dbg, rain, "rain")
  printIf(dbg, rain.new, "rain.new")
  
  matrix(c(rain.new, rain - rain.new), byrow = TRUE, nrow = 2)
}

# setAtIndices -----------------------------------------------------------------
setAtIndices <- function(x, indices, values)
{
  stopifnot(length(indices) == length(values))
  
  isNA <- is.na(indices)
  
  if (any(isNA)) {
    stop(
      "There are ", sum(isNA), " NAs in indices (", collapsed(indices, ", "),
      ")! Values: ", collapsed(values, ", ")
    )
  }
  
  x[indices] <- values
  
  x
}

# constargs --------------------------------------------------------------------
constargs <- function(id = NULL)
{
  argLists <- list(
    
    barplot_1 = list(space = 0, adj = 1, col = c("grey", "red")),
    barplot_2 = list(ylab = "rain height in mm", las = 3),
    barplot_3 = list(space = 0, col = "grey"),
    barplot_4 = list(space = c(0, 0), beside = TRUE),
    
    boxplot_1 = list(boxwex = 1, las = 3, xaxt = "n"),
    
    axis_1 = list(side = 1, tick = FALSE, las = 3)
  )
  
  if (is.null(id)) {
    argLists
  } else {
    selectElements(argLists, id)
  }
}

# onePlotPerNeighbour ----------------------------------------------------------
onePlotPerNeighbour <- function(rd, args.plot)
{
  for (i in seq(3, ncol(rd))) {
    
    height <- rd[, i]
    
    ## call the barplot function with general arguments added to arg list
    callWith(
      barplot, args.plot, constargs("barplot_3"),
      height = height, ylim = .ylim(height), 
      main = paste("Neighbour:", names(rd)[i])
    )
  }
}

# .ylim ------------------------------------------------------------------------
.ylim <- function(x, step = 1.0, extra = 0)
{
  ymax <- step * ceiling(max(x, na.rm = TRUE) / step)
  ymax <- ifelse(isTRUE(all.equal(ymax, 0)), 1, ymax)
  c(0, ymax * (1 + extra))
}

# allNeighboursInOnePlot -------------------------------------------------------
allNeighboursInOnePlot <- function(rd, args.plot, cex.legend = 1)
{
  gauges <- names(rd)[-(1:2)]
  
  ## Create matrix with neighbour data
  height <- t(as.matrix(rd[, gauges, drop = FALSE]))

  ## barplot if only one neighbour, else boxplot
  if (length(gauges) == 1) {

    main <- sprintf("Rain at nearest neighbour gauge: %s", gauges[1])
    
    callWith(barplot, args.plot, constargs("barplot_4"),
             height = height, main = main, ylim = .ylim(height, step = 0.1))
  }
  else {
    
    main <- sprintf("Rain signals of neighbours (%s)", stringList(gauges))

    timelabels <- selectElements(args.plot, "names.arg")
    
    callWith(boxplot, constargs("boxplot_1"), 
             x = height, ylim = .ylim(height), names = timelabels, 
             cex.main = cex.legend, main = main)
    
    callWith(axis, constargs("axis_1"), 
             labels = timelabels, at = seq_along(timelabels))
  }
}
