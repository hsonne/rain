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
  oldpar <- par(mfrow = c(rowsToPlot(plotperneighb, n.cols = ncol(rd)), 1))
  on.exit(par(oldpar)) 
  
  args.plot <- plotRainAtGauge(
    rd = rd, 
    barheights = barheights, 
    main = main, 
    dateFormat = dateFormat, 
    cex = cex
  )

  ## neighbours to plot?
  if (ncol(rd) > 2) {
    
    ## One plot per neighbour?
    if (isTRUE(plotperneighb)) {
      
      onePlotPerNeighbour(rd, args.plot)
    }
    else {
      
      allNeighboursInOnePlot(rd, args.plot)
    }
  }
}

# rowsToPlot -------------------------------------------------------------------
rowsToPlot <- function(plotperneighb, n.cols)
{
  if (plotperneighb) {
    n.cols - 1 # one plot for each (non-time) column
  } else {
    1 + (n.cols > 2) # no neighbours: 1, any neighbours: 2
  }
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
  ## Prepare (2 x n)-matrix for barplot with n = number of rows in rd.
  ## - 1st row of m contains new (corrected) signal heights,
  ## - 2nd row of m contains the heights by which the original signals
  ##   were corrected.
  timestamps <- rd[[1]]
  rain <- rd[[2]]
  
  heightMatrix <- prepareMatrix(rain, barheights)
  
  ## general arguments
  ## las = 3: axis labels always vertical to the axis
  args.plot <- arglist(constargs("barplot_2"), 
                       cex.main = cex["legend"], 
                       names.arg = .toTimeLabels(timestamps, dateFormat))
  
  ## call the barplot function with general arguments, constant arguments and 
  ## specifig arguments
  x <- callWith(
    barplot, args.plot, constargs("barplot_1")
    , height = heightMatrix
    , ylim = .ylim(rain, step = 0.05, extra = 0.15)
    , main = main
  )
  
  ## label the bars if required
  if (isTRUE(label)) {
    barLabels <- .toBarLabels(x = rd[, 2])
    indices <- which(barLabels != "")
    kwb.plot::addLabels(
      x[indices], barLabels[indices], y0 = rain[indices], cex = cex["barid"]
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

# .toBarLabels -----------------------------------------------------------------
.toBarLabels <- function(x, digits = 1)
{
  x <- defaultIfNA(x, 0.0)
  
  isSignal <- x > 0
  
  indices <- which(isSignal)[order(- round(x[isSignal], digits))]
  
  labels <- rep(NA, length(x))
  labels[indices] <- seq_along(indices)
  
  labels
}

# prepareMatrix ----------------------------------------------------------------
prepareMatrix <- function(rain.mm, barheights, dbg = FALSE)
{
  if (is.null(barheights)) {
    
    valid.mm <- rain.mm
    
  } else {
    
    bars <- barheights[, 1]
    heights <- barheights[, 2]
    
    valid.mm <- addAtIndices(
      x = rain.mm, 
      indices = bars,
      values = heights - rain.mm[bars] # selectColumns(rdiff, "diff")
    )
  }
  
  printIf(dbg, rain.mm, "rain.mm")
  printIf(dbg, valid.mm, "valid.mm")
  
  matrix(c(valid.mm, rain.mm - valid.mm), byrow = TRUE, nrow = 2)
}

# addAtIndices --------------------------------------------------------------
addAtIndices <- function(x, indices, values)
{
  stopifnot(length(indices) == length(values))
  
  x[indices] <- x[indices] + values
  
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
