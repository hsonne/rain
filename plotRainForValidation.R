# plotRainForValidation --------------------------------------------------------
plotRainForValidation <- function
(
  rd,
  ### rain data with columns 1: timestamp, 2: gauge data, 3, ..., n: neighbour
  ### gauges' data
  main = "Title? Use argument 'main' to set a title",
  ### plot title
  rdiff = NULL,
  ### data frame containing indices and signal differences
  label = NULL,
  ### vector of labels
  plotperneighb = FALSE,
  ### one plot per neighbour?
  nrowlab = 5,
  ### number of label rows
  dateFormat = "%H:%M",
  ### date/time format with placeholders %d (day), %m (month), %y, %Y (year),
  ### %H (hour), %M (minute), %S (second). Default: "%H:%M"
  cex = c(legend = 0.8, barid = 0.5),
  ### legend: scaling factor for legend, barid: scaling factor for bar-id labels
  dbg = FALSE
)
{
  ## number of rows/columns in rd
  dim.rain <- dim(rd)
  
  checkArgumentsOrStop(label, dim.rain)

  ## prepare matrix plot and restore old graphical parameters on exit
  oldpar <- par(mfrow = c(rowsToPlot(plotperneighb, dim.rain[2]), 1))
  on.exit(par(oldpar)) 
  
  genargs <- plotRainAtGauge(rd, rdiff, main, dateFormat, label, cex)

  ## neighbours to plot?
  if (dim.rain[2] > 2) {
    
    ## One plot per neighbour?
    if (isTRUE(plotperneighb)) {
      
      onePlotPerNeighbour(rd, genargs)
    }
    else {
      
      allNeighboursInOnePlot()
    }
  }
}

# plotRainAtGauge --------------------------------------------------------------
plotRainAtGauge <- function
(
  rd, rdiff, main, dateFormat, label = NULL, cex = c(legend = 1.0, barid = 1.0)
)
{
  ## Prepare (2 x n)-matrix for barplot with n = number of rows in rd.
  ## 1st row of m contains new (corrected) signal heights,
  ## 2nd row of m contains the heights by which the original signals
  ## were corrected.
  timestamps <- rd[[1]]
  rain.mm <- rd[[2]]
  
  height <- prepareMatrix(rain.mm = rain.mm, rdiff)
  
  main <- toPlotTitle(
    gauge = names(rd)[2], 
    datestring = hsDateStr(rd[1, 1]), 
    marked = sum(rdiff$diff), 
    main = main
  )

  datenames <- kwb.plot::niceLabels(format(timestamps, dateFormat), 12)
  
  ## general arguments
  ## las = 3: axis labels always vertical to the axis
  genargs <- arglist(constargs("barplot_2"), 
                     cex.main = cex["legend"], names.arg = datenames)
  
  ## call the barplot function with general arguments, constant arguments and 
  ## specifig arguments
  x <- callWith(
    barplot, genargs, constargs("barplot_1"), 
    height = height, ylim = .ylim(rain.mm), main = main
  )
  
  ## label the bars
  if (! is.null(label)) {
    text(x, (x %% nrowlab + 1) * 0.04 * ymax, labels = label, 
         cex = cex["barid"])
  }
  
  # Return the general arguments so that they can be reused
  genargs
}

# toPlotTitle -----------------------------------------------------------------
toPlotTitle <- function(gauge, datestring, marked, main = "Title?")
{
  paste0(
    sprintf("Gauge: %s, date: %s\n", gauge, datestring),
    #sprintf("to correct: %0.2f mm\n", cor),
    #sprintf("marked: %0.2f mm", sum(rd[[2]]) - sum(height)),
    sprintf("marked: %0.2f mm\n", marked),
    main
  )  
}

# constargs --------------------------------------------------------------------
constargs <- function(id) 
{
  argLists <- list(
    
    barplot_1 = list(space = 0, adj = 1, col = c("grey", "red")),
    barplot_2 = list(ylab = "rain height in mm", las = 3),
    barplot_3 = list(space = 0, col = "grey"),
    
    boxplot_1 = list(boxwex = 1, las = 3, xaxt = "n"),
    
    axis_1 = list(side = 1, tick = FALSE, las = 3)
  )
  
  selectElements(argLists, id)
}

# onePlotPerNeighbour ----------------------------------------------------------
onePlotPerNeighbour <- function(rd, genargs)
{
  for (i in seq(3, ncol(rd))) {
    
    height <- rd[, i]
    
    ## call the barplot function with general arguments added to arg list
    callWith(
      barplot, genargs, constargs("barplot_3"),
      height = height, ylim = .ylim(height), 
      main = paste("Neighbour:", names(rd)[i])
    )
  }
}

# .ylim ------------------------------------------------------------------------
.ylim <- function(x, step = 1.0)
{
  ymax <- step * ceiling(max(x, na.rm = TRUE) / step)
  ymax <- ifelse(isTRUE(all.equal(ymax, 0)), 1, ymax)
  c(0, ymax)
}

# allNeighboursInOnePlot -------------------------------------------------------
allNeighboursInOnePlot <- function(rd, genargs, cex.legend)
{
  gauges <- names(rd)[-(1:2)]
  
  ## Create matrix with neighbour data
  height <- t(as.matrix(rd[, gauges, drop = FALSE]))

  ## barplot if only one neighbour, else boxplot
  if (length(gauges) == 1) {

    main <- sprintf("Rain at nearest neighbour gauge: %s", gauges[1])
    
    callWith(barplot, genargs, 
             height = height, space = c(0, 0), beside = TRUE, main = main,
             ylim = .ylim(height, step = 0.1)
    )
  }
  else {
    
    main <- sprintf("Rain signals of neighbours (%s)", collapsed(gauges, ", "))

    datenames <- selectElements(genargs, "names.arg")
    
    callWith(boxplot, constargs("boxplot_1"), 
             x = height, ylim = .ylim(height), names = datenames, 
             cex.main = cex.legend, main = main
    )
    
    callWith(axis, constargs("axis_1"), 
             labels = datenames, at = seq_along(datenames)
    )
  }
}

# checkArgumentsOrStop ---------------------------------------------------------
checkArgumentsOrStop <- function(label, dim)
{
  ## If label is given it must contain as many elements as there are rows
  ## in rd
  if (! is.null(label) && length(label) != dim.rain[1]) {
    stop("label must contain as many elements as there are rows in rd!")
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

# prepareMatrix ----------------------------------------------------------------
prepareMatrix <- function(rain.mm, rdiff = NULL)
{
  if (is.null(rdiff)) {
    valid.mm <- rain.mm
  } else {
    valid.mm <- addAtIndices(
      x = rain.mm, 
      indices = selectColumns(rdiff, "idx"), 
      values = selectColumns(rdiff, "diff")
    )
  }
  
  matrix(c(valid.mm, rain.mm - valid.mm), byrow = TRUE, nrow = 2)
}

# addAtIndices --------------------------------------------------------------
addAtIndices <- function(x, indices, values)
{
  stopifnot(length(indices) == length(values))
  
  x[indices] <- x[indices] + values
  
  x
}
