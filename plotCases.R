# plotCases --------------------------------------------------------------------
plotCases <- function
(
  cases, rainData, diffs = NULL, trim = TRUE, to.pdf = TRUE, args.pdf = NULL,
  ...
  ### passed to plotCase
)
{
  if (! is.null(diffs)) {
    stopifnot(nrow(cases) == length(diffs))
  }
  
  # Filter rain data for days to be considered in the cases
  rainDays <- unique(cases$day)
  rainData <- rainData[rainData$day %in% as.character(rainDays), ]
  
  file.pdf <- callWith(preparePdfIf, args.pdf, to.pdf = to.pdf)
  on.exit(finishAndShowPdfIf(to.pdf, file.pdf))
  
  for (i in seq_len(nrow(cases))) {
    
    case <- cases[i, ]
    
    caseData <- selectCaseData(rainData, case, neighb, trim = trim)
    
    main <- toPlotTitle(case = case, marked = 0, #sum(rdiff$diff)
                        prefix = sprintf("Case %d/%d - ", i, nrow(cases)))
    
    cat("\n***", main, "\n")
    
    plotCase(case, caseData, diffinfo = diffs[[i]], main = main
      #, method = 1 
      #, ...
    )
  }
}

# toPlotTitle -----------------------------------------------------------------
toPlotTitle <- function(case, marked = 0.0, prefix = "", dateformat = "")
{
  sprintf(
    "%s%s at Gauge '%s'\nto correct: %0.2f mm\ncorrected: %0.2f mm",
    prefix, format(case$day, dateformat), case$gauge, case$corr_mm, marked
  )
}

# plotCase ---------------------------------------------------------------------
plotCase <- function
(
  case, caseData, diffinfo, method = 2, main = "main?", ...
)
{
  # What are the excluded columns?
  rd <- caseData[, -(2:3)]
  
  if (method == 1) {
    
    plotRainAtGauge(rd, main = main)
    
  } else if (method == 2) {
    
    plotRainForValidation(
      rd = rd, main = main, barheights = .toBarheights(rd, diffinfo)
      #, ...
    )
  }
}

# .toBarheights ----------------------------------------------------------------
.toBarheights <- function(rd, diffinfo)
{
  if (is.null(diffinfo)) {
    return(NULL)
  }

  diffdata <- selectElements(diffinfo, "data")
  
  values <- selectColumns(merge(
    x = rd[, 1:2], 
    y = selectColumns(diffdata, c(names(rd)[1], "value.new")), 
    all.x = TRUE
  ), "value.new")
  
  selected <- ! is.na(values)
  
  data.frame(bar = which(selected), value = values[selected])
}
