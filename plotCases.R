# plotCases --------------------------------------------------------------------
#neighb = NULL;num.neighb = 0;trim = FALSE
plotCases <- function
(
  cases, rainData, diffs = NULL, 
  neighb = NULL, num.neighb = 0, trim = TRUE, to.pdf = TRUE,
  ...
)
{
  if (! is.null(diffs)) {
    stopifnot(nrow(cases) == length(diffs))
  }

  file.pdf <- preparePdfIf(to.pdf)
  on.exit(finishAndShowPdfIf(to.pdf, file.pdf))
  
  for (i in seq_len(nrow(cases))) {
    
    case <- cases[i, ]
  
    caseData <- selectCaseData(rainData, case, neighb, num.neighb, trim = trim)
    
    main <- toPlotTitle(case = case, marked = 0, #sum(rdiff$diff)
                        prefix = sprintf("Case %d/%d - ", i, nrow(cases)))
    
    cat("\n***", main, "\n")

    plotCase(case, caseData, diffinfo = diffs[[i]], main = main)#, ...)
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
  rd <- caseData[, -(2:3)]
  
  if (method == 1) {
    plotRainAtGauge(rd, main = main)
  } else if (method == 2) {
    plotArgs <- list(
      NULL
      , rd = rd
      , main = main
      #, rdiff = data.frame(decreasingOrder = prp, diff = hts - rdd[prp, gauge])
      #, label = .barLabels(rd)
      #, ...
    )
    
    callWith(plotRainForValidation, plotArgs)
  }
}
