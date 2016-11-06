# additionalTimeColumns --------------------------------------------------------
additionalTimeColumns <- function(rd)
{
  datetimes <- selectColumns(rd, "tBeg_BWB")
  
  # Convert summer time to winter time
  datetimes.wt <- kwb.base::hsST2WT(tstamps = datetimes)
  
  data.frame(
    tDate_BWB = as.Date(datetimes),
    tBeg_DST  = (datetimes != datetimes.wt),
    tBeg_UTCp1 = datetimes.wt
  )
}

# rainToLongFormat -------------------------------------------------------------
rainToLongFormat <- function(rd)
{
  hsMatrixToListForm(
    rd,
    keyFields = c("tBeg_DST", "tBeg_BWB", "tEnd_BWB", "tDate_BWB", "tBeg_UTCp1"),
    colNamePar = "gauge",
    colNameVal = "raw_mm"
  )
}

# corrToLongFormat -------------------------------------------------------------
corrToLongFormat <- function(cd)
{
  hsMatrixToListForm(
    cd,
    keyFields = "tDate_BWB",
    parFields = setdiff(names(cd), c("tDate_BWB", "sum", "abssum")),
    colNamePar = "gauge",
    colNameVal = "corr_mm"
  )
}
