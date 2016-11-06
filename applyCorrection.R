# applyCorrection --------------------------------------------------------------
applyCorrection <- function # Apply corrections according to validation
### Apply corrections according to validation
(
  rd,
  ### rain data
  cd,
  ### original correction data
  corr,
  ### detailed correction data as returned by rainValidation
  dbg = FALSE
)
{
  list(
    rd = applyCorrectionToRain(rd, corr$rd.diff, dbg = dbg),
    cd = applyCorrectionToCorr(cd, corr$cd.diff, dbg = dbg)
  )
}

# applyCorrectionToRain --------------------------------------------------------
applyCorrectionToRain <- function
(
  rd,
  dd,
  zero.thresh = 0.001,
  dbg = FALSE
)
{
  nrrd <- nrow(rd)
  nrdd <- nrow(dd)
  
  ## Loop through diff datasets
  for (i in 1:nrdd) {
    tb <- dd$tBeg_BWB[i]
    te <- dd$tEnd_BWB[i]
    gg <- as.character(dd$gauge[i])
    idx <- (1:nrrd)[((rd$tBeg_BWB == tb) & (rd$tEnd_BWB == te))]
    rd[idx, gg] <- rd[idx, gg] + dd$diff_mm[i]
    
    ## Set values that are almost zero to zero
    if (rd[idx, gg] < zero.thresh) rd[idx, gg] <- 0
  }
  
  rd
  
}

# applyCorrectionToCorr --------------------------------------------------------
applyCorrectionToCorr <- function(cd, dd, zero.thresh = 0.001, dbg = FALSE)
{
  nrcd <- nrow(cd)
  nrdd <- nrow(dd)
  
  catIf(dbg, sprintf("nrcd: %d, nrdd: %d\n", nrcd, nrdd))
  
  ## Loop through diff datasets
  for (i in 1:nrdd) {
    gg <- as.character(dd$gauge[i])
    
    catIf(dbg, sprintf("i: %3d, gg: %s\n", i, gg))
    
    idx <- (1:nrcd)[cd$tDate_BWB == dd$tDate_BWB[i]]
    cd[idx, gg] <- cd[idx, gg] + dd$diff_mm[i]
    
    ## Set values that are almost zero to zero
    if (cd[idx, gg] < zero.thresh) {
      cd[idx, gg] <- 0
    }
    
  }
  
  ## Update sum and absolute sum
  for (i in seq_len(nrcd)) {
    values <- cd[i, 2:(ncol(cd)-3)]
    cd$sum[i] <- sum(values)
    cd$abssum[i] <- sum(abs(values))
  }
  
  cd
}
