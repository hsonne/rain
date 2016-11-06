# replaceNaInRain --------------------------------------------------------------
replaceNaInRain <- function
# Replace NA values in rain data with rain data from neighbour gauges
(
  rd,
  ### data frame with first two columns tBeg_BWB and tEnd_BWB and remaining
  ### columns containing rain heights in mm
  smode = "meanOfNearest",
  ### substitution mode
  neighb = NULL,
  ### matrix of gauge neighbours as returned by distanceToNeighbour
  mdist = NULL,
  ### matrix of distances
  maxdist = 15300,
  maxgauges = 2,
  dbg = TRUE
)
{
  ## Check prerequisites
  if (smode == "nearest") {
    if (is.null(neighb)) {
      stop("neighbour matrix 'neighb' needed!")
    }
  }
  else if (smode == "meanOfNearest") {
    if (is.null(mdist)) {
      stop("distance matrix 'mdist' needed!")
    }
  }
  else {
    stop("Unknown substitution mode:", smode)
  }
  
  cat(paste("*** NA values are substituted by mean of non-NA signals",
            sprintf("of gauges in max. %d m distance around gauge with NA value\n",
                    maxdist)))
  
  ## Find indices of rows containing at least one NA value
  rd.na <- matrix(as.integer(is.na(rd[, -c(1,2)])), nrow = nrow(rd))
  
  idx <- which(rowSums(rd.na) > 0)
  
  if (isTRUE(dbg)) {
    printIf(TRUE, idx, "Rows containing NA")
    rd.tmp <- rd[idx, ]
    rd.tmp[, -(1:2)] <- round(rd.tmp[, -(1:2)], 1)
    printIf(TRUE, rd.tmp, "Rounded values")
  }
  
  ## Loop through rows containing NAs
  for (i in idx) {
    
    ## Print timestamp
    
    #@2012-03-15
    #cat(sprintf("%s;%s", rd[i, 1], rd[i, 2]))
    
    ## If row sum is 0, set all gauge signals to 0
    if (sum(rd[i, -c(1, 2)], na.rm = TRUE) == 0) {
      rd[i, -c(1, 2)] <- 0
      
      #@2012-03-15
      #cat(";all NA -> 0.0 mm (all non-NA = 0.0 mm)\n")
    }
    else {
      
      #browser(expr=TRUE)
      
      ## Do the following modifications on copy of current row
      rd.row <- rd[i, ]
      
      printIf(dbg, rd.row, "Current row")
      
      # names of gauges holding NA value
      na.gauges <- names(rd)[is.na(rd.row)]
      
      printIf(dbg, na.gauges, "gauges with NA signal")
      
      ## Print timestamp
      #@2012-03-15
      cat(sprintf("%s", rd[i, 1]))
      
      ## Loop through columns containing NA
      for (gg in na.gauges) {
        
        if (smode == "nearest") {
          sv <- getNearestNonNA(rd[i, ], neighb[rownames(neighb) == gg, ])
          cat(sprintf(";subst(%s)=%0.2f", gg, sv))
        }
        else if (smode == "meanOfNearest") {
          sv <- getMeanOfNearest(rd[i, ], gg, mdist, maxdist, maxgauges, dbg = dbg)
        }
        
        rd.row[1, gg] <- sv
      }
      cat("\n")
      
      ## Assign the new row
      rd[i, ] <- rd.row
    }
  }
  
  ## Return rain data
  rd
}

# getNearestNonNA: helper function for replaceNaInRain -------------------------
getNearestNonNA <- function(rdrow, neighb) 
{
  sig <- NA
  k <- 0
  
  while (k < length(neighb) && is.na(sig)) {
    
    k <- k + 1
    ngb <- neighb[k]
    sig <- rdrow[, ngb]
    # cat(sprintf("Rain signal of %d. neighbour %s: %0.2f\n", k, ngb, sig))
  }
  
  sig
}

# getMeanOfNearest: helper function for replaceNaInRain ------------------------
getMeanOfNearest <- function
(
  rdrow,
  gauge,
  mdist,
  maxdist,
  maxgauges,
  dbg = TRUE
)
{
  sig <- NA
  
  ## find neighbour gauges within maxdist but no more than <maxgauges>
  ## neighbours
  
  #nnames <- names(mdist[gauge, mdist[gauge, ] < maxdist])
  ## exclude gauge name itself
  #nnames <- setdiff(nnames, gauge)
  
  inReach <- mdist[gauge, mdist[gauge, ] < maxdist]
  nnames <- names(inReach)[order(inReach)][2:(maxgauges + 1)]
  
  if (length(nnames) > 0) {
    nnames <- as.character(na.omit(nnames)) ## Remove NA
  }
  else {
    printIf(TRUE, inReach, "inReach")
    printIf(TRUE, gauge, "gauge")
    printIf(TRUE, rdrow, "drow")
  }
  
  if (length(nnames) > 0) {
    
    ## Calculate the mean of the values measured at these neighbour gauges
    rvals <- as.numeric(rdrow[1, nnames])
    
    printIf(dbg, rvals, "avail. values")
    
    sig <- mean(rvals, na.rm = TRUE)
    
    cat(sprintf(
      ";%s = %0.2f mm [= mean(%s)]", gauge, sig, paste(
        nnames, sprintf("%5.2f", rdrow[, nnames]), sep = " = ", collapse = ", "
      )
    ))
  }
  else {
    cat(sprintf(";%s = NA (no gauges within %d m!)", gauge, maxdist))
  }
  
  ## Return mean signal
  sig
}
