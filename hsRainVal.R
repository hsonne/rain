library(kwb.utils) # e.g. for hsMatrixToListForm, getObjectFromRDataFile
library(kwb.read) # for readBwbRainData, readBwbRainCorrection
library(kwb.misc)
library(lattice)
library(kwb.db) # for hsSqlQuery
library(kwb.datetime) # for hsDateStr

scriptdir <- "/home/hauke/RProgramming/github/rain"

scripts <- c("applyCorrection", "convert", "doRainValidation", "neighbour", 
             "plotCases", "plotRainForValidation", "prevalidate", "replaceNA", 
             "userDecision", "userPaths")

sourceScripts(file.path(scriptdir, paste0(scripts, ".R")))

write.to <- c(mdb = FALSE, csv = FALSE)

## Idea of a good validation procedure =========================================
##
## 01. Load raw rain signals from xls -> rd.orig
## 02. Convert rd.orig to list form -> rd.list
## 03. Write rd.list to mdb::tbl_1_RawSignal
##
## 04. Load daily correction values -> cd.orig
## 05./06. Write correction data (in "long" format) to mdb::tbl_2_CorrPerDay
##
## 07. Auto-validation(rd, cd) -> negative correction values in rd.diff
## 08. rd.diff$comment <- "auto-val in R"
## 09. Write rd.diff to mdb::tbl_3_CorrSignal
##
## 10. Go through list of days that could not be auto-validated in step 07:
##     Find calibration/false signals manually and extend tbl_3_CorrSignals
##     accordingly
##
## 11. Prepare a text file (csv) in which gauge failures are listed
## 12. Load gauge failure information from file prepared in step 11 -> fi
## 13. Write fi to mdb::tbl_4_Failure

# Get BWB rain data (from xls-files) for validation ----------------------------
if (TRUE)
{
  gauges <- NULL # DELETE AFTER TESTING!
  
  xls.dir <- .xlsdir(home = FALSE)
  
  ## Step 01: Load raw rain signals from xls
  paths <- getPathsForRainValidation(xls.dir, example = 4)
  
  file <- file.path(.testdir(), "rain.RData")
  
  if (file.exists(file)) {
    rainData <- getObjectFromRDataFile(file, "rainData")
    corrData <- getObjectFromRDataFile(file, "corrData")
  } else {
    rainData <- readBwbRainData(
      file = paths$xls.rd, use2007Driver = TRUE, sep = ";", dec = ",",
      format = "%d.%m.%y %H:%M:%S"
    )
    ## Step 04: Load daily correction values -> cd.orig
    corrData <- readBwbRainCorrection(
      file = paths$xls.cd, zerolines.rm = TRUE, dbg = TRUE, country = "de"
    )
    save(rainData, corrData, file = file)
  }
  
  # Provide a matrix containing for each gauge its nearest neighbour gauges
  neighb <- getNeighbourMatrix(gauges = names(rainData)[-(1:2)])[, 1:1, drop = FALSE]
}

# Do the validation ------------------------------------------------------------
if (FALSE)
{
  ## Step 07: Auto-validation(rd, cd) -> negative correction values in rd.diff
  system.time(out <- capture.output(
    corr <- doRainValidation(rainData, corrData, neighb = neighb, ask = FALSE)
  ))
  
  cases1 <- rbindAll(corr$RESULT)
  cases2 <- kwb.rain::getCorrectionCases(cd.orig, rd.orig)
  
  stopifnot(identical(cases1, cases2[1:3]))

  corr.rdata <- file.path(.testdir(), "corr.RData")
  #save(corr, out, cases, file = corr.rdata)
  identical(corr, getObjectFromRDataFile(corr.rdata, "corr"))
  identical(out, getObjectFromRDataFile(corr.rdata, "out"))
  
  if (write.to["mdb"]) {
    ## Step 02: Convert rd.orig to list form -> rd.list.
    ## Step 02a: append an additional date column; this will facilitate manual
    ##           browsing through the database table for wrong signals.
    ## Step 02b: append an additional column indicating if timestamp belongs to
    ##           the summer time interval in which daylight saving time (DST) is
    ##           active. So we are able to create a primary key for the mdb table.
    rd.list <- rainToLongFormat(cbind(rd.orig, additionalTimeColumns(rd.orig)))
  
    ## Step 03: Write rd.list to mdb::tbl_1_RawSignal and set primary key
    mdb <- file.path(xls.dir, "rainval.mdb")
    
    columns <- c("gauge", "tDate_BWB", "tBeg_DST", "tBeg_UTCp1",
                 "tBeg_BWB", "tEnd_BWB", "raw_mm")
    
    tbl <- hsPutTable(mdb, rd.list[, columns], "tbl_1_RawSignal")
    
    #hsSetPrimaryKey(mdb, tbl, c("gauge", "tBeg_DST", "tBeg_BWB"))
    hsSetPrimaryKey(mdb, tbl, c("gauge", "tBeg_UTCp1"))

    ## Step 05/06: Write correction data (in "long" format) to mdb::tbl_2_CorrPerDay
    tableName <- hsPutTable(mdb, corrToLongFormat(cd.orig), "tbl_2_CorrPerDay")
    hsSetPrimaryKey(mdb, tableName, c("gauge", "tDate_BWB"))
  }
  
  ## Step 08: rd.diff$comment <- "auto-val in R"
  corr$rd.diff$comment <- "auto-val in R"
  
  ## Step 09: Write rd.diff to mdb::tbl_3_CorrSignal
  if (write.to["mdb"]) {
    hsPutTable(mdb, corr$rd.diff, "tbl_3_CorrSignal")
  }
  
  ## Preparation of Step 10:
  ## - 10a: Generate pdf showing "history" of auto-validation
  pageAndPlot(corr$dbgRain, rpp = 80, cex = 0.5,
              main = "Modifications done during rain validation")
  
  ## - 10b: Print table of rain heights per day to pdf
  kwb.rain::plotDailyRainHeightTable(
    rd.orig[, -1], landscape = FALSE, ppp = 2, cex = 0.5
  )
  
  ## - 10c: Plot rain event overview to pdf; this file shall be used to decide
  ##   whether signals look like calibration signals.
  plotRainEventOverview(rd.orig[, -1])
  
  ## - 10d: Plot cumulative rain per day to pdf; EPR uses these kind of diagrams
  ##   to decide whether a rain gauge "hangs".
  
  ##   * Calculate cumulative rain heigths within each day
  rd.long <- kwb.rain::getDailyCumulativeRain(rd.orig)
  
  ##   * Write cumulated rain data in "list form" to database table (if required)
  if (write.to["mdb"]) {
    hsPutTable(mdb, rd.long, "tbl_1a_RawCumSig")
  }
  
  ##   * Plot cumulative rain per day
  kwb.rain::plotCumulativeRain(rd.long, to.pdf = TRUE)
  
  ## - 10e: Print out the pdf file generated during Step 10a, you will need it
  ##   during Step 10; all situations in which the correction value could not
  ##   be assigned to signals are indicated with "***" in column "action"
  
  ## Step 10: Go through the list generated in Step 10a, looking for gauges and
  ##          days that could not be auto-validated in Step 07. Use the diagrams
  ##          produced in Steps 10c and 10d to find calibration/false signals
  ##          manually and extend tbl_3_CorrSignals in mdb accordingly. Give
  ##          a comment on who decided for the correction.
  
  ## Step 11: Provide a table "tbl_4_CorrFailure" in mdb with columns
  ##            containing time intervals from which it is known
  ##          that gauges did not work correctly
  ##
  
  ## Step 12: Run query "qryCreateRainValid" in mdb to create a table
  ##          tbl_RainValid containing the valid rain series
  
  ## Step 13: Check tbl_RainValid for last calibration signals that have not
  ##          been documented in the Excel file of daily correction valus.
  ##          If there are values that look like calibration signals insert
  ##          additional records into "tbl_3_CorrSignal" and recreate the
  ##          tbl_RainValid by rerunning Step 12.
  
  ## Step 14: If we agree with the data in tbl_RainValid we can load it and
  ##          convert timestamps to UTC+1
  rd.valid <- hsMdbTimeSeries(mdb, "tbl_RainValid", sqlFilter = "ORDER BY ")
  
  ## write raw data to mdb in folder xls.dir and produce overview graphs and
  ## table showing rain per day
  #hsRainValAna1(mdb <- file.path(xls.dir, "rainVal.mdb"), rd.orig)
  
  ## Apply the corrections to rd and cdi
  res <- applyCorrection(rd, cd, corr)
  rd <- res$rd
  cd <- res$cd
  
  ## Convert timestamp to UTC+1 in new rain data
  rd.final <- rd
  #rd.final$myDateTime <- hsST2WT(rd.final$tEnd_BWB)
  rd.final$myDateTime <- rd.final$tEnd_BWB
  
  ## Remove original timestamp columns
  rd.final <- rd.final[, -c(1, 2)]
  
  ## Make timestamp column the first column
  n.cols <- ncol(rd.final)
  rd.final <- rd.final[, c(n.cols, 1:(n.cols - 1))]
  
  ## Save final rain data to database
  if (write.to["mdb"]) {
    mdb <- "//moby/miacso$/Daten/ACCESS/Regen/Regendaten_BWB_ab2008.mdb"
    tbl <- "tblVal_2007_withoutUserDecision_DLS" #"tblVal_2011_lastTwoDays"
    tbl <- hsPutTable(mdb, rd.final, tbl)
    
    ## Append new data to "total" table
    hsSqlQuery(mdb, paste (
      "INSERT INTO tbl_Regen_alleEZG_05min (",
      "  Zeitstempel, Bln_IV, Bln_V, Bln_IX, Bln_X, Bln_XI, Nkn_I, Nkn_II, Chb_I, Wil, Wil_a, Lbg, Hlg, Zhl_I, K?p_I_f, Kar, Spa_II",
      ")",
      "SELECT",
      "  myDateTime,  BlnIV,  BlnV,  BlnIX,  BlnX,  BlnXI,  NknI,  NknII,  ChbI,  Wil, Wila,  Lbg, Hlg, ZhlIe, KoepIf,  Kar, SpaII",
      "FROM", tbl))
  }
  
  ## Save new correction dataset to csv files
  if (write.to["csv"]) {
    write.csv2(cd, file = file.path(xls.dir, "hsValCorrDataNew2.csv"))
  }
  
  ##
  ## Manually: write screen output to hsValLog.txt...
  ##
}

# DST time to UTC1 =============================================================
if (FALSE) {
  
  ## Overwrite timestamps in first column with timestamps indicating the
  ## end of the interval, converted from summer time to winter time
  df$tBeg.UTC1 <- hsST2WT(df$tBeg_BWB)
  df$tEnd.UTC1 <- hsST2WT(df$tEnd_BWB)
  
  ## Test the result: count timestamps per day
  
  cmp <- merge(
    x = hsTStampsPerDay(df$tEnd.UTC1, "tEnd.UTC1"),
    y = hsTStampsPerDay(df$tEnd_BWB,  "tEnd_BWB"),
    by.x = "tEnd.UTC1", 
    by.y = "tEnd_BWB",
    all = TRUE, 
    suffixes = c(".UTC1", ".BWB")
  )
  
  idx <- (is.na(cmp[, 2]) | is.na(cmp[, 3]) | (cmp[, 2] != cmp[, 3]))
  
  printIf(TRUE, cmp[idx, ], "Differences in number of rows")
}

# Test hsST2WT =================================================================
if (FALSE)
{
  ts1 <- seq(as.POSIXct("2011-10-30 01:00:00"), 
             as.POSIXct("2011-10-30 04:00:00"), 300)
  
  res <- hsST2WT(ts1)
}

# Rain per day and per year ====================================================
if (FALSE)
{
  x <- rd[3:ncol(rd)]
  
  ## rain per day
  rpd <- aggregate(x, by = list(day = hsDateStr(rd$tBeg_BWB)), FUN = sum)
  
  ## rain per year
  rpy <- aggregate(x, by = list(year = format(rd$tBeg_BWB, "%Y")), FUN = sum)
}

# Create table with distances between rain gauges ==============================
if (FALSE)
{
  ## Set path to rain database
  mdb <- mdb_rain_meta()
  
  ## Get matrix of distances between gauges
  mdist <- getGaugeDistances(getGaugeInfo(mdb))
  
  ## Create matrix of neighbours from distance matrix
  neighb <- distanceToNeighbour(mdist)
  
  dist.df <- as.data.frame(mdist)
  dist.df <- cbind(fromTo = rownames(mdist), dist.df)
  
  neighb.df <- as.data.frame(neighb)
  neighb.df <- cbind(gauge = rownames(mdist), neighb.df)
  
  ## Write distance table and neighbour table to database
  hsPutTable(mdb, dist.df, "tblGaugeDist")
  hsPutTable(mdb, neighb.df, "tblNeighbour")
}

# Import pumping station coordinates ===========================================
if (FALSE)
{
  path <- "//moby/miacso$/Daten/EXTERN/BWB/Pumpwerke"
  
  info.apw <- read.csv(file.path(path, "APW.csv"))
  info.hpw <- read.csv(file.path(path, "HPW.csv"))
  info.upw <- read.csv(file.path(path, "UEPW.csv"))
  info.pw <- rbind(info.apw, info.hpw, info.upw)
  
  hsPutTable(mdb_rain_meta(), info.pw, "tblPwInfo")
}

# Prepare rain series for InfoWorks ============================================
if (FALSE)
{
  ## This has been already done for 2011 data and I will try to use this to
  ## do (almost) the same for the 2007 data
  yr <- 2007
  
  ## Get 2011 data
  if (yr == 2011) {
    mdb <- "//moby/miacso$/Daten/ACCESS/Regen/Regendaten_BWB_ab2008.mdb"
    rd2011 <- hsMdbTimeSeries(mdb, "tbl_Regen_alleEZG_05min", "Zeitstempel",
                              minDate = "2011-01-01")
    print(names(rd2011))
    #Column names: Zeitstempel,Bln_IV,Bln_V,Bln_IX,Bln_X,Bln_XI,Nkn_I,Nkn_II,
    #                          Chb_I,Wil,Wil_a,Lbg,Hlg,Zhl_I,K?p_I_f,Kar,Spa_II
    
    ## Rename columns
    names(rd2011) <- hsGaugeNamesShort(names(rd2011), underscore.rm = TRUE)
    print(names(rd2011))
    
  } else if (yr == 2007) {
    mdb <- "C:/Users/hsonne/Desktop/tmp/BwbRain/validiert_2007/rainval.mdb"
    rd2007 <- hsMdbTimeSeries(mdb, "qry_RainValid_2", "Zeitstempel")
    print(names(rd2007))
    ## BlnIV is NULL everywhere -> set type to numeric
    rd2007$BlnIV <- as.numeric(rd2007$BlnIV)
    ## Set values below threshold to 0.0
    rd2007[, -1][abs(rd2007[, -1]) < 0.0001] <- 0
    rd2011 <- rd2007
  }
  
  ## Just a last check: do values seem to be valid?
  summary(rd2011)
  
  ## There are many NAs for Bln IV, this should be checked...
  ## It is because MR set validated signals to NA and not to 0.0!
  ## Maybe this should be changed in applyCorrection, too!!!
  
  ## Get matrix of distances
  mdist <- getGaugeDistances()
  
  ## Just to make column names fit:
  mdnames <- sub("ZhlIe", "ZhlI", colnames(mdist))
  colnames(mdist) <- mdnames
  rownames(mdist) <- mdnames
  
  ## Provide another timestamp column to make replaceNaInRain work
  rd2011 <- rd2011[, c(1, seq_len(ncol(rd2011)))]
  
  ## Substitute NAs in complete timeseries of 2011
  rd2011.na.rm <- replaceNaInRain(rd = rd2011, smode = "meanOfNearest",
    mdist = mdist, maxdist = 15300, dbg = FALSE)
  
  rd2011.final <- rd2011.na.rm[,-2]
  
  ## How many NA values remained?
  rows <- rowSums(is.na(rd2011.final[, -1])) > 0
  
  rd2011.final[rows, ]
  
  ## Result for 2007 data:
  #   Zeitstempel      BlnIV BlnV      BlnIX       BlnX     BlnXI NknI      NknII       ChbI       Wil      Wila        Lbg       Hlg       ZhlI KoepIf       Kar      SpaII
  #   16882 2007-02-28 14:50:00        NaN  NaN        NaN        NaN       NaN  NaN        NaN 0.00000000 0.0000000 0.0000000        NaN       NaN 0.19995117    NaN       NaN        NaN
  #   16883 2007-02-28 14:55:00        NaN  NaN        NaN        NaN       NaN  NaN        NaN 0.00000000 0.0000000 0.0000000        NaN 0.0000000 0.30004883    NaN       NaN 0.20019531
  #   16884 2007-02-28 15:00:00 0.00000000  NaN 0.00000000 0.00000000 0.0000000  NaN        NaN 0.00000000 0.0000000 0.0000000        NaN 0.0000000 0.09997559    NaN 0.0000000 0.19995117
  #   16885 2007-02-28 15:05:00 0.15000153  NaN 0.00000000 0.30000305 0.3000031  NaN        NaN 0.00000000 0.0000000 0.0000000        NaN 0.0000000 0.40002441    NaN 0.3000031 0.00000000
  #   16886 2007-02-28 15:10:00 0.04998779  NaN 0.09997559 0.00000000 0.0000000  NaN 0.09997559 0.04998779 0.0000000 0.0000000 0.09997559 0.1000061 0.40002441    NaN 0.0000000 0.00000000
  #   16887 2007-02-28 15:15:00 0.04999542    0 0.00000000 0.09999084 0.0000000  NaN 0.00000000 0.10003662 0.1000366 0.1000061 0.00000000 0.0000000 0.09997559    NaN 0.1000061 0.09985352
  
  ## Seems to be ok if we assume that the NaNs are 0. During the generation of
  ## the InfoWorks-input file NaNs will be automatically converted to 0.
  
  ## save as csv
  if (yr == 2011) {
    write.table(
      rd2011.final,
      file = "//moby/miacso$/Daten/EXTERN/BWB/Infoworks/2011_komplett/inputFiles/rain2011forIw_15300.txt",
      dec = ".", sep = ";", na = "", quote = FALSE, row.names = FALSE
    )
  } else {
    rd2011.final[, -1] <- round(rd2011.final[, -1], digits=3)
    write.table(
      rd2011.final,
      file = "//moby/miacso$/Daten/EXTERN/BWB/Infoworks/2007_komplett_inputFiles/rain2007forIw_15300.txt",
      dec = ".", sep = ";", na = "", quote = FALSE, row.names = FALSE
    )
  }
  
  ## Provide additional "user" substitutions
  us <- data.frame(
    from = hsToPosix("2011-07-30"),
    to = hsToPosix("2011-08-02"),
    gauge = "NknI",
    substgauges = "NknII", 
    stringsAsFactors = FALSE
  )
  
  #   usEPR2007 <- list(
  #     data.frame(from = hsToPosix("2007-02-08 00:00:00"),
  #                to   = hsToPosix("2007-02-12 00:30:00"),
  #                gauge = "NknI",
  #                substgauges = "NknII", stringsAsFactors = FALSE),
  #     data.frame(from = hsToPosix("2011-07-30"),
  #                to   = hsToPosix("2011-08-02"),
  #                gauge = "NknI",
  #                substgauges = "NknII", stringsAsFactors = FALSE),
  #     data.frame(from = hsToPosix("2011-07-30"),
  #                to   = hsToPosix("2011-08-02"),
  #                gauge = "NknI",
  #                substgauges = "NknII", stringsAsFactors = FALSE),
  #     data.frame(from = hsToPosix("2011-07-30"),
  #                to   = hsToPosix("2011-08-02"),
  #                gauge = "NknI",
  #                substgauges = "NknII", stringsAsFactors = FALSE))
  #
  #   rdtmp <- rd2011.final
  
  ## Get an overview on the substitution to be done...
  rsel <- (rdtmp[[1]] >= us$from & rdtmp[[1]] <= us$to)
  rdtmpsel <- rdtmp[rsel, c("Zeitstempel", us$gauge[1], us$substgauges[1])]
  
  opar <- par(mfrow = c(2, 1))
  
  labels <- rep(NA, nrow(rdtmpsel))
  idx <- seq(1, length(labels), 24)
  labels[idx] <- format(rdtmpsel[idx, 1], "%Y-%m-%d %H:%M")
  
  barplot(rdtmpsel[[2]], names.arg = labels, las = 2)
  barplot(rdtmpsel[[3]], names.arg = labels)
  
  par(opar)
  
  ## Apply the "user" substitutions
  rdtmp[rsel, us$gauge] <- rdtmp[rsel, us$substgauges]
  
  ## save as csv
  write.table(
    rdtmp,
    file = "//moby/miacso$/Daten/EXTERN/BWB/Infoworks/2011_komplett/inputFiles/rain2011forIw_15300_us01.txt",
    dec = ".", sep = ";", na = "", quote = FALSE, row.names = FALSE)
}

# Prepare climate-change rain series for InfoWorks =============================
if (FALSE)
{
  mdb <- "C:/Users/hsonne/Desktop/tmp/BwbRain/validiert_2007/rainval.mdb"
  basedir <- file.path("//moby/miacso$/Daten/EXTERN/BWB/Infoworks",
                       "2007_komplett_inputFiles")
  
  rd2007p20 <- hsMdbTimeSeries(mdb, "qryValAfterAutoNaSubst_plus20 ", "Zeitstempel")
  rd2007p20[, -1] <- round(rd2007p20[, -1], digits=3)
  write.table(rd2007p20,
              file = file.path(basedir, "rain2007plus20perc",
                               "rain2007forIw_15300_plus20perc.txt"),
              dec = ".", sep = ";", na = "", quote = FALSE, row.names = FALSE)
  
  rd2007m20 <- hsMdbTimeSeries(mdb, "qryValAfterAutoNaSubst_minus20 ", "Zeitstempel")
  rd2007m20[, -1] <- round(rd2007m20[, -1], digits=3)
  write.table(rd2007m20,
              file = file.path(basedir,
                               "rain2007minus20perc",
                               "rain2007forIw_15300_minus20perc.txt"),
              dec = ".", sep = ";", na = "", quote = FALSE, row.names = FALSE)
}

# @2012-09-24: Save RAW rain signals of 2007 to 2012 to mdb --------------------
if (FALSE)
{
  # Raw rain data is provided by BWB in forms of Excel files in
  # //moby/miacso$/Daten/Extern/BWB/Regen_BWB/1_Raw
  # In order to have easy access to all these raw data in forms of a database
  # I have imported it to mdb by using the following R script section.
  # Before, I manually prepared one Excel file per year by copying diverse
  # parts of raw Excel files together. The prepared Excel files are here:
  # //moby/miacso$/Daten/EXTERN/BWB/Regen_BWB/2_Prep
  
  ## Path to RAW rain data mdb
  mdb <- "//moby/miacso$/Daten/ACCESS/Regen/1_RAW/BWB_Rain_RAW.mdb"
  
  ## Select input file of one year's rain data
  paths <- getPathsForRainValidation("//moby/miacso$/Daten/Extern/BWB/Regen_BWB/2_Prep")
  
  ## get data with new (character!) column tBeg_WT (winter time)
  rd <- readBwbRainData(paths$xls.rd, toUTC=TRUE, toWT = TRUE)
  
  ## Save rain data to table "tblTmpRaw" in mdb by overwriting the existing one
  tbl <- "tblTmpRaw"
  hsPutTable(mdb, rd[, c(ncol(rd), 1:(ncol(rd)-1))], tbl, overwrite = TRUE)
  
  ## Set primary key
  hsSetPrimaryKey(mdb, tbl, keyFields = "tBeg_WT")
  
  ## Now, run query "qry_02_INSERT_INTO_tblRainRaw" in mdb manually in order
  ## to append the new data to tblRainRaw...
  
  ## Repeat this section for years 2007, 2008, 2009, 2010, 2011, 2012
  ## ATTENTION! In "Regen_2008_hs.xlsx" data of Bln X starts with
  ## "[-11059] No Good Data For Calculation". This has the effect that when
  ## getting the file content, column "BlnX" is treated as character. In this
  ## specific case I went through the commands in readBwbRainData manually
  ## and did the transformation to numeric (after replacing "," with ".")!
}

# @2012-09-24: Find data gaps in raw rain data ---------------------------------
if (FALSE)
{
  ## Path to RAW rain data mdb
  mdb <- "//moby/miacso$/Daten/ACCESS/Regen/1_RAW/BWB_Rain_RAW.mdb"
  
  ## Get all (character!) timestamps (winter time)
  df <- hsGetTable(mdb, "tblRainRaw", fields = "tBeg_WT",
                   stringsAsFactors = FALSE)
  
  ## convert character timestamps to UTC timestamps and find gaps
  hsEvents(hsToPosix(df$tBeg_WT), evtSepTime = 300, signalWidth = 300)
  
  #     iBeg   iEnd                tBeg                tEnd       dur pBefore pAfter
  # 1      1 577716 2007-01-01 00:00:00 2012-06-28 22:55:00 173314800      NA 172800
  # 2 577717 599316 2012-06-30 23:00:00 2012-09-13 22:55:00   6480000  172800     NA
  
  # NICE! Data is only missing for 29/30 of June 2012.
  # -> I sent a mail to Mr. Broll asking for the missing data.
}

# @2012-09-24: Save daily correction data to mdb -------------------------------
if (FALSE)
{
  # @2012-09-25: We save the validation data also in "BWB_Rain_RAW.mdb" as it
  #              is "raw" in terms of being provided by BWB
  #mdb <- "//moby/miacso$/Daten/ACCESS/Regen/2_VAL/BWB_Rain_VAL.mdb"
  mdb <- "//moby/miacso$/Daten/ACCESS/Regen/1_RAW/BWB_Rain_RAW.mdb"
  
  ## Select input file of one year's rain validation data
  paths <- getPathsForRainValidation("//moby/miacso$/Daten/Extern/BWB/Regen_BWB/2_Prep")
  
  ## Load daily correction values -> cd.orig
  cd <- readBwbRainCorrection(file = paths$xls.cd, zerolines.rm = TRUE, dbg = TRUE)
  
  ## Save correction data to table "tblTmpCorrDay" in mdb by overwriting
  ## the existing one
  tbl <- "tblTmpCorrDay"
  hsPutTable(mdb, cd, tbl, overwrite = TRUE)
  
  ## Set primary key
  hsSetPrimaryKey(mdb, tbl, keyFields = "tDate_BWB")
}
