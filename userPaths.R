# .testdir ---------------------------------------------------------------------
.testdir <- function()
{
  kwb.utils::createDirAndReturnPath(file.path(desktop(), "tmp/RTest"))
}

# .xlsdir ----------------------------------------------------------------------
.xlsdir <- function(home = FALSE) {
  if (home) {
    "C:/Dokumente und Einstellungen/Key Hauke/Desktop/tmp/Regen/validiert_2007"
  }
  else {
    #"//moby/miacso$/Daten/EXTERN/BWB/Regen_BWB/2_VAL"
    "C:/Users/hsonne/Desktop/tmp/BwbRain/validiert_2007"
  }
}

# getPathsForRainValidation: Set example paths or let the user choose ----------
getPathsForRainValidation <- function(xls.dir = "", example = 0)
{
  ## Rain data of 2011 Q4:
  ## //moby/miacso$/Daten/EXTERN/BWB/Regen_BWB/ab2008/2011_Q4/roh/
  ##   Regenschreiberdaten-Q4-2011.xls
  ## has been saved as:
  #xls.dir <- "//moby/miacso$/Daten/EXTERN/BWB/Regen_BWB/ab2008/_VAL/validiert_2011_Q4"
  
  if (example == 0) {
    
    # Arguments to file.choose()
    args <- args_file.choose(xls.dir)
    
    xls.rd <- callWith(
      choose.files, args, caption = "Select rain gauge data file..."
    )
    
    xls.cd <- callWith(
      choose.files, args, caption = "Select rain correction data file..."
    )
    
  } else if (example == 1) {
    
    xls.rd <- file.path(xls.dir, "Regenschreiberdaten-Q4-2011_hs.xls")
    xls.cd <- file.path(xls.dir, "2011 KWB Regenschreiber Validierung_hs.xls")
    
  } else if (example == 2) {
    
    xls.dir <- "//moby/miacso$/Daten/EXTERN/BWB/Regen_BWB/2_Prep"
    
    xls.rd <- file.path(xls.dir, "Regen_2012_hs.xlsx")
    xls.cd <- file.path(xls.dir, "RegenValidierung_2012_hs.xls")
    
  } else if (example == 3) {
    
    xls.dir <- "/home/hauke/Desktop/2_Prep"
    
    xls.rd <- file.path(xls.dir, "Regen_2011_hs.csv")
    xls.cd <- file.path(xls.dir, "RegenValidierung_2011_hs.csv")
    
  } else if (example == 4) {
    
    xls.dir <- "/home/hauke/Desktop/2_Prep"
    
    xls.rd <- file.path(xls.dir, "Regen_2012_hs.csv")
    xls.cd <- file.path(xls.dir, "RegenValidierung_2012_hs.csv")
    
  } else {
    stop("Example not in 0 ... 3!")
  }
  
  list(xls.rd = xls.rd, xls.cd = xls.cd)
}

# args_file.choose -------------------------------------------------------------
args_file.choose <- function(xls.dir)
{
  extensions <- "*.xls;*.xlsx"
  
  list(
    default = file.path(xls.dir, extensions),
    filters = matrix(c("Excel files", extensions), byrow = TRUE, nrow = 1)
  )
}
