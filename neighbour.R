# getNeighbourMatrix -----------------------------------------------------------
getNeighbourMatrix <- function(gauges = NULL)
{
  if (file.exists(kwb.read::mdb_rain_meta())) {
    distanceToNeighbour(getGaugeDistances())
  } else {
    message("no neighbour data available, using random neighbours!")
    randomNeighbours(gauges)
  }
}

# randomNeighbours -------------------------------------------------------------
randomNeighbours <- function(gauges)
{
  structure(
    do.call(rbind, lapply(seq_along(gauges), function(i) sample(gauges[-i]))),
    dimnames = list(gauges, paste0("n", seq_len(length(gauges) - 1)))
  )
}

# neighbourGauges --------------------------------------------------------------
neighbourGauges <- function
(
  gauge, neighb = NULL, num.neighb = ncol(neighb)
)
{
  if (! is.null(neighb)) {
    neighb[gauge, colnames(neighb)[seq_len(num.neighb)]]
  } else {
    NULL
  }
}
