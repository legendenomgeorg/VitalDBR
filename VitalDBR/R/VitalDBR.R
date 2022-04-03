#' Loads file from URL
#' @export
#' @param file_url URL for API endpoint
load_VDB <- function(file_url) {
  con <- gzcon(url(file_url))
  txt <- readLines(con)
  return(read.csv(textConnection(txt)))
}

#' Loads a data track from VitalDB api
#' @export
#' @param tid Track id
load_trk <- function(tid){
  start <- "https://api.vitaldb.net/"
  end <- ".csv.gz"
  # start + tid + end
  url <- paste(start, tid, end, sep="")
  df = load_VDB(url)
  return(df)
}

#' Load a track belonging to a case
#' @export
#' @param tname Name of track
#' @param caseid A case id, from which you want to load the track given as tname
load_case <- function(tname, caseid){
  tracks <- load_VDB("https://api.vitaldb.net/trks.csv.gz")
  tracks <- tracks[tracks$caseid == caseid,]
  tid <- (tracks[tracks$tname == tname,])$tid
  load_trk(tid)
}

