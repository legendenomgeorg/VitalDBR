#' Loads file from URL (helper function)
#' @export
#' @param file_url URL for API endpoint
load_VDB <- function(file_url) {
  con <- gzcon(url(file_url))
  txt <- readLines(con)
  return(read.csv(textConnection(txt)))
}


#' Small helper function that converts index into hz
#' @export
#' @param x a string with an integer
#' @param freq the frequency of your dataset
fix_hz_index <- function(x,freq) {
  return(as.numeric(x)*freq)
}

#' Checks whether we are importing something that is measured in hz. If it is, it changes the row names to reflect the frequency
#' @export
#' @param df The dataframe we want to check. If
check_hz <- function(df){
  if (is.na(df[1,2]) & is.na(df[2,2] & !is.na(df[1,1]) & !is.na(df[2,1]) )){
    freq = df[2,1]
    df <- subset (df, select = -Time)
    df<- na.omit(df)
    df <- cbind(df,"Time"=1:nrow(df)*freq)
    df <- df[, c(2,1)] # reorder columns
    return(df)
  }
  else {
    return(df)
  }
}

#' Loads a data track from VitalDB api (helper function)
#' @export
#' @param tid Track id
load_trk <- function(tid){
  start <- "https://api.vitaldb.net/"
  end <- ".csv.gz"
  # start + tid + end
  url <- paste(start, tid, end, sep="")
  df = load_VDB(url)
  check_hz(df)
}

#' Main function for loading in data
#' @export
#' @param tname Name of track
#' @param caseid A case id, from which you want to load the track given as tname
load_case <- function(tname, caseid){
  tracks <- load_VDB("https://api.vitaldb.net/trks.csv.gz")
  tracks <- tracks[tracks$caseid == caseid,]
  tid <- (tracks[tracks$tname == tname,])$tid
  load_trk(tid)
}

