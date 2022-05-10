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
    df <- df[, c(2,1)] # reorder columns'
    rownames(df) <- NULL
    return(df)
  }
  else {
    rownames(df) <- NULL
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
  return(check_hz(df))
}

#' Main function for loading in data
#' @export
#' @param tname Name of track
#' @param caseid A case id, from which you want to load the track given as tname
load_case <- function(tname, caseid){
  tracks <- load_VDB("https://api.vitaldb.net/trks.csv.gz")
  tracks <- tracks[tracks$caseid == caseid,]
  tid <- (tracks[tracks$tname == tname,])$tid
  return(load_trk(tid))
}


#' Function for finding inspiration starts
#' @export
#' @param df Dataframe with AWP
get_inspiration_start <- function(df) {
  freq <- 1/(df[2,1]-df[1,1])
  df <- waveformtools::filter_signal(df, 25, sample_rate = freq, signal_col = 2) # 25 is domain knowledge
  n = 8
  before <- rep(1, n)
  after <- rep(-1, n)
  
  my_filter <- c(before, 0, after)
  convolution <- data.frame(stats::filter(x = df[,3],
              filter= my_filter, sides=1, method="convolution"))
  convolution <- cbind(convolution,df[,1])
  
  convolution <- convolution[, c(2,1)]
  names(convolution)[1] <- "time"
  names(convolution)[2] <- "values"
  convolution <- waveformtools::filter_signal(convolution, 25, 1500, signal_col = 2)
  insp_start <- waveformtools::find_peaks(convolution$values_filt,m=100, na.ignore=TRUE)
  insp_start <- convolution$time[insp_start]
  #insp_start <- insp_start[2:(length(insp_start)-1)] # removing the first and last peak since it is not representative
  insp_start <- data.frame(insp_start)
  names(insp_start)[1] <- "time"
  return(insp_start)
}

#' Function for subsetting AWP data
#' @export
#' @param df
#' @param start_sec
#' @param seconds
subset_data <- function(df, seconds, start_sec){
  hz <- 1 / df[1,1]
  start <- start_sec*hz
  df <- df[start:(start+(seconds*hz)),]
  rownames(df) <- NULL
  return(df)
}


df <- load_case('SNUADC/ART', 1)


