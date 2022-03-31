# Brugeren inputter én eller flere tnames i load_case, f.eks. c(BIS/EMG, Primus/INSP_DES, Solar8000/BT)
# Brugeren inputter også ét (og kun ét) case id. f.eks. "17"
# Funktionen samler nu alle "tids" forbundet med de forskellige tnames i case 1. ( i det her tilfælde 3 stk.)
# Funktionen kalder nu load_trks(tids), der kalder load_trk(tid) som kalder load vdb (der henter fra api) på alle tid i tids
# dermed bliver der returnet 3 tidsserier c(BIS/EMG, Primus/INSP_DES, Solar8000/BT), der alle tilhører case 17


#' Loads file from URL
#' @export
#' @param file_url URL for the VitablDB website
load_VDB <- function(file_url) {
  con <- gzcon(url(file_url))
  txt <- readLines(con)
  return(read.csv(textConnection(txt)))
}

#' Loads a data track from VitalDB api
#' @export
#' @param tid PENDING EXPLANATION OF VARIABLE
load_trk <- function(tid){
  start <- "https://api.vitaldb.net/"
  end <- ".csv.gz"
  # start + tid + end
  url <- paste(start, tid, end, sep="")
  df = load_VDB(url)
  return(df)
}

#' PENDING DESCRIPTION OF FUNCTION
#' @export
#' @param tids PENDING EXPLANATION OF VARIABLE
load_trks <- function(tids){
  data <- data.frame()
  index <- 1
  for (i in 1:nrow(tids)){
    load_trk(tids[i,])
    index <- index+1
  }
}

#' PENDING DESCRIPTION OF FUNCTION
#' @export
#' @param tnames PENDING EXPLANATION OF VARIABLE
#' @param caseid PENDING EXPLANATION OF VARIABLE
load_case <- function(tnames, caseid){
  tracks <- load_VDB("https://api.vitaldb.net/trks.csv.gz")
  tracks <- tracks[tracks$caseid == caseid,]
  tids <- data.frame(col = rep(NA, length(tnames)))
  index <- 1
  for (tname in tnames){
    tids$col[index] <- (tracks[tracks$tname == tname,])$tid
    index <- index+1
  }
  load_trks(tids)
}
