#' Loads file from URL (helper function)
#' @export
#' @param file_url URL for API endpoint. Found at https://vitaldb.net/dataset/?query=api&documentId=1_VTteoI5v-cdFkUTSnhYoGCqyq8dAC-_k50oM1QOjkU&sectionId=h.av1x1sa5y1he
#' @return A dataframe with values from the specified endpoint
#' @example
#' load_VDB("https://api.vitaldb.net/trks")
#' load_VDB("https://api.vitaldb.net/cases")
#' load_VDB("https://api.vitaldb.net/labs")
#' load_VDB(https://api.vitaldb.net/{tid})
load_VDB <- function(file_url) {
  con <- gzcon(url(file_url))
  txt <- readLines(con)
  return(read.csv(textConnection(txt)))
}

#' Checks whether we are importing something that is measured in hz. If it is, it changes the row names to reflect the frequency
#' @export
#' @param data The dataframe we want to check.
check_hz <- function(data){
  if (is.na(data[3,1])){
    freq = data[2,1]
    data <- subset (data, select = -Time)
    data<- na.omit(data)
    data <- cbind(data, "Time"=1:nrow(data)*freq)
    data <- data[, c(2,1)] # reorder columns'
    rownames(data) <- NULL
    return(data)
  }
  else {
    rownames(data) <- NULL
    return(data)
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
  data = load_VDB(url)
  return(check_hz(data))
}

#' Main function for loading in data
#' @export
#' @param tname Name of track
#' @param caseid A case id, from which you want to load the track given as tname
#' @example
#' load_case('Primus/AWP', 1)
#' load_case('SNUADC/ART', 1)
#' @return A dataframe with values from the specified track name and track id
load_case <- function(tname, caseid){
  tracks <- load_VDB("https://api.vitaldb.net/trks.csv.gz")
  tracks <- tracks[tracks$caseid == caseid,]
  tid <- (tracks[tracks$tname == tname,])$tid
  return(load_trk(tid))
}

#' Function for using convolution filter
#' @export
#' @param data Dataframe with AWP data (Time, Primus.AWP)
#' @param data_column Specify location of the AWP data column
#' @param n Specify number of -1's and 1's on each side of 0 in the convolution filter (8 by default). For n = 2, the convoultion filter is: -1, -1, 0, 1, 1
#' @value Returns a dataframe with convolution output signal
get_convolution_data <- function(data, data_column = 2, n = 8) {
  before <- rep(1, n)
  after <- rep(-1, n)

  my_filter <- c(before, 0, after)
  convolution <- data.frame(stats::filter(x = data[,data_column],
                                          filter= my_filter, sides=1, method="convolution"))
  convolution <- cbind(convolution,data[,1])
  convolution <- convolution[, c(2,1)]
  names(convolution)[1] <- "time"
  names(convolution)[2] <- "values"
  convolution <- waveformtools::filter_signal(convolution, 25, 1500, signal_col = 2)

  return (convolution)
}

#' Function for finding inspiration starts from convolution output signal
#' @export
#' @param convolution_data Dataframe with convolution_data
#' @value Returns a dataframe with inspiration start times
get_inspiration_start <- function(convolution_data) {
  convolution <- waveformtools::filter_signal(convolution_data, 25, 1500, signal_col = 2)
  insp_start <- waveformtools::find_peaks(convolution$values_filt,m=100, na.ignore=TRUE)
  insp_start <- convolution$time[insp_start]
  insp_start <- data.frame(insp_start)
  names(insp_start)[1] <- "time"
  return (insp_start)
}

#' Function for plotting Convolution Data
#' @export
#' @param convolution_data Dataframe with convolution_data
#' @value Returns a ggplot
plot_convolution_data <- function(convolution_data) {
  ggplot(convolution_data, aes(time, values_filt)) + geom_line() +theme_classic() + labs(title = 'Convolution Output Signal', x = 'Time (sec)', y = 'Output Signal') + theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold', color = '#63A0E1', family = 'Palatino'))
}

#' Function for subsetting AWP and ART data
#' @export
#' @param data The data you want to subset. In dataframe format
#' @param start_sec Which second of the data you want the subset to start at
#' @param seconds The size of the interval, starting from "start_sec"
#' @param filter Specify if a Butterworth Filter should be applied to the data (FALSE by default)
#' @param cut_freq Specify cutoff frequency for the filter (25 by default)
#' @value Returns a dataframe with a subset of AWP or ART data, with columns  Time, signal and filtered signal (If specified).
subset_data <- function(data, seconds, start_sec, filter=FALSE, cut_freq=25 ){
  hz <- 1/(data[2,1]-data[1,1])
  start <- start_sec*hz
  data <- data[start:(start+(seconds*hz)),]
  if (isTRUE(filter)){
    data <- waveformtools::filter_signal(data, cut_freq, sample_rate = hz, signal_col = 2) # 25 is domain knowledge
  }
  rownames(data) <- NULL
  return(data)
}

#' Function for plotting AWP data
#' @export
#' @param awp_data Dataframe with AWP data
#' @param add_insp_start Specify if Inspiration Start Times should be plotted
#' @param insp_start_data Inspiration start data
#' @value Returns a ggplot
plot_awp <- function(awp_data, add_insp_start = 'no', insp_start_data = NULL) {
  if (add_insp_start != 'no') {
    ggplot(sub_awp, aes(Time, Primus.AWP)) + geom_line() +theme_classic() + labs(title = 'Airway Pressure Wave \n with Inspiration Start', x = 'Time (sec)', y = 'AWP (hPa)') + theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold', color = '#63A0E1', family = 'Palatino')) + geom_vline(aes(xintercept = time), color = '#63A0E1', linetype = 'longdash', size = 0.8,
                                                                                                                                                                                                                                                                                                             data = insp_start_data)
  }

  else {
    ggplot(sub_awp, aes(Time, Primus.AWP)) + geom_line() +theme_classic() + labs(title = 'Airway Pressure Wave', x = 'Time (sec)', y = 'AWP (hPa)') + theme(plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = '#63A0E1', family = 'Palatino'))
  }
}


#' Function for plotting ART data
#' @export
#' @param art_data Dataframe with ART data
#' @param insp_start_data Inspiration start data
#' @param beats_data Beats data
#' @value Returns a ggplot
plot_art <- function(art_data, insp_start_data, beats_data) {
ggplot(sub_art, aes(Time, SNUADC.ART)) +
  geom_line() +
  geom_vline(aes(xintercept = time), color = '#63A0E1', linetype = 'longdash', size = 0.8,
             data = insp_start_data) +
  geom_point(aes(x = time,
                 y = dia,
                 colour = "Diastole"),
             data = beats_data, size = 5) +
  geom_point(aes(x = time_systole,
                 y = sys,
                 colour = "Systole"), size = 5,
             data = beats_data) +
  theme_classic() +
  labs(title = 'Arterial Pressure Waveform', x = 'Time (sec)', y = 'ART (mmHg)') +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = '#63A0E1', family = 'Palatino')) +
  labs(color='Type')
}

#' Function for plotting Pulse Pressure
#' @export
#' @param insp_start_data Inspiration start data
#' @param beats_data Beats data
#' @value Returns a ggplot
pp_plot <- function(insp_start_data, beats_data) {
  ggplot(beats_data, aes(time, PP)) +
    geom_line() +
    geom_point() +
    geom_vline(aes(xintercept = time), color = '#63A0E1', linetype = 'longdash', size = 0.8,
               data = insp_start_data) + theme_classic() + labs(title = 'Pulse Pressure', x = 'Time (sec)', y = 'PP (mmHg)') +
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = '#63A0E1', family = 'Palatino'))
}

#' Function for plotting Pulse Pressure where different respiratory cycles are indicated
#' @export
#' @param beats_indexed_data Beats with indexes data
#' @param insp_start_data Inspiration start data
pp_plot_color <- function(beats_indexed_data, insp_start_data) {
  beats_indexed_data <- beats_indexed_data[!is.na(beats_indexed_data$ann_n), ]
  min <- min(na.omit(beats_indexed$ann_n))
  max <- max(na.omit(beats_indexed$ann_n))
  myvec <- to_vec(for (x in c(min:max)) for (y in c('Cycle no.')) paste(y,x))


  ggplot(beats_indexed_data, aes(time, PP)) +
    geom_line() +
    # insp_n is a unique (consecutive) number for each respiratory cycle
    geom_point(aes(color = as.factor(ann_n)), show.legend = TRUE, size = 3) +
    geom_vline(aes(xintercept = time), color = '#63A0E1', linetype = 'longdash', size = 0.8,
               data = insp_start_data) +
    theme_classic() + labs(title = 'By time in seconds', x = 'Time (sec)', y = 'PP (mmHg)') +
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = '#63A0E1', family = 'Palatino'), plot.subtitle = element_text(hjust = 0.5, size = 15, face = 'bold', color = '#63A0E1', family = 'Palatino')) +
    scale_color_discrete(name = 'Respiratory Cycle', breaks = c(min:max), labels = myvec)



}

#' Function for plotting Pulse Pressure by position in respiratory cycle
#' @export
#' @param beats_indexed_data
pp_plot_insp <- function(beats_indexed_data) {
  ggplot(beats_indexed_data,
         aes(ann_rel_index, PP, group = as.factor(ann_n), color = as.factor(ann_n))) +
    geom_line(alpha = 0.4, show.legend = FALSE) +
    # insp_n is a unique (consecutive) number for each respiratory cycle
    geom_point(aes(color = as.factor(ann_n)), show.legend = FALSE, size = 3) +
    theme_classic() +
    labs(title = 'By position in the respiratory cycle', x = 'Index to Inspiration Start (Relative)', y = 'PP (mmHg)') +
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = '#63A0E1', family = 'Palatino'), plot.subtitle = element_text(hjust = 0.5, size = 15, face = 'bold', color = '#63A0E1', family = 'Palatino'))}

#' Function for plotting pp_plot_insp and pp_plot_color
#' @export
#' @param beats_indexed_data
#' @param insp_start_data
pp_plot_color_and_index <- function(beats_indexed_data, insp_start_data) {
  pp_plot_color(beats_indexed, insp_start) + pp_plot_insp(beats_indexed) +  plot_annotation(title = 'Pulse Pressure', ) & theme(plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = '#63A0E1', family = 'Palatino'))
}
