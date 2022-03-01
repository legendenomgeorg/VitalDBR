#' Create DataFrame of arterial pressure waveform annotations.
#'
#' Position and value of systolic and diastolic pressures.
#' Each row represents one beat with first a diastolic and then a systolic value.
#'
#' .noise_pos_after_sys is the sum of positive changes in ABP excluding the
#' systole and the dicrotic notch, divided by the beat length in seconds.
#' 2 mmhg/s seems like a good cutoff.
#'
#' @param data Data.frame or vector of arterial blood pressure.
#' @param abp_col Index or name of column with abp data
#' @param min_PP Minimum ratio of PP/mean pressure.
#' If a beat has a lower PP, it is considered a fluctuation around the threshold.
#' The dicrotic notch is a common cause of such fluctuations.
#'
#' @param min_beat_width_s Minimum beat width (in seconds)
#' The default is 0.3 seconds
#'
#' @param win_size_avg Window size for moving avg calculation.
#' This is used to calculate the cutoff value for when a peak represents a new beat and not just noise.
#' Lower values increase flexibility. Use visualize_abp_peak_detection() to dial in.
#'
#' @param time_col Vector with the same length as `abp`, used to indicate the timing of each sample.
#' If this variable is provided, sample indexes will be substituted for this vector.
#'
#' @param show.plot Show diagnostic plot
#'
#' @param include_waveform Include waveform for each beat in output
#'
#' @param sample_rate Used if data is a vector of samples.
#'
#' @export
find_abp_beats <- function(data,
                           abp_col = 2,
                           time_col = 1,
                           min_PP = 0.20,
                           min_beat_width_s = 0.3,
                           win_size_avg = 2000,
                           show.plot = FALSE,
                           include_waveform = FALSE,
                           sample_rate = NULL) {
  
  assertthat::not_empty(data)
  
  
  if (is.vector(data)) {
    
    abp <- data
  } else {
    abp <- data[[abp_col]]
    time_vector <- data[[time_col]]
  }
  
  if(is.na(abp[1])) {
    return(NA)
  }
  
  if (is.null(sample_rate)) {
    sample_rate <- attr(data, 'signal.samplerate')
    min_beat_width <- as.integer(min_beat_width_s * 125)
    if (is.null(sample_rate)) message('Sample rate not set. Returning beat length in samples ')
  }
  
  #create cuttoff pressure from average pressure (movingaves is fast)
  moving_mean_p <- accelerometry::movingaves(
    #repeat avg of last measurements (* movingavg_win) to get equal lengths
    c(abp,rep(mean(tail(abp, win_size_avg)), win_size_avg-1)),
    window = win_size_avg)
  
  # group cycles
  # finds every crossing of cutoff pressure, and keeps only changes from high to low
  # abp < cutoff gives a series of T/F:  TTTTFFFFTTTTFFFFTT
  # diff find only changes (+ = +1):     000-000+000-000+00
  # == 1 keeps only +1 change:           000000010000000100
  # (cumsum creates groups:               000000011111111222)
  cross_index <- which(c(diff(abp < (moving_mean_p * 1.1)), 0) == 1)
  
  cross_groups <- splitAt(abp, cross_index, trim_ends = FALSE)
  
  # Split abp by diastoles
  dia_index <- purrr::map_int(cross_groups, which.min) + c(0, cross_index - 1)
  
  # Check that no beats are shorter than mean beat width.
  while (any(diff(dia_index) < min_beat_width)) {
    short_i <- which(diff(dia_index) < min_beat_width)[1]
    
    # If a shorter interval is found. Keep the index with the lowest abp (the true diastole)
    # If the shorter index is the last, just remove it.
    if (short_i == length(dia_index) || abp[dia_index[short_i+1]] < abp[dia_index[short_i]]) {
      dia_index <- dia_index[-short_i]
    }
    else dia_index <- dia_index[-(short_i + 1)]
    
  }
  
  beat_groups <- splitAt(abp, dia_index, trim_ends = FALSE) # Do not trim, since the start is needed for timing.
  
  res <- dplyr::tibble(beat_wave = beat_groups)
  res <- dplyr::mutate(res,
                       dia = purrr::map_dbl(beat_wave, ~.x[1]),
                       sys = purrr::map_dbl(beat_wave, ~max(.x)),
                       which.sys = purrr::map_int(beat_wave, ~which.max(.x)),
                       segment_len = purrr::map_int(beat_wave, ~length(.x)),
                       dia_pos = dplyr::lag(cumsum(segment_len), default = 1),
                       sys_pos = which.sys + dia_pos - 1, # Both are 1 indexed, so to make the global position 1 indexed subtract 1
                       PP = sys - dia)
  
  if(!is.null(sample_rate)) res$segment_len <- res$segment_len * 1/sample_rate
  
  res <- dplyr::filter(res,
                       #PP/moving_mean_p[segment_start] > min_PP,
                       dia_pos > 20, # First min cannot be among the first 20 samples
                       dia_pos < length(abp) - 100) # and last min cannot be among the last 60
  # (to avoid detecting a dicrotic notch as a diastole)
  
  
  # Detect Noise
  pos_after_sys <- function(x) {
    if (is.null(sample_rate)) return(NA)
    
    diff_x <- diff(x)
    
    slopes <- rle(diff_x > 0)
    
    # Three positive slopes are accepted
    # (systole (+ deflection) and dichrotic notch)
    pos_slopes <- which(slopes$values == TRUE)
    
    slopes$values[head(pos_slopes, 3)] <- FALSE
    
    sum(diff_x[inverse.rle(slopes)]) / (length(diff_x) * (1/sample_rate))
  }
  
  res <- dplyr::mutate(res,
                       # Wiggliness is calculated similarly to gam smooth funtion wiggliness
                       # Sum of squared 2nd derivative.
                       .noise_wiggliness = purrr::map_dbl(beat_wave, ~sum(diff(diff(.x))^2)),
                       .noise_pos_after_sys = purrr::map_dbl(beat_wave, pos_after_sys))
  
  if(!include_waveform) {
    res$beat_wave <- NULL
  }
  
  res <- dplyr::select(res,
                       dia_pos,
                       dia,
                       sys_pos,
                       sys,
                       PP,
                       beat_len = segment_len,
                       contains('.noise'),
                       contains('beat_wave'))
  
  if (show.plot) {
    plot(abp, type = 'l')
    points(res$dia_pos, res$dia)
    points(res$sys_pos, res$sys)
    lines(moving_mean_p * 1.1, col = 'red')
  }
  if (is.vector(data)) {
    res
  } else {
    
    res <- dplyr::mutate(res, time = time_vector[dia_pos],
                         time_systole = time_vector[sys_pos])
    dplyr::select(res,
                  time,
                  dia,
                  time_systole,
                  sys,
                  PP,
                  beat_len,
                  dplyr::everything())
  }
  
}

#' Split vector at index 
#' Adapted from SO: <https://stackoverflow.com/a/19274414/1498656>
#'
#' @param x input vector
#' @param pos index vector
#' @param trim_ends exclude first and last group
#'
#' @export
splitAt <- function(x, pos, trim_ends = FALSE) {
  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is.vector(pos))
  assertthat::assert_that(all(pos > 0))
  
  out <- list()
  pos2 <- if (trim_ends) pos else c(1, pos, length(x)+1)
  
  for (i in seq_along(pos2[-1])) {
    out[[i]] <- x[pos2[i]:(pos2[i+1]-1)]
  }
  return(out)
}

#' Add indexing of signal time in relation to some recurring event.
#'
#' Index is the time since the most recent occurrence of some event e.g. QRS complex
#'
#' @param data A data frame with at least a time column
#' @param time_event A vector of time stamps for some event
#' @param time_col Index or name of time column
#' @param prefix Prefix to new columns
#' @param incl_prev_lengths Include length of previous 3 cycles as variables
#' @export
add_time_since_event <- function(data, time_events, time_col = 1, prefix = 'ann',
                                 incl_prev_lengths = FALSE) {
  if (nrow(data) == 0) stop("No Data")
  if (length(time_events) == 0) stop("No events")
  
  if (!is.data.frame(data)) {
    time_vec <- data
    data <- dplyr::tibble(time = data)
  } else {
    time_vec <- data[[time_col]]
  }
  
  
  if (("POSIXct" %in% class(time_vec) & "POSIXct" %in% class(time_events)) ||
      ("hms" %in% class(time_vec) & "hms" %in% class(time_events))) {
    time_vec <- as.numeric(time_vec, units = 'secs')
    time_events <- as.numeric(time_events, units = 'secs')
  }
  
  
  time_events <- sort(time_events)
  
  # add cycle length of each event (e.g. resp cycle length)
  # The last cycle does not end, and therefore does not have a length
  cycle_length <- diff(time_events)
  
  if (incl_prev_lengths) {
    cycle_length_previous_1 <- dplyr::lag(cycle_length)
    cycle_length_previous_2 <- dplyr::lag(cycle_length, 2)
    cycle_length_previous_3 <- dplyr::lag(cycle_length, 3)
  }
  
  # Create for loop, that goes through time_vec, and finds latest event.
  # Time since last event
  ann_index  <- rep(NA, length(time_vec))
  cycle_len  <- rep(NA, length(time_vec))
  ann_n      <- rep(NA, length(time_vec))
  
  if (incl_prev_lengths) {
    cycle_len_prev_1 <- rep(NA, length(time_vec))
    cycle_len_prev_2 <- rep(NA, length(time_vec))
    cycle_len_prev_3 <- rep(NA, length(time_vec))
  }
  
  i_ann <- 1L
  
  for (i in 1:length(time_vec)) {
    # If event is after current time, check next time
    if (time_events[i_ann] > time_vec[i]) next
    # If the next event is also before the current time_vec, increment i_ann by one.
    while (i_ann < length(time_events) && time_events[i_ann + 1] < time_vec[i]) {
      i_ann <- i_ann + 1L
    }
    
    ann_index[i]      <- time_vec[i] - time_events[i_ann]
    cycle_len[i]      <- cycle_length[i_ann]
    ann_n[i]          <- i_ann
    
    if (incl_prev_lengths) {
      cycle_len_prev_1[i] <- cycle_length_previous_1[i_ann]
      cycle_len_prev_2[i] <- cycle_length_previous_2[i_ann]
      cycle_len_prev_3[i] <- cycle_length_previous_3[i_ann]
    }
    
  }
  
  res <- dplyr::tibble(index = ann_index,
                       n = ann_n,
                       cycle_len = cycle_len,
                       rel_index = index / cycle_len)
  
  if (incl_prev_lengths) {
    res <- dplyr::bind_cols(res,
                            data.frame(
                              cycle_len_prev_1,
                              cycle_len_prev_2,
                              cycle_len_prev_3
                            ))
  }
  
  names(res) <- paste(prefix, names(res), sep = '_')
  
  dplyr::bind_cols(data, res)
}
