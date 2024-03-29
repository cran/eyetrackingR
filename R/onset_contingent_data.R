#' Make onset-contingent data.
#'
#' Divide trials into which AOI participants started on. Calculate switches away from this AOI, using
#' a rolling window to determine what length consitutes a switch. Augment original data with a column
#' indicating whether each row is a switch-away sample.
#'
#' @param data            The original (verified) data
#' @param onset_time        When to check for participants' "starting" AOI?
#' @param fixation_window_length       Which AOI is currently being fixated is determined by taking a rolling
#'   average of this length (ms). This is the width of window for rolling average.
#' @param target_aoi      Which AOI is the target that should be switched *to*
#' @param distractor_aoi  Which AOI is the distractor that should be switched *from* (default = !target_aoi)
#'
#' @examples 
#' \dontrun{
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
#'                                     rezero = FALSE)
#' inanimate_trials <- subset(response_window, grepl('(Spoon|Bottle)', Trial))
#' onsets <- make_onset_data(inanimate_trials, onset_time = 15500, target_aoi='Inanimate')
#' }
#' 
#' @export
#' @return Original dataframe augmented with column indicating switch away from target AOI
make_onset_data <- function(data, onset_time, fixation_window_length = NULL, target_aoi, distractor_aoi = NULL) {
  ## Helper Function:
  na_replace_rollmean <- function(col, fixation_window_length_rows) {
    if ( sum(!is.na(col)) == 0 ) return(as.numeric(NA)) # no data
    
    # RcppRoll::roll_mean(x = as.numeric(col), n = fixation_window_length_rows, align = 'left', fill = NA_real_)
    # browser()
    out <- zoo::rollapply(as.numeric(col), FUN = mean, na.rm = TRUE, width = fixation_window_length_rows, partial = TRUE, 
                          fill = NA, align = "left")
    
    return(out)
  }

  ## Prelims:
  if ( 'time_sequence_data' %in% class(data) ) {
    stop("This function should be used on your original data (after processed by `make_eyetrackingr_data`), ",
         "not on the output of `make_time_sequence_data`.")
  }
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  if (is.null(distractor_aoi)) {
    distractor_aoi <- paste0("Not_", target_aoi)
    data[[distractor_aoi]] <- !data[[target_aoi]]
  }
  time_col <- as.name(data_options$time_column)
  
  ## Translate TimeWindow units from time to number of rows (for rolling mean):
  # df_time_per_row <- group_by_(data, .dots = c(data_options$participant_column, data_options$trial_column) )
  # df_time_per_row <- summarize_(df_time_per_row,
  #                              .dots = list(TimePerRow = interp(~mean(diff(TIME_COL)), TIME_COL = time_col)
  #                                           ))
  
  df_time_per_row <- group_by(data, 
                              !!sym(data_options$participant_column),
                              !!sym(data_options$trial_column))
  df_time_per_row <- summarize(df_time_per_row,
                                TimePerRow = mean(diff(!!time_col)))

  time_per_row <- round(mean(df_time_per_row[["TimePerRow"]]))
  if (is.null(fixation_window_length)) 
    fixation_window_length <- time_per_row
  
  fixation_window_length_rows <- fixation_window_length / time_per_row
  stopifnot(fixation_window_length_rows >= 1)
  
  if (fixation_window_length_rows > 1) {
    message('Smoothing in make_onset_data() using fixation_window_length_rows is experimental. We
            recommend looking closely at the output to validate it.')
  }

  ## Determine First AOI, Assign Switch Value for each timepoint
  
  # Group by Participant*Trial:
  # df_grouped <- group_by_(data, .dots = list(data_options$participant_column, data_options$trial_column) )
  df_grouped <- group_by(data, 
                         !!sym(data_options$participant_column),
                         !!sym(data_options$trial_column) )
  
  # We assume data are in chronological order
  # df_grouped <- arrange_(df_grouped, .dots = list(data_options$time_column) )
  df_grouped <- arrange(df_grouped, !!data_options$time_column) #here

  # Create a rolling-average of 'inside-aoi' logical for target and distractor, to give a smoother estimate of fixations
  # df_smoothed <- mutate_(df_grouped,
  #                       .dots = list(.Target    = interp(~na_replace_rollmean(TARGET_AOI, fixation_window_length_rows), TARGET_AOI = as.name(target_aoi)),
  #                                    .Distractor= interp(~na_replace_rollmean(DISTRACTOR_AOI, fixation_window_length_rows), DISTRACTOR_AOI = as.name(distractor_aoi)),
  #                                    .Time      = interp(~TIME_COL, TIME_COL = time_col)
  #                                    ))
  df_smoothed <- mutate(df_grouped,
                         .Target = na_replace_rollmean(!!sym(target_aoi), fixation_window_length_rows),
                         .Distractor = na_replace_rollmean(!!sym(distractor_aoi), fixation_window_length_rows), 
                         .Time = !!sym(time_col))#here
                        
  # Calculate FirstAOI
  # For any trials where no data for onset timepoint is available, find the closest timepoint.
  df_first_aoi <- mutate(df_smoothed,
                         # these columns were useful for debugging:
                         #.MinTimeIndex = as.numeric(ifelse(length(which.min(abs(.Time - onset_time))) == 1, which.min(abs(.Time - onset_time)), NA)),
                         #.MinTime = as.numeric(ifelse(.MinTimeIndex, .Time[which.min(abs(.Time - onset_time))], NA)),
                         #.TargetAtMinTime = as.numeric(.Target[.MinTimeIndex]),
                         #.ClosestTime = as.numeric(ifelse(!is.na(.TargetAtMinTime) & !is.na(.MinTime), .MinTime, NA)),
                         
                         # .ClosestTime allows this function to work if people removed trackloss from their dataset, but otherwise
                         # it serves no purpose
                        .ClosestTime = as.numeric(ifelse(length(which.min(abs(.Time - onset_time)))==1 & !is.na(.Target[which.min(abs(.Time - onset_time))]),
                                                        yes = .Time[which.min(abs(.Time - onset_time))],
                                                        no = NA)),
                        FirstAOI = as.character(ifelse(length(which(.Time==.ClosestTime))==1, 
                                                       yes = ifelse(.Target[which(.Time==.ClosestTime)] >= .Distractor[which(.Time==.ClosestTime)], target_aoi, distractor_aoi),
                                                       no  = NA))
                        
                        # if we didn't care about datasets that have entirely removed trackloss, we could use this function:
                        #FirstAOI = as.character(ifelse(!is.na(.Target[which(.Time == onset_time)]),
                        #                               yes = ifelse(.Target[which(.Time == onset_time)] >= .Distractor[which(.Time == onset_time)], target_aoi, distractor_aoi),
                        #                               NA))
  )
  
  df_first_aoi <- ungroup(df_first_aoi)

  # (1) If closest timepoint was too far away from onset window, record FirstAOI as unknown
  # (2) Set FirstAOI to NA if 'FirstAOI' returned NA (but was coerced to a character string) by previous mutate()
  # (3) Create a column specifying whether they have switched away from FirstAOI
  out <- mutate(df_first_aoi,
         FirstAOI  = ifelse(!is.na(.ClosestTime) & abs(.ClosestTime-onset_time) <= fixation_window_length, FirstAOI, NA),
         WhichAOI  = ifelse(!is.na(.Target) & !is.na(.Distractor) & .Target > .Distractor, target_aoi, ifelse(!is.na(.Target) & !is.na(.Distractor) & .Target < .Distractor, distractor_aoi, NA)),
         SwitchAOI = ifelse(!is.na(WhichAOI), FirstAOI != WhichAOI, NA),
         FirstAOI  = factor(FirstAOI),
         WhichAOI  = factor(WhichAOI)
    )
  #out <- select(out, -.Target, -.Distractor, -.Time, -.ClosestTime)
  if (mean(is.na(out$FirstAOI)) > .5) warning("Very few trials have a legitimate first AOI! Possible incorrect onset time?")

  # Assign class information:
  out <- as.data.frame(out)
  class(out)  <- unique(c('onset_data', 'eyetrackingR_df', class(out)))
  attr(out, 'eyetrackingR')  <- list(data_options = data_options,
                                     onset_contingent = list(
                                       distractor_aoi = distractor_aoi,
                                       target_aoi = target_aoi,
                                       onset_time = onset_time,
                                       fixation_window_length = fixation_window_length))

  return(out)
}

#' Summarize data into time-to-switch from initial AOI.
#'
#' Take trials split by initial-AOI, and determine how quickly participants switch away from that AOI
#' @export
make_switch_data <- function(data, predictor_columns, summarize_by) {
  UseMethod("make_switch_data")
}
#' @describeIn make_switch_data
#'   
#' @param data               The output of \code{make_onset_data}
#' @param predictor_columns  Variables/covariates of interest when analyzing time-to-switch
#' @param summarize_by       Should the data be summarized along, e.g., participants, items, etc.? 
#'   If so, give column name(s) here. If left blank, will leave trials distinct. The former is 
#'   needed for more traditional analyses (t.tests, ANOVAs), while the latter is preferable for 
#'   mixed-effects models (lmer)
#' @export
#' 
#' @examples 
#' \dontrun{
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
#'                                     rezero = FALSE)
#' inanimate_trials <- subset(response_window, grepl('(Spoon|Bottle)', Trial))
#' onsets <- make_onset_data(inanimate_trials, onset_time = 15500, 
#'                           fixation_window_length = 100, target_aoi='Inanimate')
#'                           
#' df_switch <- make_switch_data(onsets, predictor_columns = "MCDI_Total", 
#'              summarize_by = "ParticipantName")
#' plot(df_switch, "MCDI_Total")
#' }
#'                           
#' @return A dataframe indicating initial AOI and time-to-switch from that AOI for each
#'   trial/subject/item/etc.
make_switch_data.onset_data <- function(data, predictor_columns=NULL, summarize_by = NULL) {

  data_options = attr(data, "eyetrackingR")$data_options

  if (is.null(summarize_by)) {
    summarize_by <- c(data_options$participant_column,
                     data_options$trial_column,
                     data_options$item_columns)
  }

  time_col <- as.name(data_options$time_column)

  df_cleaned <- filter(data, !is.na(FirstAOI))
  # df_grouped <- group_by_(data,
  #                        .dots = c(summarize_by,
  #                                  "FirstAOI",
  #                                  predictor_columns))
  # 
  # df_summarized <- summarize_(df_grouped,
  #                           .dots = list(FirstSwitch = interp(~ifelse(sum(which(SwitchAOI)) > 0,
  #                                                                     min(TIME_COL[SwitchAOI], na.rm=TRUE),
  #                                                                     NA), TIME_COL = as.name(time_col))
  #                           ))

  
  df_grouped <- group_by(data,
                          !!!syms(summarize_by),
                                    FirstAOI,
                                    !!!syms(predictor_columns))
  
  df_summarized <- summarize(df_grouped,
                              FirstSwitch = ifelse(sum(which(SwitchAOI)) > 0,
                                                   min((!!sym(time_col))[SwitchAOI], na.rm=TRUE),  NA))
                              
  df_summarized <- as.data.frame(df_summarized)
  class(df_summarized) <- unique(c('switch_data', 'eyetrackingR_df', class(df_summarized)))
  attr(df_summarized, "eyetrackingR") <- list(data_options = data_options)

  return(df_summarized)
}

#' Plot onset-contingent data
#' 
#' Divide trials into which AOI participants started on; plot proportion looking away from that AOI.
#' 
#' @param x The output of the \code{make_onset_data} function
#' @param predictor_columns Column(s) by which to facet the data. Maximum two columns. Will perform
#'   median split if numeric.
#' @param ... Ignored
#' @export
#' @return A ggplot object

plot.onset_data <- function(x, predictor_columns=NULL, ...) {

  ## Prelims:
  attrs <- attr(x, "eyetrackingR")
  data_options <- attrs$data_options
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  onset_attr <- attrs$onset_contingent
  if (is.null(onset_attr)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  if (length(predictor_columns)>2) stop("Maximum two predictor columns.")

  # set smoothing_window_size based on fixation_window_length in attr's
  smoothing_window_size <- onset_attr$fixation_window_length / 4

  # clean out unknown first AOIs, perform median splits if necessary:
  df_clean <- x[ !is.na(x[["FirstAOI"]]) , ]
  numeric_predictor_cols <- sapply(predictor_columns, function(col) is.numeric(df_clean[[col]]))
  for (i in seq_along(predictor_columns)) {
    if (! identical(TRUE, predictor_columns[i] %in% colnames(df_clean)) ) stop("The column ", predictor_columns[i], " is not in the dataset.")
    df_clean <- df_clean[ !is.na(df_clean[[predictor_columns[i]]]) , ]
    if (numeric_predictor_cols[i]) {
      message("Column '", predictor_columns[i], "' is numeric, performing median split for visualization.")
      the_median <- median(df_clean[[predictor_columns[i]]], na.rm=TRUE)
      df_clean[[predictor_columns[i]]] <- ifelse(df_clean[[predictor_columns[i]]] > the_median, 
                                                yes = paste0("High (>",the_median,")"),
                                                no = "Low")
    }
    df_clean[[predictor_columns[i]]] <- paste(predictor_columns[i], ":", df_clean[[predictor_columns[i]]])
  }

  # summarize by time bin:
  df_clean[[".Time"]] <- floor(df_clean[[data_options$time_column]] / smoothing_window_size) * smoothing_window_size
  
  # df_grouped <- group_by_(df_clean,
  #                        .dots = c(predictor_columns, data_options$participant_column, ".Time", "FirstAOI"))
  df_grouped <- group_by(df_clean,
                          !!!syms(predictor_columns), !!sym(data_options$participant_column), .Time, FirstAOI)
  df_smoothed <- summarize(df_grouped, SwitchAOI = mean(SwitchAOI, na.rm=TRUE))

  # collapse into lines for graphing:
  # df_summarized <- group_by_(df_smoothed, .dots = c(".Time", "FirstAOI", predictor_columns) )
  df_summarized <- group_by(df_smoothed, 
                            .Time, FirstAOI,!!!syms(predictor_columns) )
  df_summarized <- summarize(df_summarized, SwitchAOI = mean(SwitchAOI, na.rm=TRUE))

  # compute grayed area:
  # df_plot <- group_by_(df_summarized, .dots = c(".Time", predictor_columns) )
  df_plot <- group_by(df_summarized, .Time, !!!syms(predictor_columns) )
  df_plot <- mutate(df_plot,
                    Max= max(SwitchAOI),
                    Min= min(SwitchAOI),
                    Top= ifelse(length(which(FirstAOI==onset_attr$distractor_aoi)), SwitchAOI[FirstAOI==onset_attr$distractor_aoi], 0)
                    )
  df_plot$Max <- with(df_plot, ifelse(Max==Top, Max, NA))
  df_plot$Min <- with(df_plot, ifelse(Max==Top, Min, NA))

  ## Graph:
  if (is.null(predictor_columns)) {
    color_factor <- NULL
  } else {
    color_factor <- predictor_columns[1]
  }
  
  # make FirstAOI lines consistently solid==target
  df_plot$FirstAOI <- factor(df_plot$FirstAOI, levels=c(onset_attr$target_aoi,onset_attr$distractor_aoi))
  
  g <- ggplot(df_plot, aes_string(x = ".Time", y = "SwitchAOI",
                                  group = "FirstAOI",
                                  color = color_factor)) +
    geom_line(size=1.5, aes(linetype=FirstAOI)) +
    geom_ribbon(aes(ymin= Min, ymax= Max), fill= "gray", alpha= .2, colour= NA) +
    coord_cartesian(xlim=c(onset_attr$onset_time, max(df_plot$.Time) )) +
    ylab("Proportion Switch Looking") +
    xlab("Time") +
    guides(colour='none')

  ## Add Facets for Conditions:
  if (length(predictor_columns)>1) return(g+facet_grid(as.formula(paste(predictor_columns, collapse="~"))))
  if (length(predictor_columns)>0) return(g+facet_grid(as.formula(paste(predictor_columns, "~ ."))))
  return(g)

}

#' Plot mean switch-from-initial-AOI times.
#' 
#' Boxplot of mean switch time aggregated by subjects within each FirstAOI, potentially faceted by
#' predictor_columns.
#' 
#' @param x The output of the \code{make_switch_data} function
#' @param predictor_columns Column(s) by which to facet the data. Maximum two columns. Will perform
#'   median split if numeric.
#' @param ... Ignored
#' @export
#' @return A ggplot object

plot.switch_data <- function(x, predictor_columns=NULL, ...) {

  ## Prelims:
  data_options <- attr(x, "eyetrackingR")$data_options
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  if (length(predictor_columns) > 2) {
    stop("Maximum two predictor columns.")
  }

  ## Prepare for Graphing:
  data <- filter(x, !is.na(FirstAOI))
  # df_grouped <- group_by_(data, .dots = c(data_options$participant_column, predictor_columns, "FirstAOI"))
  df_grouped <- group_by(data, !!sym(data_options$participant_column), !!!syms(predictor_columns), FirstAOI)
  df_plot <- summarize(df_grouped, MeanFirstSwitch = mean(FirstSwitch))

  ## Figure out predictor columns: 
  # color:
  if (is.null(predictor_columns)) {
    color_factor <- NULL
  } else {
    color_factor <- predictor_columns[1]
  }
  # numeric/relabel:
  numeric_predictor_cols = sapply(predictor_columns, function(col) is.numeric(df_plot[[col]]))
  for (i in seq_along(predictor_columns)) {
    if (! identical(TRUE, predictor_columns[i] %in% colnames(data)) ) stop("The column ", predictor_columns[i], " is not in the dataset.")
    if (numeric_predictor_cols[i]) {
      message("Column '", predictor_columns[i], "' is numeric, performing median split for visualization.")
      the_median <- median(df_plot[[predictor_columns[i]]], na.rm=TRUE)
      df_plot[[predictor_columns[i]]] <- ifelse(df_plot[[predictor_columns[i]]] > the_median, 
                                                yes = paste0("High (>",the_median,")"),
                                                no = "Low")
    }
    df_plot[[predictor_columns[i]]] <- paste(predictor_columns[i], ":", df_plot[[predictor_columns[i]]])
  }
  
  ## Graph:
  g <- ggplot(df_plot, aes_string(x = "FirstAOI", y = "MeanFirstSwitch",
                             color = color_factor)) +
    geom_boxplot() +
    geom_point(position = position_jitter(.1)) +
    coord_flip() +
    ylab("Mean Switch Time") +
    xlab("Onset AOI") +
    guides(colour='none')

  ## Add Facets for Conditions:
  if (length(predictor_columns)>1) return(g+facet_grid(as.formula(paste(predictor_columns, collapse="~"))))
  if (length(predictor_columns)>0) return(g+facet_grid(as.formula(paste(predictor_columns, "~ ."))))

  return(g)

}
