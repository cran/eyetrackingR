# Loading/Cleaning/Describing Data ------------------------------------------------------------------------

#' Convert raw data for use in eyetrackingR
#' 
#' This should be the first function you use when using eyetrackingR for a project (potentially with
#' the exception of `add_aoi`, if you need to add AOIs). This function takes your raw dataframe, as 
#' well as information about your dataframe. It confirms that all the columns are the right format, 
#' based on this information. Further if \code{treat_non_aoi_looks_as_missing} is set to TRUE, it 
#' converts non-AOI looks to missing data (see the "Preparing your data" vignette for more 
#' information).
#' 
#' eyetrackingR is designed to deal with data in a (relatively) raw form,
#' where each row specifies a sample. Each row should represent an equally spaced unit of time
#' (e.g., if your eye-tracker's sample rate is 100hz, then each row corresponds to the
#' eye-position every 10ms). This is in contrast to the more parsed data that the software bundled
#' with eye-trackers can sometimes output (e.g., already parsed into saccades or fixations). For
#' eyetrackingR, the simplest data is the best. This also maximizes compatibility: eyetrackingR
#' will work with any eye-tracker's data (e.g., Eyelink, Tobii, etc.), since it requires the most
#' basic format.
#' 
#' @param data               Your original data. See details section below.  
#' @param participant_column Column name for participant identifier
#' @param trackloss_column   Column name indicating trackloss
#' @param time_column        Column name indicating time
#' @param trial_column       Column name indicating trial identifier
#' @param item_columns       Column names indicating items (optional)
#' @param aoi_columns        Names of AOIs
#' @param treat_non_aoi_looks_as_missing This is a logical indicating how you would like to perform
#'   "proportion-looking" calculations, which are central to eyetrackingR's eyetracking analyses. If set to
#'   TRUE, any samples that are not in any of the AOIs (defined with the \code{aoi_columns} 
#'   argument) are treated as missing data; when it comes time for eyetrackingR to calculate 
#'   proportion looking to an AOI, this will be calculated as "time looking to that AOI divided by 
#'   time looking to all other AOIs." In contrast, if this parameter is set to FALSE, proportion 
#'   looking to an AOI will be calculated as "time looking to that AOI divided by total time 
#'   looking."
#'   
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#'   
#' @export
#' @return Dataframe ready for use in eyetrackingR.

make_eyetrackingr_data <- function(data, 
                                   participant_column,
                                   trackloss_column,
                                   time_column,
                                   trial_column,
                                   aoi_columns,
                                   treat_non_aoi_looks_as_missing,
                                   item_columns = NULL) {
  ## Helpers:
  as.numeric2 <- function(x) as.numeric(as.character(x))
  is.logical2 <- function(x) {
    if (is.logical(x)) {
      return(TRUE)
    } else if (is.numeric(x)) {
      return(FALSE)
    } else {
      stop("One of your columns (", col, ") could not be converted to the correct format (TRUE/FALSE), ",
           "please do so manually.", call. = FALSE)
    }
  }
  check_then_convert <- function(x, checkfunc, convertfunc, colname) {
    if (!checkfunc(x)) {
      message("Converting ", colname, " to proper type.")
      x <- convertfunc(x)
    } 
    if (colname=="Trackloss" & any(is.na(x))) {
      warning("Found NAs in trackloss column, these will be treated as TRACKLOSS=FALSE.", immediate. = TRUE, call. = FALSE)
      x <- if_else(is.na(x), FALSE, x)
    }
    return(x)
  }
  
  ## Data Options:
  data_options = list(participant_column = participant_column,
                        trackloss_column = trackloss_column,
                        time_column = time_column,
                        trial_column = trial_column,
                        item_columns = item_columns,
                        aoi_columns = aoi_columns,
                        treat_non_aoi_looks_as_missing = treat_non_aoi_looks_as_missing)
  
  ## Check for Reserved Column Name:
  if (data_options$time_column == "Time") {
    stop("We apologize for the inconvenience, but your `time_column` cannot be called 'Time' ",
         "(this is a reserved name that eyetrackingR uses). Please rename.")
  } 
  if ("Time" %in% colnames(data)) {
    warning("Your dataset has a column called 'Time', but this column name is reserved for eyetrackingR. Will rename to 'TimeOriginal'...")
    data$TimeOriginal <- data$Time
    data$Time <- NULL
  }
  
  ## Verify Columns:
  out <- data
  col_type_converter <- list(participant_column = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                             time_column = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                             trial_column = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                             trackloss_column = function(x) check_then_convert(x, is.logical2, as.logical, "Trackloss"),
                             item_columns = function(x) check_then_convert(x, is.factor, as.factor, "Item"),
                             aoi_columns = function(x) check_then_convert(x, is.logical2, as.logical, "AOI"))
  
  for (col in names(col_type_converter)) {
    for (i in seq_along(data_options[[col]])) {
      if (is.null(out[[data_options[[col]][i]]]))
        stop("Data are missing: ", col)
      out[[data_options[[col]][i]]] <- col_type_converter[[col]](out[[data_options[[col]][i]]])
    }
  }
  
  ## Deal with Non-AOI looks:
  if (treat_non_aoi_looks_as_missing) {
    any_aoi <- rowSums(as.matrix(out[,data_options$aoi_columns,drop=FALSE]), na.rm = TRUE) > 0
    out[[data_options$trackloss_column]][!any_aoi] <- TRUE
  }
  
  ## Set All AOI rows with trackloss to NA:
  # this ensures that any calculations of proportion-looking will not include trackloss in the denominator
  for (aoi in data_options$aoi_columns) {
    out[[aoi]][ out[[data_options$trackloss_column]] ] <- NA
  }
  
  # Check for duplicate values of Trial column within Participants
  duplicates <- out %>%
    group_by_at(.vars = c(data_options$participant_column, data_options$trial_column, data_options$time_column) ) %>%
    count() %>%
    ungroup() %>%
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    print(duplicates)
    stop("It appears that `trial_column` is not unique within participants. See above for a summary ",
         "of which participant*trials have duplicate timestamps. eyetrackingR requires that each participant ",
         "only have a single trial with the same `trial_column` value. If you repeated items in your experiment, ",
         "use `item_column` to specify the name of the item, and set `trial_column` to a unique value ",
         "(e.g., the trial index).")
  }
  
  ## Assign attribute:
  out <- arrange_at(.tbl = out, .vars = c(participant_column, trial_column, time_column))
  out <- as_tibble(out)
  class(out) <- c("eyetrackingR_data", "eyetrackingR_df", class(out))
  attr(out, "eyetrackingR") <- list(data_options = data_options)
  return(out)
  
}

#' Add an area-of-interest to your dataset, based on x-y coordinates and the AOI rectangle.
#' 
#' Eyetracking-R requires that there is a column for each area-of-interest, specifying whether the gaze is 
#' within that area for each sample. This function creates an AOI column if needed.
#' 
#' Many eyetracking software packages export your data with a column corresponding to each AOI; however, if
#' your software does not do this, or if you had to define or revise your AOIs after running the experiment,
#' then this function will add the necessary AOI columns for you. The function takes two dataframes: (1) your
#' original data, (2) a dataframe specifying the bounding box for the AOI. The latter can specify a different
#' bounding box for each trial, each subject, each image, or even each video-frame-- anything you like. The
#' two dataframes are simply joined by matching any columns they have in common (case sensitive!), so if
#' there's a unique AOI for each "Trial" in the \code{aoi_dataframe}, and there's a "Trial" column in the
#' \code{data} dataframe, then the unique AOI coordinates for each trial will be used.
#' 
#' @param data Your data
#' @param aoi_dataframe A dataframe specifying the bounding-box for the AOI
#' @param x_col,y_col What are the column names for the x and y coordinates in your dataset?
#' @param aoi_name What is the name of this AOI?
#' @param x_min_col,x_max_col What are the column names for the left and right edge of the AOI-bounding box?
#'   Default "L","R"
#' @param y_min_col,y_max_col What are the column names for the top and bottom edge of the AOI-bounding box?
#'   Default "T","B"
#' @export
#' @return Dataset with a new column indicating whether gaze is in the AOI
add_aoi <- function(data, aoi_dataframe,
                     x_col, y_col,
                     aoi_name,
                     x_min_col = "L", x_max_col = "R",
                     y_min_col = "T", y_max_col = "B"
) {
  
  stopifnot(is.character(aoi_name))
  
  if (nrow(aoi_dataframe) > nrow(distinct(aoi_dataframe)))
    warning("Your `aoi_dataframe` has duplicate rows.")
  
  ## Helper
  .inside_rect = function(pt, ltrb) {
    if (is.null(dim(pt))) {
      pt = as.matrix(pt)
      pt = t(pt)
    }
    if (is.null(dim(ltrb))) {
      ltrb = as.matrix(ltrb)
      ltrb = t(ltrb)
    }
    if (dim(pt)[2]   != 2) stop('First argument should be a vector of length 2 or an n-by-2 matrix')
    if (dim(ltrb)[2] != 4) stop('Second argument should be a vector of length 4 or an n-by-4 matrix')
    
    return(
      pt[,1] >= ltrb[,1] &
      pt[,1] <= ltrb[,3] &
      pt[,2] >= ltrb[,2] &
      pt[,2] <= ltrb[,4] 
    )
  }
  
  stopifnot(all(c(x_col,y_col) %in% colnames(data)))
  stopifnot(all(c(x_min_col,x_max_col, y_min_col, y_max_col) %in% colnames(aoi_dataframe)))
  
  if (x_col %in% c(x_min_col, x_max_col))
    stop("The name of `x_col` cannot be the same as the name of `x_min_col` or `x_max_col`.")
  if (y_col %in% c(y_min_col, y_max_col))
    stop("The name of `y_col` cannot be the same as the name of `y_min_col` or `y_max_col`.")
  
  
  ## Join AOI info to dataset
  if (any(colnames(data) %in% colnames(aoi_dataframe))) {
    df_joined <- left_join(data, aoi_dataframe)
  } else {
    if (nrow(aoi_dataframe) > 1) {
      stop("Your `aoi_dataframe` has more than one row, but it doesn't have any columns that match the columns in your data, so it's not clear how to map these rows onto this data.")
    } else {

      df_joined <- data
      for (aoi_col in colnames(aoi_dataframe)) {
        df_joined[[aoi_col]] <- aoi_dataframe[[aoi_col]]
      }
      
    }
  }
  
  ## Make AOI column
  message("Making ", aoi_name, " AOI...")
  
  data[[aoi_name]] <- .inside_rect(pt    = cbind(df_joined[[x_col]], df_joined[[y_col]]),
                                   ltrb  = cbind(df_joined[[x_min_col]], df_joined[[y_min_col]], df_joined[[x_max_col]], df_joined[[y_max_col]])
  )
  
  return(data)
}


#' Extract a subset of the dataset within a time-window in each trial.
#' 
#' One of the more annoying aspects of preparing raw eyetracking data is filtering data down into the relevant
#' window within the trial, since for many experiments the precise start and end time of this window can vary 
#' from trial to trial. This function allows for several approaches to subsetting data into the relevant time-
#' window-- see 'Details' below.
#' 
#' \enumerate{
#'   \item The trial start/end times can be indicated by a message that is sent (e.g., TRIAL_START) in a 
#' particular row for each trial. In this case, the timestamp of that row is used.
#'   \item The trial start/end times can be indicated in by a column that specifies trial start/end times for each
#' trial.
#'   \item The trial start/end times can be indicated by the actual start and stop time, the same across all
#' trials (the simplest case).
#' }
#' 
#' If you only have a start time but the end time doesn't need adjusting, then leave the end time argument blank;
#' and vice versa.
#' 
#' This function can either rezero your data (the trial start time you select is the new zero-time-point), or 
#' not. The former is useful when performing initial data-cleaning (e.g., different trial-starts on each 
#' trial, as indicated by a message), and the latter is useful if you want to "zoom in" on a particular 
#' portion of your data while keeping obvious the fact that there were other parts of the trial (e.g., an 
#' image always appears 5000ms-7000ms in the trial, so for one analysis you are only interested in this 
#' portion).
#' 
#' @param data               Your original dataset
#' @param rezero             Should the beginning of the window be considered the zero point of the timestamp?
#'   Default TRUE
#' @param remove             Should everything before the beginning and after the end of the window be removed? 
#'   Default TRUE. If set to FALSE and \code{rezero} is set to FALSE, an error is thrown (since in this case,
#'   the function would not do anything).
#' @param window_start_msg   For method (1). A message that is present only in the row whose time corresponds 
#'   to the trial start time. Common for eyetrackers that send a message at trial/stimuli start.
#' @param window_end_msg     For method (1). A message that is present only in the row whose time corresponds 
#'   to the trial end time. Common for eyetrackers that send a message at trial-end/keypress/lookaway/etc.
#' @param msg_col            For method (1). If you are indicating the trial start/end with a message column,
#'   this is the name of that column.
#' @param window_start_col   For method (2). A column that gives the start time for each trial.
#' @param window_end_col     For method (2). A column that gives the end time for each trial.
#' @param window_start_time  For method (3). Number indicating a start time that applies to all trials.
#' @param window_end_time    For method (3). Number indicating an end time that applies to all trials.
#' @param quiet              Suppress messages? Default FALSE
#' 
#' @examples
#' data("word_recognition")
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' 
#' # zoom in to 15500-21000ms
#' response_window <- subset_by_window(data,
#'                                     window_start_time = 15500,
#'                                     window_end_time = 21000, rezero = FALSE, remove = TRUE)
#' 
#' # zoom in to 15500-21000ms and re-zero so timestamps start at 0
#' response_window <- subset_by_window(data,
#'                                     window_start_time = 15500, 
#'                                     window_end_time = 21000, 
#'                                     rezero = TRUE, 
#'                                     remove = TRUE)
#' 
#' # keep all data, but re-zero it
#' response_window <- subset_by_window(data,
#'                                     window_start_time = 0, 
#'                                     rezero = TRUE, 
#'                                     remove = FALSE)
#' 
#' @export
#' @return Subsetted data

subset_by_window <- function(data, 
                             rezero = TRUE,
                             remove = TRUE,
                             window_start_msg = NULL, window_end_msg = NULL, msg_col = NULL,
                             window_start_col = NULL, window_end_col = NULL,
                             window_start_time= NULL, window_end_time= NULL,
                             quiet = FALSE
                             ) {
  
  ## Helper:
  .safe_msg_checker = function(msg_vec, msg, time_vec, ppt_vec, trial_vec) {
    bool = (msg_vec==msg)
    if (length(which(bool)) != 1) {
      warning("The message ", msg, 
              " does not appear *exactly* one time for participant '", ppt_vec[1], 
              "' on trial '", trial_vec[1], "'. Trial will be removed from dataset.")
      if (is.integer(time_vec)) return(NA_integer_)
      else return(NA_real_)
    }
    return(time_vec[which(bool)])
  }
  
  # Prelims:
  orig_classes <- class(data)
  data_options <- attr(data, "eyetrackingR")$data_options
  colname_symbols <- purrr::map(data_options[grep(names(data_options), pattern="column$")], as.name) 
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  if (!(rezero | remove)) stop("If both 'rezero' and 'remove' are FALSE, then this function doesn't do anything!")
  
  # Which method?
  start_method_num <- !purrr::map_lgl( list(window_start_msg, window_start_col, window_start_time), is.null )
  stop_method_num <- !purrr::map_lgl( list(window_end_msg, window_end_col, window_end_time), is.null )
  if ( sum(start_method_num) > 1 | sum(stop_method_num) > 1 ) {
    stop("Please use exactly one of the methods for start/stop time (msg, column, or time).")
  }
  
  # Start Time:
  if (!any(start_method_num)) {
    if (rezero) stop("Rezero must be set to FALSE if no start time specified.")
    start_method_num <- c(FALSE, FALSE, TRUE)
    window_start_time <- -Inf
  }
  if (which(start_method_num) == 1) {
    # Message:
    if (!is.character(msg_col)) stop("Please enter a column name for the message column (in quotes).", call. = FALSE)
    data[[msg_col]] <- as.character(data[[msg_col]])
    data <- group_by_at(.tbl = data, .vars = c(data_options$participant_column, data_options$trial_column))
    data <- mutate(.data = data, 
                   .WindowStart = .safe_msg_checker(msg_vec = !!as.name(msg_col),
                                                    msg = window_start_msg,
                                                    time_vec = !!colname_symbols$time_column,
                                                    ppt_vec = !!colname_symbols$participant_column,
                                                    trial_vec = !!colname_symbols$trial_column )
    )
    data <- ungroup(data)
  } else if (which(start_method_num) == 2) {
    # Column:
    data$.WindowStart <- data[[window_start_col]]
  } else if (which(start_method_num) == 3) {
    # Single Number:
    data$.WindowStart <- window_start_time
  } 
  
  # Stop Time:
  if (!any(stop_method_num)) {
    stop_method_num <- c(FALSE, FALSE, TRUE)
    window_end_time <- Inf
  }
  if (which(stop_method_num) == 1) {
    # Message:
    if (!is.character(msg_col)) stop("Please enter a column name for the message column (in quotes).", call. = FALSE)
    data[[msg_col]] <- as.character(data[[msg_col]])
    data <- group_by_at(.tbl = data, .vars = c(data_options$participant_column, data_options$trial_column))
    data <- mutate(.data = data, 
                   .WindowEnd = .safe_msg_checker(msg_vec = !!as.name(msg_col),
                                                  msg = window_end_msg,
                                                  time_vec = !!colname_symbols$time_column,
                                                  ppt_vec = !!colname_symbols$participant_column,
                                                  trial_vec = !!colname_symbols$trial_column )
    )
    data <- ungroup(data)
  } else if (which(stop_method_num) == 2) {
    # Column:
    data$.WindowEnd <- data[[window_end_col]]
  } else if (which(stop_method_num) == 3) {
    # Single Number:
    data$.WindowEnd <- window_end_time
  } 
  
  #
  if (!quiet) {
    new_len <- round(mean(data$.WindowEnd - data$.WindowStart, na.rm=TRUE),2)
    if (!is.infinite(new_len)) message("Avg. window length in new data will be ", new_len) 
  }
  
  # Subset
  df_subsetted <- filter(.data = data,
                         !is.na(.WindowEnd),
                         !is.na(.WindowStart))
  
  if (remove) {
    after_start <- df_subsetted[[data_options$time_column]] >= df_subsetted$.WindowStart
    before_end <- df_subsetted[[data_options$time_column]] < df_subsetted$.WindowEnd
    df_subsetted <- df_subsetted[which(after_start&before_end), , drop=FALSE]
  } 
  
  # Rezero
  if (rezero) {
    df_grouped <- group_by_at(df_subsetted, .vars = c(data_options$participant_column, data_options$trial_column))
    df_rezeroed <- mutate(.data = df_grouped, 
                          !!colname_symbols$time_column := (!!colname_symbols$time_column) - .WindowStart)
    out <- ungroup(df_rezeroed)
  } else {
    out <- df_subsetted
  }

  out <- select(out, -.WindowStart, -.WindowEnd)
  
  attr(out, "eyetrackingR") <- list(data_options = data_options)
  class(out) <- orig_classes
  
  out
}

#' Analyze trackloss.
#'
#' Get information on trackloss in your data.
#'
#' @param data The output of \code{make_eyetrackingr_data}
#' 
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' 
#' tl_analysis <- trackloss_analysis(data)
#' 
#' @export
#' @return A dataframe describing trackloss by-trial and by-participant

trackloss_analysis <- function(data) {

  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  colname_symbols <- purrr::map(data_options[grep(names(data_options), pattern="column$")], as.name) 
  
  # Get Trackloss-by-Trial:
  df_grouped_trial <- group_by_at(.tbl = data, .vars = c(data_options$participant_column, data_options$trial_column))
  df_trackloss_by_trial <- mutate(.data = df_grouped_trial, 
                                  SumTracklossForTrial = sum(!!colname_symbols$trackloss_column, na.rm = TRUE),
                                  TotalTrialLength = n(),
                                  TracklossForTrial = SumTracklossForTrial/TotalTrialLength)
  
  # Get Trackloss-by-Participant:
  df_grouped_ppt <- group_by_at(.tbl = df_trackloss_by_trial, .vars = c(data_options$participant_column))
  df_trackloss_by_ppt <- mutate(df_grouped_ppt,
                                SumTracklossForParticipant = sum(!!colname_symbols$trackloss_column, na.rm = TRUE),
                                TotalParticipantLength = n(),
                                TracklossForParticipant = SumTracklossForParticipant/TotalParticipantLength)

  # Get Z-Scores:
  df_grouped <- group_by_at(df_trackloss_by_ppt, .vars = c(data_options$participant_column, data_options$trial_column))
  df_summarized <- summarize(df_grouped,
                             Samples = mean(TotalTrialLength, na.rm = TRUE),
                             TracklossSamples = mean(SumTracklossForTrial, na.rm = TRUE),
                             TracklossForTrial = mean(TracklossForTrial, na.rm = TRUE),
                             TracklossForParticipant = mean(TracklossForParticipant, na.rm = TRUE))
  df_summarized <- ungroup(df_summarized)

  return(df_summarized)
}


#' Clean data by removing high-trackloss trials/subjects.
#'
#' Remove trials/participants with too much trackloss, with a customizable threshold.
#' 
#' @param data Data already run through \code{make_eyetrackingr_data}
#' @param participant_prop_thresh            Maximum proportion of trackloss for participants
#' @param trial_prop_thresh                  Maximum proportion of trackloss for trials
#' @param window_start_time,window_end_time  Time-window within which you want trackloss analysis to
#'   be based. Allows you to keep the entire trial window for data, but clean based on the trackloss
#'   within a subset of it
#'   
#' @examples
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' 
#' # scrub all trials with greater than 25% trackloss, and all
#' # participants with greater than 25% trackloss on average
#' # during the timeperiod 15500-2100
#' data_clean <- clean_by_trackloss(data,
#'                                  participant_prop_thresh = .25, 
#'                                  trial_prop_thresh = .25,
#'                                  window_start_time = 15500, 
#'                                  window_end_time = 21000
#' )
#' 
#' # scrub all trials with greater than 25% trackloss, but leave participants with a high average
#' data_clean <- clean_by_trackloss(data,
#'                                  trial_prop_thresh = .25,
#'                                  window_start_time = 15500, 
#'                                  window_end_time = 21000
#' )
#'   
#' @export
#' @return Cleaned data
clean_by_trackloss <- function(data,
                               participant_prop_thresh = 1,
                               trial_prop_thresh = 1,
                               window_start_time = -Inf, window_end_time = Inf) {
  
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  # Helpful Column:
  data$.TrialID <- paste(data[[data_options$participant_col]], data[[data_options$trial_col]], sep = "_")

  # Trackloss Analysis:
  message("Performing Trackloss Analysis...")
  data_tl <- subset_by_window(data = data, quiet=TRUE,
                              window_start_time = window_start_time, window_end_time = window_end_time )
  tl <- trackloss_analysis(data_tl)
  tl$.TrialID <- paste(tl[[data_options$participant_col]], tl[[data_options$trial_col]], sep = "_")

  # Bad Trials:
  if (trial_prop_thresh < 1) {
    message("Will exclude trials whose trackloss proportion is greater than : ", trial_prop_thresh)
    exclude_trials_props <- tl$.TrialID[tl$TracklossForTrial > trial_prop_thresh]
    message(paste("\t...removed ", length(exclude_trials_props), " trials."))
  } else {
    exclude_trials_props <- c()
  }

  # Bad Participants
  if (participant_prop_thresh < 1) {
    message("Will exclude participants whose trackloss proportion is greater than : ", participant_prop_thresh)
    exclude_ppts_prop <- unique(tl[[data_options$participant_column]][tl$TracklossForParticipant > participant_prop_thresh])
    message(paste("\t...removed ", length(exclude_ppts_prop), " participants."))
  } else {
    exclude_ppts_prop <- c()
  }

  exclude_trials <- c(exclude_trials_props, unique( data$.TrialID[data[[data_options$participant_col]] %in% exclude_ppts_prop] ))

  # Remove:
  data_clean <- filter(data, ! .TrialID %in% exclude_trials)
  data_clean$.TrialID <- NULL

  return(data_clean)

}

#' Describe dataset
#' 
#' Returns descriptive statistics about a column of choice. A simple convenience function that wraps
#' \code{dplyr::group_by} and \code{dplyr::summarize}, allowing a quick glance at the data.
#' 
#' @param data Data already run through \code{make_eyetrackingr_data}
#' @param describe_column The column to return descriptive statistics about.
#' @param group_columns Any columns to group by when calculating descriptive statistics (e.g., participants,
#'  conditions, etc.)
#' @param quantiles Numeric vector of length two with quantiles to compute (default: \code{c(.025, .975)}).
#'  
#'
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' describe_data(data, describe_column = "Animate", group_columns = "ParticipantName")
#'  
#' @export
#' @return A dataframe giving descriptive statistics for the \code{describe_column}, including mean, SD, var,
#' min, max, and number of trials
describe_data <- function(data, describe_column, group_columns, quantiles = c(.025, .975)) {

  # Data options:
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }

  # Group, Summarize
  dc_sym <- as.name(describe_column)
  df_grouped <- group_by_at(data, .vars = group_columns)
  df_summarized <- summarize(df_grouped,
                             Mean = mean(!!dc_sym, na.rm=TRUE),
                             SD = sd(!!dc_sym, na.rm=TRUE),
                             LowerQ = quantile(!!dc_sym, probs = quantiles[1], na.rm = TRUE),
                             UpperQ = quantile(!!dc_sym, probs = quantiles[2], na.rm = TRUE),
                             Min = min(!!dc_sym, na.rm=TRUE),
                             Max = max(!!dc_sym, na.rm=TRUE),
                             N = n() )
  if (data_options$trial_column %in% colnames(data))
    df_summarized <- left_join(x = df_summarized, 
                               y = summarize(df_grouped, NumTrials = n_distinct(!!as.name(data_options$trial_column), na.rm = TRUE)),
                               by = group_columns)
  out <- ungroup(df_summarized)
  class(out) <- c("eyetrackingR_data_summary", class(out))
  attr(out, "eyetrackingR") <- list(data_options = data_options, 
                                    describe_column = describe_column,
                                    group_columns = group_columns)
  return(out)

}

#' Plot some summarized data from eyetrackingR
#' 
#' Plots the data returned from \code{describe_data}. Like that function, this is a convenient 
#' wrapper good for sanity checks.
#' 
#' @param x The data returned by \code{make_time_window_data()}
#' @param ... Ignored
#' @export
#' @return A ggplot object
plot.eyetrackingR_data_summary <- function(x, ...) {
  attrs <- attr(x, "eyetrackingR")
  if (length(attrs$group_columns) > 1) group_col <- attrs$group_columns[2]
  else group_col <- NULL
  g <- ggplot(x, aes_string(x=attrs$group_columns[1], y="Mean", group=group_col)) +
    stat_summary(fun.y='mean', geom='point') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(attrs$describe_column)
  if (!is.null(group_col))
    g <- g + stat_summary(fun.y='mean', geom='line') 
  return(g)
}
