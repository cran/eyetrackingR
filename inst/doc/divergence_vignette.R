## ----results="hide"------------------------------------------------------
set.seed(42)

library("Matrix")
library("lme4")
library("ggplot2")
library("eyetrackingR")

data("word_recognition")
data <- make_eyetrackingr_data(word_recognition, 
                       participant_column = "ParticipantName",
                       trial_column = "Trial",
                       time_column = "TimeFromTrialOnset",
                       trackloss_column = "TrackLoss",
                       aoi_columns = c('Animate','Inanimate'),
                       treat_non_aoi_looks_as_missing = TRUE
                )

# subset to response window post word-onset
response_window <- subset_by_window(data, 
                                    window_start_time = 15500, 
                                    window_end_time = 21000, 
                                    rezero = FALSE)

# analyze amount of trackloss by subjects and trials
(trackloss <- trackloss_analysis(data = response_window))

# remove trials with > 25% of trackloss
response_window_clean <- clean_by_trackloss(data = response_window,
                                            trial_prop_thresh = .25)

# create Target condition column
response_window_clean$Target <- as.factor( ifelse(test = grepl('(Spoon|Bottle)', response_window_clean$Trial), 
                                       yes = 'Inanimate', 
                                       no  = 'Animate') )

## ---- warning=FALSE------------------------------------------------------
response_time <- make_time_sequence_data(response_window_clean,
                                  time_bin_size = 100, 
                                  predictor_columns = c("Target"),
                                  aois = "Animate",
                                  summarize_by = "ParticipantName"
                            )

# visualize timecourse
plot(response_time, predictor_column = "Target") + 
  theme_light() +
  coord_cartesian(ylim = c(0,1))

## ---- warning=FALSE------------------------------------------------------
bootstrapped_familiar <- make_boot_splines_data(response_time, 
                                              predictor_column = 'Target', 
                                              within_subj = TRUE, 
                                              samples = 1000, 
                                              alpha = .05,
                                              smoother = "smooth.spline") 

## ---- warning=FALSE------------------------------------------------------
plot(bootstrapped_familiar)

## ---- warning=FALSE------------------------------------------------------
bootstrap_analysis_familiar <- analyze_boot_splines(bootstrapped_familiar)
summary(bootstrap_analysis_familiar)

## ---- warning=FALSE------------------------------------------------------
alpha = .05
num_sub = length(unique((response_window_clean$ParticipantName)))
threshold_t = qt(p = 1 - alpha/2, 
                 df = num_sub-1) # pick threshold t based on alpha = .05 two tailed

## ---- warning=FALSE------------------------------------------------------
df_timeclust <- make_time_cluster_data(response_time, 
                                      test= "t.test",
                                      paired = TRUE,
                                      predictor_column = "Target", 
                                      threshold = threshold_t) 
plot(df_timeclust) +
  ylab("T-Statistic")
summary(df_timeclust)

## ---- warning=FALSE------------------------------------------------------
clust_analysis <- analyze_time_clusters(df_timeclust, within_subj = TRUE, paired=TRUE, 
                                        samples=100) # in practice, you should use a lot more
summary(clust_analysis)

plot(clust_analysis)

