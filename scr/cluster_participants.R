# cluster_participants.R

# Assign predefined preference groups and attach participant metadata by study code.
attach_preference_groups <- function(data, demographics) {
  code <- sub("_demographics\\.csv$", "", basename(demographics$source_file))
  code <- sub("^.*_study_", "", code)
  trial_codes <- if ("subject" %in% names(data)) data$subject else data$code
  participant_index <- match(trial_codes, code)
  stopifnot(!anyDuplicated(code), !anyNA(participant_index))

  scores <- classify_preferences(demographics)
  demographics[names(scores)] <- scores
  # Numeric IDs are dataset-local. The filename code is the join key.
  demographics$code <- code
  demographics$id <- seq_len(nrow(demographics))
  demographics$music_involvement <- demographics$`Music Involvement`
  demographics$age_started <- demographics$`Age Started Music`
  demographics$years_training <- demographics$`Years of Musical Training`

  data[names(scores)] <- scores[participant_index, ]
  data$id <- demographics$id[participant_index]
  data$code <- trial_codes
  data$subject <- NULL

  list(data = data, demographics = demographics, code = code)
}

