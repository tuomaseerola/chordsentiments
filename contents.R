# Step 1. Run from the experiment folder with Rscript --vanilla contents.R.
if (!file.exists("scr/read_data.R")) {
  stop("Run contents.R from the experiment folder.")
}
source("scr/libraries.R")

# Read one matched pair of exports per participant, then score each trial.
source("scr/read_data.R")
source("scr/prepare_chords.R")
source("scr/explore_data.R")

# Assign preference groups and join participant metadata to trials by study code.
source("scr/music_pref_variable_demographics.R")
source("scr/cluster_participants.R")
preferences <- attach_preference_groups(data, demographics)
data <- preferences$data
demographics <- preferences$demographics
rm(preferences)

# Join stimulus-level predictors and selected-word norms.
source("scr/chord_predictors.R")
source("scr/get_warriner_scores.R")
source("scr/valence_arousal_explanation.R")

# Run diagnostics on the data just built. Save only after preprocessing succeeds.
source("scr/quality_control.R")
saveRDS(data, "data/data_N445.rds")
# Include the survey fields the dashboard cannot derive from trial data.
dashboard_fields <- c(
  "id", "code", "cluster", "preference_assignment",
  "Country of Residence Age 0-7", "Country of Residence Age 7+",
  "English Proficiency (1-6)", "Age Started Music"
)
write.csv(demographics[dashboard_fields], "data/cluster_assignments.csv", row.names = FALSE)

# Step 2. Start the dashboard separately with shiny::runApp(launch.browser = TRUE).
# determine which subsets to keep

# Step 3. Use accuracy_ordinal_R1.qmd with the saved trial data.
