# The external word norms are required for the selected-word predictors.
# Set WARRINER_CSV when the original CSV lives outside the project data folder.
warriner_path <- Sys.getenv("WARRINER_CSV", "data/Ratings_Warriner_et_al.csv")
if (!file.exists(warriner_path)) {
  stop("Word norms CSV not found at ", warriner_path,
       ". Set WARRINER_CSV to the original Ratings_Warriner_et_al.csv file.")
}
warriner <- read.csv(warriner_path) |>
  transmute(word_key = trimws(Word), word_V = V.Mean.Sum, word_A = A.Mean.Sum)
stopifnot(!anyDuplicated(warriner$word_key))

# Preserve case because the norms distinguish some words from proper names.
# Keep unrated trials with missing norm scores.
data <- data |>
  mutate(word_key = trimws(Selected.Word)) |>
  left_join(warriner, by = "word_key", relationship = "many-to-one") |>
  select(-word_key)
message(sum(is.na(data$word_V) | is.na(data$word_A)), " trials with missing word norms.")
rm(warriner_path, warriner)
