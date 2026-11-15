# Read the two export types once and pair them by filename stem.
results_files <- list.files("data", pattern = "_results\\.csv$", full.names = TRUE)
demographics_files <- sub("_results\\.csv$", "_demographics.csv", results_files)
if (!length(results_files)) stop("No results exports found in data/.")
if (any(!file.exists(demographics_files))) {
  stop("Missing matching demographics exports: ",
       paste(basename(demographics_files[!file.exists(demographics_files)]), collapse = ", "))
}
participant_codes <- stringr::str_match(basename(results_files), "_study_([^_]+)_results\\.csv$")[, 2]
stopifnot(!anyNA(participant_codes), !anyDuplicated(participant_codes))

demographics <- local({
  # The rest of the export contains repeated instrument and skill rows.
  # Extract survey fields explicitly, retaining missing fields for individual participants.
  fields <- c("Age", "Gender", "Country of Residence Age 0-7",
              "Country of Residence Age 7+", "English Proficiency (1-6)",
              "Music Involvement", "Age Started Music", "Years of Musical Training",
              "Music Styles Played", "Music Genres")
  omsi_levels <- c("Non-musician", "Music-loving non-musician", "Amateur musician",
                   "Serious amateur musician", "Semi-professional musician", "Professional musician")
  purrr::map2(demographics_files, participant_codes, function(file, code) {
    csv <- read.csv(file, header = FALSE, colClasses = "character", fill = TRUE)
    keys <- trimws(csv[[1]])
    if (anyDuplicated(keys[keys %in% fields])) {
      stop("Duplicate survey field in ", basename(file))
    }
    values <- trimws(csv[[2]][match(fields, keys)])
    values[values %in% c("", "Not provided")] <- NA_character_
    tibble::as_tibble_row(c(list(source_file = basename(file), code = code),
                           setNames(as.list(values), fields)))
  }) |>
    purrr::list_rbind() |>
    mutate(
      Age = as.numeric(Age),
      Gender = factor(Gender),
      `Age Started Music` = as.numeric(`Age Started Music`),
      # The upper survey response is top-coded at 30 years.
      `Years of Musical Training` = as.numeric(sub("^30\\+$", "30", `Years of Musical Training`)),
      `Music Involvement` = factor(`Music Involvement`, levels = omsi_levels)
    )
})

data <- purrr::map2(results_files, participant_codes, function(file, code) {
  trials <- read.csv(file, fileEncoding = "UTF-8")
  required <- c("Harmony", "Pitch.Class", "Selected.Word", "Feels.Adequate",
                "Custom.Word", "Response.Time..ms.", "Timestamp")
  if (!all(required %in% names(trials)) || !nrow(trials)) {
    stop("Missing columns or trial rows in ", basename(file))
  }
  mutate(trials, code = code, Custom.Word = as.character(Custom.Word))
}) |>
  purrr::list_rbind() |>
  left_join(
    transmute(demographics, code, age = Age, gender = as.character(Gender),
              omsi = `Music Involvement`, training_years = `Years of Musical Training`,
              styles_played = `Music Styles Played`,
              music_genres = strsplit(`Music Genres`, ",")),
    by = "code", relationship = "many-to-one"
  )
message("Read ", nrow(data), " trials from ", nrow(demographics), " paired exports.")
rm(results_files, demographics_files, participant_codes)
