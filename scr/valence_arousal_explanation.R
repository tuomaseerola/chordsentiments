# Each dimension uses its own trial-weighted median. Repeated selections
# contribute repeatedly, so these cutoffs describe the observed responses.
valence_median <- median(data$word_V, na.rm = TRUE)
arousal_median <- median(data$word_A, na.rm = TRUE)
data <- data |>
  mutate(
    valence_binary = if_else(word_V > valence_median, "High Valence", "Low Valence"),
    arousal_binary = if_else(word_A > arousal_median, "High Arousal", "Low Arousal")
  )
rm(valence_median, arousal_median)
