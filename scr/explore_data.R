# Derive the original six-level OMSI score and the self-reported musician split.
data <- data |>
  mutate(
    omsiNum = as.integer(omsi),
    omsiB = factor(if_else(omsiNum <= 2L, "Non-musician", "Musician"),
                   levels = c("Non-musician", "Musician"))
  )

# Pitch class is the shared 0–11 interval index in each mode. Chord uses the
# original predictor labels for indices 6 and 10 in both modes.
chord_levels <- c("Root", "min2nd", "Maj2nd", "min3rd", "Maj3rd", "Perf4th",
                  "Aug4th", "Perf5th", "min6th", "Maj6th", "Dom7th", "Maj7th")
stopifnot(!anyNA(data$Group), all(data$Pitch.Class %in% 0:11),
          all((as.integer(data$Group) - 1L) %% 12L == data$Pitch.Class))
data <- data |>
  mutate(
    Mode = factor(if_else(startsWith(as.character(Group), "MAJ_"), "Major", "Minor")),
    Chord = factor(chord_levels[Pitch.Class + 1L], levels = chord_levels)
  )

# Timing is diagnostic only. No trials are excluded by the 2.6-second threshold.
response_timing <- data |>
  summarise(trials = n(), missing_time = sum(is.na(Response.Time..ms.)),
            below_2_6_seconds = sum(Response.Time..ms. < 2600, na.rm = TRUE),
            median_seconds = median(Response.Time..ms., na.rm = TRUE) / 1000)
print(response_timing)

response_time_plot <- ggplot(data, aes(Response.Time..ms. / 1000)) +
  geom_histogram(binwidth = 0.25, na.rm = TRUE) +
  geom_vline(xintercept = 2.6, linetype = "dashed", colour = "firebrick") +
  coord_cartesian(xlim = c(0, 10)) +
  labs(x = "Response time (seconds)", y = "Trials") +
  theme_minimal()

# Divide by all scored trials in each cell before selecting the two categories.
response_by_omsi_plot <- data |>
  filter(category %in% c("Suggested", "Close", "Distant", "Unrelated")) |>
  count(Mode, Chord, omsiB, category) |>
  group_by(Mode, Chord, omsiB) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  filter(category %in% c("Suggested", "Close")) |>
  ggplot(aes(Chord, proportion, fill = category)) +
  geom_col() +
  facet_grid(omsiB ~ Mode) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(x = "Chord", y = "Share of scored trials", fill = "Response category") +
  theme_minimal()
