# prepare_chords.R
# Part of chord naming data with Hamm and Lahdelma
# T. Eerola, 27/8/2025


# Map exported Harmony labels to the study’s chord identifiers.
data$Group<-'Undefined'
data$Group[data$Harmony =='MAJOR G: Root'] <- 'MAJ_Root'
data$Group[data$Harmony =='MAJOR G(♭9): min 2nd'] <- 'MAJ_min2nd'
data$Group[data$Harmony =='MAJOR G2: Maj 2nd'] <- 'MAJ_Maj2nd'
data$Group[data$Harmony =='MAJOR G(♯9): min 3rd'] <- 'MAJ_min3rd'
data$Group[data$Harmony =='MAJOR G: Maj 3rd'] <- 'MAJ_Maj3rd'
data$Group[data$Harmony =='MAJOR Gadd4: Perf 4th'] <- 'MAJ_Perf4th'
data$Group[data$Harmony =='MAJOR G(♯4): Aug 4th'] <- 'MAJ_Aug4th'
data$Group[data$Harmony =='MAJOR G: Perf 5th'] <- 'MAJ_Perf5th'
data$Group[data$Harmony =='MAJOR G(♭6): min 6th'] <- 'MAJ_min6th'
data$Group[data$Harmony =='MAJOR G6: Maj 6th'] <- 'MAJ_Maj6th'
data$Group[data$Harmony =='MAJOR G7: Dom 7th'] <- 'MAJ_Dom7th'
data$Group[data$Harmony =='MAJOR G∆7: Maj 7th'] <- 'MAJ_Maj7th'

data$Group[data$Harmony =='MINOR Gm: Root'] <- 'MIN_Root'
data$Group[data$Harmony =='MINOR Gm(♭9): min 2nd'] <- 'MIN_min2nd'
data$Group[data$Harmony =='MINOR Gm2: Maj 2nd'] <- 'MIN_Maj2nd'
data$Group[data$Harmony =='MINOR Gm: min 3rd'] <- 'MIN_min3rd'
data$Group[data$Harmony =='MINOR Gm(add3): Maj 3rd'] <- 'MIN_Maj3rd'
data$Group[data$Harmony =='MINOR Gm4: Perf 4th'] <- 'MIN_Perf4th'
data$Group[data$Harmony =='MINOR Gm(♭5): Dim 5th'] <- 'MIN_Dim5th'
data$Group[data$Harmony =='MINOR Gm: Perf 5th'] <- 'MIN_Perf5th'
data$Group[data$Harmony =='MINOR Gm♭6: min 6th'] <- 'MIN_min6th'
data$Group[data$Harmony =='MINOR Gm6: Maj 6th'] <- 'MIN_Maj6th'
data$Group[data$Harmony =='MINOR Gm7: min 7th'] <- 'MIN_min7th'
data$Group[data$Harmony =='MINOR Gm∆7: Maj 7th'] <- 'MIN_Maj7th'

data$Group <- factor(data$Group, levels = c('MAJ_Root','MAJ_min2nd','MAJ_Maj2nd','MAJ_min3rd','MAJ_Maj3rd',
                                            'MAJ_Perf4th','MAJ_Aug4th','MAJ_Perf5th','MAJ_min6th','MAJ_Maj6th',
                                            'MAJ_Dom7th','MAJ_Maj7th',
                                            'MIN_Root','MIN_min2nd','MIN_Maj2nd','MIN_min3rd','MIN_Maj3rd',
                                            'MIN_Perf4th','MIN_Dim5th','MIN_Perf5th','MIN_min6th','MIN_Maj6th',
                                            'MIN_min7th','MIN_Maj7th'))

# Match exact chord and term pairs after normalizing case and whitespace.
normalize_word <- function(word) stringr::str_to_lower(stringr::str_squish(word))

legacy <- read.csv("scr/word_groups_analysis_results.csv") |>
  mutate(chord_key = sub("_target[0-9]+$", "", tolower(Group)),
         word_key = normalize_word(Word))

# Preserve the legacy distance field for analyses that still use it.
legacy_key <- legacy |>
  mutate(target_category = case_when(
    Distance == 0 ~ "identical",
    Distance > 0 & Distance < 1 ~ "close",
    Distance >= 1 ~ "far"
  )) |>
  select(chord_key, word_key, target_category)

categories <- c("Suggested", "Close", "Distant", "Unrelated")
revised_key <- read.csv("scr/chord_to_terms_finalUPDATED2.csv") |>
  pivot_longer(-CHORD, names_to = "category", values_to = "word_key") |>
  transmute(chord_key = tolower(CHORD),
            word_key = normalize_word(word_key),
            category = stringr::str_to_title(tolower(category)))
stopifnot(setequal(revised_key$chord_key, tolower(levels(data$Group))))

# Major G7 responses use the legacy vocabulary. Reconstruct its four-level key
# by ranking the documented distances within each of the three target quartets.
# Score the entire chord with this key, including the exact term "complex".
dom7_key <- legacy |>
  filter(chord_key == "maj_dom7th") |>
  group_by(Group) |>
  arrange(Distance, .by_group = TRUE) |>
  mutate(category = categories[row_number()]) |>
  ungroup() |>
  select(chord_key, word_key, category)
stopifnot(all(table(dom7_key$category) == 3L), nrow(dom7_key) == 12L)

scoring_key <- bind_rows(filter(revised_key, chord_key != "maj_dom7th"), dom7_key)
data <- data |>
  mutate(chord_key = tolower(Group), word_key = normalize_word(Selected.Word)) |>
  left_join(legacy_key, by = c("chord_key", "word_key"), relationship = "many-to-one") |>
  left_join(scoring_key, by = c("chord_key", "word_key"), relationship = "many-to-one") |>
  # Keep unlisted selections for audit and term counts. Scored analyses
  # exclude these rows explicitly using the None category.
  mutate(target_category = factor(target_category, levels = c("identical", "close", "far")),
         category = factor(coalesce(category, "None"), levels = c(categories, "None"))) |>
  select(-chord_key, -word_key)

rm(normalize_word, legacy, legacy_key, categories, revised_key, dom7_key, scoring_key)
