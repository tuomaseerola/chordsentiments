# chord_predictors.R
# Part of chord naming data with Hamm and Lahdelma
# T. Eerola, 27/8/2025

library(incon)

chord<-c("Root","min2nd","Maj2nd","min3rd","Maj3rd","Perf4th","Aug4th","Perf5th","min6th","Maj6th","Dom7th","Maj7th")


#### Define chords -----
# Source: the 29 August 2025 MIDI note list from Dennis. In that list,
# Root and Perf5th have identical pitch vectors within each mode, so these
# acoustic predictors cannot distinguish them. Preserve the supplied voicings.

MAJ_MID <- list()
MAJ_MID[[1]]<-c(43, 50, 55, 59, 62, 67, 71, 74, 79)
MAJ_MID[[2]]<-c(43, 50, 55, 59, 62, 67, 71, 74, 80)
MAJ_MID[[3]]<-c(43, 50, 55, 59, 62, 67, 71, 74, 81)
MAJ_MID[[4]]<-c(43, 50, 55, 59, 62, 67, 71, 74, 82)
MAJ_MID[[5]]<-c(43, 50, 55, 59, 62, 67, 71, 74, 83)
MAJ_MID[[6]]<-c(43, 50, 59, 60, 62, 67, 71, 72, 74, 79)
MAJ_MID[[7]]<-c(43, 50, 59, 61, 62, 67, 71, 73, 74, 79)
MAJ_MID[[8]]<-c(43, 50, 55, 59, 62, 67, 71, 74, 79)
MAJ_MID[[9]]<-c(43, 50, 59, 62, 63, 67, 71, 74, 75, 79)
MAJ_MID[[10]]<-c(43, 50, 59, 62, 64, 67, 71, 74, 76, 79)
MAJ_MID[[11]]<-c(43, 50, 53, 59, 62, 65, 67, 71, 74, 77, 79)
MAJ_MID[[12]]<-c(43, 50, 54, 59, 62, 66, 67, 71, 74, 78, 79)

MIN_MID <- list()
MIN_MID[[1]]<-c(43, 50, 55, 58, 62, 67, 70, 74, 79)
MIN_MID[[2]]<-c(43, 50, 56, 58, 62, 67, 68, 70, 74, 79, 80)
MIN_MID[[3]]<-c(43, 50, 57, 58, 62, 67, 69, 70, 74, 79, 81)
MIN_MID[[4]]<-c(43, 50, 58, 62, 67, 70, 74, 79, 82)
MIN_MID[[5]]<-c(43, 50, 58, 59, 62, 67, 70, 71, 74, 79, 83)
MIN_MID[[6]]<-c(43, 50, 58, 60, 62, 67, 70, 72, 74, 79)
MIN_MID[[7]]<-c(43, 50, 58, 61, 62, 67, 70, 73, 74, 79)
MIN_MID[[8]]<-c(43, 50, 55, 58, 62, 67, 70, 74, 79)
MIN_MID[[9]]<-c(43, 50, 58, 62, 63, 67, 70, 74, 75, 79)
MIN_MID[[10]]<-c(43, 50, 58, 62, 64, 67, 70, 74, 76, 79)
MIN_MID[[11]]<-c(43, 50, 53, 58, 62, 65, 67, 70, 74, 77, 79)
MIN_MID[[12]]<-c(43, 50, 54, 58, 62, 66, 67, 70, 74, 78, 79)

# Stable model labels avoid depending on the order of the package model table.
model_labels <- c(hut = "hutch_78_roughness", har = "har_18_harmonicity",
                  wan = "wang_13_roughness", cor = "har_19_corpus",
                  jon = "jl_12_tonal", com = "har_19_composite")
stopifnot(all(model_labels %in% incon_models$label))
models <- tibble(
  Mode = rep(c("Major", "Minor"), each = length(chord)),
  Chord = rep(chord, times = 2),
  midi = c(MAJ_MID, MIN_MID)
)
for (predictor in names(model_labels)) {
  models[[predictor]] <- vapply(models$midi, incon, numeric(1),
                               model = model_labels[[predictor]])
}
models$midi <- NULL

# Imre’s tonal-dissonance annotations follow the same Major-then-Minor
# condition order. td is binary; tda, tdr, and tdi are alternative codings.
models$td <- c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1)
models$tda <- c(0, 2, 0, 2, 0, 0, 0, 0, 2, 0, 1, 0, 0, 1, 0, 0, 2, 0, 2, 0, 0, 0, 0, 2)
models$tdr <- c(0,2,0,2,0,0,1,0,2,0,1,0,0,1,0,0,2,0,2,0,0,1,0,2)
models$tdi <- c(0,2,0,2,0,0,1,0,2,0,1,0,0,1,0,0,2,0,1,0,0,1,0,1)

# Each stimulus has one predictor row. Keep the trial order and factor levels.
stopifnot(nrow(anti_join(data, models, by = c("Mode", "Chord"))) == 0L)
models$Mode <- factor(models$Mode, levels = levels(data$Mode))
models$Chord <- factor(models$Chord, levels = levels(data$Chord))
data <- left_join(data, models, by = c("Mode", "Chord"), relationship = "many-to-one")
rm(chord, MAJ_MID, MIN_MID, models, model_labels, predictor)
