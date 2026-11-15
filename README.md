# README
Dennis Hamm chord and term data with Imre Lahdelma
28/8/2025, updated in 15 November 2026
Version by T. Eerola

This is an experiment data concerning affect terms for scale tones (12) played within major and minor context.

## Step 1. Preprocess the experiment exports

Run from the experiment folder with R 4.1 or later and the packages
`tidyverse`, `here`, `incon`, `withr`, and `patchwork`. The scripts use
dplyr 1.1 or later and purrr 1.0 or later.

Place the original Warriner word norms at `data/Ratings_Warriner_et_al.csv`
or set the `WARRINER_CSV` environment variable to its path. The CSV must
contain `Word`, `V.Mean.Sum`, and `A.Mean.Sum`.

`contents.R`

The script pairs results and demographics exports by filename, scores trials,
assigns preference groups, joins chord and word predictors, and runs recruitment
and response diagnostics. It writes `data/data_N445.rds` and
`data/cluster_assignments.csv`. Source it in R to also retain the
`demographics`, `response_timing`, `response_time_plot`,
`response_by_omsi_plot`, and `quality_control_plot` objects.

- **Response scoring.** Exact chord–term matches assign Suggested, Close,
  Distant, or Unrelated. Major dominant seventh uses the legacy distance rank
  within each target quartet. Unmatched trials remain in the data as `None`.
  The 2.6-second timing cutoff is diagnostic only.
- **Preference groups.** The `cluster` field stores one of four predefined
  preference groups. Each distinct listening choice or playing style contributes
  one point, split across equally specific matches. Playing receives weight
  OMSI / 7 and listening receives the remainder. Ties use the higher-weight
  source, then the prevalence among clear assignments within the same OMSI
  level and overall, and finally alphabetical order. The assignment reason
  identifies ties and the Mainstream fallback when neither source is classified.
- **Recruitment source.** Prolific is inferred from the April 24 filename window,
  including the likely pilot. Organic identifies the remaining exports. Word
  distribution permutations shuffle source labels within preference group at
  the participant level and require exchangeability within those groups.
- **Chord predictors.** The supplied MIDI vectors for Root and Perf5th are
  identical within each mode, so the acoustic predictors are identical for those
  pairs. The interval labels and tonal-dissonance annotations are kept separately.
- **Word norms.** Valence and arousal describe the selected word. They are
  available after a response has been made. Binary summaries use the separate
  trial-weighted medians of valence and arousal.

## Step 2. Explore the dashboard

Install `shiny`, `dplyr`, and `ggplot2`, then run from the experiment folder:

```r
shiny::runApp(launch.browser = TRUE)
```

This is available at [https://musicscience.shinyapps.io/experiment/](https://musicscience.shinyapps.io/experiment/)

Choose two to four properties. Each graph shows preference groups within
recruitment source. The response controls switch between counts and proportions.
Each proportion uses the participant's scored trials as the denominator.

For shinyapps.io, deploy only the app and its prepared inputs. Run this from
the experiment folder after configuring your shinyapps.io account in
`rsconnect`:

```r
rsconnect::deployApp(
  appFiles = c("app.R", "data/data_N445.rds", "data/cluster_assignments.csv")
)
```

The app directly uses Shiny, dplyr, and ggplot2. The participant export includes
the four survey fields used by the dashboard.
The prepared files must come from the same preprocessing run. The deployment
command bundles only the three listed files.

## Step 3. Fit the ordinal models

