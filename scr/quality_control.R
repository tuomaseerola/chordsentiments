# Step 1 diagnostics. Source after building data and demographics in contents.R.
source("scr/demographics_responses.R")

# Recruitment is inferred from the April filename window, including pilot sessions.
demographics$source <- recruitment_source(demographics$source_file)
demographics <- add_response_summaries(demographics, data)
data$source <- demographics$source[match(data$code, demographics$code)]
stopifnot(!anyNA(data$source))

# Preserve all selected terms for vocabulary comparisons, including unscored
# trials. Missing selections cannot contribute to a word distribution.
word_trials <- filter(data, !is.na(Selected.Word), nzchar(trimws(Selected.Word)))
print(count(demographics, cluster, source))

# Pooled entropy describes how spread out word selections are. Normalized
# entropy uses each cell's own observed vocabulary as its denominator.
H <- function(x) {
  p <- as.numeric(prop.table(table(x)))
  -sum(p * log2(p))
}
H_by_cell <- word_trials |>
  group_by(cluster, source) |>
  summarise(
    n_choices = n(), n_words = n_distinct(Selected.Word),
    H = H(Selected.Word),
    H_norm = if (n_words > 1L) H / log2(n_words) else NA_real_,
    .groups = "drop"
  )

# Count term selections and the share of participants who selected each term.
cell_totals <- word_trials |>
  group_by(cluster, source) |>
  summarise(n_choices = n(), n_ids = n_distinct(id), .groups = "drop")
top_terms_id <- word_trials |>
  group_by(cluster, source, Selected.Word) |>
  summarise(n = n(), ids_using = n_distinct(id), .groups = "drop") |>
  left_join(cell_totals, by = c("cluster", "source"), relationship = "many-to-one") |>
  mutate(prop = n / n_choices, prop_ids = ids_using / n_ids) |>
  group_by(cluster, source) |>
  slice_max(n, n = 10, with_ties = FALSE) |>
  ungroup()
top_terms <- select(top_terms_id, cluster, source, Selected.Word, n, prop)

## Assess how much agreement there is among groups


M <- word_trials |>
  mutate(group = paste(cluster, source, sep = "_")) |>
  count(group, Selected.Word) |>
  pivot_wider(names_from = group, values_from = n, values_fill = 0) |>
  tibble::column_to_rownames("Selected.Word") |>
  as.matrix()
P <- prop.table(M, 2)                      # word proportions per cell

# Jensen-Shannon distance (0 = identical, 1 = no overlap, in bits)
jsd <- function(p, q) {
  m  <- (p + q) / 2
  kl <- function(a, b) { i <- a > 0; sum(a[i] * log2(a[i] / b[i])) }
  sqrt(0.5 * kl(p, m) + 0.5 * kl(q, m))
}
JSD <- outer(colnames(P), colnames(P),
             Vectorize(function(a, b) jsd(P[, a], P[, b])))
dimnames(JSD) <- list(colnames(P), colnames(P))

# Cosine similarity of the proportion vectors
cosim <- crossprod(P) / outer(sqrt(colSums(P^2)), sqrt(colSums(P^2)))

# Overlap of the top-10 words (Jaccard)
top10 <- setNames(lapply(seq_len(ncol(P)), function(i) {
  counts <- P[, i]
  head(names(sort(counts[counts > 0], decreasing = TRUE)), 10)
}), colnames(P))
jacc  <- outer(colnames(P), colnames(P),
               Vectorize(function(a, b)
                 length(intersect(top10[[a]], top10[[b]])) /
                   length(union(top10[[a]], top10[[b]]))))
dimnames(jacc) <- dimnames(JSD)

word_distribution_tree <- if (ncol(P) > 1L) hclust(as.dist(JSD)) else NULL

## Split half
# Estimate how different two samples from the same cell can look just from sampling.

split_half <- function(d, B = 200) {
  ids <- unique(d$id)
  if (length(ids) < 2L) return(NA_real_)
  replicate(B, {
    h <- sample(ids, length(ids) %/% 2)
    a <- table(factor(d$Selected.Word[d$id %in% h],  levels = rownames(P)))
    b <- table(factor(d$Selected.Word[!d$id %in% h], levels = rownames(P)))
    jsd(a / sum(a), b / sum(b))
  })
}

floor_tbl <- withr::with_seed(1, word_trials |>
  group_by(cluster, source) |>
  group_modify(~ tibble(floor = mean(split_half(.x)))))

# Compare Organic and Prolific word distributions within each preference
# group. Shuffle sources at the participant level so repeated trials remain
# together and cluster composition is fixed. These tests are exploratory:
# inference assumes source labels are exchangeable within each cluster.
participant_words <- word_trials |>
  count(id, Selected.Word) |>
  pivot_wider(names_from = Selected.Word, values_from = n, values_fill = 0) |>
  left_join(select(demographics, id, cluster, source), by = "id", relationship = "one-to-one")

res <- withr::with_seed(1, participant_words |>
  group_by(cluster) |>
  filter(n_distinct(source) == 2L) |>
  group_modify(function(cell, key) {
    counts <- as.matrix(select(cell, -id, -source))
    source_distance <- function(labels) {
      totals <- rowsum(counts, labels)
      proportions <- totals / rowSums(totals)
      jsd(proportions[1, ], proportions[2, ])
    }
    observed <- source_distance(cell$source)
    permuted <- replicate(2000, source_distance(sample(cell$source)))
    tibble(JSD = observed, null_mean = mean(permuted),
           p = (1 + sum(permuted >= observed)) / (1 + length(permuted)))
  }) |>
  ungroup() |>
  mutate(p_holm = p.adjust(p, method = "holm")))

#### Describe the backgrounds of the groups based on ------------

library(patchwork)

demographics$music_involvementNUM <- as.numeric(demographics$music_involvement)

d <- demographics |>
  mutate(group = interaction(cluster, source, sep = " / "))

num_vars <- c("Age", "music_involvementNUM",
              "age_started", "years_training", "styles_count",
              "styles_jazz", "styles_class", "styles_mainstream",
              "styles_mainstream_complex", "genres_jazz", "genres_class",
              "genres_mainstream", "genres_mainstream_complex",
              "suggested_prop", "close_prop", "distant_prop", "unrelated_prop")
cat_vars <- c("Gender", "style_max", "genre_max", "Country of Residence Age 7+")

# 0. Cell sizes (check this first)
n_tbl <- d |> count(cluster, source)

# 1. Overview heatmap: standardised group means + which variables differ most
long <- d |>
  select(group, cluster, source, all_of(num_vars)) |>
  pivot_longer(all_of(num_vars), names_to = "var", values_to = "value") |>
  group_by(var) |> mutate(z = as.numeric(scale(value))) |> ungroup()

eta <- long |>
  group_by(var) |>
  summarise(eta2 = {
    available <- !is.na(value)
    if (n_distinct(group[available]) > 1L && n_distinct(value[available]) > 1L) {
      summary(lm(value ~ group))$r.squared
    } else NA_real_
  }, .groups = "drop")

heat <- long |>
  group_by(var, group) |> summarise(z = mean(z, na.rm = TRUE), .groups = "drop") |>
  left_join(eta, by = "var") |>
  mutate(var = reorder(var, eta2))

p_heat <- ggplot(heat, aes(group, var, fill = z)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.1f", z)), size = 3) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c",
                       limits = c(-2, 2), oob = scales::squish, name = "z (mean)") +
  labs(x = NULL, y = NULL, title = "Group means (standardised); variables sorted by R² across groups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 2. Distributions of the numeric variables
p_num <- ggplot(long |> filter(var %in% c("Age", "years_training", "age_started",
                                          "music_involvementNUM", "styles_count", "genres_count")),
                aes(cluster, value, colour = source, fill = source)) +
  geom_violin(alpha = .15, position = position_dodge(.8), scale = "width", na.rm = TRUE) +
  geom_boxplot(width = .15, position = position_dodge(.8), outlier.shape = NA, fill = "white") +
  facet_wrap(~ var, scales = "free_y") +
  labs(x = NULL, y = NULL, title = "Numeric background variables") +
  theme_minimal() + theme(legend.position = "bottom")

# 3. Categorical composition (proportions within each group)
cat_long <- d |>
  select(group, all_of(cat_vars)) |>
  mutate(across(all_of(cat_vars), as.character)) |>
  pivot_longer(all_of(cat_vars), names_to = "var", values_to = "level") |>
  mutate(level = forcats::fct_lump_n(factor(level), 6, other_level = "Other")) |>
  count(var, group, level) |>
  group_by(var, group) |> mutate(prop = n / sum(n))

p_cat <- ggplot(cat_long, aes(group, prop, fill = level)) +
  geom_col() + facet_wrap(~ var, scales = "free", ncol = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Categorical background variables") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))

quality_control_plot <- (p_heat / p_num / p_cat) + plot_layout(heights = c(1.2, 1.2, 1.4))

