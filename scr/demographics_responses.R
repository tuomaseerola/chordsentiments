response_categories <- c("Suggested", "Close", "Distant", "Unrelated")

add_response_summaries <- function(demographics, data,
                                   measure = c("proportion", "count")) {
  measure <- match.arg(measure)
  category_col <- intersect(c("category", "Category"), names(data))[1]
  stopifnot(!is.na(category_col), "id" %in% names(demographics),
            "id" %in% names(data), !anyDuplicated(demographics$id),
            !anyNA(demographics$id), !anyNA(data$id),
            all(data$id %in% demographics$id))

  # Unscored selections do not enter the count matrix or proportion denominator.
  categories <- factor(data[[category_col]], levels = response_categories)
  counts <- table(factor(data$id, levels = demographics$id), categories)
  values <- unclass(counts)
  if (measure == "proportion") {
    totals <- rowSums(values)
    values <- values / replace(totals, totals == 0, NA_real_)
  }

  colnames(values) <- paste0(tolower(response_categories),
                             if (measure == "count") "_n" else "_prop")
  demographics[colnames(values)] <- as.data.frame(values)
  demographics
}

# April 24 recruitment window inferred from filenames and the project notes.
recruitment_source <- function(source_file) {
  timestamp <- substr(basename(source_file), 1, 23)
  factor(dplyr::if_else(
    dplyr::between(timestamp, "2025-04-24T12-41-40-666", "2025-04-24T16-08-08-408"),
    "Prolific", "Organic"
  ), levels = c("Organic", "Prolific"))
}
