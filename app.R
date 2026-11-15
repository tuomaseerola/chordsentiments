library(shiny)
library(dplyr)
library(ggplot2)

# The deployment bundle contains only this app and the two prepared exports.
trials <- readRDS("data/data_N445.rds")
assignments <- read.csv("data/cluster_assignments.csv", check.names = FALSE,
                        colClasses = c(code = "character"))
stopifnot(!anyNA(trials$code), !anyNA(assignments$code), !anyDuplicated(assignments$code),
          setequal(trials$code, assignments$code))

response_categories <- c("Suggested", "Close", "Distant", "Unrelated")
preference_groups <- c("Jazz", "Classical", "Mainstream", "Mainstream-Complex")
source_levels <- c("Organic", "Prolific")

# One row per participant. Join by study code because numeric IDs are dataset-local.
participant_fields <- c(
  "code", "source", "age", "gender", "omsi", "omsiB", "training_years",
  "styles_count", "genres_count", "style_max", "genre_max",
  grep("^(styles|genres)_(jazz|class|mainstream|mainstream_complex)$",
       names(trials), value = TRUE)
)
participants <- trials[match(assignments$code, trials$code), participant_fields]
participants$cluster <- factor(assignments$cluster, levels = preference_groups)
participants$source <- factor(participants$source, levels = source_levels)
participants$preference_assignment <- assignments$preference_assignment
stopifnot(!anyNA(participants$cluster), !anyNA(participants$source))

# Older prepared exports omit these fields. Offer them when present.
survey_properties <- c(
  "Country of residence, ages 0–7" = "Country of Residence Age 0-7",
  "Country of residence, age 7+" = "Country of Residence Age 7+",
  "English proficiency" = "English Proficiency (1-6)",
  "Age started music" = "Age Started Music"
)
available_survey <- survey_properties[survey_properties %in% names(assignments)]
participants[unname(available_survey)] <- assignments[unname(available_survey)]

# Count and summarize only within participants. Unscored choices stay in the
# timing and word summaries but do not enter response proportion denominators.
summarize_trials <- function(rows) {
  counts <- as.integer(table(factor(rows$category, levels = response_categories)))
  names(counts) <- tolower(response_categories)
  scored <- sum(counts)
  score_share <- function(mode) {
    eligible <- rows$Mode %in% mode & rows$category %in% response_categories
    mean(rows$category[eligible] == "Suggested")
  }
  c(setNames(counts, paste0(names(counts), "_n")),
    setNames(counts / scored, paste0(names(counts), "_prop")),
    scored_trials = scored,
    median_response_seconds = median(rows$Response.Time..ms., na.rm = TRUE) / 1000,
    fast_response_share = mean(rows$Response.Time..ms. < 2600, na.rm = TRUE),
    adequate_share = mean(rows$Feels.Adequate == "Yes", na.rm = TRUE),
    distinct_words = length(unique(rows$Selected.Word[!is.na(rows$Selected.Word)])),
    suggested_or_close_share = sum(counts[c("suggested", "close")]) / scored,
    major_suggested_share = score_share("Major"),
    minor_suggested_share = score_share("Minor"))
}
trial_rows <- split(seq_len(nrow(trials)), trials$code)
summaries <- lapply(trial_rows[participants$code], function(i) summarize_trials(trials[i, ]))
participants <- cbind(participants, as.data.frame(do.call(rbind, summaries)))

genre_suffixes <- c("jazz", "class", "mainstream", "mainstream_complex")
proportion_properties <- c("fast_response_share", "adequate_share",
                           "suggested_or_close_share", "major_suggested_share",
                           "minor_suggested_share")
property_groups <- list(
  Demographics = c(Age = "age", Gender = "gender",
                   available_survey[available_survey != "Age Started Music"]),
  "Musical background" = c(
    "Music involvement (OMSI)" = "omsi",
    "Musician status (binary OMSI)" = "omsiB",
    available_survey[available_survey == "Age Started Music"],
    "Years of musical training" = "training_years",
    "Styles played" = "styles_count",
    "Genres listened to" = "genres_count",
    "Dominant playing style" = "style_max",
    "Dominant listening genre" = "genre_max",
    "Preference assignment method" = "preference_assignment"
  ),
  "Playing scores" = setNames(paste0("styles_", genre_suffixes), paste(preference_groups, "playing score")),
  "Listening scores" = setNames(paste0("genres_", genre_suffixes), paste(preference_groups, "listening score")),
  "Response categories" = setNames(tolower(response_categories), paste(response_categories, "responses")),
  "Trial summaries" = c(
    "Scored trials" = "scored_trials",
    "Median response time (seconds)" = "median_response_seconds",
    "Responses faster than 2.6 seconds" = "fast_response_share",
    "Responses marked adequate" = "adequate_share",
    "Distinct selected words" = "distinct_words",
    "Suggested or Close responses" = "suggested_or_close_share",
    "Suggested responses in major" = "major_suggested_share",
    "Suggested responses in minor" = "minor_suggested_share"
  )
)
property_labels <- unlist(unname(property_groups))

ui <- fluidPage(
  titlePanel("Participant demographics and responses"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "properties", "Choose 2–4 properties", choices = property_groups,
        selected = tolower(response_categories), multiple = TRUE,
        options = list(maxItems = 4)
      ),
      radioButtons("measure", "Response categories",
                   choices = c(Proportion = "prop", Count = "n"), inline = TRUE),
      helpText("Response proportions use each participant’s trials in the four scored categories."),
      tableOutput("group_sizes")
    ),
    mainPanel(uiOutput("plots"))
  )
)

property_plot <- function(d, property, measure) {
  label <- names(property_labels)[match(property, property_labels)]
  is_response <- property %in% tolower(response_categories)
  column <- if (is_response) paste0(property, "_", measure) else property
  is_percent <- (is_response && measure == "prop") || property %in% proportion_properties
  d$value <- d[[column]]
  numeric_property <- is.numeric(d$value)

  if (!numeric_property) {
    d$value <- as.character(d$value)
    d$value[is.na(d$value) | !nzchar(d$value)] <- "Missing / tied"
    if (column %in% c("Country of Residence Age 0-7", "Country of Residence Age 7+")) {
      common <- head(names(sort(table(d$value), decreasing = TRUE)), 6)
      d$value[!d$value %in% common] <- "Other"
    }
  }

  plot <- ggplot(d, aes(x = cluster)) +
    facet_wrap(vars(source), drop = FALSE) +
    scale_x_discrete(drop = FALSE) +
    labs(x = "Preference cluster", title = label) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1),
          legend.position = "bottom")

  if (numeric_property) {
    axis_label <- if (is_response) {
      if (is_percent) "Share of scored trials" else "Scored trials"
    } else label
    plot <- plot +
      geom_boxplot(aes(y = value, fill = source), width = 0.6,
                   outlier.shape = NA, alpha = 0.45, na.rm = TRUE) +
      geom_point(aes(y = value, colour = source),
                 position = position_jitter(width = 0.13, height = 0, seed = 1),
                 alpha = 0.35, size = 1.1, na.rm = TRUE) +
      scale_fill_manual(values = c(Organic = "#37837D", Prolific = "#596BC2")) +
      scale_colour_manual(values = c(Organic = "#205C58", Prolific = "#3C4791")) +
      labs(y = axis_label) +
      guides(fill = "none", colour = "none")
    if (is_percent) {
      plot <- plot + scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                                         limits = c(0, 1))
    }
    return(plot)
  }

  plot +
    geom_bar(aes(fill = value), position = "fill") +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    labs(y = "Share of participants", fill = label)
}

server <- function(input, output, session) {
  output$group_sizes <- renderTable({
    count(participants, Cluster = cluster, Source = source, name = "Participants", .drop = FALSE)
  })

  output$plots <- renderUI({
    validate(need(length(input$properties) %in% 2:4, "Choose 2–4 properties."))
    tagList(lapply(seq_along(input$properties), function(i) {
      plotOutput(paste0("property_", i), height = "350px")
    }))
  })

  for (i in seq_len(4)) {
    local({
      index <- i
      output[[paste0("property_", index)]] <- renderPlot({
        req(length(input$properties) >= index, input$measure)
        property_plot(participants, input$properties[index], input$measure)
      })
    })
  }
}

shinyApp(ui, server)
