# Score each distinct listening choice and playing style once.
preference_groups <- c("Jazz", "Classical", "Mainstream", "Mainstream-Complex")

playing_terms <- list(
  jazz = c('jazz', 'fusion', 'progressive r&b', 'broadway', 'neo-soul', 'gospel', 'bossa', 'samba', 'kompa', 'musical', 'theatre', 'theater', 'nu-jazz', 'improvised', 'impro', 'brazilian', 'showtunes', 'jass', 'post bop', 'be-bop', 'hard bop', 'afro-caribbian', 'free improv' ),
  classical = c('classica', 'orchestra', 'film', 'film score', 'chamber', 'choral', 'gregorian chant', 'consort', 'musical', 'theatre', 'theater', 'video game', 'anime', 'game soundtrack', 'movie soundtrack', 'ost', 'renaissance', 'baroque', 'concert band','roamntic', 'romantic', 'neoclassical', 'opera' ),
  mainstream = c('pop', 'jamband', 'dance', 'bam', 'easy listening', 'soft rock', 'pop-rock', 'rock and roll', 'afrobeat', 'hardcore', 'children', 'grunge', 'ballad', 'brazilian popular', 'americana', 'rockabilly', 'singer songwriter', 'latin american', 'songwriter', 'lyric', 'latin', 'modern rock', 'beat', 'synthwave', 'contemporary', 'contemporary christian', 'ccm', 'religious', 'chillout', 'blues', 'punk', 'punk rock', 'hard rock', 'groove', 'rock', 'rap', 'electronic', 'house', 'ska', 'techno', 'jungle', 'idm', 'drum and bass', 'drum n bass', 'dnb', 'ambient', 'juke', 'rolling stones', 'j-pop', 'kpop', 'k-pop', 'folk', 'folk rock', 'edm', 'christian', 'jrock', 'uruguay', 'reggae', 'country', 'gnawa', 'argentinian folklore' ),
  mainstream_complex = c('funk', 'soul', 'psychedelic', 'choro', 'french song', 'jazzy pop', 'ethnic', 'alternative', 'alt rock', 'indie', 'progressive rock', 'leftfield', 'metal', 'church music', 'experimental', 'hip hop', 'hiphop', 'hip-hop', 'r&b', 'rnb', 'rhythm and blues', 'klezmer', 'balkan', 'eastern europe', 'sophistipop', 'sophisti-pop', 'heavy metal', 'math-rock', 'noise', 'arabic', 'world', 'ethiopian', 'african', 'turkish', 'hindustani', 'no wave', 'new wave', 'asian', 'middle eastern', 'salsa', 'bluegrass','brazilia', 'jazz-pop', 'modern orchestra', 'tango', 'celestial dream pop', 'sunshine pop', 'game music', 'math rock', 'prog', 'cinematic')
)
names(playing_terms) <- preference_groups

listening_terms <- list(
  jazz = c('jazz', 'fusion', 'progressive r&b', 'broadway', 'neo-soul', 'gospel', 'bossa', 'musical', 'theatre', 'theater', 'pat metheny', 'jacob collier', 'edith piaf', 'hugo fattoruso', 'eldar djangirov', 'jesús molina', 'clare fischer', 'nu-jazz' ),
  classical = c('classica', 'orchestra', 'film', 'chamber', 'musical', 'theatre', 'theater', 'hania rani', 'cziner', 'video game', 'game soundtrack', 'movie soundtrack', 'ost', 'louis cole', 'charles valentin alkan', 'renaissance', 'baroque','pipe organ', 'sacred', 'continuo', 'modern orchestra', 'cinematic'),
  mainstream = c('pop', 'punk', 'rock', 'rap', 'electronic', 'house', 'techno', 'jungle', 'idm', 'drum and bass', 'dnb', 'em beiheld', 'ambient', 'grateful dead', 'above and beyond', 'footwork', 'juke', 'rolling stones', 'the chicks', 'todd snider', 'john prine', 'j-pop', 'such ades', 'kpop', 'k-pop', 'folk', 'budots', 'edm', 'christian', 'jess ray', 'taylor leonhardt', 'christa wells', 'jrock', 'yaosobi', 'zutomayo', 'the back horn', 'arc en ciel', 'uruguay', 'reggae', 'country', 'abba', 'bonney m', 'one direction', 'heavy metal', 'gabriel', 'mélissa polinar', 'zayn', '5sos', 'mf doom', 'georges brassens', 'huun-huur-tu', 'rob zombie', 'kacey musgraves', 'slipknot', 'lamb of god', 'aphex twin', 'ethel cain', 'candombe', 'gnawa', 'argentinian folklore', 'lady gaga', 'paramore','high dopamine', 'soundscapes', 'romanian folk', 'jangle pop', 'popular music', '80s electropop', 'polka', 'kanye', 'gospel (amateur)', 'bollywood'),
  mainstream_complex = c('funk', 'alternative', 'indie', 'progressive rock', 'metal', 'experimental', 'hip hop', 'hiphop', 'hip-hop', 'r&b', 'klezmer', 'balkan', 'alchemist', 'beatles', 'eastern europe', 'sophistipop', 'sophisti-pop', 'sondheim', 'mermen', 'action bronson', 'regina spektor', 'math-rock', 'noise', 'arabic', 'world', 'socolofsky', 'ethiopian', 'african', 'turkish', 'hindustani', 'no wave', 'new wave', 'asian', 'middle eastern', 'salsa', 'havana d', 'los que son', 'luna sea', 'cold play', 'jeremy passion', 'paul kim', 'bluegrass', 'louis cole', 'jacob collier', 'chris thile', 'kendrick lamar', 'edith piaf', 'spazza', 'eduardo mateo', 'urbano moraes', 'jorginho gularte', 'evanescence', 'jazz-pop', 'modern orchestra', 'tango', 'celestial dream pop', 'sunshine pop', 'game music', 'math rock', 'prog', 'cinematic')
)
names(listening_terms) <- preference_groups

# Each survey checkbox is one entry, even when its label names several styles.
listening_choices <- c(
  "traditional classical (1600s-1900s)" = "Classical",
  "modern & contemporary classical (1900s-present)" = "Classical",
  "orchestral film & broadway scores" = "Classical",
  "jazz (classic, fusion & modern)" = "Jazz",
  "bossa nova & latin jazz" = "Jazz",
  "jazz-influenced pop" = "Jazz",
  "funk & progressive r&b" = "Mainstream-Complex",
  "neo-soul" = "Mainstream-Complex",
  "r&b & gospel" = "Mainstream-Complex",
  "art pop" = "Mainstream-Complex",
  "chamber pop & experimentalism" = "Mainstream-Complex",
  "alternative & indie rock" = "Mainstream-Complex",
  "progressive rock & progressive metal" = "Mainstream-Complex"
)

preference_entries <- function(text) {
  text <- stringr::str_to_lower(dplyr::coalesce(as.character(text), ""))
  # Commas within the survey's Jazz checkbox and free-text parentheses are literal.
  entries <- stringr::str_split(text, ",(?![^()]*\\))")[[1]]
  entries <- stringr::str_squish(stringr::str_remove(entries, "^other:\\s*"))
  unique(setdiff(entries, c("", "other", "none", "none selected", "not provided")))
}

score_preferences <- function(entries, dictionary, choices = character()) {
  scores <- setNames(numeric(length(dictionary)), names(dictionary))
  terms <- unlist(dictionary, use.names = FALSE)
  groups <- rep(names(dictionary), lengths(dictionary))
  terms[terms == "classica"] <- "classical"
  terms[terms == "brazilia"] <- "brazilian"
  # Dictionary terms are literal phrases, with word boundaries.
  patterns <- paste0("(?<![[:alnum:]])\\Q", terms, "\\E(?![[:alnum:]])")
  for (entry in entries) {
    matched_groups <- unname(choices[entry])
    if (is.na(matched_groups)) {
      hits <- which(stringr::str_detect(entry, patterns))
      if (!length(hits)) next
      exact <- hits[terms[hits] == entry]
      if (length(exact)) hits <- exact
      hits <- hits[nchar(terms[hits]) == max(nchar(terms[hits]))]
      matched_groups <- unique(groups[hits])
    }
    scores[matched_groups] <- scores[matched_groups] + 1 / length(matched_groups)
  }
  scores
}

dominant_preference <- function(scores) {
  leaders <- which(abs(scores - max(scores)) < 1e-10)
  if (sum(scores) == 0 || length(leaders) != 1L) return(NA_character_)
  names(scores)[leaders]
}

classify_preferences <- function(demographics) {
  played <- lapply(demographics$`Music Styles Played`, preference_entries)
  listened <- lapply(demographics$`Music Genres`, preference_entries)
  playing <- t(vapply(played, score_preferences, numeric(4), dictionary = playing_terms))
  listening <- t(vapply(listened, score_preferences, numeric(4),
                        dictionary = listening_terms, choices = listening_choices))
  colnames(playing) <- colnames(listening) <- preference_groups

  # The factor order in read_data.R encodes the original OMSI scale from 1 to 6.
  involvement <- as.integer(demographics$`Music Involvement`)
  playing_weight <- involvement / 7
  playing_weight[is.na(playing_weight)] <- 0.5
  # Normalize within source so the number of entries does not set its weight.
  played_share <- playing / pmax(1, rowSums(playing))
  listened_share <- listening / pmax(1, rowSums(listening))
  combined <- played_share * playing_weight + listened_share * (1 - playing_weight)
  # A single source still counts when the other source is missing or unclassified.
  combined[rowSums(listening) == 0, ] <- played_share[rowSums(listening) == 0, ]
  combined[rowSums(playing) == 0, ] <- listened_share[rowSums(playing) == 0, ]

  group <- apply(combined, 1, dominant_preference)
  # A tie is resolved only among the leading categories and remains auditable.
  reason <- rep("weighted preferences", length(group))
  for (i in which(is.na(group))) {
    if (sum(combined[i, ]) == 0) {
      group[i] <- "Mainstream"
      reason[i] <- "fallback: no classified preference"
      next
    }
    tied <- abs(combined[i, ] - max(combined[i, ])) < 1e-10
    primary <- if (playing_weight[i] > 0.5) playing else listening
    strongest <- max(primary[i, tied])
    tied <- tied & abs(primary[i, ] - strongest) < 1e-10
    reason[i] <- "tie: stronger preference in primary source"
    if (sum(tied) == 1L) group[i] <- preference_groups[tied]
    if (is.na(group[i])) {
      # Use the most frequent clear assignment among tied labels, within OMSI first.
      clear <- reason == "weighted preferences" & !is.na(group)
      same_omsi <- !is.na(involvement) & involvement %in% involvement[i]
      labels <- preference_groups[tied]
      counts <- function(use) as.integer(table(factor(group[use], levels = labels)))
      group[i] <- labels[order(-counts(clear & same_omsi), -counts(clear), labels)[1]]
      reason[i] <- "tie: prevalence within OMSI and overall"
    }
  }

  suffixes <- c("jazz", "class", "mainstream", "mainstream_complex")
  colnames(playing) <- paste0("styles_", suffixes)
  colnames(listening) <- paste0("genres_", suffixes)
  tibble::as_tibble(cbind(playing, listening)) |>
    dplyr::mutate(
      styles_count = lengths(played),
      genres_count = lengths(listened),
      style_max = factor(apply(playing, 1, dominant_preference), levels = preference_groups),
      genre_max = factor(apply(listening, 1, dominant_preference), levels = preference_groups),
      cluster = factor(group, levels = preference_groups),
      preference_assignment = reason
    )
}
