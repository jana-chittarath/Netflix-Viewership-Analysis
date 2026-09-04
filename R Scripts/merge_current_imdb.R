# ==============================================================
# Merge CURRENT IMDb rating + vote count into the tv_shows dataset
# Purpose: compare each show's 2021 catalog-snapshot rating (in
# tv_shows.csv) against its rating and vote count TODAY, using IMDb's
# official bulk data (imdb_data/title.basics.tsv, imdb_data/title.ratings.tsv).
#
# Note on dating: tv_shows.csv was originally assumed to be a "2020"
# snapshot, but its own content (titles released as late as 2021) and
# its Kaggle "last updated" date (Aug 2, 2021) both show it's actually
# from 2021. We call this variable IMDb_catalog_2021 to distinguish it
# from IMDb_rating_2021/IMDb_ratecount_2021 elsewhere in the project,
# which come from a *different* 2021 source (a separately-downloaded
# IMDb ratings snapshot, ~June 2021) — close in time, but not the same
# file, so they get different names.
# ==============================================================

library(data.table)

# --- Set the working directory ----------------------------------------------
# Always anchor to the project root (not "wherever this script file is"),
# since the project is now split into "Original data/" and "R Scripts/"
# subfolders — a script-location-relative setwd would break depending on
# whether this is run from RStudio or the command line.
setwd("~/Documents/MSBX 5415/Group Project")

# --- Read in the 2021 catalog snapshot ---------------------------------------
tv_shows <- fread("Original data/tv_shows.csv", encoding = "UTF-8")
setnames(tv_shows, "Rotten Tomatoes", "Rotten_Tomatoes")

# Pull the "before" rating out of "9.4/10" text, same trick as elsewhere.
tv_shows[, IMDb_catalog_2021 := as.numeric(sub("/10", "", IMDb))]

# --- Read IMDb's current bulk data -------------------------------------------
# title.basics.tsv: one row per title (movie, show, episode, ...) with its
# name, type, and start year.
# na.strings = "\\N" because that is the literal text IMDb uses for "missing"
# in these files, instead of a blank cell.
#
# We exclude tvEpisode entirely (an episode of some other show sharing our
# title by coincidence is never the "real" match for a whole catalog entry —
# e.g. IMDb has 24 different tvEpisode rows literally named "Breaking Bad",
# episodes of unrelated shows, alongside the one real tvSeries). Everything
# else (movies, shorts, TV movies, etc.) is kept, because tv_shows.csv turns
# out to not be pure TV despite its name — some catalog rows are actually
# movies that were miscategorized by the original Kaggle dataset's author.
basics <- fread(
  "Original data/imdb_data/title.basics.tsv",
  na.strings = "\\N",
  quote = "",
  select = c("tconst", "titleType", "primaryTitle", "originalTitle", "startYear")
)
basics <- basics[titleType != "tvEpisode"]
basics[, startYear := as.integer(startYear)]
basics[, is_tv_type := titleType %in% c("tvSeries", "tvMiniSeries")]

# title.ratings.tsv: one row per title with its CURRENT average rating and
# CURRENT number of votes — this is "as of today," unlike the fixed 2021
# catalog text baked into tv_shows.csv.
ratings <- fread(
  "Original data/imdb_data/title.ratings.tsv",
  na.strings = "\\N"
)

# Join the two IMDb files together on tconst (IMDb's internal ID) so each
# row has: title text, start year, current rating, current vote count.
imdb_current <- merge(basics, ratings, by = "tconst")

# --- Tiered matching, run in two passes --------------------------------------
# Pass 1 searches only real TV entries (tvSeries/tvMiniSeries) — our
# preferred, most trustworthy match. Pass 2 only runs for rows Pass 1
# couldn't match, and searches everything else (movies, shorts, TV movies,
# etc.) — this recovers catalog rows that turn out to not actually be TV
# shows (see the note above basics), so we get a rating/vote count for them
# too instead of silently dropping them, while still recording what type
# they actually are.
#
# Within each pass:
# Tier 1: exact match on primary title + exact year.
# Tier 2: exact match on original-language title + exact year (catches shows
#         whose primary listed title differs from the Kaggle dataset's).
# Tier 3: exact title match with year off by at most 1 (release-year typos /
#         regional year differences).
# Tier 4: strip a known franchise prefix the Kaggle catalog uses but IMDb
#         doesn't (e.g. "Marvel's Daredevil" -> "Daredevil" on IMDb).
# (A tier 5 that stripped a subtitle after a colon, e.g. "Tiger King:
# Murder, Mayhem and Madness" -> "Tiger King," was removed 2026-08-09
# after producing confirmed bad matches — see the note above find_match.)
# Anything still unmatched, or matching more than one IMDb title, is left as
# NA rather than guessed — an ambiguous match is worse than no match, in
# both passes.

norm_title <- function(x) tolower(trimws(x))

# Franchise labels the Kaggle catalog sometimes prepends that IMDb's own
# title text does not include.
strip_known_prefix <- function(x) {
  trimws(sub("^(marvel's|dc's|disney's|netflix presents:)\\s*", "", x, ignore.case = TRUE))
}

tv_shows[, title_norm := norm_title(Title)]
tv_shows[, title_noprefix := strip_known_prefix(title_norm)]
imdb_current[, primary_norm := norm_title(primaryTitle)]
imdb_current[, original_norm := norm_title(originalTitle)]

# Runs the tiered search against whichever candidate pool is passed in.
# Returns the matching IMDb row (as a 1-row data.table) plus a label
# describing which tier found it, or NULL if no tier resolved to exactly
# one candidate.
#
# The "subtitle stripped" tier was removed entirely on 2026-08-09 after
# finding real, confirmed bad matches from it — not just in the broad
# all-types fallback pool, but in the curated TV-only pool too:
#   - "True: Wonderful Wishes" and "True: Magical Friends" both stripped
#     to "True" and both wrongly matched an unrelated 2018 short film
#     literally titled "True."
#   - "Clip: Lego Jurassic World Video Game Walkthrough" and "Clip:
#     Adventures of Buttman..." both stripped to "Clip" and both wrongly
#     matched an unrelated 2017 music series literally titled "Clip."
#   - "Dave Chappelle: Equanimity & The Bird Revelation" stripped to
#     "Dave Chappelle" and wrongly matched a different, generic "Dave
#     Chappelle" tvSeries entry — the actually-correct IMDb entry for
#     this special is a separate tvSpecial titled "Dave Chappelle:
#     Equanimity" (tt7806998), which simple prefix/subtitle stripping
#     has no way to find.
# Truncating a title at the first colon frequently lands on a short,
# generic word (a name, "Clip," "True") that collides with something
# totally unrelated. Tiers 1-3 (exact/near-exact full-title matches)
# don't have this problem. The "prefix stripped" tier (tier 4) is kept,
# since it only removes a specific known franchise label rather than
# truncating at an arbitrary point, and produced no confirmed bad
# matches — use_fuzzy_tiers still gates tier 4, kept off for the broad
# fallback pool for the same "don't guess in a noisy pool" reasoning.
find_match <- function(show, pool, use_fuzzy_tiers = TRUE) {
  cand <- pool[primary_norm == show$title_norm & startYear == show$Year]
  tier <- "Exact (title + year)"

  if (nrow(cand) != 1) {
    cand2 <- pool[original_norm == show$title_norm & startYear == show$Year]
    if (nrow(cand2) == 1) { cand <- cand2; tier <- "Exact (title + original-language year)" }
  }
  if (nrow(cand) != 1) {
    cand3 <- pool[primary_norm == show$title_norm & abs(startYear - show$Year) <= 1]
    if (nrow(cand3) == 1) { cand <- cand3; tier <- "Fuzzy (title exact, year +-1)" }
  }
  if (use_fuzzy_tiers && nrow(cand) != 1 && show$title_noprefix != show$title_norm) {
    cand4 <- pool[primary_norm == show$title_noprefix & startYear == show$Year]
    if (nrow(cand4) == 1) { cand <- cand4; tier <- "Fuzzy (prefix stripped, exact year)" }
  }

  if (nrow(cand) == 1) list(row = cand, tier = tier) else NULL
}

pool_tv  <- imdb_current[is_tv_type == TRUE]
pool_all <- imdb_current  # already excludes tvEpisode, everything else allowed

result_list <- vector("list", nrow(tv_shows))

for (i in seq_len(nrow(tv_shows))) {
  show <- tv_shows[i]

  result <- find_match(show, pool_tv)
  broader_type <- FALSE

  if (is.null(result)) {
    result <- find_match(show, pool_all, use_fuzzy_tiers = FALSE)
    broader_type <- !is.null(result)
  }

  if (is.null(result)) {
    result_list[[i]] <- data.table(tconst = NA_character_, averageRating = NA_real_,
                                    numVotes = NA_integer_, imdb_title_type = NA_character_,
                                    Match_Confidence_Current = "Unmatched")
  } else {
    tier_label <- if (broader_type) paste0(result$tier, " [non-TV type]") else result$tier
    result_list[[i]] <- data.table(tconst = result$row$tconst, averageRating = result$row$averageRating,
                                    numVotes = result$row$numVotes, imdb_title_type = result$row$titleType,
                                    Match_Confidence_Current = tier_label)
  }
}

matched <- rbindlist(result_list)
tv_shows <- cbind(tv_shows, matched)
setnames(tv_shows, c("averageRating", "numVotes"), c("IMDb_current", "IMDb_ratecount_current"))
# tconst is IMDb's permanent internal ID for a title — we keep it here so a
# later script can look this exact same show up in a differently-dated
# ratings file (e.g. a 2021 snapshot) without having to re-match by name.
# imdb_title_type records what IMDb actually classifies this title as
# (tvSeries, tvMiniSeries, movie, tvMovie, short, ...) — use this to filter
# to real TV content in any downstream analysis that needs it, rather than
# assuming every row in this "TV shows" catalog is actually a TV show.

# --- The comparison we actually want ----------------------------------------
# How much did each show's rating move between the 2021 catalog snapshot and today?
tv_shows[, rating_change := IMDb_current - IMDb_catalog_2021]

# --- Save the result ----------------------------------------------------------
tv_shows[, c("title_norm", "title_noprefix") := NULL]
# scipen = 100 avoids scientific notation for large vote-count numbers.
fwrite(tv_shows, "tv_shows_with_current_imdb.csv", scipen = 100)

# --- Quick summary so we can see the match quality and the headline result --
cat("Match confidence breakdown:\n")
print(table(tv_shows$Match_Confidence_Current, useNA = "always"))

cat("\nIMDb title type breakdown (matched rows only):\n")
print(sort(table(tv_shows$imdb_title_type, useNA = "no"), decreasing = TRUE))

cat("\nRows with both an old and current rating (usable for comparison):",
    sum(!is.na(tv_shows$IMDb_catalog_2021) & !is.na(tv_shows$IMDb_current)), "\n")

cat("\nMean rating change, all matched shows:",
    round(mean(tv_shows$rating_change, na.rm = TRUE), 4), "\n")
