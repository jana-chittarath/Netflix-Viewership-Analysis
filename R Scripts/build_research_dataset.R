# ==============================================================
# Build the final research dataset: one row per matched Netflix catalog
# title, combining:
#   1. The historical catalog (2021 snapshot — its own titles run up to
#      2021 and Kaggle lists it as last updated Aug 2, 2021, so it is
#      NOT actually a 2020 dataset despite earlier notes calling it
#      that) — rating quality, release year, age rating, and historical
#      platform differentiation (single-platform vs. shared, as of 2021).
#   2. IMDb's 2021 rating-count snapshot — "established popularity" /
#      audience awareness, matched via each show's permanent tconst
#      (chosen deliberately: it predates the 2023 viewing period, so
#      it can't leak information from the outcome we're predicting).
#   3. Netflix's full-year 2023 engagement data — hours viewed and
#      global availability (the outcome variables).
#
# Final unit of analysis: one row per catalog title present in all
# three sources. This intentionally includes every IMDb type the
# matching found (tvSeries, tvMiniSeries, movie, tvMovie, short,
# tvSpecial, tvShort, video, videoGame) — nothing is filtered out by
# type, since tv_shows.csv turns out to not be pure TV (see
# merge_current_imdb.R's header). Use the imdb_title_type column to
# filter down to just TV yourself if a given analysis needs that.
# ==============================================================

library(data.table)

# Always anchor to the project root — the project is split into "Original
# data/" and "R Scripts/" subfolders, so a script-location-relative setwd
# would break depending on how this is run.
setwd("~/Documents/MSBX 5415/Group Project")

norm_title <- function(x) tolower(trimws(x))

# --- 1. Historical catalog, restricted to Netflix titles -------------------
# (Netflix's own engagement report can only ever contain Netflix content,
# so we restrict here rather than relying on title text alone to filter.)
catalog <- fread("tv_shows_with_current_imdb.csv")
catalog <- catalog[Netflix == 1]
catalog[, n_platforms := Netflix + Hulu + `Prime Video` + `Disney+`]
catalog[, exclusive   := n_platforms == 1]
catalog[, title_norm  := norm_title(Title)]

# tv_shows.csv isn't purely TV despite its name (merge_current_imdb.R now
# tags every matched row with IMDb's own type classification, and some
# catalog rows turn out to be movies/shorts/etc.). We deliberately do NOT
# filter those out here — every category IMDb could match (tvSeries,
# tvMiniSeries, movie, tvMovie, short, tvSpecial, tvShort, video,
# videoGame) stays in the data, labeled via imdb_title_type, so you can
# filter to just TV yourself if a given analysis needs that, without
# losing the other rows for good.
cat("IMDb type breakdown in the Netflix catalog subset:\n")
print(table(catalog$imdb_title_type, useNA = "ifany"))

# Duplicate-title check: if two catalog rows normalize to the same title, a
# text-based join could silently attach the wrong row's data. None expected
# among Netflix titles, but we check rather than assume.
dupe_titles <- catalog[, .N, by = title_norm][N > 1, title_norm]
if (length(dupe_titles) > 0) {
  cat("WARNING:", length(dupe_titles), "duplicate normalized titles in the Netflix catalog subset:\n")
  print(catalog[title_norm %in% dupe_titles, .(Title, Year)])
}
cat("Step 1 - Netflix catalog rows (2021 snapshot):", nrow(catalog), "\n")

# --- 2. IMDb 2021 established-popularity snapshot, joined via tconst -------
ratings_2021 <- fread("Original data/imdb_data/IMDb_title_ratings.tsv")
setnames(ratings_2021, c("averageRating", "numVotes"), c("IMDb_rating_2021", "IMDb_ratecount_2021"))

catalog <- merge(catalog, ratings_2021, by = "tconst", all.x = TRUE)
cat("Step 2 - rows with a 2021 IMDb popularity value:",
    sum(!is.na(catalog$IMDb_ratecount_2021)), "of", nrow(catalog), "\n")

# --- 3. Netflix full-year 2023 engagement data, matched via title ----------
# netflix_2023_fullyear.csv can now have MORE THAN ONE row per normalized
# title (build_netflix_2023_fullyear.R deliberately splits e.g. a TV show
# from an unrelated same-named movie, or two unrelated franchises in
# different languages that happen to share an English title — see that
# script's header for the "Death Note" / "Kingdom" examples that motivated
# this). So this can't be a plain one-line merge anymore: for each catalog
# show we may have several candidate Netflix groups to choose from, and we
# have to pick the right one deliberately rather than let a merge silently
# duplicate rows or mix in the wrong candidate.
netflix_2023 <- fread("netflix_2023_fullyear.csv")
netflix_2023[, title_norm := norm_title(base_title)]
netflix_2023[, release_year := suppressWarnings(as.integer(substr(Release_Date_Earliest, 1, 4)))]

# Picks the right Netflix group for one catalog show, or NULL if it can't
# be done without guessing.
match_netflix_group <- function(title_norm_val, year_val) {
  # Prefer groups that had an actual season/series label — that's the
  # strongest signal something is a TV show entry rather than a movie or
  # special sharing the same name. Only fall back to unlabeled groups if
  # no labeled candidate exists at all (some genuine single-season shows
  # are listed without an explicit "Season 1").
  cand <- netflix_2023[title_norm == title_norm_val & had_season_label == TRUE]
  tier <- "labeled"
  if (nrow(cand) == 0) {
    cand <- netflix_2023[title_norm == title_norm_val & had_season_label == FALSE]
    tier <- "unlabeled"
  }
  if (nrow(cand) == 0) return(NULL)
  if (nrow(cand) == 1) return(list(row = cand, tier = tier))

  # More than one candidate at this preference level (e.g. two unrelated
  # franchises with the same English title, like "Kingdom") — try to break
  # the tie using how close each candidate's earliest release date is to
  # our catalog's release year. If that's still tied, or no candidate has
  # usable date info, don't guess.
  cand[, year_diff := abs(release_year - year_val)]
  cand_dated <- cand[!is.na(year_diff)]
  if (nrow(cand_dated) == 0) return(NULL)
  best <- cand_dated[year_diff == min(year_diff)]
  if (nrow(best) == 1) return(list(row = best, tier = paste0(tier, ", year-disambiguated")))
  NULL
}

match_results <- vector("list", nrow(catalog))
for (i in seq_len(nrow(catalog))) {
  m <- match_netflix_group(catalog$title_norm[i], catalog$Year[i])
  if (is.null(m)) {
    match_results[[i]] <- data.table(title_norm = catalog$title_norm[i], Netflix_Match_Tier = NA_character_)
  } else {
    match_results[[i]] <- cbind(m$row, data.table(Netflix_Match_Tier = m$tier))
  }
}
netflix_matched <- rbindlist(match_results, fill = TRUE)
netflix_matched[, catalog_row := seq_len(.N)]
catalog[, catalog_row := seq_len(.N)]

research_data <- merge(catalog, netflix_matched, by = "catalog_row", suffixes = c("", ".nflx"))
research_data <- research_data[!is.na(Hours_Viewed_2023)]
cat("Step 3 - rows after matching to Netflix 2023 engagement data:", nrow(research_data), "\n")
cat("Match tier breakdown:\n")
print(table(research_data$Netflix_Match_Tier, useNA = "no"))

# --- Final unit of analysis: require established popularity too ------------
# (needed for Q3/Q4's regressions; Q1's descriptive stats could technically
# use the broader Step-3 set, but we keep one consistent final table so
# every question draws from the same population.)
research_data <- research_data[!is.na(IMDb_ratecount_2021)]
cat("Step 4 - final rows with catalog + 2021 popularity + 2023 engagement:", nrow(research_data), "\n\n")

# --- Tidy up: keep and rename only what the research questions need --------
final_cols <- research_data[, .(
  title                      = Title,
  year                       = Year,
  age                        = Age,
  imdb_rating_catalog_2021   = IMDb_catalog_2021,
  rotten_tomatoes            = Rotten_Tomatoes,
  hulu                       = Hulu,
  prime_video                = `Prime Video`,
  disney_plus                = `Disney+`,
  n_platforms,
  exclusive,
  imdb_ratecount_2021        = IMDb_ratecount_2021,
  hours_viewed_2023          = Hours_Viewed_2023,
  views_2023                 = Views_2023,
  available_globally_ever    = Available_Globally_Ever,
  available_globally_always  = Available_Globally_Always,
  release_date_earliest      = Release_Date_Earliest,
  n_distinct_seasons,
  tconst,
  imdb_title_type
)]

# Deliberately NOT adding a manual row for the 2023 live-action "One Piece"
# (decided 2026-08-10). It has no row in the 2021 catalog (it didn't exist
# yet), so unlike every other row here it would carry no 2021 rating,
# Rotten Tomatoes, popularity, or platform data — inconsistent with the
# rest of the dataset and with Step 4's requirement that every row have a
# 2021 popularity value. Its hours still exist, correctly separated from
# the real "One Piece" anime, in netflix_2023_fullyear.csv if ever needed.

# Enforce true one-row-per-series uniqueness before saving.
stopifnot(!any(duplicated(final_cols$title)))

# scipen = 100 forces plain numbers (e.g. 71600000) instead of scientific
# notation (7.16e+07) for large "round" values like hours_viewed/views —
# fwrite's default picks whichever is shorter to write, which for round
# numbers is often scientific notation.
fwrite(final_cols, "netflix_research_dataset.csv", scipen = 100)
cat("Saved netflix_research_dataset.csv --", nrow(final_cols), "rows, one per matched TV series.\n\n")

cat("Preview:\n")
print(head(final_cols, 10))
