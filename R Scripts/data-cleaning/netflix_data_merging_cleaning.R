# ==============================================================
# NETFLIX TV SHOW ANALYSIS — FULL DATA PIPELINE
# One script, start to finish: raw files in "Original data/" -> the
# final analysis-ready dataset, netflix_analysis_ready.csv.
#
# This consolidates four separate scripts (merge_current_imdb.R,
# build_netflix_2023_fullyear.R, build_research_dataset.R,
# prepare_analysis_dataset.R) into one linear file, in the order they
# actually run. Running this script alone reproduces every output file
# those four produce.
#
# THE FOUR STAGES:
#   PART 1 — Match the 2021 historical catalog to IMDb's current bulk
#            data by title + year, so every show gets IMDb's permanent
#            ID (tconst). -> tv_shows_with_current_imdb.csv
#   PART 2 — Combine Netflix's two 2023 "What We Watched" engagement
#            reports into one row per show. -> netflix_2023_fullyear.csv
#   PART 3 — Three-way merge: catalog + a 2021 IMDb popularity snapshot
#            (joined via tconst) + Netflix 2023 viewing data (joined
#            via title). -> netflix_research_dataset.csv
#   PART 4 — Final cleaning: filter to real TV series, parse/label
#            missing values, add log-transformed columns for modeling.
#            -> netflix_analysis_ready.csv (the file everything else
#            in the project builds on)
#
# WHY THREE DIFFERENT "2021"s: the historical catalog (tv_shows.csv,
# despite once being mislabeled "2020") is its own ~August 2021
# snapshot. A SEPARATE Kaggle file (IMDb_title_ratings.tsv) is a
# genuinely different ~June 2021 IMDb ratings snapshot, used
# specifically as "established popularity" — deliberately dated BEFORE
# the 2023 Netflix viewing data it's meant to help predict, so it can't
# leak information from the outcome. IMDb's *current* bulk data
# (imdb_data/) is used only to find each show's permanent ID, not as
# an analysis variable itself.
# ==============================================================

library(data.table)
library(readxl)

# Always anchor to the project root — the project is split into
# "Original data/" and "R Scripts/" subfolders.
setwd("~/Documents/MSBX 5415/Group Project")


# ================================================================
# PART 1 — Match the catalog to IMDb, get each show's permanent ID
# ================================================================

# --- Read the 2021 catalog snapshot -----------------------------------------
tv_shows <- fread("Original data/tv_shows.csv", encoding = "UTF-8")
setnames(tv_shows, "Rotten Tomatoes", "Rotten_Tomatoes")
tv_shows[, IMDb_catalog_2021 := as.numeric(sub("/10", "", IMDb))]

# --- Read IMDb's current bulk data -------------------------------------------
# We exclude tvEpisode entirely (an episode of some unrelated show sharing our
# title by coincidence is never the right match for a whole catalog entry).
# Everything else (movies, shorts, TV movies, etc.) is kept, because
# tv_shows.csv turns out to not be pure TV despite its name — some catalog
# rows are actually movies miscategorized by the original dataset's author.
basics <- fread(
  "Original data/imdb_data/title.basics.tsv",
  na.strings = "\\N", quote = "",
  select = c("tconst", "titleType", "primaryTitle", "originalTitle", "startYear")
)
basics <- basics[titleType != "tvEpisode"]
basics[, startYear := as.integer(startYear)]
basics[, is_tv_type := titleType %in% c("tvSeries", "tvMiniSeries")]

ratings <- fread("Original data/imdb_data/title.ratings.tsv", na.strings = "\\N")
imdb_current <- merge(basics, ratings, by = "tconst")

# --- Tiered matching, run in two passes --------------------------------------
# Pass 1 searches only real TV entries (our preferred, most trustworthy
# match). Pass 2 only runs for rows Pass 1 couldn't match, and searches
# everything else, recovering catalog rows that turn out not to be TV shows.
#
# Tiers: (1) exact title + exact year; (2) exact original-language title +
# exact year; (3) exact title + year off by at most 1; (4) strip a known
# franchise prefix IMDb doesn't use (e.g. "Marvel's Daredevil" -> "Daredevil").
# A fifth tier that stripped subtitles after a colon was tried and removed —
# it produced confirmed bad matches (e.g. two unrelated titles both
# truncating to the single word "True" and both matching an unrelated short
# film). Any tier only accepts a match that resolves to exactly ONE
# candidate — ambiguous matches are left blank rather than guessed, in both
# passes. Tier 4 is only allowed in the TV-preferred pass, not the broader
# fallback pass, for the same "don't guess in a noisy pool" reasoning.
norm_title <- function(x) tolower(trimws(x))
strip_known_prefix <- function(x) {
  trimws(sub("^(marvel's|dc's|disney's|netflix presents:)\\s*", "", x, ignore.case = TRUE))
}

tv_shows[, title_norm := norm_title(Title)]
tv_shows[, title_noprefix := strip_known_prefix(title_norm)]
imdb_current[, primary_norm := norm_title(primaryTitle)]
imdb_current[, original_norm := norm_title(originalTitle)]

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
# tconst is IMDb's permanent ID — kept so later steps can look this same show
# up in a differently-dated ratings file without re-matching by name.
tv_shows[, rating_change := IMDb_current - IMDb_catalog_2021]
tv_shows[, c("title_norm", "title_noprefix") := NULL]

fwrite(tv_shows, "tv_shows_with_current_imdb.csv", scipen = 100)
cat("PART 1 done — tv_shows_with_current_imdb.csv:", nrow(tv_shows), "rows,",
    sum(!is.na(tv_shows$tconst)), "matched to an IMDb ID.\n\n")


# ================================================================
# PART 2 — Combine Netflix's two 2023 engagement reports
# ================================================================

# Jan-Jun: one combined sheet, TV + movies mixed, 4 columns (no Runtime/Views).
# Jul-Dec: separate "TV"/"Film" sheets, 6 columns. Every row is kept here —
# nothing is guessed-and-dropped by type.
strip_alt_title <- function(title) sub(" // .*$", "", title)
strip_season_label <- function(title) {
  trimws(sub(":\\s*(Season\\s*[0-9]+|Limited Series|Miniseries|Part\\s*[0-9]+|Series\\s*[0-9]+)\\s*$",
             "", title, ignore.case = TRUE))
}
extract_alt_title <- function(title) {
  has_alt <- grepl(" // ", title, fixed = TRUE)
  ifelse(has_alt, sub("^.* // ", "", title), NA_character_)
}
# Pulls the number out of a "Season N" / "Series N" / "Part N" label, e.g.
# "Trailer Park Boys: Season 5" -> 5. Returns NA for "Limited Series,"
# "Miniseries" (no number), or titles with no season label at all. Written
# as a one-title-at-a-time function (applied via vapply below), since
# regmatches() silently drops non-matching elements instead of returning
# NA for them when given a vector directly — that would misalign the
# result with the original rows.
extract_season_number_one <- function(title) {
  m <- regmatches(title, regexpr("(?:Season|Series|Part)\\s*([0-9]+)\\s*$", title, ignore.case = TRUE, perl = TRUE))
  if (length(m) == 0) return(NA_integer_)
  suppressWarnings(as.integer(gsub("[^0-9]", "", m)))
}
# Known bare-number sequels (a trailing number with NO colon and no
# "Season"/"Series"/"Part" keyword, e.g. "Stranger Things 4") confirmed by
# manual verification to be genuine TV season numbering for that specific
# show, not a coincidentally-same-named movie franchise. A generic "strip
# any trailing number" rule was tested and rejected (2026-08-10) — it
# produced real false positives: "Hotel Transylvania 2," "Taken 2," and
# "Spider-Man 2/3" are movies, not TV seasons, and would have wrongly
# merged into an unrelated TV catalog entry that happens to share a
# similar name. Add a show here only after confirming there's no
# competing movie franchise using the same name + number.
known_bare_number_shows <- c("Stranger Things")

is_known_bare_number_title <- function(title) {
  title %in% known_bare_number_shows ||
    grepl(paste0("^(", paste(known_bare_number_shows, collapse = "|"), ") [0-9]+$"), title)
}
known_bare_number_base <- function(title) {
  for (show in known_bare_number_shows) {
    if (title == show || grepl(paste0("^", show, " [0-9]+$"), title)) return(show)
  }
  title
}
known_bare_number_season <- function(title) {
  for (show in known_bare_number_shows) {
    if (title == show) return(1L)
    if (grepl(paste0("^", show, " [0-9]+$"), title)) {
      return(suppressWarnings(as.integer(sub(paste0("^", show, " "), "", title))))
    }
  }
  NA_integer_
}
# The writing script of a title's alt-title stands in for "is this actually
# the same real-world show" — two unrelated franchises sharing an English
# name will usually have alt-titles in different scripts.
detect_script_one <- function(alt) {
  if (is.na(alt) || alt == "") return("none")
  # Hangul/Hiragana/Katakana use explicit Unicode block ranges, not PCRE's
  # \p{Hangul} etc. script properties -- found 2026-08-10 that \p{Hangul}
  # also matches punctuation shared across CJK scripts via Unicode's
  # Script_Extensions data (e.g. U+30FB KATAKANA MIDDLE DOT, used
  # constantly in Japanese titles like "space::skypiea"), which was
  # wrongly tagging plain Japanese alt-titles as "Korean" and splitting a
  # single show's own rows (e.g. One Piece, Naruto Shippuden) into two
  # groups. Block ranges only match characters that actually belong to
  # that script's dedicated Unicode block.
  if (grepl("[가-힣ᄀ-ᇿ㄰-㆏ꥠ-꥿ힰ-퟿]", alt, perl = TRUE)) return("Korean")
  if (grepl("[぀-ヿㇰ-ㇿ]", alt, perl = TRUE)) return("Japanese")
  if (grepl("\\p{Han}", alt, perl = TRUE)) return("Chinese_or_Kanji")
  if (grepl("\\p{Cyrillic}", alt, perl = TRUE)) return("Cyrillic")
  if (grepl("\\p{Arabic}", alt, perl = TRUE)) return("Arabic")
  if (grepl("\\p{Thai}", alt, perl = TRUE)) return("Thai")
  if (grepl("\\p{Hebrew}", alt, perl = TRUE)) return("Hebrew")
  if (grepl("\\p{Devanagari}", alt, perl = TRUE)) return("Devanagari")
  "Latin"
}

h1 <- setDT(read_excel("Original data/What_We_Watched_A_Netflix_Engagement_Report_2023Jan-Jun.xlsx",
                        sheet = "Engagement", skip = 3))
h1[, `:=`(Runtime = NA_character_, Views = NA_real_)]

h2 <- setDT(read_excel("Original data/What_We_Watched_A_Netflix_Engagement_Report_2023Jul-Dec.xlsx",
                        sheet = "TV", skip = 3))
h2[, `Release Date` := as.character(`Release Date`)]

netflix_2023 <- rbindlist(list(h1, h2), use.names = TRUE)
netflix_2023[, title_no_alt      := strip_alt_title(Title)]
netflix_2023[, is_known_override := vapply(title_no_alt, is_known_bare_number_title, logical(1))]
netflix_2023[, base_title        := strip_season_label(title_no_alt)]
netflix_2023[, had_season_label  := (title_no_alt != base_title) | is_known_override]
netflix_2023[is_known_override == TRUE,
             base_title := vapply(title_no_alt[is_known_override], known_bare_number_base, character(1))]
netflix_2023[, alt_title_script  := vapply(extract_alt_title(Title), detect_script_one, character(1))]
netflix_2023[, season_number     := vapply(title_no_alt, extract_season_number_one, integer(1))]
netflix_2023[is_known_override == TRUE,
             season_number := vapply(title_no_alt[is_known_override], known_bare_number_season, integer(1))]
netflix_2023[, is_known_override := NULL]

# Special case: "One Piece" mixes the long-running anime (labeled by story
# arc, e.g. "ONE PIECE: East Blue" -- none say "Season N") with a 2023
# live-action adaptation (the ONLY "One Piece" row labeled "Season 1") and
# several anime movies/specials. Left alone, the catalog match later would
# treat "ONE PIECE: Season 1" as the anime's season-labeled match (no arc
# row qualifies), wrongly attaching the live-action's 541.9M hours to the
# 1999 anime -- found and confirmed 2026-08-10. Separated here: the real
# anime (arc entries only), the live-action (its own group, text-distinct
# from "One Piece" so it's never picked up by the catalog match -- it has
# no 2021 catalog entry anyway, since it didn't exist yet), and
# movies/specials (left as their own individual, un-grouped titles).
netflix_2023[title_no_alt == "ONE PIECE: Season 1",
             `:=`(base_title = "One Piece (Live Action)", had_season_label = TRUE)]
netflix_2023[grepl("^ONE PIECE: ", title_no_alt) & title_no_alt != "ONE PIECE: Season 1",
             `:=`(base_title = "One Piece", had_season_label = FALSE)]

# Group rows together only if they share the same (a) cleaned base title,
# (b) whether a season label was present, and (c) alt-title script. This
# keeps a real show's own seasons summed together, while keeping unrelated
# same-named content apart (e.g. the Death Note anime vs. an unrelated
# Death Note movie; a Korean vs. Japanese show both called "Kingdom"). It
# also fixes a capitalization-inconsistency bug: Netflix's report isn't
# consistently capitalized between the two halves (e.g. "A Boyfriend For
# My Wife" vs. "A boyfriend for my wife"), which was splitting one show's
# hours across two rows before this grouping key was normalized to
# lowercase/trimmed.
netflix_2023[, group_key := paste(tolower(trimws(base_title)), had_season_label, alt_title_script, sep = "||")]
setorder(netflix_2023, -`Hours Viewed`)
display_title <- netflix_2023[, .(base_title = base_title[1]), by = group_key]

netflix_2023_series <- netflix_2023[, .(
  had_season_label           = had_season_label[1],
  alt_title_script           = alt_title_script[1],
  Hours_Viewed_2023          = sum(`Hours Viewed`, na.rm = TRUE),
  Views_2023                 = if (all(is.na(Views))) NA_real_ else sum(Views, na.rm = TRUE),
  Available_Globally_Ever    = any(`Available Globally?` == "Yes", na.rm = TRUE),
  Available_Globally_Always  = all(`Available Globally?` == "Yes"),
  Release_Date_Earliest      = min(`Release Date`, na.rm = TRUE),
  n_rows_aggregated          = .N,
  # Actual distinct-season count, as opposed to n_rows_aggregated (which
  # also counts a season twice if it appeared in both half-year reports).
  # A group with no numbered season label anywhere (a movie, special, or
  # unnumbered "Limited Series") counts as 1 — a single installment.
  n_distinct_seasons         = {
    nums <- unique(na.omit(season_number))
    if (length(nums) == 0) 1L else length(nums)
  }
), by = group_key]
netflix_2023_series <- merge(netflix_2023_series, display_title, by = "group_key")
netflix_2023_series[, group_key := NULL]

fwrite(netflix_2023_series, "netflix_2023_fullyear.csv", scipen = 100)
cat("PART 2 done — netflix_2023_fullyear.csv:", nrow(netflix_2023_series), "grouped rows",
    "from", nrow(netflix_2023), "raw report rows.\n\n")


# ================================================================
# PART 3 — Three-way merge: catalog + 2021 IMDb popularity + 2023 viewing
# ================================================================

# --- 1. Catalog, restricted to Netflix titles -------------------------------
catalog <- fread("tv_shows_with_current_imdb.csv")
catalog <- catalog[Netflix == 1]
catalog[, n_platforms := Netflix + Hulu + `Prime Video` + `Disney+`]
catalog[, exclusive   := n_platforms == 1]
catalog[, title_norm  := norm_title(Title)]

# --- 2. IMDb 2021 established-popularity snapshot, joined via tconst -------
# Deliberately a genuine ~June 2021 dated file (separate from the catalog's
# own ~August 2021 vintage) — this predates the 2023 viewing data below, so
# it can't leak the outcome into the predictor.
ratings_2021 <- fread("Original data/imdb_data/IMDb_title_ratings.tsv")
setnames(ratings_2021, c("averageRating", "numVotes"), c("IMDb_rating_2021", "IMDb_ratecount_2021"))
catalog <- merge(catalog, ratings_2021, by = "tconst", all.x = TRUE)

# --- 3. Netflix 2023 engagement data, matched via title ---------------------
# netflix_2023_fullyear.csv can have more than one row per normalized title
# (Part 2 deliberately splits unrelated same-named content), so this needs a
# disambiguation function rather than a plain merge.
netflix_agg <- fread("netflix_2023_fullyear.csv")
netflix_agg[, title_norm := norm_title(base_title)]
netflix_agg[, release_year := suppressWarnings(as.integer(substr(Release_Date_Earliest, 1, 4)))]

match_netflix_group <- function(title_norm_val, year_val) {
  # Prefer season-labeled groups (strongest signal of a real TV entry).
  cand <- netflix_agg[title_norm == title_norm_val & had_season_label == TRUE]
  tier <- "labeled"
  if (nrow(cand) == 0) {
    cand <- netflix_agg[title_norm == title_norm_val & had_season_label == FALSE]
    tier <- "unlabeled"
  }
  if (nrow(cand) == 0) return(NULL)
  if (nrow(cand) == 1) return(list(row = cand, tier = tier))
  # More than one candidate (e.g. two unrelated franchises sharing an
  # English title) — break the tie using release-year proximity; if still
  # tied or undated, don't guess.
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

# --- 4. Final unit of analysis: require established popularity too ---------
research_data <- research_data[!is.na(IMDb_ratecount_2021)]

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
# Rotten Tomatoes, popularity, or platform data -- inconsistent with the
# rest of the dataset and with Step 4's requirement that every row have a
# 2021 popularity value. Its hours still exist, correctly separated from
# the real "One Piece" anime, in netflix_2023_fullyear.csv if ever needed.

stopifnot(!any(duplicated(final_cols$title)))  # true one-row-per-title check

fwrite(final_cols, "netflix_research_dataset.csv", scipen = 100)
cat("PART 3 done — netflix_research_dataset.csv:", nrow(final_cols), "rows",
    "(catalog + 2021 popularity + 2023 viewing, all categories still labeled).\n\n")


# ================================================================
# PART 4 — Final cleaning -> the analysis-ready dataset
# ================================================================

df <- fread("netflix_research_dataset.csv")

# Filter to real TV series only. netflix_research_dataset.csv deliberately
# keeps every matched category (movies, shorts, etc.) labeled rather than
# dropped, for provenance — but the actual analysis should run on TV only.
n_before <- nrow(df)
df <- df[imdb_title_type %in% c("tvSeries", "tvMiniSeries")]

# Rotten Tomatoes: "82/100" -> 82.
df[, rotten_tomatoes_numeric := as.numeric(sub("/100", "", rotten_tomatoes))]

# Age: missing -> its own "Unknown" category, not dropped.
df[age == "" | is.na(age), age := "Unknown"]
df[, age := factor(age, levels = c("all", "7+", "13+", "16+", "18+", "Unknown"))]

# Log-transformed versions of the two heavily right-skewed variables — use
# these for modeling, not the raw columns. log1p (log(1+x)) instead of
# log() as a safety habit against a future zero value, even though neither
# column currently has one.
df[, log_hours_viewed_2023    := log1p(hours_viewed_2023)]
df[, log_imdb_ratecount_2021  := log1p(imdb_ratecount_2021)]

# Notes for modeling (no further data change needed):
#  - imdb_rating_catalog_2021 (~15 rows) and views_2023 (~170 rows) are left
#    as NA — exclude only from models that actually use those variables.
#  - release_date_earliest is missing for a few hundred records and is left
#    as-is; use plain "year" as the main release-timing variable.

fwrite(df, "netflix_analysis_ready.csv", scipen = 100)

# Also write a copy as netflix_research_data.csv — the filename the shared
# team GitHub repo's existing EDA script (netflix_analysis.R) already
# expects. Same data, just under the name a teammate's script was already
# written against, so nothing on their end needs to change.
fwrite(df, "netflix_research_data.csv", scipen = 100)

cat("PART 4 done — netflix_analysis_ready.csv:", nrow(df), "rows,", ncol(df), "columns",
    "(dropped", n_before - nrow(df), "non-TV records).\n")
cat("Also wrote netflix_research_data.csv (same data, for the shared repo's existing script).\n")
