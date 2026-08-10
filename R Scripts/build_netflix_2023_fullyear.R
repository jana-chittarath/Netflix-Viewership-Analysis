# ==============================================================
# Combine Netflix's two 2023 "What We Watched" engagement reports
# (Jan-Jun + Jul-Dec) into one full-year 2023 view.
#
# The two halves have different shapes:
#   - Jan-Jun: one combined "Engagement" sheet, TV shows and movies
#     mixed together, only 4 columns (no Runtime/Views).
#   - Jul-Dec: separate "TV" and "Film" sheets, 6 columns.
#
# IMPORTANT — this script does NOT try to guess which rows are TV vs.
# movies vs. specials and drop anything. Every row is kept; what
# changed (2026-08-07) is HOW rows get grouped together, to fix a real
# contamination bug found while reviewing the data: stripping season
# labels like "Season 1"/"Series 2" to find each show's base title
# could accidentally merge together things that are NOT the same show
# just because they share an English name — e.g. the "Death Note"
# anime TV series, the unrelated 2017 Netflix "Death Note" movie, and
# a Japanese sequel movie were all landing under one "Death Note" row,
# inflating the TV show's hours with two different movies' hours. And
# "Kingdom" turned out to be TWO different franchises (a Korean zombie
# series and an unrelated Japanese series) that both happened to use
# recognized season labels and reduce to the same English title.
#
# Fix: group rows together only if they share the same (a) cleaned
# base title, (b) whether a season/series label was present at all,
# and (c) the writing script (Korean/Japanese/Cyrillic/etc.) of their
# original-language alt-title, when they have one. This keeps a show's
# real seasons summed together (they share all three), while keeping
# unrelated same-named content apart (a movie has no season label; two
# unrelated franchises in different languages have different scripts).
# It is not a perfect fix — two different pieces of content in the
# *same* language that both use season labels still can't be told
# apart from title text alone — but it resolves every concrete case
# found during review.
# ==============================================================

library(readxl)
library(data.table)

# Always anchor to the project root — the project is split into "Original
# data/" and "R Scripts/" subfolders, so a script-location-relative setwd
# would break depending on how this is run.
setwd("~/Documents/MSBX 5415/Group Project")

# --- Helpers -------------------------------------------------------------

# Drop everything from " // " onward (the original-language alt-title),
# e.g. "The Glory: Season 1 // 더 글로리: 시즌 1" -> "The Glory: Season 1".
strip_alt_title <- function(title) sub(" // .*$", "", title)

# Drop a trailing ": Season N", ": Limited Series", ": Miniseries",
# ": Part N", or ": Series N" — only right at the end of the string, so we
# don't chop a title that just happens to contain one of these words in
# the middle (e.g. "Queen Charlotte: A Bridgerton Story" stays untouched).
strip_season_label <- function(title) {
  trimws(sub(":\\s*(Season\\s*[0-9]+|Limited Series|Miniseries|Part\\s*[0-9]+|Series\\s*[0-9]+)\\s*$",
             "", title, ignore.case = TRUE))
}

# The part after " // ", if any — the original-language alt-title.
extract_alt_title <- function(title) {
  has_alt <- grepl(" // ", title, fixed = TRUE)
  ifelse(has_alt, sub("^.* // ", "", title), NA_character_)
}

# What writing script the alt-title uses. This is our stand-in for "is
# this actually the same real-world show," since Netflix's report has no
# ID system at all — two unrelated franchises sharing an English name
# will usually have alt-titles in different scripts.
detect_script_one <- function(alt) {
  if (is.na(alt) || alt == "") return("none")
  if (grepl("\\p{Hangul}", alt, perl = TRUE)) return("Korean")
  if (grepl("\\p{Hiragana}|\\p{Katakana}", alt, perl = TRUE)) return("Japanese")
  if (grepl("\\p{Han}", alt, perl = TRUE)) return("Chinese_or_Kanji")
  if (grepl("\\p{Cyrillic}", alt, perl = TRUE)) return("Cyrillic")
  if (grepl("\\p{Arabic}", alt, perl = TRUE)) return("Arabic")
  if (grepl("\\p{Thai}", alt, perl = TRUE)) return("Thai")
  if (grepl("\\p{Hebrew}", alt, perl = TRUE)) return("Hebrew")
  if (grepl("\\p{Devanagari}", alt, perl = TRUE)) return("Devanagari")
  "Latin"
}

# --- Load Jan-Jun 2023 (combined TV + movies, 4 columns) -------------------
h1 <- read_excel("Original data/What_We_Watched_A_Netflix_Engagement_Report_2023Jan-Jun.xlsx",
                  sheet = "Engagement", skip = 3)
setDT(h1)
h1[, `:=`(Runtime = NA_character_, Views = NA_real_)]  # H1 doesn't have these

# --- Load Jul-Dec 2023 (TV sheet only — Netflix already split this half) ---
h2 <- read_excel("Original data/What_We_Watched_A_Netflix_Engagement_Report_2023Jul-Dec.xlsx",
                  sheet = "TV", skip = 3)
setDT(h2)
h2[, `Release Date` := as.character(`Release Date`)]  # match H1's date type

# --- Stack both halves into one full-year list ------------------------------
netflix_2023 <- rbindlist(list(h1, h2), use.names = TRUE)

netflix_2023[, title_no_alt      := strip_alt_title(Title)]
netflix_2023[, base_title        := strip_season_label(title_no_alt)]
netflix_2023[, had_season_label  := title_no_alt != base_title]
netflix_2023[, alt_title_script  := vapply(extract_alt_title(Title), detect_script_one, character(1))]

# Netflix's own report isn't consistently capitalized between the two
# halves (e.g. "A Boyfriend For My Wife" in one half, "A boyfriend for my
# wife" in the other) — grouping by the raw title would treat those as two
# different shows and split their hours. We normalize case/whitespace for
# grouping, and keep whichever original casing had the most hours as the
# display title.
netflix_2023[, group_key := paste(tolower(trimws(base_title)), had_season_label, alt_title_script, sep = "||")]
setorder(netflix_2023, -`Hours Viewed`)
display_title <- netflix_2023[, .(base_title = base_title[1]), by = group_key]

# --- Aggregate multiple rows (seasons, casing variants, or the same group
# appearing in both halves) up to one row per group --------------------------
# had_season_label and alt_title_script are kept as real output columns
# (not just used internally) — every row stays in the data, labeled with
# why it was grouped the way it was, rather than being dropped.
netflix_2023_series <- netflix_2023[, .(
  had_season_label           = had_season_label[1],
  alt_title_script           = alt_title_script[1],
  Hours_Viewed_2023          = sum(`Hours Viewed`, na.rm = TRUE),
  Views_2023                 = if (all(is.na(Views))) NA_real_ else sum(Views, na.rm = TRUE),
  Available_Globally_Ever    = any(`Available Globally?` == "Yes", na.rm = TRUE),
  Available_Globally_Always  = all(`Available Globally?` == "Yes"),
  Release_Date_Earliest      = min(`Release Date`, na.rm = TRUE),
  n_rows_aggregated          = .N
), by = group_key]
netflix_2023_series <- merge(netflix_2023_series, display_title, by = "group_key")
netflix_2023_series[, group_key := NULL]

cat("Rows before aggregation:", nrow(netflix_2023), "\n")
cat("Distinct groups after aggregating seasons/halves:", nrow(netflix_2023_series), "\n")
cat("Groups combining more than 1 row:", sum(netflix_2023_series$n_rows_aggregated > 1), "\n")
cat("Base titles that now split into more than one group (e.g. a show + an unrelated same-named movie):",
    sum(duplicated(tolower(trimws(netflix_2023_series$base_title))) |
        duplicated(tolower(trimws(netflix_2023_series$base_title)), fromLast = TRUE)), "\n\n")

# scipen = 100 keeps large hours/views numbers as plain digits instead of
# scientific notation (e.g. 200000 instead of 2e+05).
fwrite(netflix_2023_series, "netflix_2023_fullyear.csv", scipen = 100)
cat("Saved netflix_2023_fullyear.csv\n")
