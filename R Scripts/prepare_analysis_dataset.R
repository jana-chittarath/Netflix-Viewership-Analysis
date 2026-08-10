# ==============================================================
# Take netflix_research_dataset.csv (the three-way merge) and finish
# the "clean missing, suspicious, extreme values" step of data prep,
# producing an analysis-ready file for the EDA/visualization/
# inference/modeling steps that follow.
#
# What this does, and why:
#   - Filters imdb_title_type down to tvSeries/tvMiniSeries only. The
#     broader netflix_research_dataset.csv deliberately keeps every
#     matched category (movies, shorts, etc.) labeled rather than
#     dropped, for provenance — but the actual TV-show analysis should
#     run on TV shows only.
#   - Parses rotten_tomatoes from text ("82/100") to a plain number,
#     since no analysis or model can use it as text.
#   - Fills missing age with the label "Unknown" rather than dropping
#     those rows — age becomes its own visible category in any group
#     comparison instead of silently vanishing.
#   - Adds log-transformed versions of hours_viewed_2023 and
#     imdb_ratecount_2021 alongside the originals. Both are heavily
#     right-skewed (a handful of massive hits dominate the raw scale),
#     which is normal for viewing/vote-count data but distorts
#     regressions and plots if used raw. log1p (log(1+x)) is used
#     instead of plain log() as a safety habit in case a future run
#     of this pipeline ever produces a zero value, even though neither
#     column currently has one. USE THESE log columns for modeling, not
#     the raw hours_viewed_2023/imdb_ratecount_2021.
#
# Notes for later modeling steps (no data change needed for these —
# just how to use what's already here):
#   - imdb_rating_catalog_2021 is missing for ~17 rows. Exclude those
#     rows only from models that actually use this variable, rather
#     than dropping them from the whole dataset.
#   - views_2023 is missing for ~176 rows. hours_viewed_2023 (the main
#     outcome variable) has no missing values, so this only matters if
#     a model specifically analyzes views.
#   - release_date_earliest is missing for a few hundred records (left
#     as-is, not filled or derived) — use plain "year" as the main
#     release-timing variable instead.
# ==============================================================

library(data.table)

# Always anchor to the project root — the project is split into "Original
# data/" and "R Scripts/" subfolders, so a script-location-relative setwd
# would break depending on how this is run.
setwd("~/Documents/MSBX 5415/Group Project")

df <- fread("netflix_research_dataset.csv")

# --- TV-only filter for actual analysis --------------------------------------
n_before_tv_filter <- nrow(df)
df <- df[imdb_title_type %in% c("tvSeries", "tvMiniSeries")]
cat("Filtered to tvSeries/tvMiniSeries: dropped", n_before_tv_filter - nrow(df),
    "non-TV records, kept", nrow(df), "\n\n")

# --- Rotten Tomatoes: "82/100" -> 82 -----------------------------------------
df[, rotten_tomatoes_numeric := as.numeric(sub("/100", "", rotten_tomatoes))]

# --- Age: missing -> "Unknown", not dropped ---------------------------------
df[age == "" | is.na(age), age := "Unknown"]
df[, age := factor(age, levels = c("all", "7+", "13+", "16+", "18+", "Unknown"))]

# --- Log-transformed versions of the two skewed variables -------------------
df[, log_hours_viewed_2023    := log1p(hours_viewed_2023)]
df[, log_imdb_ratecount_2021  := log1p(imdb_ratecount_2021)]

# --- Sanity checks: flag anything out of a plausible range -------------------
# IMDb ratings should be 0-10, Rotten Tomatoes 0-100. Print anything outside
# that range rather than silently trusting the data.
bad_imdb <- df[!is.na(imdb_rating_catalog_2021) & (imdb_rating_catalog_2021 < 0 | imdb_rating_catalog_2021 > 10)]
bad_rt   <- df[!is.na(rotten_tomatoes_numeric) & (rotten_tomatoes_numeric < 0 | rotten_tomatoes_numeric > 100)]
cat("Rows with an out-of-range IMDb rating:", nrow(bad_imdb), "\n")
cat("Rows with an out-of-range Rotten Tomatoes score:", nrow(bad_rt), "\n\n")

cat("Missingness after cleaning:\n")
miss <- sapply(df, function(x) sum(is.na(x) | x == ""))
print(miss[miss > 0])

fwrite(df, "netflix_analysis_ready.csv", scipen = 100)
cat("\nSaved netflix_analysis_ready.csv --", nrow(df), "rows,", ncol(df), "columns.\n")
