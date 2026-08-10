# ==============================================================
# Compare each show's IMDb rating and vote count in 2021 vs. today,
# and test whether that trend differs by 2021 platform-exclusivity
# status (from the historical catalog snapshot — itself dated 2021,
# not 2020 as originally assumed; see merge_current_imdb.R's header
# for why).
#
# Builds on merge_current_imdb.R's output (tv_shows_with_current_imdb.csv),
# which matches each catalog show to its IMDb tconst (permanent ID) and
# pulls today's rating/votes. Because tconst never changes for a title, we
# reuse it here to look the same show up in a genuine 2021 snapshot
# (IMDb_title_ratings.tsv) — giving us a real 5-year comparison, unlike an
# earlier attempt that accidentally compared two near-simultaneous pulls.
# ==============================================================

library(data.table)

# Always anchor to the project root — the project is split into "Original
# data/" and "R Scripts/" subfolders, so a script-location-relative setwd
# would break depending on how this is run.
setwd("~/Documents/MSBX 5415/Group Project")

# --- Load today's match (has tconst, today's rating + votes) ---------------
tv_shows <- fread("tv_shows_with_current_imdb.csv")

# merge_current_imdb.R now also matches catalog rows that turn out to be
# movies/shorts/etc. (tv_shows.csv isn't purely TV despite its name — see
# that script's header). This project's findings are about TV shows
# specifically, so we drop rows matched to a non-TV type, while keeping
# still-unmatched rows (imdb_title_type is NA) in the file as before.
n_before_type_filter <- nrow(tv_shows)
tv_shows <- tv_shows[is.na(imdb_title_type) | imdb_title_type %in% c("tvSeries", "tvMiniSeries")]
cat("Rows dropped as non-TV IMDb types:", n_before_type_filter - nrow(tv_shows), "\n")

# --- Load the 2021 IMDb ratings snapshot ------------------------------------
# Same three columns as IMDb's live title.ratings.tsv, just frozen as of
# June 2021: tconst, averageRating, numVotes.
ratings_2021 <- fread("Original data/imdb_data/IMDb_title_ratings.tsv")
setnames(ratings_2021, c("averageRating", "numVotes"), c("IMDb_rating_2021", "IMDb_ratecount_2021"))

# --- Join on tconst — the permanent ID, not the title text -----------------
# This is why matching only had to happen once: tconst identifies the same
# show in 2021 and today.
tv_shows <- merge(tv_shows, ratings_2021, by = "tconst", all.x = TRUE)

# --- Rename today's columns to make the two time points obvious ------------
setnames(tv_shows,
         c("IMDb_current", "IMDb_ratecount_current"),
         c("IMDb_rating_2026", "IMDb_ratecount_2026"))

# --- Genuine 5-year change (2021 -> 2026) -----------------------------------
tv_shows[, rating_change_2021_2026    := IMDb_rating_2026 - IMDb_rating_2021]
tv_shows[, ratecount_change_2021_2026 := IMDb_ratecount_2026 - IMDb_ratecount_2021]

# --- Platform info from the 2021 catalog snapshot (the only platform data
# we have — not necessarily each show's platform status today) -------------
tv_shows[, n_platforms := Netflix + Hulu + `Prime Video` + `Disney+`]
tv_shows[, exclusive   := n_platforms == 1]

# --- Match quality for the 2021 join ----------------------------------------
cat("Rows with both a 2021 and 2026 vote count (usable for comparison):",
    sum(!is.na(tv_shows$IMDb_ratecount_2021) & !is.na(tv_shows$IMDb_ratecount_2026)), "\n\n")

# --- The actual tests --------------------------------------------------------
# Does a show's rating move differently over 2021-2026 depending on whether
# it was exclusive to one platform in the 2021 catalog snapshot?
cat("Rating change 2021->2026, by 2021 exclusivity status:\n")
print(t.test(rating_change_2021_2026 ~ exclusive, data = tv_shows))

# Does vote-count growth over 2021-2026 differ by 2021 exclusivity status?
# This is the real version of the test — a genuine 5-year gap, unlike the
# earlier attempt that compared two nearly-simultaneous downloads.
cat("\nVote count change 2021->2026, by 2021 exclusivity status:\n")
print(t.test(ratecount_change_2021_2026 ~ exclusive, data = tv_shows))

# --- Save the combined result ------------------------------------------------
# scipen = 100 avoids scientific notation for large vote-count numbers.
fwrite(tv_shows, "tv_shows_rating_trends.csv", scipen = 100)
