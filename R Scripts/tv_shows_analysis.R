# ==============================================================
# Netflix Competitive Positioning Analysis
# Main script: setup + data import
# ==============================================================

# --- Set the working directory -------------------------------------------
# Always anchor to the project root — the project is split into "Original
# data/" and "R Scripts/" subfolders, so a script-location-relative setwd
# would break depending on how this is run.
setwd("~/Documents/MSBX 5415/Group Project")

# --- Read in the data ------------------------------------------------------
# read.csv() loads a comma-separated file into a data frame (R's version of
# a spreadsheet/table). stringsAsFactors = FALSE keeps text columns as plain
# text instead of R automatically converting them to categorical "factors".
tv_shows <- read.csv("Original data/tv_shows.csv", stringsAsFactors = FALSE)

# --- Quick sanity checks -----------------------------------------------
# str() prints each column's name and data type, plus a preview of values.
str(tv_shows)

# head() shows the first 6 rows so we can eyeball that the import looks right.
head(tv_shows)

# dim() reports the number of rows and columns (rows first, then columns).
dim(tv_shows)

# ==============================================================
# Q3 — What predicts a highly-rated show?
# Multiple regression, run two ways: treating every show's rating as
# equally trustworthy, and then accounting for how many people actually
# voted on it.
# ==============================================================

# --- Read the enriched dataset ----------------------------------------------
# This version has an extra IMDb_ratecount column (how many people voted on
# IMDb) that the plain tv_shows.csv does not have, so we use it here instead.
library(readxl)
enriched <- read_excel("tv_shows_with_imdb_ratingcounts.csv.xlsx")

# --- Clean up the columns we need -------------------------------------------

# IMDb and Rotten Tomatoes arrive as text like "9.4/10" and "100/100".
# sub() strips everything from the "/" onward, leaving just the number, then
# as.numeric() turns that leftover text into an actual number R can do math with.
enriched$IMDb_num <- as.numeric(sub("/10", "", enriched$IMDb))
enriched$RT_num   <- as.numeric(sub("/100", "", enriched$`Rotten Tomatoes`))

# IMDb_ratecount is also stored as text, and unmatched shows have the literal
# word "NA" instead of a real missing value, so we convert it the same way.
# (R will warn "NAs introduced by coercion" here — that warning is expected,
# not a bug: it's just R telling us it turned those "NA" strings into real NAs.)
enriched$ratecount_num <- as.numeric(enriched$IMDb_ratecount)

# Add up the four 0/1 platform flag columns to get how many platforms a show
# is on, then flag a show as "exclusive" if that count is exactly 1.
enriched$n_platforms <- enriched$Netflix + enriched$Hulu + enriched$Prime + enriched$Disney
enriched$exclusive   <- enriched$n_platforms == 1

# Age arrives as text (e.g. "18+"). factor() tells R to treat it as a
# category rather than free text, which is what a regression needs.
enriched$Age <- factor(enriched$Age)

# --- Model 1: plain (unweighted) regression ---------------------------------
# The "everyone's rating counts equally" version: a show rated by 50 people
# influences the fitted line exactly as much as a show rated by 500,000.
q3_plain <- lm(IMDb_num ~ Year + Age + exclusive, data = enriched)
summary(q3_plain)

# --- Model 2: weighted regression --------------------------------------------
# EASY WAY TO THINK ABOUT THIS:
# Imagine two restaurants both show a 4.5-star average. One has 3 reviews,
# the other has 30,000. You'd trust the 30,000-review average far more —
# it's much less likely to be a fluke. A weighted regression applies that
# same logic to every show: shows with more IMDb votes get to "count more"
# when fitting the line, because their rating is a more reliable number.
#
# We weight by log(votes) rather than the raw vote count so that a
# mega-popular show with millions of votes doesn't completely drown out
# every other show in the model — it still gets extra trust, just not
# unlimited trust.
q3_weighted <- lm(IMDb_num ~ Year + Age + exclusive,
                   data = enriched,
                   weights = log(ratecount_num))
summary(q3_weighted)

# --- Compare the two ----------------------------------------------------------
# If the estimated effects of Year/Age/exclusive are close between q3_plain
# and q3_weighted, that's reassuring — it means the result isn't just being
# driven by a handful of low-vote, noisy ratings.
# Note: q3_weighted uses fewer rows than q3_plain. About 1,286 shows have no
# vote count (they were unmatched in the IMDb merge) and get dropped
# automatically whenever they're used as a weight.
cbind(plain = coef(q3_plain), weighted = coef(q3_weighted)[names(coef(q3_plain))])

# ==============================================================
# Does releasing a show on more platforms raise its rating over time?
# We can't watch a single show's platform status change (the Kaggle
# snapshot is one point in time), but we CAN compare each show's own
# rating from the 2021 catalog snapshot to its rating TODAY (pulled fresh
# from IMDb's bulk data by merge_current_imdb.R) and see if that change
# looks different for shows that were exclusive in 2021 vs. shows that
# were already shared.
# ==============================================================

library(data.table)
current <- fread("tv_shows_with_current_imdb.csv")

# Same platform-count / exclusive logic as before, using this file's columns.
current[, n_platforms := Netflix + Hulu + `Prime Video` + `Disney+`]
current[, exclusive   := n_platforms == 1]

# A few of the matched "current rating" values are almost certainly wrong —
# not because the merge code has a bug, but because two genuinely different
# shows can share the exact same title and release year (a title collision),
# and our matching has no way to tell them apart without an IMDb ID in the
# original Kaggle data. abs(rating_change) > 3 is not a realistic amount for
# a real show's rating to move in ~5 years, so we treat those as probable
# mismatches and set them aside rather than let them distort the average.
current[, likely_mismatch := abs(rating_change) > 3 & !is.na(rating_change)]
cat("Flagged as likely title-collision mismatches:", sum(current$likely_mismatch, na.rm = TRUE), "\n")

clean <- current[!is.na(rating_change) & !likely_mismatch]

# --- Has the average rating moved at all since 2021? ------------------------
summary(clean$rating_change)

# --- The actual test: does the rating change differ for exclusive vs. shared? ---
rating_change_test <- t.test(rating_change ~ exclusive, data = clean)
rating_change_test

clean[, .(mean_change = mean(rating_change), n = .N), by = exclusive]
