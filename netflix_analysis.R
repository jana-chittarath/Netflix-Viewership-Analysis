# NETFLIX TV SHOW RESEARCH PROJECT
# Part 1: Data preparation and Research Questions 1-6
#
# Guiding question:
# What distinguishes Netflix's blockbusters, sleeper hits, and underperforming
# TV shows, and how can Netflix use those differences to improve content
# promotion, distribution, acquisition, and retention decisions?
#
# Research Question 1:
# How are viewing hours, established IMDb awareness, rating quality, catalog
# depth, title format, audience classification, global availability, and
# historical catalog differentiation distributed across Netflix TV shows?
#
# Research Question 2:
# How do viewing hours differ between globally and regionally available shows,
# historically single-platform and shared shows, and high- and low-awareness
# shows?
#
# Research Question 3:
# How accurately can established popularity, measured by IMDb rating count,
# predict Netflix viewing hours?
#
# Research Question 4:
# How much do quality, content characteristics, global availability, and
# historical catalog differentiation improve predictions beyond established
# popularity?
#
# Research Question 5:
# Which titles receive substantially more or less viewing than the selected
# content-informed model predicts?
#
# Research Question 6:
# Which titles and title profiles emerge as candidates for promotion, continued
# investment, strategic replication, or further review?

# 0. Identify the project directory and create an output folder.
# This makes the script reproducible even when it is launched from another folder.
args <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", args, value = TRUE)

if (length(script_argument) == 1) {
  script_path <- normalizePath(sub("^--file=", "", script_argument))
  project_dir <- dirname(script_path)
} else {
  project_dir <- getwd()
}

input_file <- file.path(project_dir, "netflix_analysis_ready.csv")
output_dir <- file.path(project_dir, "eda_outputs", "research_question_1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rq2_output_dir <- file.path(project_dir, "eda_outputs", "research_question_2")
dir.create(rq2_output_dir, recursive = TRUE, showWarnings = FALSE)
rq3_output_dir <- file.path(project_dir, "eda_outputs", "research_question_3")
dir.create(rq3_output_dir, recursive = TRUE, showWarnings = FALSE)
rq4_output_dir <- file.path(project_dir, "eda_outputs", "research_question_4")
dir.create(rq4_output_dir, recursive = TRUE, showWarnings = FALSE)
rq5_output_dir <- file.path(project_dir, "eda_outputs", "research_question_5")
dir.create(rq5_output_dir, recursive = TRUE, showWarnings = FALSE)
rq6_output_dir <- file.path(project_dir, "eda_outputs", "research_question_6")
dir.create(rq6_output_dir, recursive = TRUE, showWarnings = FALSE)

# Set the requested seed for reproducibility. The current analyses are
# deterministic, but retaining the seed ensures consistency in later modeling.
set.seed(123)

# Use a consistent Netflix-inspired palette across all visualizations.
# Red and dark red highlight focal categories; charcoal and gray provide
# readable comparisons without relying on low-contrast all-red charts.
netflix_red <- "#E50914"
netflix_dark_red <- "#B20710"
netflix_black <- "#141414"
netflix_gray <- "#737373"
netflix_light_gray <- "#B3B3B3"

report_file <- file.path(project_dir, "eda_outputs", "netflix_analysis_console_output.txt")
sink(report_file, split = TRUE)

cat("NETFLIX RESEARCH ANALYSIS: SETUP AND RESEARCH QUESTION 1\n")
cat("========================================================\n\n")

# 1. Load the combined data and verify that the expected columns are present.
# This is a beginning data-integrity step required before answering any question.
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

netflix <- read.csv(
  input_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "null")
)

required_columns <- c(
  "title",
  "year",
  "imdb_rating_catalog_2021",
  "rotten_tomatoes",
  "n_platforms",
  "exclusive",
  "imdb_ratecount_2021",
  "hours_viewed_2023",
  "available_globally_ever",
  "available_globally_always",
  "n_seasons_aggregated",
  "n_distinct_seasons",
  "age",
  "imdb_title_type",
  "tconst"
)

missing_required_columns <- setdiff(required_columns, names(netflix))
if (length(missing_required_columns) > 0) {
  stop(
    "The following required columns are missing: ",
    paste(missing_required_columns, collapse = ", ")
  )
}

cat("1. DATA LOADING AND STRUCTURE\n")
cat("Rows:", format(nrow(netflix), big.mark = ","), "\n")
cat("Columns:", ncol(netflix), "\n")
cat("Unique titles:", format(length(unique(netflix$title)), big.mark = ","), "\n")
cat("Unique IMDb IDs:", format(length(unique(netflix$tconst)), big.mark = ","), "\n")
cat("Release-year range:", min(netflix$year), "to", max(netflix$year), "\n")
cat("Title types:\n")
print(table(netflix$imdb_title_type))

# Interpretation:
# The corrected file contains one unique title and IMDb ID per row, so
# the current unit of analysis is one record per matched TV series or miniseries.
# No duplicate title or IMDb identifiers need to be removed at this stage.
cat("\nInterpretation: The file contains one record per matched title, with no duplicate title or IMDb identifiers.\n\n")

# 2. Create analysis-ready variables without changing the source CSV.
# Rotten Tomatoes is converted from text such as "79/100" to the number 79.
# Log transformations are created because hours and rating counts are expected
# to be strongly right-skewed. log1p() safely handles possible zero values.
netflix$rotten_tomatoes_numeric <- suppressWarnings(
  as.numeric(sub("/.*$", "", netflix$rotten_tomatoes))
)
netflix$log_hours_viewed_2023 <- log1p(netflix$hours_viewed_2023)
netflix$log_imdb_ratecount_2021 <- log1p(netflix$imdb_ratecount_2021)
netflix$global_ever_label <- ifelse(
  netflix$available_globally_ever,
  "Globally available at least once",
  "Never globally available"
)
netflix$global_always_label <- ifelse(
  netflix$available_globally_always,
  "Always globally available",
  "Not always globally available"
)
netflix$differentiation_label <- ifelse(
  is.na(netflix$exclusive),
  "Historical status unavailable",
  ifelse(
    netflix$exclusive,
    "Historically single-platform",
    "Historically shared"
  )
)

# 3. Assess missingness and basic validity.
# This addresses the data-quality portion of Research Question 1 and identifies
# variables that may require special treatment in later models.
missing_summary <- data.frame(
  variable = names(netflix),
  missing_n = colSums(is.na(netflix)),
  missing_pct = 100 * colMeans(is.na(netflix)),
  row.names = NULL
)
missing_summary <- missing_summary[order(-missing_summary$missing_n), ]

cat("2. MISSINGNESS AND VALIDITY CHECKS\n")
print(missing_summary[missing_summary$missing_n > 0, ], row.names = FALSE)
cat("Duplicate titles:", sum(duplicated(netflix$title)), "\n")
cat("Duplicate IMDb IDs:", sum(duplicated(netflix$tconst)), "\n")
cat("Nonpositive viewing-hour values:", sum(netflix$hours_viewed_2023 <= 0, na.rm = TRUE), "\n")
cat("Nonpositive IMDb rating-count values:", sum(netflix$imdb_ratecount_2021 <= 0, na.rm = TRUE), "\n")
cat("Rotten Tomatoes conversion failures:", sum(is.na(netflix$rotten_tomatoes_numeric)), "\n")

# Interpretation:
# Hours viewed and IMDb rating counts are complete for all 1,260 titles, which
# supports the main descriptive analysis and later baseline regression. IMDb
# score is missing for 15 titles. Age and earliest release date have much more
# missingness, so they should not be included in a primary model without a
# deliberate missing-data strategy.
cat("\nInterpretation: Viewing hours and season counts are complete. Missing historical awareness, quality, and platform fields are concentrated in newly separated titles that lack valid 2021 catalog measures; release date remains the largest source of missingness.\n\n")

write.csv(
  missing_summary,
  file.path(output_dir, "rq1_missingness_summary.csv"),
  row.names = FALSE
)

# 4. Explore the distribution of total Netflix viewing hours.
# This directly addresses the viewing-hours component of Research Question 1.
quantile_probabilities <- c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1)
hours_quantiles <- quantile(
  netflix$hours_viewed_2023,
  probs = quantile_probabilities,
  na.rm = TRUE
)

sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

cat("3. VIEWING-HOURS DISTRIBUTION\n")
cat("Mean hours:", format(round(mean(netflix$hours_viewed_2023)), big.mark = ","), "\n")
cat("Median hours:", format(median(netflix$hours_viewed_2023), big.mark = ","), "\n")
cat("Raw-hours skewness:", round(sample_skewness(netflix$hours_viewed_2023), 2), "\n")
cat("Log-hours skewness:", round(sample_skewness(netflix$log_hours_viewed_2023), 2), "\n")
cat("Viewing-hours quantiles:\n")
print(hours_quantiles)

# Interpretation:
# Viewing hours are extremely right-skewed: the mean is about 42.9 million,
# while the median is only 8.3 million. The maximum exceeds 1.3 billion hours.
# The log transformation reduces skewness from about 6.03 to -0.10, making log
# viewing hours much more suitable for later visualization and regression.
cat("\nInterpretation: A small number of titles dominate raw viewing hours. The log transformation produces an approximately symmetric distribution and will be preferred in later models.\n\n")

png(
  file.path(output_dir, "01_viewing_hours_raw_and_log.png"),
  width = 1500,
  height = 700,
  res = 150
)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
hist(
  netflix$hours_viewed_2023 / 1e6,
  breaks = 40,
  col = netflix_red,
  border = "white",
  main = "Raw Netflix viewing hours",
  xlab = "Hours viewed in 2023 (millions)",
  ylab = "Number of titles"
)
abline(v = median(netflix$hours_viewed_2023) / 1e6, lwd = 2, lty = 2)
hist(
  netflix$log_hours_viewed_2023,
  breaks = 40,
  col = netflix_dark_red,
  border = "white",
  main = "Log-transformed viewing hours",
  xlab = "log(1 + hours viewed in 2023)",
  ylab = "Number of titles"
)
abline(v = median(netflix$log_hours_viewed_2023), lwd = 2, lty = 2)
dev.off()

# 5. Explore established audience awareness using 2021 IMDb rating counts.
# This addresses the IMDb-awareness component of Research Question 1.
awareness_quantiles <- quantile(
  netflix$imdb_ratecount_2021,
  probs = quantile_probabilities,
  na.rm = TRUE
)

cat("4. IMDb AWARENESS DISTRIBUTION\n")
cat("Mean IMDb rating count:", format(round(mean(netflix$imdb_ratecount_2021, na.rm = TRUE)), big.mark = ","), "\n")
cat("Median IMDb rating count:", format(median(netflix$imdb_ratecount_2021, na.rm = TRUE), big.mark = ","), "\n")
cat("Raw-count skewness:", round(sample_skewness(netflix$imdb_ratecount_2021), 2), "\n")
cat("Log-count skewness:", round(sample_skewness(netflix$log_imdb_ratecount_2021), 2), "\n")
cat("IMDb rating-count quantiles:\n")
print(awareness_quantiles)

# Interpretation:
# IMDb awareness is even more concentrated than viewing hours. The median title
# has about 2,037 ratings, but the maximum exceeds 1.5 million. Therefore, raw
# IMDb rating count should not be used untransformed in later regression models.
cat("\nInterpretation: IMDb awareness is highly concentrated among a small number of titles. The log rating count is the more appropriate scale for comparisons and modeling.\n\n")

# 6. Explore IMDb and Rotten Tomatoes rating quality.
# This addresses the rating-quality component of Research Question 1.
rating_summary <- data.frame(
  measure = c("IMDb score", "Rotten Tomatoes score"),
  available_n = c(
    sum(!is.na(netflix$imdb_rating_catalog_2021)),
    sum(!is.na(netflix$rotten_tomatoes_numeric))
  ),
  mean = c(
    mean(netflix$imdb_rating_catalog_2021, na.rm = TRUE),
    mean(netflix$rotten_tomatoes_numeric, na.rm = TRUE)
  ),
  median = c(
    median(netflix$imdb_rating_catalog_2021, na.rm = TRUE),
    median(netflix$rotten_tomatoes_numeric, na.rm = TRUE)
  ),
  standard_deviation = c(
    sd(netflix$imdb_rating_catalog_2021, na.rm = TRUE),
    sd(netflix$rotten_tomatoes_numeric, na.rm = TRUE)
  ),
  minimum = c(
    min(netflix$imdb_rating_catalog_2021, na.rm = TRUE),
    min(netflix$rotten_tomatoes_numeric, na.rm = TRUE)
  ),
  maximum = c(
    max(netflix$imdb_rating_catalog_2021, na.rm = TRUE),
    max(netflix$rotten_tomatoes_numeric, na.rm = TRUE)
  )
)

cat("5. RATING-QUALITY DISTRIBUTIONS\n")
print(rating_summary, row.names = FALSE)

# Interpretation:
# IMDb scores are centered near 7.1 with a median of 7.3, so the matched sample
# is concentrated around moderately favorable ratings. Rotten Tomatoes scores
# are centered in the mid-50s and show more dispersion. IMDb and Rotten Tomatoes
# should be treated as distinct audience and critic-oriented quality measures.
cat("\nInterpretation: IMDb scores are concentrated around 7, whereas Rotten Tomatoes scores are more dispersed. The measures capture related but nonidentical dimensions of perceived quality.\n\n")

write.csv(
  rating_summary,
  file.path(output_dir, "rq1_rating_summary.csv"),
  row.names = FALSE
)

# Rating and awareness summaries are retained in the printed output and CSV.
# A separate three-panel graph is intentionally omitted to keep RQ1 focused.

# 7. Explore global availability and historical catalog differentiation.
# This addresses the two categorical components of Research Question 1.
categorical_summary <- rbind(
  data.frame(
    measure = "Global availability ever",
    category = c("Never globally available", "Globally available at least once"),
    count = as.integer(table(factor(
      netflix$global_ever_label,
      levels = c("Never globally available", "Globally available at least once")
    )))
  ),
  data.frame(
    measure = "Global availability always",
    category = c("Not always globally available", "Always globally available"),
    count = as.integer(table(factor(
      netflix$global_always_label,
      levels = c("Not always globally available", "Always globally available")
    )))
  ),
  data.frame(
    measure = "Historical differentiation",
    category = c(
      "Historically shared",
      "Historically single-platform",
      "Historical status unavailable"
    ),
    count = as.integer(table(factor(
      netflix$differentiation_label,
      levels = c(
        "Historically shared",
        "Historically single-platform",
        "Historical status unavailable"
      )
    )))
  )
)
categorical_summary$percent <- 100 * categorical_summary$count / nrow(netflix)

cat("6. GLOBAL AVAILABILITY AND HISTORICAL DIFFERENTIATION\n")
print(categorical_summary, row.names = FALSE)
cat("\nNumber of historical platforms:\n")
print(table(netflix$n_platforms))

# Interpretation:
# About 72.1% of titles were globally available at least once and 65.6% were
# always globally available. Historical differentiation is highly imbalanced:
# 91.0% were observed on Netflix alone in the historical four-platform catalog.
# Later comparisons must report group sizes and avoid overstating evidence from
# the much smaller historically shared group.
cat("\nInterpretation: Most matched titles had broad global availability, and historical single-platform titles dominate the sample. The imbalance in differentiation groups will matter in later inference.\n\n")

write.csv(
  categorical_summary,
  file.path(output_dir, "rq1_categorical_summary.csv"),
  row.names = FALSE
)

png(
  file.path(output_dir, "03_global_availability_and_differentiation.png"),
  width = 800,
  height = 700,
  res = 150
)
par(mfrow = c(1, 1), mar = c(7, 5, 4, 1), cex.main = 0.9)

draw_percentage_bars <- function(values, labels, colors, title) {
  percentages <- 100 * values / sum(values)
  positions <- barplot(
    percentages,
    names.arg = labels,
    las = 1,
    col = colors,
    ylim = c(0, 110),
    ylab = "% of TV shows in the dataset",
    main = title
  )
  text(
    positions,
    percentages,
    labels = sprintf("%.1f%%\n(n=%s)", percentages, format(values, big.mark = ",")),
    pos = 3,
    cex = 0.85
  )
}

draw_percentage_bars(
  c(sum(!netflix$available_globally_ever), sum(netflix$available_globally_ever)),
  c("Regional only", "Global in at least\none half-year"),
  c(netflix_light_gray, netflix_red),
  "Was the show available globally in 2023?"
)
dev.off()

# 8. Explore catalog depth, title format, and intended audience.
# Distinct seasons measures content volume. The aggregated-entry count is kept
# separate because it counts report rows across both half-year source files.
season_quantiles <- quantile(
  netflix$n_distinct_seasons,
  probs = quantile_probabilities,
  na.rm = TRUE
)
title_type_summary <- as.data.frame(table(netflix$imdb_title_type))
names(title_type_summary) <- c("title_type", "count")
title_type_summary$percent <- 100 * title_type_summary$count / nrow(netflix)
age_summary <- as.data.frame(table(netflix$age, useNA = "ifany"))
names(age_summary) <- c("age_group", "count")
age_summary$percent <- 100 * age_summary$count / nrow(netflix)

cat("7. CATALOG DEPTH, FORMAT, AND AUDIENCE\n")
cat("Distinct-season quantiles:\n")
print(season_quantiles)
cat("\nTitle types:\n")
print(title_type_summary, row.names = FALSE)
cat("\nAge classifications:\n")
print(age_summary, row.names = FALSE)

# Interpretation:
# Most titles have relatively few seasons, while a small number of long-running
# series create a long upper tail. Series dominate miniseries, and Unknown is a
# material age-classification category that must be retained explicitly later.
cat("\nInterpretation: Catalog depth is concentrated among short-run titles, with a small long-running tail. Standard series dominate miniseries. Age classifications are usable, but the Unknown group is large enough that it must remain an explicit category.\n\n")

content_profile_summary <- rbind(
  data.frame(
    dimension = "Title type",
    category = as.character(title_type_summary$title_type),
    count = title_type_summary$count,
    percent = title_type_summary$percent
  ),
  data.frame(
    dimension = "Age classification",
    category = as.character(age_summary$age_group),
    count = age_summary$count,
    percent = age_summary$percent
  )
)
write.csv(
  content_profile_summary,
  file.path(output_dir, "rq1_content_profile_summary.csv"),
  row.names = FALSE
)

# Catalog-depth, format, and audience summaries are retained in the output and
# CSV but are not graphed in RQ1. They can be visualized later only if a later
# result shows that one of these characteristics is strategically important.

# 9. Explore the bivariate pattern between established awareness and viewing.
# This remains part of Research Question 1 visualization. The fitted line is
# descriptive only; the formal predictive regression belongs to Question 3.
awareness_viewing_correlation <- cor(
  netflix$log_imdb_ratecount_2021,
  netflix$log_hours_viewed_2023,
  use = "complete.obs"
)

cat("8. PRELIMINARY AWARENESS-VIEWING PATTERN\n")
cat(
  "Correlation between log IMDb rating count and log viewing hours:",
  round(awareness_viewing_correlation, 3),
  "\n"
)

# Interpretation:
# The log-scale correlation is approximately 0.532, showing a moderate positive
# relationship. Established awareness appears relevant, but the wide dispersion
# around the trend suggests that awareness alone will leave substantial room for
# sleeper hits and underperformers in the later predictive analysis.
cat("Interpretation: Established awareness and later viewing have a moderate positive relationship, but substantial title-level variation remains unexplained.\n\n")

png(
  file.path(output_dir, "04_awareness_vs_viewing.png"),
  width = 1100,
  height = 850,
  res = 150
)
plot(
  netflix$log_imdb_ratecount_2021,
  netflix$log_hours_viewed_2023,
  pch = 16,
  cex = 0.65,
  col = adjustcolor(netflix_red, alpha.f = 0.35),
  xlab = "Established awareness: log(1 + IMDb rating count in 2021)",
  ylab = "Netflix engagement: log(1 + viewing hours in 2023)",
  main = "Established IMDb awareness and later Netflix viewing"
)
abline(
  lm(log_hours_viewed_2023 ~ log_imdb_ratecount_2021, data = netflix),
  lwd = 3,
  col = netflix_black
)
legend(
  "topleft",
  legend = sprintf("Pearson correlation = %.3f", awareness_viewing_correlation),
  bty = "n"
)
dev.off()

# 10. Print the most extreme raw values as a final data-quality check.
# These are not yet classified as blockbusters or sleeper hits; this simply
# verifies which observations create the long upper tails in Research Question 1.
top_viewing <- netflix[
  order(netflix$hours_viewed_2023, decreasing = TRUE),
  c("title", "hours_viewed_2023", "imdb_ratecount_2021")
][1:10, ]

top_awareness <- netflix[
  order(netflix$imdb_ratecount_2021, decreasing = TRUE),
  c("title", "imdb_ratecount_2021", "hours_viewed_2023")
][1:10, ]

cat("9. EXTREME-VALUE CHECK\n")
cat("Ten titles with the highest raw viewing hours:\n")
print(top_viewing, row.names = FALSE)
cat("\nTen titles with the highest IMDb rating counts:\n")
print(top_awareness, row.names = FALSE)

# Interpretation:
# The most-viewed and most-aware lists overlap only partially. This reinforces
# the project premise that established recognition is important but does not
# fully determine Netflix engagement. Formal classification will occur later.
cat("\nInterpretation: The upper tails are genuine title observations, and the most-viewed titles are not identical to the most-established titles.\n\n")

write.csv(
  top_viewing,
  file.path(output_dir, "rq1_top_viewing_titles.csv"),
  row.names = FALSE
)
write.csv(
  top_awareness,
  file.path(output_dir, "rq1_top_awareness_titles.csv"),
  row.names = FALSE
)

# 11. Finish Research Question 1.
cat("RESEARCH QUESTION 1 COMPLETE\n")
cat("Research Question 1 outputs saved to:", normalizePath(output_dir), "\n\n")

# -----------------------------------------------------------------------------
# RESEARCH QUESTION 2
# How do viewing hours differ by global reach, historical catalog status, and
# established IMDb awareness?
# -----------------------------------------------------------------------------

cat("RESEARCH QUESTION 2\n")
cat("===================\n\n")

# 12. Define high and low awareness using the sample median IMDb rating count.
# The median gives two understandable groups without imposing an arbitrary
# external cutoff. Missing awareness values remain unclassified.
awareness_cutoff <- median(netflix$imdb_ratecount_2021, na.rm = TRUE)
netflix$awareness_group <- ifelse(
  is.na(netflix$imdb_ratecount_2021),
  NA,
  ifelse(
    netflix$imdb_ratecount_2021 > awareness_cutoff,
    "High awareness",
    "Low awareness"
  )
)

cat("Awareness cutoff (sample median IMDb rating count):",
    format(awareness_cutoff, big.mark = ","), "\n\n")

# 13. Produce group summaries on the original scale for business interpretation.
# Medians are emphasized because viewing hours remain highly skewed in raw form.
group_summary <- function(group, comparison_name) {
  keep <- !is.na(group) & !is.na(netflix$hours_viewed_2023)
  split_hours <- split(netflix$hours_viewed_2023[keep], group[keep])
  data.frame(
    comparison = comparison_name,
    group = names(split_hours),
    n = vapply(split_hours, length, integer(1)),
    median_hours = vapply(split_hours, median, numeric(1)),
    mean_hours = vapply(split_hours, mean, numeric(1)),
    row.names = NULL
  )
}

rq2_group_summary <- rbind(
  group_summary(
    ifelse(netflix$available_globally_ever, "Global", "Regional only"),
    "Global reach"
  ),
  group_summary(
    ifelse(
      is.na(netflix$exclusive),
      NA,
      ifelse(netflix$exclusive, "Netflix only among tracked platforms", "Shared")
    ),
    "Historical catalog status"
  ),
  group_summary(netflix$awareness_group, "Established IMDb awareness")
)

cat("GROUP SUMMARIES\n")
print(rq2_group_summary, row.names = FALSE)

# 14. Use Welch two-sample t-tests on log viewing hours.
# RQ1 showed that log viewing hours are approximately symmetric. Welch tests do
# not require the two groups to have equal variances or equal sample sizes.
run_welch_test <- function(group, comparison_name, reference_group, focal_group) {
  analysis_data <- data.frame(
    log_hours = netflix$log_hours_viewed_2023,
    group = group
  )
  analysis_data <- analysis_data[
    complete.cases(analysis_data) &
      analysis_data$group %in% c(reference_group, focal_group),
  ]
  analysis_data$group <- factor(
    analysis_data$group,
    levels = c(reference_group, focal_group)
  )
  test <- t.test(log_hours ~ group, data = analysis_data, var.equal = FALSE)
  group_means <- tapply(analysis_data$log_hours, analysis_data$group, mean)
  log_difference <- unname(group_means[focal_group] - group_means[reference_group])
  data.frame(
    comparison = comparison_name,
    reference_group = reference_group,
    focal_group = focal_group,
    mean_log_difference = log_difference,
    approximate_geometric_mean_ratio = exp(log_difference),
    t_statistic = -unname(test$statistic),
    degrees_of_freedom = unname(test$parameter),
    p_value = test$p.value,
    confidence_low_log = -unname(test$conf.int[2]),
    confidence_high_log = -unname(test$conf.int[1])
  )
}

global_group <- ifelse(
  netflix$available_globally_ever,
  "Global",
  "Regional only"
)
catalog_group <- ifelse(
  is.na(netflix$exclusive),
  NA,
  ifelse(netflix$exclusive, "Netflix only among tracked platforms", "Shared")
)

rq2_test_results <- rbind(
  run_welch_test(
    global_group,
    "Global reach",
    "Regional only",
    "Global"
  ),
  run_welch_test(
    catalog_group,
    "Historical catalog status",
    "Shared",
    "Netflix only among tracked platforms"
  ),
  run_welch_test(
    netflix$awareness_group,
    "Established IMDb awareness",
    "Low awareness",
    "High awareness"
  )
)

cat("\nWELCH TESTS ON LOG VIEWING HOURS\n")
print(rq2_test_results, row.names = FALSE, digits = 4)

# Interpretation:
# Each geometric-mean ratio compares the focal group with the reference group.
# A ratio above 1 indicates higher typical viewing for the focal group. These
# are unadjusted comparisons and do not establish that group membership caused
# the difference; later regression will account for multiple characteristics.
cat("\nInterpretation: Global reach and established awareness correspond with higher viewing in these unadjusted comparisons. Historical Netflix-only status among the tracked platforms does not provide the same clear viewing advantage. These results describe group differences, not causal effects.\n\n")

write.csv(
  rq2_group_summary,
  file.path(rq2_output_dir, "rq2_group_summary.csv"),
  row.names = FALSE
)
write.csv(
  rq2_test_results,
  file.path(rq2_output_dir, "rq2_welch_test_results.csv"),
  row.names = FALSE
)

# 15. Visualize all three comparisons in one compact figure.
# The data are analyzed on the log scale, but the y-axis is labeled in raw
# viewing-hour units so the figure remains understandable to a business audience.
png(
  file.path(rq2_output_dir, "rq2_viewing_hours_group_comparisons.png"),
  width = 1800,
  height = 700,
  res = 150
)
par(mfrow = c(1, 3), mar = c(7, 5, 4, 1), cex.main = 0.9)

viewing_axis_at <- log1p(c(1e5, 1e6, 1e7, 1e8, 1e9))
viewing_axis_labels <- c("0.1M", "1M", "10M", "100M", "1B")

draw_viewing_boxplot <- function(group, levels, labels, colors, title) {
  keep <- !is.na(group) & !is.na(netflix$log_hours_viewed_2023)
  plot_group <- factor(group[keep], levels = levels)
  boxplot(
    netflix$log_hours_viewed_2023[keep] ~ plot_group,
    names = labels,
    col = colors,
    border = netflix_black,
    outline = FALSE,
    axes = FALSE,
    xlab = "",
    ylab = "Viewing hours (log scale)",
    main = title
  )
  axis(1, at = seq_along(labels), labels = labels, tick = FALSE)
  axis(2, at = viewing_axis_at, labels = viewing_axis_labels, las = 1)
  box()
}

draw_viewing_boxplot(
  global_group,
  c("Regional only", "Global"),
  c("Regional only", "Global"),
  c(netflix_light_gray, netflix_red),
  "Viewing by global reach"
)
draw_viewing_boxplot(
  catalog_group,
  c("Shared", "Netflix only among tracked platforms"),
  c("Also on tracked\ncompetitor", "Netflix only among\ntracked platforms"),
  c(netflix_light_gray, netflix_dark_red),
  "Viewing by 2021 catalog status"
)
draw_viewing_boxplot(
  netflix$awareness_group,
  c("Low awareness", "High awareness"),
  c("Low awareness", "High awareness"),
  c(netflix_light_gray, netflix_red),
  "Viewing by established awareness"
)
dev.off()

# 16. Create a presentation-ready Netflix-themed results table for RQ2.
# This graphic summarizes the three comparisons using concise business language.
png(
  file.path(rq2_output_dir, "rq2_slide_results_table.png"),
  width = 1800,
  height = 720,
  res = 150,
  bg = netflix_black
)
par(mar = c(0, 0, 0, 0), bg = netflix_black)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(
  0.04, 0.91,
  "WHAT DRIVES DIFFERENCES IN NETFLIX VIEWING?",
  adj = c(0, 0.5), col = "white", cex = 1.7, font = 2
)
segments(0.04, 0.855, 0.96, 0.855, col = netflix_red, lwd = 5)

column_x <- c(0.05, 0.35, 0.61, 0.82)
headers <- c("COMPARISON", "MEDIAN VIEWING", "ESTIMATED DIFFERENCE", "TEST RESULT")
for (i in seq_along(headers)) {
  text(column_x[i], 0.78, headers[i], adj = c(0, 0.5),
       col = netflix_light_gray, cex = 1.0, font = 2)
}

table_rows <- list(
  c("Global reach", "Global: 8.4M\nRegional: 7.2M", "Global ≈ 32% higher", "p = 0.042"),
  c("2021 catalog status", "Netflix-only: 7.7M\nShared: 12.4M", "Netflix-only ≈ 33% lower", "p = 0.055"),
  c("IMDb awareness", "High: 15.6M\nLow: 3.6M", "High ≈ 5× higher", "p < 0.001")
)
row_y <- c(0.62, 0.42, 0.22)

for (r in seq_along(table_rows)) {
  if (r %% 2 == 1) {
    rect(0.035, row_y[r] - 0.085, 0.965, row_y[r] + 0.085,
         col = "#202020", border = NA)
  }
  for (i in seq_along(table_rows[[r]])) {
    text(
      column_x[i], row_y[r], table_rows[[r]][i],
      adj = c(0, 0.5),
      col = if (i == 3) netflix_red else "white",
      cex = if (i == 1) 1.12 else 1.02,
      font = if (i %in% c(1, 3)) 2 else 1
    )
  }
}

text(
  0.04, 0.07,
  "Welch tests use log-transformed viewing hours. Results are unadjusted comparisons, not causal effects.",
  adj = c(0, 0.5), col = netflix_light_gray, cex = 0.88
)
dev.off()

cat("RESEARCH QUESTION 2 COMPLETE\n")
cat("Research Question 2 outputs saved to:", normalizePath(rq2_output_dir), "\n\n")

# -----------------------------------------------------------------------------
# RESEARCH QUESTION 3
# How accurately can established IMDb popularity predict Netflix viewing hours?
# -----------------------------------------------------------------------------

cat("RESEARCH QUESTION 3\n")
cat("===================\n\n")

# 17. Build the baseline modeling dataset.
# The outcome and predictor use the log1p transformations justified in RQ1.
# Only the two records missing IMDb awareness are excluded.
rq3_model_data <- netflix[
  is.finite(netflix$log_hours_viewed_2023) &
    is.finite(netflix$log_imdb_ratecount_2021),
  c(
    "title", "hours_viewed_2023", "imdb_ratecount_2021",
    "log_hours_viewed_2023", "log_imdb_ratecount_2021"
  )
]

# 18. Create the requested reproducible 70/30 train-test split.
# The model is estimated only on training titles; the test titles provide an
# out-of-sample evaluation of predictive accuracy.
set.seed(123)
training_rows <- sample(
  seq_len(nrow(rq3_model_data)),
  size = floor(0.70 * nrow(rq3_model_data)),
  replace = FALSE
)
rq3_train <- rq3_model_data[training_rows, ]
rq3_test <- rq3_model_data[-training_rows, ]

cat("Modeling titles:", nrow(rq3_model_data), "\n")
cat("Training titles:", nrow(rq3_train), "\n")
cat("Testing titles:", nrow(rq3_test), "\n\n")

# 19. Fit the one-predictor baseline linear regression on the training data.
rq3_baseline_model <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021,
  data = rq3_train
)
rq3_model_summary <- summary(rq3_baseline_model)
rq3_coefficient <- coef(rq3_baseline_model)[["log_imdb_ratecount_2021"]]
rq3_change_for_10_percent <- ((1.10 ^ rq3_coefficient) - 1) * 100

cat("BASELINE MODEL\n")
print(rq3_model_summary)
cat(
  "Approximate viewing change for 10% more IMDb ratings:",
  round(rq3_change_for_10_percent, 2), "%\n\n"
)

# Interpretation:
# The slope measures how established IMDb awareness relates to later Netflix
# viewing on a percentage scale. Statistical significance indicates a reliable
# relationship, while predictive usefulness is evaluated separately below.
cat("Interpretation: Established IMDb awareness is a statistically significant positive predictor. A 10% increase in rating count corresponds with an estimated percentage increase in viewing shown above, but predictive accuracy must be judged on the held-out test titles.\n\n")

# 20. Generate train and test predictions and calculate accuracy metrics.
rq3_train$predicted_log_hours <- predict(
  rq3_baseline_model,
  newdata = rq3_train
)
rq3_test$predicted_log_hours <- predict(
  rq3_baseline_model,
  newdata = rq3_test
)

prediction_metrics <- function(actual, predicted, dataset_name) {
  errors <- actual - predicted
  data.frame(
    dataset = dataset_name,
    titles = length(actual),
    r_squared = 1 - sum(errors^2) / sum((actual - mean(actual))^2),
    rmse_log = sqrt(mean(errors^2)),
    mae_log = mean(abs(errors)),
    approximate_mae_factor = exp(mean(abs(errors)))
  )
}

rq3_validation_results <- rbind(
  prediction_metrics(
    rq3_train$log_hours_viewed_2023,
    rq3_train$predicted_log_hours,
    "Training"
  ),
  prediction_metrics(
    rq3_test$log_hours_viewed_2023,
    rq3_test$predicted_log_hours,
    "Testing"
  )
)

cat("PREDICTIVE VALIDATION\n")
print(rq3_validation_results, row.names = FALSE, digits = 4)

# Interpretation:
# Test R-squared measures the proportion of variation explained in titles that
# were not used to fit the model. The approximate MAE factor translates the log
# MAE into an intuitive multiplicative error; for example, 3 means predictions
# are typically off by roughly a factor of three in either direction.
cat("\nInterpretation: The held-out test R-squared shows that awareness explains a meaningful but limited share of viewing differences. Similar training and testing results indicate little evidence of overfitting, while the remaining error confirms that awareness alone is not sufficient for title-level decisions.\n\n")

write.csv(
  rq3_validation_results,
  file.path(rq3_output_dir, "rq3_validation_metrics.csv"),
  row.names = FALSE
)
write.csv(
  rq3_test,
  file.path(rq3_output_dir, "rq3_test_predictions.csv"),
  row.names = FALSE
)

# 21. Perform the requested residual analysis on the training model.
# Only residuals versus fitted values and the residual distribution are shown.
rq3_residuals <- residuals(rq3_baseline_model)
rq3_fitted <- fitted(rq3_baseline_model)

png(
  file.path(rq3_output_dir, "rq3_residual_diagnostics.png"),
  width = 1500,
  height = 700,
  res = 150
)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
plot(
  rq3_fitted,
  rq3_residuals,
  pch = 16,
  cex = 0.65,
  col = adjustcolor(netflix_red, alpha.f = 0.35),
  xlab = "Fitted log viewing hours",
  ylab = "Residual",
  main = "Residuals vs. fitted values"
)
abline(h = 0, col = netflix_black, lwd = 2, lty = 2)
hist(
  rq3_residuals,
  breaks = 35,
  col = netflix_dark_red,
  border = "white",
  xlab = "Residual",
  ylab = "Number of training titles",
  main = "Distribution of residuals"
)
abline(v = 0, col = netflix_black, lwd = 2, lty = 2)
dev.off()

# 22. Plot actual versus predicted viewing for the held-out titles.
png(
  file.path(rq3_output_dir, "rq3_actual_vs_predicted.png"),
  width = 1000,
  height = 850,
  res = 150
)
plot(
  rq3_test$predicted_log_hours,
  rq3_test$log_hours_viewed_2023,
  pch = 16,
  cex = 0.75,
  col = adjustcolor(netflix_red, alpha.f = 0.45),
  xlab = "Predicted log viewing hours",
  ylab = "Actual log viewing hours",
  main = "Baseline predictions for held-out TV shows"
)
abline(a = 0, b = 1, col = netflix_black, lwd = 2, lty = 2)
dev.off()

# Interpretation:
# Points on the dashed line are predicted accurately. Vertical distance from the
# line represents prediction error. Wide scatter indicates that awareness alone
# cannot distinguish many sleeper hits and underperformers.
cat("Residual interpretation: Residuals are centered near zero, but their spread shows substantial unexplained title-level variation. The actual-versus-predicted plot likewise shows that IMDb awareness alone provides only a baseline forecast.\n\n")

# 23. Create a presentation-ready Netflix-themed validation table.
# Values are taken directly from the reproducible train-test analysis above.
png(
  file.path(rq3_output_dir, "rq3_slide_validation_table.png"),
  width = 1800,
  height = 700,
  res = 150,
  bg = netflix_black
)
par(mar = c(0, 0, 0, 0), bg = netflix_black)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(
  0.04, 0.90,
  "HOW ACCURATELY DOES IMDb AWARENESS PREDICT VIEWING?",
  adj = c(0, 0.5), col = "white", cex = 1.55, font = 2
)
segments(0.04, 0.845, 0.96, 0.845, col = netflix_red, lwd = 5)

rq3_column_x <- c(0.05, 0.29, 0.48, 0.65, 0.82)
rq3_headers <- c("SAMPLE", "TITLES", "R²", "RMSE", "MAE")
for (i in seq_along(rq3_headers)) {
  text(
    rq3_column_x[i], 0.75, rq3_headers[i],
    adj = c(0, 0.5), col = netflix_light_gray, cex = 1.0, font = 2
  )
}

rq3_rows <- list(
  c("Training", "875", "0.269", "1.701", "1.364"),
  c("Testing", "375", "0.320", "1.623", "1.263")
)
rq3_row_y <- c(0.57, 0.36)

for (r in seq_along(rq3_rows)) {
  rect(
    0.035, rq3_row_y[r] - 0.085, 0.965, rq3_row_y[r] + 0.085,
    col = if (r == 2) "#2A1113" else "#202020", border = NA
  )
  for (i in seq_along(rq3_rows[[r]])) {
    text(
      rq3_column_x[i], rq3_row_y[r], rq3_rows[[r]][i],
      adj = c(0, 0.5),
      col = if (r == 2 && i == 3) netflix_red else "white",
      cex = 1.15,
      font = if (i == 1 || (r == 2 && i == 3)) 2 else 1
    )
  }
}

text(
  0.04, 0.14,
  "70/30 split with seed 123. RMSE and MAE use log-transformed viewing hours.",
  adj = c(0, 0.5), col = netflix_light_gray, cex = 0.88
)
dev.off()

cat("RESEARCH QUESTION 3 COMPLETE\n")
cat("Research Question 3 outputs saved to:", normalizePath(rq3_output_dir), "\n\n")

# -----------------------------------------------------------------------------
# RESEARCH QUESTION 4
# How much do quality, content, distribution, and catalog characteristics
# improve prediction beyond the established-awareness baseline?
# -----------------------------------------------------------------------------

cat("RESEARCH QUESTION 4\n")
cat("===================\n\n")

# 24. Create additional predictors using simple, interpretable transformations.
# No interactions, polynomial terms, or automated variable selection are used.
netflix$rotten_tomatoes_per_10 <- netflix$rotten_tomatoes_numeric / 10
netflix$log_seasons <- log1p(netflix$n_distinct_seasons)
netflix$title_age_2023 <- 2023 - netflix$year
netflix$title_type <- factor(
  netflix$imdb_title_type,
  levels = c("tvMiniSeries", "tvSeries")
)
netflix$age_group <- factor(
  netflix$age,
  levels = c("16+", "18+", "7+", "all", "Unknown")
)
netflix$global_any <- as.integer(netflix$available_globally_ever)
netflix$global_both_periods <- as.integer(netflix$available_globally_always)
netflix$historically_single_platform <- as.integer(netflix$exclusive)

rq4_required <- c(
  "log_hours_viewed_2023", "log_imdb_ratecount_2021",
  "imdb_rating_catalog_2021", "rotten_tomatoes_per_10",
  "log_seasons", "title_age_2023", "title_type", "age_group",
  "global_any", "global_both_periods", "historically_single_platform",
  "n_platforms"
)

# Use the RQ3 split assignments and then retain complete cases for every RQ4
# predictor. This prevents model comparisons from being driven by sample changes.
rq4_train <- netflix[netflix$title %in% rq3_train$title, ]
rq4_test <- netflix[netflix$title %in% rq3_test$title, ]
rq4_train <- rq4_train[complete.cases(rq4_train[, rq4_required]), ]
rq4_test <- rq4_test[complete.cases(rq4_test[, rq4_required]), ]

cat("Common training titles:", nrow(rq4_train), "\n")
cat("Common testing titles:", nrow(rq4_test), "\n")
cat("Titles excluded for incomplete RQ4 predictors:",
    nrow(netflix) - nrow(rq4_train) - nrow(rq4_test), "\n\n")

# 25. Estimate the prespecified model progression.
# Each model adds one conceptually related block of predictors.
rq4_model_1 <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021,
  data = rq4_train
)
rq4_model_2 <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021 +
    imdb_rating_catalog_2021 + rotten_tomatoes_per_10,
  data = rq4_train
)
rq4_model_3 <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021 +
    imdb_rating_catalog_2021 + rotten_tomatoes_per_10 +
    log_seasons + title_age_2023 + title_type + age_group,
  data = rq4_train
)
rq4_model_4 <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021 +
    imdb_rating_catalog_2021 + rotten_tomatoes_per_10 +
    log_seasons + title_age_2023 + title_type + age_group +
    global_any + global_both_periods + historically_single_platform +
    n_platforms,
  data = rq4_train
)

rq4_models <- list(
  "Awareness baseline" = rq4_model_1,
  "Awareness + quality" = rq4_model_2,
  "Content-informed" = rq4_model_3,
  "Full business model" = rq4_model_4
)

# 26. Evaluate every model on the same held-out titles.
rq4_model_metrics <- do.call(
  rbind,
  lapply(names(rq4_models), function(model_name) {
    model <- rq4_models[[model_name]]
    predictions <- predict(model, newdata = rq4_test)
    errors <- rq4_test$log_hours_viewed_2023 - predictions
    data.frame(
      model = model_name,
      predictors = length(coef(model)) - 1,
      training_adjusted_r_squared = summary(model)$adj.r.squared,
      test_r_squared = 1 - sum(errors^2) /
        sum((rq4_test$log_hours_viewed_2023 -
               mean(rq4_test$log_hours_viewed_2023))^2),
      test_rmse_log = sqrt(mean(errors^2)),
      test_mae_log = mean(abs(errors)),
      row.names = NULL
    )
  })
)

# Save the awareness-only predictions on the common RQ4 testing sample. These
# values keep the standalone baseline summary and the model-progression table
# on exactly the same 371 held-out titles.
rq4_awareness_test_predictions <- data.frame(
  title = rq4_test$title,
  actual_log_hours = rq4_test$log_hours_viewed_2023,
  predicted_log_hours = as.numeric(predict(rq4_model_1, newdata = rq4_test))
)
rq4_awareness_test_predictions$residual <-
  rq4_awareness_test_predictions$actual_log_hours -
  rq4_awareness_test_predictions$predicted_log_hours
write.csv(
  rq4_awareness_test_predictions,
  file.path(rq4_output_dir, "rq4_awareness_baseline_test_predictions.csv"),
  row.names = FALSE
)

cat("MODEL PROGRESSION\n")
print(rq4_model_metrics, row.names = FALSE, digits = 4)

# 27. Test whether each added predictor block improves training-sample fit.
# These partial F-tests complement—but do not replace—the held-out accuracy.
rq4_nested_tests_raw <- anova(
  rq4_model_1, rq4_model_2, rq4_model_3, rq4_model_4
)
rq4_nested_tests <- data.frame(
  comparison = c(
    "Add rating quality",
    "Add content characteristics",
    "Add distribution and catalog measures"
  ),
  added_degrees_of_freedom = rq4_nested_tests_raw$Df[2:4],
  f_statistic = rq4_nested_tests_raw$F[2:4],
  p_value = rq4_nested_tests_raw$`Pr(>F)`[2:4]
)

cat("\nNESTED MODEL TESTS\n")
print(rq4_nested_tests, row.names = FALSE, digits = 4)

# Select the simplest model within 0.01 of the best held-out R-squared. This
# avoids adding an entire predictor block for a negligible accuracy change and
# is a prespecified parsimony rule, not backward elimination.
best_test_r_squared <- max(rq4_model_metrics$test_r_squared)
eligible_models <- which(
  rq4_model_metrics$test_r_squared >= best_test_r_squared - 0.01
)
selected_index <- eligible_models[
  which.min(rq4_model_metrics$predictors[eligible_models])
]
rq4_selected_name <- rq4_model_metrics$model[selected_index]
rq4_selected_model <- rq4_models[[rq4_selected_name]]

cat("\nSelected model:", rq4_selected_name, "\n")
cat("Selected-model coefficients:\n")
print(summary(rq4_selected_model)$coefficients)

# Interpretation:
# The important question is whether each block improves held-out R-squared and
# reduces RMSE/MAE. If the final distribution block does not improve those
# metrics, the more parsimonious content-informed model is preferred.
cat("\nInterpretation: Rating quality provides a modest improvement over awareness alone. Content depth, title age, format, and audience classification provide the largest gain. The distribution and historical catalog block is retained only if it improves held-out accuracy; otherwise the simpler content-informed model is preferred.\n\n")

write.csv(
  rq4_model_metrics,
  file.path(rq4_output_dir, "rq4_model_progression_metrics.csv"),
  row.names = FALSE
)
write.csv(
  rq4_nested_tests,
  file.path(rq4_output_dir, "rq4_nested_model_tests.csv"),
  row.names = FALSE
)

rq4_selected_coefficients <- data.frame(
  term = rownames(summary(rq4_selected_model)$coefficients),
  summary(rq4_selected_model)$coefficients,
  row.names = NULL,
  check.names = FALSE
)
write.csv(
  rq4_selected_coefficients,
  file.path(rq4_output_dir, "rq4_selected_model_coefficients.csv"),
  row.names = FALSE
)

# 28. Visualize the improvement in held-out predictive accuracy.
png(
  file.path(rq4_output_dir, "rq4_model_progression.png"),
  width = 1300,
  height = 800,
  res = 150
)
model_colors <- c(
  netflix_light_gray, netflix_gray, netflix_red, netflix_black
)
bar_positions <- barplot(
  rq4_model_metrics$test_r_squared,
  names.arg = c("Awareness", "Awareness +\nquality", "Content-\ninformed", "Full business"),
  col = model_colors,
  border = NA,
  ylim = c(0, max(rq4_model_metrics$test_r_squared) + 0.10),
  las = 1,
  ylab = "Test-set R²",
  main = "Content characteristics provide the largest predictive gain"
)
text(
  bar_positions,
  rq4_model_metrics$test_r_squared,
  labels = sprintf("%.3f", rq4_model_metrics$test_r_squared),
  pos = 3,
  font = 2,
  cex = 1.0
)
dev.off()

# Create a slide-ready Netflix-themed table of the RQ4 model progression.
png(
  file.path(rq4_output_dir, "rq4_slide_model_comparison_table.png"),
  width = 1800,
  height = 820,
  res = 150,
  bg = netflix_black
)
par(mar = c(0, 0, 0, 0), bg = netflix_black)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(
  0.04, 0.92,
  "WHICH INFORMATION IMPROVES VIEWING PREDICTIONS?",
  adj = c(0, 0.5), col = "white", cex = 1.6, font = 2
)
segments(0.04, 0.87, 0.96, 0.87, col = netflix_red, lwd = 5)

rq4_column_x <- c(0.05, 0.40, 0.61, 0.75, 0.88)
rq4_headers <- c("MODEL", "PREDICTORS", "TEST R²", "RMSE", "MAE")
for (i in seq_along(rq4_headers)) {
  text(
    rq4_column_x[i], 0.80, rq4_headers[i],
    adj = c(0, 0.5), col = netflix_light_gray, cex = 0.98, font = 2
  )
}

rq4_table_labels <- c(
  "Awareness baseline",
  "Awareness + quality",
  "Content-informed  •  SELECTED",
  "Full business model"
)
rq4_table_rows <- lapply(seq_len(nrow(rq4_model_metrics)), function(i) {
  c(
    rq4_table_labels[i],
    as.character(rq4_model_metrics$predictors[i]),
    sprintf("%.3f", rq4_model_metrics$test_r_squared[i]),
    sprintf("%.3f", rq4_model_metrics$test_rmse_log[i]),
    sprintf("%.3f", rq4_model_metrics$test_mae_log[i])
  )
})
rq4_row_y <- c(0.68, 0.53, 0.38, 0.23)

for (r in seq_along(rq4_table_rows)) {
  rect(
    0.035, rq4_row_y[r] - 0.062, 0.965, rq4_row_y[r] + 0.062,
    col = if (r == 3) "#351012" else if (r %% 2 == 1) "#202020" else netflix_black,
    border = NA
  )
  for (i in seq_along(rq4_table_rows[[r]])) {
    text(
      rq4_column_x[i], rq4_row_y[r], rq4_table_rows[[r]][i],
      adj = c(0, 0.5),
      col = if (r == 3 && i %in% c(1, 3)) netflix_red else "white",
      cex = 1.08,
      font = if (i == 1 || (r == 3 && i == 3)) 2 else 1
    )
  }
}

text(
  0.04, 0.09,
  "All models evaluated on the same 371 held-out titles. RMSE and MAE use log viewing hours.",
  adj = c(0, 0.5), col = netflix_light_gray, cex = 0.88
)
dev.off()

# 29. Produce residuals-versus-fitted and residual-distribution plots for every
# estimated training model, as required for all subsequent model analyses.
for (i in seq_along(rq4_models)) {
  model <- rq4_models[[i]]
  safe_name <- c("model_1", "model_2", "model_3", "model_4")[i]
  png(
    file.path(rq4_output_dir, paste0("rq4_", safe_name, "_residual_diagnostics.png")),
    width = 1500,
    height = 700,
    res = 150
  )
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
  plot(
    fitted(model), residuals(model),
    pch = 16, cex = 0.65,
    col = adjustcolor(netflix_red, alpha.f = 0.35),
    xlab = "Fitted log viewing hours", ylab = "Residual",
    main = paste(names(rq4_models)[i], "— residuals vs. fitted")
  )
  abline(h = 0, col = netflix_black, lwd = 2, lty = 2)
  hist(
    residuals(model), breaks = 35,
    col = netflix_dark_red, border = "white",
    xlab = "Residual", ylab = "Number of training titles",
    main = paste(names(rq4_models)[i], "— residual distribution")
  )
  abline(v = 0, col = netflix_black, lwd = 2, lty = 2)
  dev.off()
}

# 30. Show actual versus predicted values for the selected held-out model.
rq4_test_predictions <- rq4_test[, c("title", "hours_viewed_2023")]
rq4_test_predictions$actual_log_hours <- rq4_test$log_hours_viewed_2023
rq4_test_predictions$predicted_log_hours <- predict(
  rq4_selected_model,
  newdata = rq4_test
)
rq4_test_predictions$predicted_hours <- pmax(
  0,
  expm1(rq4_test_predictions$predicted_log_hours)
)
rq4_test_predictions$residual <-
  rq4_test_predictions$actual_log_hours -
  rq4_test_predictions$predicted_log_hours

write.csv(
  rq4_test_predictions,
  file.path(rq4_output_dir, "rq4_selected_model_test_predictions.csv"),
  row.names = FALSE
)

png(
  file.path(rq4_output_dir, "rq4_selected_actual_vs_predicted.png"),
  width = 1000,
  height = 850,
  res = 150
)
plot(
  rq4_test_predictions$predicted_log_hours,
  rq4_test_predictions$actual_log_hours,
  pch = 16, cex = 0.75,
  col = adjustcolor(netflix_red, alpha.f = 0.45),
  xlab = "Predicted log viewing hours",
  ylab = "Actual log viewing hours",
  main = paste(rq4_selected_name, "predictions")
)
abline(a = 0, b = 1, col = netflix_black, lwd = 2, lty = 2)
dev.off()

cat("Residual interpretation: The selected model residuals remain centered near zero. Compared with the awareness baseline, their smaller spread indicates improved prediction, although extreme positive and negative residuals remain for later title classification.\n\n")

cat("RESEARCH QUESTION 4 COMPLETE\n")
cat("Research Question 4 outputs saved to:", normalizePath(rq4_output_dir), "\n\n")

# -----------------------------------------------------------------------------
# RESEARCH QUESTION 5
# Which titles receive substantially more or less viewing than the selected
# content-informed model predicts?
# -----------------------------------------------------------------------------

cat("RESEARCH QUESTION 5\n")
cat("===================\n\n")

# 31. Create the Question 5 modeling sample using the predictors in the selected
# content-informed model. Distribution variables are not required because they
# did not materially improve held-out accuracy in Question 4.
rq5_required <- c(
  "log_hours_viewed_2023", "log_imdb_ratecount_2021",
  "imdb_rating_catalog_2021", "rotten_tomatoes_per_10",
  "log_seasons", "title_age_2023", "title_type", "age_group"
)
rq5_data <- netflix[complete.cases(netflix[, rq5_required]), ]

cat("Titles in the full dataset:", nrow(netflix), "\n")
cat("Titles eligible for Question 5:", nrow(rq5_data), "\n")
cat("Titles excluded for missing model inputs:",
    nrow(netflix) - nrow(rq5_data), "\n\n")

# 32. Generate 10-fold out-of-fold predictions with seed 123.
# Every prediction comes from a model that did not use that title for fitting.
rq5_formula <- formula(rq4_model_3)
set.seed(123)
rq5_folds <- sample(rep(1:10, length.out = nrow(rq5_data)))
rq5_data$predicted_log_hours <- NA_real_

for (fold in 1:10) {
  fold_model <- lm(
    rq5_formula,
    data = rq5_data[rq5_folds != fold, ]
  )
  rq5_data$predicted_log_hours[rq5_folds == fold] <- predict(
    fold_model,
    newdata = rq5_data[rq5_folds == fold, ]
  )
}

# 33. Calculate actual-versus-expected performance measures.
# The residual is the primary statistical measure. The viewing-yield ratio is
# the same difference translated into a more intuitive multiplicative scale.
rq5_data$predicted_hours <- pmax(0, expm1(rq5_data$predicted_log_hours))
rq5_data$residual_log <-
  rq5_data$log_hours_viewed_2023 - rq5_data$predicted_log_hours
rq5_data$viewing_yield_ratio <- exp(rq5_data$residual_log)
rq5_data$raw_viewing_rank <- rank(
  -rq5_data$hours_viewed_2023,
  ties.method = "min"
)
rq5_data$viewing_yield_rank <- rank(
  -rq5_data$residual_log,
  ties.method = "min"
)
rq5_data$rank_improvement <-
  rq5_data$raw_viewing_rank - rq5_data$viewing_yield_rank

# Define substantial outperformance and underperformance using the top and
# bottom 10% of out-of-fold residuals. The middle 80% remain expected performers.
rq5_lower_threshold <- unname(quantile(rq5_data$residual_log, 0.10))
rq5_upper_threshold <- unname(quantile(rq5_data$residual_log, 0.90))
rq5_data$performance_group <- ifelse(
  rq5_data$residual_log >= rq5_upper_threshold,
  "Substantially above prediction",
  ifelse(
    rq5_data$residual_log <= rq5_lower_threshold,
    "Substantially below prediction",
    "Within expected range"
  )
)

rq5_oof_r_squared <- 1 -
  sum((rq5_data$log_hours_viewed_2023 - rq5_data$predicted_log_hours)^2) /
  sum((rq5_data$log_hours_viewed_2023 -
         mean(rq5_data$log_hours_viewed_2023))^2)
rq5_oof_rmse <- sqrt(mean(rq5_data$residual_log^2))
rq5_oof_mae <- mean(abs(rq5_data$residual_log))

cat("OUT-OF-FOLD PERFORMANCE\n")
cat("R-squared:", round(rq5_oof_r_squared, 3), "\n")
cat("RMSE:", round(rq5_oof_rmse, 3), "\n")
cat("MAE:", round(rq5_oof_mae, 3), "\n")
cat("Lower residual threshold:", round(rq5_lower_threshold, 3), "\n")
cat("Upper residual threshold:", round(rq5_upper_threshold, 3), "\n")
cat("\nPerformance groups:\n")
print(table(rq5_data$performance_group))

# 34. Rank titles above and below their content-informed expectations.
rq5_output_columns <- c(
  "title", "hours_viewed_2023", "predicted_hours",
  "residual_log", "viewing_yield_ratio", "imdb_ratecount_2021",
  "n_distinct_seasons", "raw_viewing_rank", "viewing_yield_rank",
  "rank_improvement", "performance_group"
)

rq5_above <- rq5_data[
  order(rq5_data$residual_log, decreasing = TRUE),
  rq5_output_columns
]
rq5_below <- rq5_data[
  order(rq5_data$residual_log, decreasing = FALSE),
  rq5_output_columns
]

cat("\nTOP 15 TITLES ABOVE PREDICTION\n")
print(rq5_above[1:15, ], row.names = FALSE, digits = 4)
cat("\nTOP 15 TITLES BELOW PREDICTION\n")
print(rq5_below[1:15, ], row.names = FALSE, digits = 4)

# Interpretation:
# Positive residuals identify titles that converted their awareness, quality,
# and content profile into more viewing than expected. Negative residuals show
# the reverse. These are performance-screening signals, not causal estimates or
# automatic recommendations to promote, acquire, renew, or remove a title.
cat("\nInterpretation: The rankings identify titles whose viewing differs most from a content-informed expectation. Large positive residuals indicate unusually strong viewing yield; large negative residuals indicate unrealized expected viewing. Business action still depends on rights, costs, and marketing information not present in the dataset.\n\n")

write.csv(
  rq5_data[, rq5_output_columns],
  file.path(rq5_output_dir, "rq5_all_title_performance_rankings.csv"),
  row.names = FALSE
)
write.csv(
  rq5_above[1:25, ],
  file.path(rq5_output_dir, "rq5_top_25_above_prediction.csv"),
  row.names = FALSE
)
write.csv(
  rq5_below[1:25, ],
  file.path(rq5_output_dir, "rq5_top_25_below_prediction.csv"),
  row.names = FALSE
)

# 35. Visualize the most extreme positive and negative viewing gaps.
rq5_plot_above <- rq5_above[1:10, ]
rq5_plot_below <- rq5_below[1:10, ]

png(
  file.path(rq5_output_dir, "rq5_titles_furthest_from_prediction.png"),
  width = 1800,
  height = 900,
  res = 150
)
par(mfrow = c(1, 2), mar = c(5, 12, 4, 2))
barplot(
  rev(rq5_plot_above$residual_log),
  names.arg = rev(rq5_plot_above$title),
  horiz = TRUE,
  las = 1,
  col = netflix_red,
  border = NA,
  xlab = "Out-of-fold residual (log viewing hours)",
  main = "Most above prediction"
)
abline(v = 0, col = netflix_black, lwd = 1.5)
barplot(
  rq5_plot_below$residual_log,
  names.arg = rq5_plot_below$title,
  horiz = TRUE,
  las = 1,
  col = netflix_gray,
  border = NA,
  xlab = "Out-of-fold residual (log viewing hours)",
  main = "Most below prediction"
)
abline(v = 0, col = netflix_black, lwd = 1.5)
dev.off()

# 36. Show the distribution used to establish the top/bottom-decile thresholds.
png(
  file.path(rq5_output_dir, "rq5_residual_distribution_and_thresholds.png"),
  width = 1000,
  height = 750,
  res = 150
)
hist(
  rq5_data$residual_log,
  breaks = 40,
  col = netflix_dark_red,
  border = "white",
  xlab = "Actual minus predicted log viewing hours",
  ylab = "Number of titles",
  main = "Out-of-fold viewing-performance gaps"
)
abline(v = 0, col = netflix_black, lwd = 2, lty = 2)
abline(v = c(rq5_lower_threshold, rq5_upper_threshold),
       col = netflix_red, lwd = 2)
legend(
  "topleft",
  legend = c("Prediction", "Bottom/top 10% thresholds"),
  col = c(netflix_black, netflix_red),
  lty = c(2, 1),
  lwd = 2,
  bty = "o",
  bg = "white",
  box.col = "white",
  cex = 0.9,
  inset = 0.02
)
dev.off()

cat("RESEARCH QUESTION 5 COMPLETE\n")
cat("Research Question 5 outputs saved to:", normalizePath(rq5_output_dir), "\n\n")

# -----------------------------------------------------------------------------
# RESEARCH QUESTION 6
# Which titles and profiles warrant promotion, continued investment, strategic
# replication, or further review based on viewing yield and audience headroom?
# -----------------------------------------------------------------------------

cat("RESEARCH QUESTION 6\n")
cat("===================\n\n")

# 37. Define transparent business-screening thresholds.
# These thresholds create a manageable portfolio shortlist. They are not claims
# about profitability, ownership, or the causal effect of a business action.
rq6_awareness_median <- median(
  rq5_data$imdb_ratecount_2021,
  na.rm = TRUE
)
rq6_viewing_median <- median(rq5_data$hours_viewed_2023)
rq6_viewing_upper_quartile <- unname(
  quantile(rq5_data$hours_viewed_2023, 0.75)
)

rq5_data$business_segment <- "Expected performer"

# Low-awareness titles with top-decile viewing yield and at least median viewing
# have both demonstrated engagement and remaining awareness headroom.
promotion_condition <-
  rq5_data$residual_log >= rq5_upper_threshold &
  rq5_data$imdb_ratecount_2021 <= rq6_awareness_median &
  rq5_data$hours_viewed_2023 >= rq6_viewing_median
rq5_data$business_segment[promotion_condition] <-
  "Promotion candidate"

# Remaining top-decile yield titles are efficient relative to expectations but
# may have lower absolute scale or already-high awareness.
efficient_condition <-
  rq5_data$residual_log >= rq5_upper_threshold &
  rq5_data$business_segment == "Expected performer"
rq5_data$business_segment[efficient_condition] <-
  "Efficient niche / replication signal"

# High-scale titles that meet or exceed expectations are portfolio anchors.
leader_condition <-
  rq5_data$hours_viewed_2023 >= rq6_viewing_upper_quartile &
  rq5_data$residual_log >= 0 &
  rq5_data$business_segment == "Expected performer"
rq5_data$business_segment[leader_condition] <-
  "Proven engagement leader"

# High-awareness titles in the bottom yield decile indicate unrealized expected
# demand and warrant diagnosis rather than an automatic negative decision.
unrealized_condition <-
  rq5_data$residual_log <= rq5_lower_threshold &
  rq5_data$imdb_ratecount_2021 > rq6_awareness_median
rq5_data$business_segment[unrealized_condition] <-
  "Unrealized demand review"

# Low-awareness, bottom-decile titles provide limited evidence of current demand.
low_evidence_condition <-
  rq5_data$residual_log <= rq5_lower_threshold &
  rq5_data$imdb_ratecount_2021 <= rq6_awareness_median
rq5_data$business_segment[low_evidence_condition] <-
  "Low-evidence review"

rq6_segment_order <- c(
  "Promotion candidate",
  "Efficient niche / replication signal",
  "Proven engagement leader",
  "Unrealized demand review",
  "Low-evidence review",
  "Expected performer"
)
rq5_data$business_segment <- factor(
  rq5_data$business_segment,
  levels = rq6_segment_order
)

# 38. Summarize the resulting portfolio segments.
rq6_segment_summary <- data.frame(
  segment = rq6_segment_order,
  titles = as.integer(table(rq5_data$business_segment)[rq6_segment_order]),
  row.names = NULL
)
rq6_segment_summary$percent <-
  100 * rq6_segment_summary$titles / nrow(rq5_data)

segment_medians <- do.call(
  rbind,
  lapply(rq6_segment_order, function(segment_name) {
    segment_data <- rq5_data[
      rq5_data$business_segment == segment_name,
    ]
    data.frame(
      segment = segment_name,
      median_actual_hours = median(segment_data$hours_viewed_2023),
      median_predicted_hours = median(segment_data$predicted_hours),
      median_imdb_awareness = median(segment_data$imdb_ratecount_2021),
      median_viewing_yield = median(segment_data$viewing_yield_ratio),
      global_share_pct = 100 * mean(segment_data$available_globally_ever)
    )
  })
)
rq6_segment_summary <- merge(
  rq6_segment_summary,
  segment_medians,
  by = "segment",
  sort = FALSE
)
rq6_segment_summary <- rq6_segment_summary[
  match(rq6_segment_order, rq6_segment_summary$segment),
]

cat("BUSINESS SEGMENT SUMMARY\n")
print(rq6_segment_summary, row.names = FALSE, digits = 4)

# Interpretation:
# Promotion candidates combine demonstrated above-expectation viewing with
# awareness headroom and meaningful absolute scale. Proven leaders combine high
# scale with performance at or above expectations. Negative-yield groups are
# diagnostic review lists, not automatic cancellation or licensing decisions.
cat("\nInterpretation: The segments separate demonstrated audience conversion, high-scale portfolio value, and unrealized expected demand. They prioritize where Netflix should investigate or test an action; they do not determine the action without cost and rights information.\n\n")

# 39. Create title-level candidate lists with contextual action language.
rq5_data$screening_action <- ifelse(
  rq5_data$business_segment == "Promotion candidate" &
    !rq5_data$available_globally_ever,
  "Promotion and distribution-rights review",
  ifelse(
    rq5_data$business_segment == "Promotion candidate",
    "Promotion/discovery test",
    ifelse(
      rq5_data$business_segment == "Efficient niche / replication signal",
      "Review profile for replication or targeted discovery",
      ifelse(
        rq5_data$business_segment == "Proven engagement leader",
        "Rights, continuity, and investment review",
        ifelse(
          rq5_data$business_segment == "Unrealized demand review",
          "Diagnose availability, positioning, or audience fit",
          ifelse(
            rq5_data$business_segment == "Low-evidence review",
            "Review only with cost and strategic context",
            "Maintain and monitor"
          )
        )
      )
    )
  )
)

rq6_candidate_columns <- c(
  "title", "business_segment", "screening_action",
  "hours_viewed_2023", "predicted_hours", "viewing_yield_ratio",
  "imdb_ratecount_2021", "rotten_tomatoes_numeric",
  "n_distinct_seasons", "age", "available_globally_ever"
)
rq6_candidates <- rq5_data[
  rq5_data$business_segment != "Expected performer",
  rq6_candidate_columns
]
rq6_candidates <- rq6_candidates[
  order(
    rq6_candidates$business_segment,
    -rq6_candidates$viewing_yield_ratio
  ),
]

write.csv(
  rq6_segment_summary,
  file.path(rq6_output_dir, "rq6_segment_summary.csv"),
  row.names = FALSE
)
write.csv(
  rq6_candidates,
  file.path(rq6_output_dir, "rq6_title_candidates.csv"),
  row.names = FALSE
)
write.csv(
  rq5_data,
  file.path(rq6_output_dir, "rq6_all_titles_with_segments.csv"),
  row.names = FALSE
)

cat("TOP PROMOTION CANDIDATES\n")
promotion_titles <- rq5_data[
  rq5_data$business_segment == "Promotion candidate",
  rq6_candidate_columns
]
promotion_titles <- promotion_titles[
  order(promotion_titles$viewing_yield_ratio, decreasing = TRUE),
]
print(head(promotion_titles, 15), row.names = FALSE, digits = 4)

cat("\nTOP PROVEN ENGAGEMENT LEADERS\n")
leader_titles <- rq5_data[
  rq5_data$business_segment == "Proven engagement leader",
  rq6_candidate_columns
]
leader_titles <- leader_titles[
  order(leader_titles$hours_viewed_2023, decreasing = TRUE),
]
print(head(leader_titles, 15), row.names = FALSE, digits = 4)

cat("\nSTRONGEST UNREALIZED-DEMAND FLAGS\n")
review_titles <- rq5_data[
  rq5_data$business_segment == "Unrealized demand review",
  rq6_candidate_columns
]
review_titles <- review_titles[
  order(review_titles$viewing_yield_ratio, decreasing = FALSE),
]
print(head(review_titles, 15), row.names = FALSE, digits = 4)

# 40. Compare the observable profiles of top-yield titles with the full catalog.
# Lift above 1 means the characteristic is more common in the top residual decile.
rq5_data$season_profile <- ifelse(
  rq5_data$n_distinct_seasons == 1,
  "One season",
  ifelse(rq5_data$n_distinct_seasons <= 3, "Two to three seasons", "Four+ seasons")
)
rq5_data$high_critic_score <-
  rq5_data$rotten_tomatoes_numeric >=
  median(rq5_data$rotten_tomatoes_numeric, na.rm = TRUE)
rq5_data$low_awareness <-
  rq5_data$imdb_ratecount_2021 <= rq6_awareness_median
rq6_high_yield <- rq5_data$residual_log >= rq5_upper_threshold

profile_share <- function(condition) {
  c(
    full_catalog_pct = 100 * mean(condition, na.rm = TRUE),
    high_yield_pct = 100 * mean(condition[rq6_high_yield], na.rm = TRUE)
  )
}

rq6_profile_summary <- rbind(
  data.frame(profile = "Low IMDb awareness", t(profile_share(rq5_data$low_awareness))),
  data.frame(profile = "High critic score", t(profile_share(rq5_data$high_critic_score))),
  data.frame(profile = "Globally available", t(profile_share(rq5_data$available_globally_ever))),
  data.frame(profile = "Regular series", t(profile_share(rq5_data$title_type == "tvSeries"))),
  data.frame(profile = "One season", t(profile_share(rq5_data$season_profile == "One season"))),
  data.frame(profile = "Two to three seasons", t(profile_share(rq5_data$season_profile == "Two to three seasons"))),
  data.frame(profile = "Four+ seasons", t(profile_share(rq5_data$season_profile == "Four+ seasons"))),
  data.frame(profile = "Audience: all", t(profile_share(rq5_data$age_group == "all"))),
  data.frame(profile = "Audience: 7+", t(profile_share(rq5_data$age_group == "7+")))
)
rq6_profile_summary$percentage_point_difference <-
  rq6_profile_summary$high_yield_pct -
  rq6_profile_summary$full_catalog_pct
rq6_profile_summary$representation_ratio <-
  rq6_profile_summary$high_yield_pct /
  rq6_profile_summary$full_catalog_pct

cat("\nHIGH-YIELD TITLE PROFILE COMPARISON\n")
print(rq6_profile_summary, row.names = FALSE, digits = 4)
cat("\nInterpretation: Profile differences are descriptive signals for future content review. Because several characteristics were already included in the prediction model, these comparisons identify composition patterns rather than causal drivers of overperformance.\n\n")

write.csv(
  rq6_profile_summary,
  file.path(rq6_output_dir, "rq6_high_yield_profile_summary.csv"),
  row.names = FALSE
)

# 41. Visualize the number of titles in each screening segment.
png(
  file.path(rq6_output_dir, "rq6_business_segment_counts.png"),
  width = 1250,
  height = 850,
  res = 150
)
par(mar = c(5, 20, 4, 2))
segment_plot <- rq6_segment_summary[
  rq6_segment_summary$segment != "Expected performer",
]
segment_plot$display_label <- c(
  "Promotion candidates",
  "Efficient niche / replication",
  "Proven leaders",
  "Unrealized-demand review",
  "Low-evidence review"
)
segment_positions <- barplot(
  rev(segment_plot$titles),
  names.arg = rev(segment_plot$display_label),
  horiz = TRUE,
  las = 1,
  cex.names = 0.82,
  col = rev(c(
    netflix_red, netflix_dark_red, netflix_black,
    netflix_gray, netflix_light_gray
  )),
  border = NA,
  xlim = c(0, max(segment_plot$titles) * 1.22),
  xlab = "Number of titles",
  main = "Title-level business screening segments"
)
text(
  rev(segment_plot$titles),
  segment_positions,
  labels = rev(segment_plot$titles),
  pos = 4,
  font = 2
)
dev.off()

# 42. Visualize audience headroom and viewing yield together.
png(
  file.path(rq6_output_dir, "rq6_awareness_and_viewing_yield_map.png"),
  width = 1200,
  height = 900,
  res = 150
)
segment_colors <- c(
  "Promotion candidate" = netflix_red,
  "Efficient niche / replication signal" = netflix_dark_red,
  "Proven engagement leader" = netflix_black,
  "Unrealized demand review" = netflix_gray,
  "Low-evidence review" = netflix_light_gray,
  "Expected performer" = adjustcolor(netflix_light_gray, alpha.f = 0.25)
)
plot(
  rq5_data$log_imdb_ratecount_2021,
  rq5_data$residual_log,
  pch = 16,
  cex = 0.75,
  col = segment_colors[as.character(rq5_data$business_segment)],
  xlab = "Established awareness: log(1 + IMDb rating count)",
  ylab = "Viewing yield: actual minus predicted log hours",
  main = "Audience headroom and content-informed viewing yield"
)
abline(h = c(rq5_lower_threshold, 0, rq5_upper_threshold),
       col = c(netflix_gray, netflix_black, netflix_red),
       lty = c(2, 1, 2), lwd = c(1.5, 1.5, 1.5))
abline(v = log1p(rq6_awareness_median),
       col = netflix_black, lty = 3, lwd = 1.5)
legend(
  "topright",
  legend = rq6_segment_order[1:5],
  col = segment_colors[rq6_segment_order[1:5]],
  pch = 16,
  bty = "n",
  cex = 0.78
)
dev.off()

# 43. Visualize which observable profiles are more or less represented among
# titles in the top viewing-yield decile.
rq6_profile_plot <- rq6_profile_summary[
  order(rq6_profile_summary$percentage_point_difference),
]
profile_display_names <- c(
  "Low IMDb awareness" = "Low awareness",
  "High critic score" = "High critic score",
  "Globally available" = "Global availability",
  "Regular series" = "Regular series",
  "One season" = "One season",
  "Two to three seasons" = "2–3 seasons",
  "Four+ seasons" = "4+ seasons",
  "Audience: all" = "All-audience",
  "Audience: 7+" = "Audience 7+"
)
rq6_profile_plot$display_label <- unname(
  profile_display_names[rq6_profile_plot$profile]
)
png(
  file.path(rq6_output_dir, "rq6_high_yield_profile_differences.png"),
  width = 1250,
  height = 850,
  res = 150
)
par(mar = c(5, 14, 4, 2))
profile_positions <- barplot(
  rq6_profile_plot$percentage_point_difference,
  names.arg = rq6_profile_plot$display_label,
  horiz = TRUE,
  las = 1,
  cex.names = 0.9,
  col = ifelse(
    rq6_profile_plot$percentage_point_difference >= 0,
    netflix_red,
    netflix_gray
  ),
  border = NA,
  xlab = "Difference from full catalog (percentage points)",
  main = "Profiles more common among top-yield titles"
)
abline(v = 0, col = netflix_black, lwd = 1.5)
dev.off()

cat("RESEARCH QUESTION 6 COMPLETE\n")
cat("Research Question 6 outputs saved to:", normalizePath(rq6_output_dir), "\n\n")

sink()
