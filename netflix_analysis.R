# NETFLIX TV SHOW RESEARCH PROJECT
# Parts 1 and 2: Data preparation, Research Question 1, and Research Question 2
#
# Guiding question:
# What distinguishes Netflix's blockbusters, sleeper hits, and underperforming
# TV shows, and how can Netflix use those differences to improve content
# promotion, distribution, acquisition, and retention decisions?
#
# Research Question 1:
# How are viewing hours, IMDb awareness, rating quality, global availability,
# and historical catalog differentiation distributed across Netflix TV shows?
#
# This script intentionally stops after Research Question 2. It does not run
# predictive regressions or title classifications. Those sections will be added
# only after this work is reviewed.

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

input_file <- file.path(project_dir, "netflix_research_data.csv")
output_dir <- file.path(project_dir, "eda_outputs", "research_question_1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rq2_output_dir <- file.path(project_dir, "eda_outputs", "research_question_2")
dir.create(rq2_output_dir, recursive = TRUE, showWarnings = FALSE)

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
# The combined file contains 1,260 unique titles and 1,260 unique IMDb IDs, so
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
  netflix$exclusive,
  "Historically single-platform",
  "Historically shared"
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
cat("\nInterpretation: The main outcome and awareness measure are complete. IMDb score is missing for 15 titles, while age and release-date fields have substantial missingness.\n\n")

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
cat("Mean IMDb rating count:", format(round(mean(netflix$imdb_ratecount_2021)), big.mark = ","), "\n")
cat("Median IMDb rating count:", format(median(netflix$imdb_ratecount_2021), big.mark = ","), "\n")
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

png(
  file.path(output_dir, "02_awareness_and_rating_distributions.png"),
  width = 1600,
  height = 650,
  res = 150
)
par(mfrow = c(1, 3), mar = c(5, 5, 4, 1))
hist(
  netflix$log_imdb_ratecount_2021,
  breaks = 35,
  col = netflix_gray,
  border = "white",
  main = "Established IMDb awareness",
  xlab = "log(1 + IMDb rating count)",
  ylab = "Number of titles"
)
hist(
  netflix$imdb_rating_catalog_2021,
  breaks = seq(1, 10, by = 0.25),
  col = netflix_red,
  border = "white",
  main = "IMDb rating quality",
  xlab = "IMDb score",
  ylab = "Number of titles"
)
hist(
  netflix$rotten_tomatoes_numeric,
  breaks = seq(0, 100, by = 5),
  col = netflix_dark_red,
  border = "white",
  main = "Rotten Tomatoes quality",
  xlab = "Rotten Tomatoes score",
  ylab = "Number of titles"
)
dev.off()

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
    category = c("Historically shared", "Historically single-platform"),
    count = as.integer(table(factor(
      netflix$differentiation_label,
      levels = c("Historically shared", "Historically single-platform")
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
  width = 1500,
  height = 700,
  res = 150
)
par(mfrow = c(1, 3), mar = c(8, 5, 4, 1))

draw_percentage_bars <- function(values, labels, colors, title) {
  percentages <- 100 * values / sum(values)
  positions <- barplot(
    percentages,
    names.arg = labels,
    las = 2,
    col = colors,
    ylim = c(0, 110),
    ylab = "% of matched titles",
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
  c("Never", "At least once"),
  c(netflix_light_gray, netflix_red),
  "Global availability: ever"
)
draw_percentage_bars(
  c(sum(!netflix$available_globally_always), sum(netflix$available_globally_always)),
  c("Not always", "Always"),
  c(netflix_light_gray, netflix_red),
  "Global availability: always"
)
draw_percentage_bars(
  c(sum(!netflix$exclusive), sum(netflix$exclusive)),
  c("Shared", "Single-platform"),
  c(netflix_light_gray, netflix_dark_red),
  "Historical differentiation"
)
dev.off()

# 8. Explore the bivariate pattern between established awareness and viewing.
# This remains part of Research Question 1 visualization. The fitted line is
# descriptive only; the formal predictive regression belongs to Question 3.
awareness_viewing_correlation <- cor(
  netflix$log_imdb_ratecount_2021,
  netflix$log_hours_viewed_2023,
  use = "complete.obs"
)

cat("7. PRELIMINARY AWARENESS-VIEWING PATTERN\n")
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

# 9. Print the most extreme raw values as a final data-quality check.
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

cat("8. EXTREME-VALUE CHECK\n")
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

# 10. Mark Research Question 1 complete before beginning the approved next part.
cat("RESEARCH QUESTION 1 COMPLETE\n")
cat("Research Question 1 outputs saved to:", normalizePath(output_dir), "\n\n")

# ============================================================================
# RESEARCH QUESTION 2
# How do viewing hours differ between globally and regionally available shows,
# historically single-platform and shared shows, and high- and low-awareness
# shows?
#
# This section combines descriptive comparisons with statistical inference.
# Welch tests compare mean log viewing hours and tolerate unequal variances and
# group sizes. Wilcoxon rank-sum tests provide a distribution-based sensitivity
# check. Holm adjustments account for the three comparisons within each family.
# ============================================================================

# 11. Define the three Research Question 2 comparison groups.
# High awareness is defined using the sample median IMDb rating count. The
# continuous rating count will still be retained for later regression analysis;
# the median split is used here only to provide an interpretable group comparison.
awareness_cutoff <- median(netflix$imdb_ratecount_2021, na.rm = TRUE)
netflix$high_awareness <- netflix$imdb_ratecount_2021 > awareness_cutoff
netflix$awareness_label <- ifelse(
  netflix$high_awareness,
  "High awareness",
  "Low awareness"
)

comparison_definitions <- list(
  list(
    name = "Global availability",
    group = netflix$available_globally_ever,
    reference = "Never globally available",
    target = "Globally available at least once"
  ),
  list(
    name = "Historical differentiation",
    group = netflix$exclusive,
    reference = "Historically shared",
    target = "Historically single-platform"
  ),
  list(
    name = "Established awareness",
    group = netflix$high_awareness,
    reference = paste0("Low awareness (<= ", format(awareness_cutoff, big.mark = ","), " ratings)"),
    target = paste0("High awareness (> ", format(awareness_cutoff, big.mark = ","), " ratings)")
  )
)

# 12. Calculate descriptive statistics for every comparison group.
# Both raw-hour summaries and log-hour summaries are reported. Raw medians are
# easy to communicate, while log summaries align with the inferential tests.
summarize_comparison_groups <- function(definition) {
  group <- definition$group
  build_row <- function(select_group, label) {
    raw_hours <- netflix$hours_viewed_2023[select_group]
    log_hours <- netflix$log_hours_viewed_2023[select_group]
    data.frame(
      comparison = definition$name,
      group = label,
      n = length(raw_hours),
      raw_mean_hours = mean(raw_hours),
      raw_median_hours = median(raw_hours),
      log_mean = mean(log_hours),
      log_median = median(log_hours),
      log_sd = sd(log_hours),
      stringsAsFactors = FALSE
    )
  }
  rbind(
    build_row(!group, definition$reference),
    build_row(group, definition$target)
  )
}

rq2_group_summary <- do.call(
  rbind,
  lapply(comparison_definitions, summarize_comparison_groups)
)

cat("\nRESEARCH QUESTION 2\n")
cat("===================\n\n")
cat("9. GROUP DEFINITIONS AND DESCRIPTIVE RESULTS\n")
cat("High-awareness cutoff: sample median of", format(awareness_cutoff, big.mark = ","), "IMDb ratings\n\n")
print(rq2_group_summary, row.names = FALSE)

# Interpretation:
# High-awareness titles have a much higher median viewing level than low-
# awareness titles: 15.6 million versus 3.75 million hours. Globally available
# titles have a modestly higher median than never-global titles: 8.5 million
# versus 7.55 million. Historically shared titles have a higher median than
# historically single-platform titles: 12.4 million versus 7.95 million.
cat("\nInterpretation: The largest descriptive gap is between high- and low-awareness titles. Global availability shows a modest median difference, while historically shared titles have the higher median engagement.\n\n")

write.csv(
  rq2_group_summary,
  file.path(rq2_output_dir, "rq2_group_summary.csv"),
  row.names = FALSE
)

# 13. Run Welch tests, Wilcoxon sensitivity tests, and calculate effect sizes.
# Positive differences and effect sizes mean the target group has higher log
# viewing than the reference group. Hedges' g corrects standardized mean
# differences for small-sample bias.
calculate_hedges_g <- function(outcome, group) {
  target_values <- outcome[group]
  reference_values <- outcome[!group]
  target_n <- length(target_values)
  reference_n <- length(reference_values)
  pooled_sd <- sqrt(
    ((target_n - 1) * var(target_values) +
       (reference_n - 1) * var(reference_values)) /
      (target_n + reference_n - 2)
  )
  cohen_d <- (mean(target_values) - mean(reference_values)) / pooled_sd
  correction <- 1 - 3 / (4 * (target_n + reference_n) - 9)
  correction * cohen_d
}

run_comparison_tests <- function(definition) {
  group <- definition$group
  outcome <- netflix$log_hours_viewed_2023
  welch_result <- t.test(outcome ~ group)
  wilcoxon_result <- wilcox.test(outcome ~ group, exact = FALSE)
  target_minus_reference <- mean(outcome[group]) - mean(outcome[!group])

  data.frame(
    comparison = definition$name,
    reference_group = definition$reference,
    target_group = definition$target,
    log_mean_difference = target_minus_reference,
    geometric_mean_ratio = exp(target_minus_reference),
    ci_low = -welch_result$conf.int[2],
    ci_high = -welch_result$conf.int[1],
    hedges_g = calculate_hedges_g(outcome, group),
    welch_p = welch_result$p.value,
    wilcoxon_p = wilcoxon_result$p.value,
    stringsAsFactors = FALSE
  )
}

rq2_test_results <- do.call(
  rbind,
  lapply(comparison_definitions, run_comparison_tests)
)
rq2_test_results$welch_p_holm <- p.adjust(rq2_test_results$welch_p, method = "holm")
rq2_test_results$wilcoxon_p_holm <- p.adjust(rq2_test_results$wilcoxon_p, method = "holm")

cat("10. GROUP-COMPARISON TESTS\n")
print(rq2_test_results, row.names = FALSE)

# Interpretation:
# Established awareness produces the clearest difference: high-awareness titles
# have about 4.90 times the geometric-mean viewing of low-awareness titles, with
# a large Hedges' g of 0.88 and adjusted p-values far below 0.001. Global
# availability has a small positive effect size (g = 0.13), but neither adjusted
# test reaches 0.05. Historical single-platform titles show lower engagement than
# shared titles (g = -0.22); the adjusted Wilcoxon result is significant, while
# the adjusted Welch result is not, so the evidence is mixed rather than decisive.
cat("\nInterpretation: Awareness produces a large, robust engagement difference. Global availability shows only a small, statistically inconclusive difference. Historically shared titles tend to have higher engagement, but the two tests provide mixed evidence after adjustment.\n\n")

write.csv(
  rq2_test_results,
  file.path(rq2_output_dir, "rq2_test_results.csv"),
  row.names = FALSE
)

# 14. Visualize the full log-viewing distributions for all three comparisons.
# Netflix red highlights the focal target group, while gray shows its reference.
png(
  file.path(rq2_output_dir, "05_rq2_group_boxplots.png"),
  width = 1650,
  height = 700,
  res = 150
)
par(mfrow = c(1, 3), mar = c(8, 5, 4, 1))

boxplot(
  netflix$log_hours_viewed_2023 ~ factor(
    netflix$available_globally_ever,
    levels = c(FALSE, TRUE),
    labels = c("Never global", "Global at least once")
  ),
  col = c(netflix_light_gray, netflix_red),
  outline = FALSE,
  las = 2,
  xlab = "",
  ylab = "log(1 + viewing hours)",
  main = "Global availability"
)
boxplot(
  netflix$log_hours_viewed_2023 ~ factor(
    netflix$exclusive,
    levels = c(FALSE, TRUE),
    labels = c("Historically shared", "Single-platform")
  ),
  col = c(netflix_light_gray, netflix_dark_red),
  outline = FALSE,
  las = 2,
  xlab = "",
  ylab = "log(1 + viewing hours)",
  main = "Historical differentiation"
)
boxplot(
  netflix$log_hours_viewed_2023 ~ factor(
    netflix$high_awareness,
    levels = c(FALSE, TRUE),
    labels = c("Low awareness", "High awareness")
  ),
  col = c(netflix_light_gray, netflix_red),
  outline = FALSE,
  las = 2,
  xlab = "",
  ylab = "log(1 + viewing hours)",
  main = "Established IMDb awareness"
)
dev.off()

# 15. Visualize effect sizes and 95% confidence intervals.
# The vertical zero line represents no mean difference in log viewing. Effects
# to the right favor the target group; effects to the left favor the reference.
png(
  file.path(rq2_output_dir, "06_rq2_adjusted_group_differences.png"),
  width = 1250,
  height = 800,
  res = 150
)
par(mar = c(5, 12, 4, 2))
plot_order <- rev(seq_len(nrow(rq2_test_results)))
plot(
  rq2_test_results$log_mean_difference[plot_order],
  seq_along(plot_order),
  xlim = range(c(rq2_test_results$ci_low, rq2_test_results$ci_high)) * 1.12,
  ylim = c(0.5, length(plot_order) + 0.5),
  pch = 19,
  cex = 1.5,
  col = netflix_red,
  yaxt = "n",
  xlab = "Target minus reference: mean difference in log viewing hours",
  ylab = "",
  main = "Viewing-hour differences with 95% confidence intervals"
)
axis(
  2,
  at = seq_along(plot_order),
  labels = rq2_test_results$comparison[plot_order],
  las = 1
)
segments(
  rq2_test_results$ci_low[plot_order],
  seq_along(plot_order),
  rq2_test_results$ci_high[plot_order],
  seq_along(plot_order),
  lwd = 3,
  col = netflix_black
)
points(
  rq2_test_results$log_mean_difference[plot_order],
  seq_along(plot_order),
  pch = 19,
  cex = 1.5,
  col = netflix_red
)
abline(v = 0, lty = 2, lwd = 2, col = netflix_gray)
dev.off()

# 16. End after Research Question 2 as requested.
cat("RESEARCH QUESTION 2 COMPLETE\n")
cat("No predictive regressions or title classifications were run.\n")
cat("Research Question 2 outputs saved to:", normalizePath(rq2_output_dir), "\n")

sink()
