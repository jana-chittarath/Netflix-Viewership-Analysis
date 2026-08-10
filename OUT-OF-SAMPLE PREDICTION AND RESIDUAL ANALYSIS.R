install.packages(c("tidyverse","caret","ggrepel"))
# ============================================================
# Netflix Viewing: Out-of-Sample Prediction & Residual Analysis
# ============================================================
# Which titles get substantially more/less viewing than a model
# trained on catalog attributes (ratings, platforms, etc.) predicts?
#
# Method: 10-fold cross-validated linear regression predicting
# log(hours_viewed_2023). Out-of-fold predictions are used so
# every title's residual is a genuine out-of-sample residual
# (never predicted by a model that saw that title during training).
# ============================================================

# ---- 0. Packages ----
# install.packages(c("tidyverse", "caret", "ggrepel"))
library(tidyverse)
library(caret)
library(ggrepel)

set.seed(42)  # match Python analysis for reproducibility

# ---- 1. Load data ----
# Update the path below to wherever the CSV lives on your machine
csv_path <- "/Users/alex/Desktop/netflix_analysis_ready(netflix_analysis_ready).csv"

df <- read_csv(csv_path, locale = locale(encoding = "latin1"),
               show_col_types = FALSE)

# ---- 2. Clean / prep ----
df <- df %>%
  mutate(
    imdb_rating_catalog_2021 = ifelse(
      is.na(imdb_rating_catalog_2021),
      median(imdb_rating_catalog_2021, na.rm = TRUE),
      imdb_rating_catalog_2021
    ),
    age = as.factor(age),
    imdb_title_type = as.factor(imdb_title_type),
    exclusive = as.factor(exclusive),
    available_globally_ever = as.factor(available_globally_ever)
  )

# Features available "pre-viewing" (catalog attributes only —
# nothing derived from 2023 viewing itself)
model_vars <- c(
  "log_hours_viewed_2023",       # target
  "imdb_rating_catalog_2021",
  "rotten_tomatoes_numeric",
  "n_platforms",
  "log_imdb_ratecount_2021",
  "n_seasons_aggregated",
  "year",
  "age",
  "imdb_title_type",
  "exclusive",
  "available_globally_ever"
)

model_df <- df %>% select(all_of(model_vars)) %>% drop_na()
# keep the row index so we can rejoin predictions to titles later
model_df$.row_id <- seq_len(nrow(model_df))
df_for_join <- df[model_df$.row_id, ]  # aligned title lookup

# ---- 3. 10-fold cross-validated linear regression ----
# savePredictions = "final" keeps exactly one out-of-fold
# prediction per row (no repeats), which is what we need.
ctrl <- trainControl(method = "cv", number = 10, savePredictions = "final")

cv_model <- train(
  log_hours_viewed_2023 ~ imdb_rating_catalog_2021 + rotten_tomatoes_numeric +
    n_platforms + log_imdb_ratecount_2021 + n_seasons_aggregated + year +
    age + imdb_title_type + exclusive + available_globally_ever,
  data = model_df,
  method = "lm",
  trControl = ctrl
)

# Out-of-sample R^2 (this is the honest, cross-validated fit quality —
# NOT the in-sample R^2 you'd get from summary(lm(...)))
oos_r2 <- cor(cv_model$pred$pred, cv_model$pred$obs)^2
cat("Out-of-sample (10-fold CV) R^2:", round(oos_r2, 4), "\n")

# ---- 4. Attach out-of-fold predictions back to titles ----
# cv_model$pred$rowIndex maps back to rows of model_df
preds <- cv_model$pred %>%
  arrange(rowIndex) %>%
  select(rowIndex, pred)

results <- df_for_join %>%
  mutate(.row_id = model_df$.row_id) %>%
  left_join(preds, by = c(".row_id" = "rowIndex")) %>%
  rename(predicted_log_hours = pred) %>%
  mutate(
    residual              = log_hours_viewed_2023 - predicted_log_hours,
    residual_z            = as.numeric(scale(residual)),
    predicted_hours_viewed_2023 = exp(predicted_log_hours),
    ratio_actual_to_predicted   = hours_viewed_2023 / predicted_hours_viewed_2023
  )

cat("Residual std (log scale):", round(sd(results$residual), 4), "\n")
cat("Titles with |z| > 2:", sum(abs(results$residual_z) > 2), "\n")
cat("Titles with |z| > 3:", sum(abs(results$residual_z) > 3), "\n")

# ---- 5. Top over- and under-performers ----
top_over <- results %>%
  arrange(desc(residual_z)) %>%
  select(title, year, imdb_rating_catalog_2021, n_platforms,
         hours_viewed_2023, predicted_hours_viewed_2023,
         ratio_actual_to_predicted, residual_z) %>%
  head(15)

top_under <- results %>%
  arrange(residual_z) %>%
  select(title, year, imdb_rating_catalog_2021, n_platforms,
         hours_viewed_2023, predicted_hours_viewed_2023,
         ratio_actual_to_predicted, residual_z) %>%
  head(15)

cat("\n=== TOP 15 OVER-PERFORMERS (actual >> predicted) ===\n")
print(top_over, width = Inf)

cat("\n=== TOP 15 UNDER-PERFORMERS (actual << predicted) ===\n")
print(top_under, width = Inf)

# Save the full residual table for further use
write_csv(results, "netflix_residual_analysis_full.csv")

# ---- 6. Plot: predicted vs. actual, outliers highlighted ----
results <- results %>%
  mutate(outlier_flag = ifelse(abs(residual_z) > 2, "Substantial outlier (|z| > 2)", "Typical"))

# Label only the most extreme points so the plot stays readable
label_df <- results %>%
  filter(abs(residual_z) > 2.4)

p <- ggplot(results, aes(x = predicted_log_hours, y = log_hours_viewed_2023)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = outlier_flag, size = outlier_flag, alpha = outlier_flag)) +
  geom_text_repel(
    data = label_df,
    aes(label = title),
    size = 3, max.overlaps = 20, segment.size = 0.3
  ) +
  scale_color_manual(values = c("Typical" = "grey60",
                                "Substantial outlier (|z| > 2)" = "#d95926")) +
  scale_size_manual(values = c("Typical" = 1, "Substantial outlier (|z| > 2)" = 2.2)) +
  scale_alpha_manual(values = c("Typical" = 0.35, "Substantial outlier (|z| > 2)" = 0.95)) +
  labs(
    title = "Predicted vs. actual viewing (10-fold out-of-sample CV)",
    subtitle = paste0("Out-of-sample R\u00B2 = ", round(oos_r2, 3),
                      "  |  ", sum(abs(results$residual_z) > 2),
                      " of ", nrow(results), " titles are substantial outliers (|z| > 2)"),
    x = "Model-predicted log(hours viewed 2023)",
    y = "Actual log(hours viewed 2023)",
    color = NULL, size = NULL, alpha = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p)

ggsave("netflix_residual_plot.png", p, width = 10, height = 7.5, dpi = 200)

# ============================================================
# Notes:
# - This uses out-of-fold CV predictions, so residuals reflect
#   genuine out-of-sample error (each title's prediction comes
#   from a model that never saw that title during training).
# - Predictors are catalog attributes only (ratings, platform
#   count, season count, etc.) -- nothing derived from 2023
#   viewing itself, so there's no leakage into the target.
# - Very long-running shows (large n_seasons / rating-count) can
#   get extrapolated predictions from a linear model; consider a
#   tree-based model (randomForest, xgboost) if this is a concern.
# ============================================================
setwd("C:/Users/YourName/Downloads")
csv_path <- "netflix_analysis_ready_netflix_analysis_ready_.csv"
# ============================================================
# Netflix Viewing: Out-of-Sample Prediction & Residual Analysis
# ============================================================
# Which titles get substantially more/less viewing than a model
# trained on catalog attributes (ratings, platforms, etc.) predicts?
#
# Method: 10-fold cross-validated linear regression predicting
# log(hours_viewed_2023). Out-of-fold predictions are used so
# every title's residual is a genuine out-of-sample residual
# (never predicted by a model that saw that title during training).
# ============================================================

# ---- 0. Packages ----
# install.packages(c("tidyverse", "caret", "ggrepel"))
library(tidyverse)
library(caret)
library(ggrepel)

set.seed(42)  # match Python analysis for reproducibility

# ---- 1. Load data ----
# Update the path below to wherever the CSV lives on your machine
csv_path <- "netflix_analysis_ready_netflix_analysis_ready_.csv"

df <- read_csv(csv_path, locale = locale(encoding = "latin1"),
               show_col_types = FALSE)

# ---- 2. Clean / prep ----
df <- df %>%
  mutate(
    imdb_rating_catalog_2021 = ifelse(
      is.na(imdb_rating_catalog_2021),
      median(imdb_rating_catalog_2021, na.rm = TRUE),
      imdb_rating_catalog_2021
    ),
    age = as.factor(age),
    imdb_title_type = as.factor(imdb_title_type),
    exclusive = as.factor(exclusive),
    available_globally_ever = as.factor(available_globally_ever)
  )

# Features available "pre-viewing" (catalog attributes only —
# nothing derived from 2023 viewing itself)
model_vars <- c(
  "log_hours_viewed_2023",       # target
  "imdb_rating_catalog_2021",
  "rotten_tomatoes_numeric",
  "n_platforms",
  "log_imdb_ratecount_2021",
  "n_seasons_aggregated",
  "year",
  "age",
  "imdb_title_type",
  "exclusive",
  "available_globally_ever"
)

model_df <- df %>% select(all_of(model_vars)) %>% drop_na()
# keep the row index so we can rejoin predictions to titles later
model_df$.row_id <- seq_len(nrow(model_df))
df_for_join <- df[model_df$.row_id, ]  # aligned title lookup

# ---- 3. 10-fold cross-validated linear regression ----
# savePredictions = "final" keeps exactly one out-of-fold
# prediction per row (no repeats), which is what we need.
ctrl <- trainControl(method = "cv", number = 10, savePredictions = "final")

cv_model <- train(
  log_hours_viewed_2023 ~ imdb_rating_catalog_2021 + rotten_tomatoes_numeric +
    n_platforms + log_imdb_ratecount_2021 + n_seasons_aggregated + year +
    age + imdb_title_type + exclusive + available_globally_ever,
  data = model_df,
  method = "lm",
  trControl = ctrl
)

# Out-of-sample R^2 (this is the honest, cross-validated fit quality —
# NOT the in-sample R^2 you'd get from summary(lm(...)))
oos_r2 <- cor(cv_model$pred$pred, cv_model$pred$obs)^2
cat("Out-of-sample (10-fold CV) R^2:", round(oos_r2, 4), "\n")

# ---- 4. Attach out-of-fold predictions back to titles ----
# cv_model$pred$rowIndex maps back to rows of model_df
preds <- cv_model$pred %>%
  arrange(rowIndex) %>%
  select(rowIndex, pred)

results <- df_for_join %>%
  mutate(.row_id = model_df$.row_id) %>%
  left_join(preds, by = c(".row_id" = "rowIndex")) %>%
  rename(predicted_log_hours = pred) %>%
  mutate(
    residual              = log_hours_viewed_2023 - predicted_log_hours,
    residual_z            = as.numeric(scale(residual)),
    predicted_hours_viewed_2023 = exp(predicted_log_hours),
    ratio_actual_to_predicted   = hours_viewed_2023 / predicted_hours_viewed_2023
  )

cat("Residual std (log scale):", round(sd(results$residual), 4), "\n")
cat("Titles with |z| > 2:", sum(abs(results$residual_z) > 2), "\n")
cat("Titles with |z| > 3:", sum(abs(results$residual_z) > 3), "\n")

# ---- 5. Top over- and under-performers ----
top_over <- results %>%
  arrange(desc(residual_z)) %>%
  select(title, year, imdb_rating_catalog_2021, n_platforms,
         hours_viewed_2023, predicted_hours_viewed_2023,
         ratio_actual_to_predicted, residual_z) %>%
  head(15)

top_under <- results %>%
  arrange(residual_z) %>%
  select(title, year, imdb_rating_catalog_2021, n_platforms,
         hours_viewed_2023, predicted_hours_viewed_2023,
         ratio_actual_to_predicted, residual_z) %>%
  head(15)

cat("\n=== TOP 15 OVER-PERFORMERS (actual >> predicted) ===\n")
print(top_over, width = Inf)

cat("\n=== TOP 15 UNDER-PERFORMERS (actual << predicted) ===\n")
print(top_under, width = Inf)

# Save the full residual table for further use
write_csv(results, "netflix_residual_analysis_full.csv")

# ---- 6. Plot: predicted vs. actual, outliers highlighted ----
results <- results %>%
  mutate(outlier_flag = ifelse(abs(residual_z) > 2, "Substantial outlier (|z| > 2)", "Typical"))

# Label only the most extreme points so the plot stays readable
label_df <- results %>%
  filter(abs(residual_z) > 2.4)

p <- ggplot(results, aes(x = predicted_log_hours, y = log_hours_viewed_2023)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = outlier_flag, size = outlier_flag, alpha = outlier_flag)) +
  geom_text_repel(
    data = label_df,
    aes(label = title),
    size = 3, max.overlaps = 20, segment.size = 0.3
  ) +
  scale_color_manual(values = c("Typical" = "grey60",
                                "Substantial outlier (|z| > 2)" = "#d95926")) +
  scale_size_manual(values = c("Typical" = 1, "Substantial outlier (|z| > 2)" = 2.2)) +
  scale_alpha_manual(values = c("Typical" = 0.35, "Substantial outlier (|z| > 2)" = 0.95)) +
  labs(
    title = "Predicted vs. actual viewing (10-fold out-of-sample CV)",
    subtitle = paste0("Out-of-sample R\u00B2 = ", round(oos_r2, 3),
                      "  |  ", sum(abs(results$residual_z) > 2),
                      " of ", nrow(results), " titles are substantial outliers (|z| > 2)"),
    x = "Model-predicted log(hours viewed 2023)",
    y = "Actual log(hours viewed 2023)",
    color = NULL, size = NULL, alpha = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p)

ggsave("netflix_residual_plot.png", p, width = 10, height = 7.5, dpi = 200)

# ============================================================
# Notes:
# - This uses out-of-fold CV predictions, so residuals reflect
#   genuine out-of-sample error (each title's prediction comes
#   from a model that never saw that title during training).
# - Predictors are catalog attributes only (ratings, platform
#   count, season count, etc.) -- nothing derived from 2023
#   viewing itself, so there's no leakage into the target.
# - Very long-running shows (large n_seasons / rating-count) can
#   get extrapolated predictions from a linear model; consider a
#   tree-based model (randomForest, xgboost) if this is a concern.
# ============================================================
