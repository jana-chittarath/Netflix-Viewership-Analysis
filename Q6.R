# ============================================================
# Research Question 6
# "Which titles and title profiles emerge as candidates for
#  promotion, continued investment, strategic replication, or
#  further review?"
#
# Method: Residual-based title classification, profile
#         comparison, and business segmentation
#
# This picks up where RQ4 (expanded regression) and RQ5
# (residual analysis) leave off: it (a) refits the expanded
# model, (b) extracts residuals, (c) classifies every title
# into a business-relevant category using the project's
# definitions, and (d) profiles/compares those categories.
# ============================================================

# ---- 0. Packages ----------------------------------------------------
# install.packages(c("tidyverse", "broom"))  # run once if needed
library(tidyverse)
library(broom)

df <- read_csv(
  "netflix_analysis_ready(netflix_analysis_ready).csv",
  locale = locale(encoding = "latin1")
)

View(df)
head(df)
str(df)
colnames(df)
# ---- 1. Load data -----------------------------------------------------
# Update the path/filename if your file lives somewhere else
library(readr)
netflix_analysis_ready_netflix_analysis_ready_ <- read_csv("netflix_analysis_ready(netflix_analysis_ready).csv", locale = locale(encoding = "latin1")
View(netflix_analysis_ready_netflix_analysis_ready_)


head(df)
str(df)

# ---- 2. Keep complete cases for the variables the model needs ---------
model_vars <- c(
  "log_hours_viewed_2023",
  "log_imdb_ratecount_2021",
  "rotten_tomatoes_numeric",
  "available_globally_ever",
  "exclusive"
)

df_model <- df %>%
  filter(if_all(all_of(model_vars), ~ !is.na(.)))

cat("Rows used for modeling:", nrow(df_model), "of", nrow(df), "total rows\n")

# ---- 3. Refit the expanded model (this is the RQ4 model) --------------
# log hours viewed ~ established popularity + rating quality +
#                     global availability + historical differentiation
expanded_model <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021 +
    rotten_tomatoes_numeric +
    available_globally_ever +
    exclusive,
  data = df_model
)

summary(expanded_model)

# ---- 4. Fitted values, residuals, standardized residuals (RQ5) --------
df_model <- df_model %>%
  mutate(
    predicted_log_hours = fitted(expanded_model),
    residual            = resid(expanded_model),
    residual_z          = as.numeric(scale(residual))  # standardized
  )

# ---- 5. Set classification thresholds ----------------------------------
# "High awareness" / "high viewing" = top quartile
# "Low engagement"                  = bottom quartile of viewing
# "Substantially" over/under predicted = 1 SD in residual
awareness_cutoff  <- quantile(df_model$log_imdb_ratecount_2021, 0.75, na.rm = TRUE)
viewing_cutoff_hi <- quantile(df_model$log_hours_viewed_2023, 0.75, na.rm = TRUE)
viewing_cutoff_lo <- quantile(df_model$log_hours_viewed_2023, 0.25, na.rm = TRUE)
resid_hi <- 1
resid_lo <- -1

# ---- 6. Classify every title (project definitions) ----------------------
df_model <- df_model %>%
  mutate(
    title_category = case_when(
      # Blockbuster: high awareness AND high viewing
      log_imdb_ratecount_2021 >= awareness_cutoff &
        log_hours_viewed_2023 >= viewing_cutoff_hi ~ "Blockbuster",
      
      # Sleeper hit: less-established, but overperforms the model
      log_imdb_ratecount_2021 < awareness_cutoff &
        residual_z >= resid_hi ~ "Sleeper Hit",
      
      # High-profile underperformer: well-known, underperforms the model
      log_imdb_ratecount_2021 >= awareness_cutoff &
        residual_z <= resid_lo ~ "High-Profile Underperformer",
      
      # Low-engagement: near the bottom of the viewing distribution
      log_hours_viewed_2023 <= viewing_cutoff_lo ~ "Low-Engagement Title",
      
      # Everything else
      TRUE ~ "Middle of the Pack"
    )
  )

table(df_model$title_category)

# ---- 7. Profile comparison across categories -----------------------------
profile_summary <- df_model %>%
  group_by(title_category) %>%
  summarise(
    n_titles                = n(),
    mean_hours_viewed       = mean(hours_viewed_2023, na.rm = TRUE),
    mean_imdb_ratecount     = mean(imdb_ratecount_2021, na.rm = TRUE),
    mean_rt_score           = mean(rotten_tomatoes_numeric, na.rm = TRUE),
    pct_available_globally  = mean(available_globally_ever, na.rm = TRUE) * 100,
    pct_exclusive           = mean(exclusive, na.rm = TRUE) * 100,
    mean_residual           = mean(residual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_hours_viewed))

print(profile_summary)

# ---- 8. Pull the strongest example titles in each category ---------------
top_examples <- df_model %>%
  group_by(title_category) %>%
  slice_max(order_by = residual, n = 5, with_ties = FALSE) %>%
  select(title, title_category, hours_viewed_2023, imdb_ratecount_2021,
         rotten_tomatoes_numeric, residual) %>%
  arrange(title_category)

print(top_examples)

# ---- 9. Visualize the classification -------------------------------------
ggplot(df_model, aes(x = log_imdb_ratecount_2021, y = log_hours_viewed_2023,
                     color = title_category)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "black", linetype = "dashed") +
  labs(
    title    = "Netflix Title Classification: Awareness vs. Viewing",
    subtitle = "Dashed line = model-predicted viewing given awareness",
    x        = "Log(IMDb Rating Count) — Established Popularity",
    y        = "Log(Hours Viewed, 2023)",
    color    = "Category"
  ) +
  theme_minimal()

# ---- 10. (Optional, per proposal) logistic regression on sleeper hits ----
# "Optionally use logistic regression to examine which characteristics
#  help distinguish sleeper hits"
df_model <- df_model %>%
  mutate(is_sleeper_hit = ifelse(title_category == "Sleeper Hit", 1, 0))

sleeper_model <- glm(
  is_sleeper_hit ~ rotten_tomatoes_numeric + available_globally_ever + exclusive,
  data = df_model,
  family = binomial
)

summary(sleeper_model)
exp(coef(sleeper_model))  # odds ratios, easier to interpret

# ---- 11. Save outputs for the writeup -------------------------------------
write.csv(df_model, "netflix_titles_classified.csv", row.names = FALSE)
write.csv(profile_summary, "title_category_profile_summary.csv", row.names = FALSE)
write.csv(top_examples, "title_category_top_examples.csv", row.names = FALSE)

