# NETFLIX TV SHOW RESEARCH PROJECT
# Research Question 3

# Question:
# How accurately can established popularity, measured by IMDb rating 
#count, predict Netflix viewing hours?

# Method:
# One-predictor linear regression using log-transformed variables,
# followed by predictive validation with an 70/30 train-test split.

#1. Looking at the variables
library(tidyverse)

netflix_analysis_ready <- read.csv(
  "netflix_analysis_ready.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "null")
)

glimpse(netflix_analysis_ready)

#2. Create the modeling set
model_data <- netflix_analysis_ready |>
  select(
    title,
    hours_viewed_2023,
    imdb_ratecount_2021,
    log_hours_viewed_2023,
    log_imdb_ratecount_2021
  ) |>
  filter(
    !is.na(log_hours_viewed_2023),
    !is.na(log_imdb_ratecount_2021),
    is.finite(log_hours_viewed_2023),
    is.finite(log_imdb_ratecount_2021)
  )

nrow(model_data)

#3. Check the raw viewing data
summary(model_data$hours_viewed_2023)

ggplot(model_data, aes(x = hours_viewed_2023)) +
  geom_histogram(
    bins = 40,
    fill = "#E50914",
    color = "white"
  ) +
  labs(
    title = "Distribution of Netflix Viewing Hours",
    x = "Hours Viewed in 2023",
    y = "Number of Shows"
  ) +
  theme_minimal()
# The raw data appears to be strongly right-skewed with a small number of shows
# receiving extremely high viewing hours.
# so, applying a log transformation:

#4. Applying the log transformation 
# log1p(x) calculates log(1 + x).
# Adding 1 makes the transformation safe if a value is zero.
netflix <- netflix_analysis_ready |>
  mutate(
    log_hours_viewed_2023 = log1p(hours_viewed_2023),
    log_imdb_ratecount_2021 = log1p(imdb_ratecount_2021)
  )

#5. Analyzing the logged viewing hours
ggplot(model_data, aes(x = log_hours_viewed_2023)) +
  geom_histogram(
    bins = 40,
    fill = "#B20710",
    color = "white"
  ) +
  labs(
    title = "Distribution of Log Netflix Viewing Hours",
    x = "Log Viewing Hours",
    y = "Number of Shows"
  ) +
  theme_minimal()
# The logged values allows the distribution to become much more normalized

#6. Examine the logged IMDb popularity distribution
ggplot(model_data, aes(x = log_imdb_ratecount_2021)) +
  geom_histogram(
    bins = 40,
    fill = "#737373",
    color = "white"
  ) +
  labs(
    title = "Distribution of Established IMDb Popularity",
    x = "Log IMDb Rating Count in 2021",
    y = "Number of Shows"
  ) +
  theme_minimal()
# The logged values here also allow the distribution to become more normalized

#7. Graph established popularity against viewing
ggplot(
  model_data,
  aes(
    x = log_imdb_ratecount_2021,
    y = log_hours_viewed_2023
  )
) +
  geom_point(
    alpha = 0.4,
    color = "#E50914"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "#141414"
  ) +
  labs(
    title = "Established Popularity vs. Netflix Viewing",
    x = "Log IMDb Rating Count in 2021",
    y = "Log Netflix Viewing Hours in 2023"
  ) +
  theme_minimal()
# The positive correlation indicates that more-established shows generally 
# received more Netflix viewing.

#8. Fit the regression using all titles
baseline_full <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021,
  data = model_data
)

summary(baseline_full)
# The p value is 2.2e-16 suggesting there is sufficient evidence to suggest that
# IMDb rating count is related to viewing. 

# The R-squared value of 0.2816 suggests that established IMDb popularity 
# explains approximately 28.2% of the variation in logged Netflix viewing hours.

#9. Interpreting the coefficient accurately
popularity_coefficient <- coef(
  baseline_full
)[["log_imdb_ratecount_2021"]]

viewing_change_for_10_percent <- (
  (1.10 ^ popularity_coefficient) - 1
) * 100

popularity_coefficient
viewing_change_for_10_percent
# The regression coefficient is approximately 0.49. Based on that coefficient, 
# a 10% increase in IMDb rating count is associated with approximately a 4.8% 
# increase in expected Netflix viewing hours.

#10. Check regression assumptions
par(mfrow = c(2, 2))
plot(baseline_full)
par(mfrow = c(1, 1))

#Predictive Validation
#11. Set the seed and create a 70/30 split
set.seed(123)

training_rows <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.70 * nrow(model_data)),
  replace = FALSE
)

train_data <- model_data[training_rows, ]
test_data <- model_data[-training_rows, ]

nrow(train_data)
nrow(test_data)

#12. Fit the model to only the training data
baseline_train <- lm(
  log_hours_viewed_2023 ~ log_imdb_ratecount_2021,
  data = train_data
)

summary(baseline_train)
# The model now learns the relationship using only 70% of the shows.
# The 30% test sample has not been used to calculate the regression equation. 

#13. Predict viewing for the training data

train_data$predicted_log_hours <- predict(
  baseline_train,
  newdata = train_data
)

train_actual <- train_data$log_hours_viewed_2023
train_predicted <- train_data$predicted_log_hours

#14. Calculate training metrics
training_rmse <- sqrt(
  mean((train_actual - train_predicted)^2)
)

training_mae <- mean(
  abs(train_actual - train_predicted)
)

training_r2 <- 1 - (
  sum((train_actual - train_predicted)^2) /
    sum((train_actual - mean(train_actual))^2)
)

#15. Predict viewing in the test data
test_data$predicted_log_hours <- predict(
  baseline_train,
  newdata = test_data
)
#The regression equation learned from the training shows is applied to the test shows.

#16. Calculate prediction accuracy and testing metrics
test_data$predicted_log_hours <- predict(
  baseline_train,
  newdata = test_data
)

test_actual <- test_data$log_hours_viewed_2023
test_predicted <- test_data$predicted_log_hours

testing_rmse <- sqrt(
  mean((test_actual - test_predicted)^2)
)

testing_mae <- mean(
  abs(test_actual - test_predicted)
)


testing_r2 <- 1 - (
  sum((test_actual - test_predicted)^2) /
    sum((test_actual - mean(test_actual))^2)
)
# The test R-squared value is 0.3119 meaning established popularity explains 
# approximately 31.2% of the variation in the test shows’ logged viewing hours.

#17. Placing the validation results into one table
validation_results <- tibble(
  Dataset = c("Training", "Testing"),
  Titles = c(
    nrow(train_data),
    nrow(test_data)
  ),
  `R-squared` = c(
    training_r2,
    testing_r2
  ),
  RMSE = c(
    training_rmse,
    testing_rmse
  ),
  MAE = c(
    training_mae,
    testing_mae
  )
) |>
  mutate(
    `R-squared` = round(`R-squared`, 3),
    RMSE = round(RMSE, 3),
    MAE = round(MAE, 3)
  )

validation_results
# The RMSE and MAE values decrease showing lower prediction errors and less
# distance between the actual and predicted log values. The R-squared value
# also increases showing established popularity can be explained by 31.2% of the
# data as opposed to around 27% of the data showing overall improvement.

#18. Graph predicted versus actual viewing
ggplot(
  test_data,
  aes(
    x = predicted_log_hours,
    y = log_hours_viewed_2023
  )
) +
  geom_point(
    alpha = 0.55,
    color = "#E50914"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#141414"
  ) +
  labs(
    title = "Predicted vs. Actual Netflix Viewing",
    subtitle = "Predictions for the 30% test sample",
    x = "Predicted Log Viewing Hours",
    y = "Actual Log Viewing Hours"
  ) +
  theme_minimal()

#19. Final results
cat("Full-sample R-squared:", round(summary(baseline_full)$r.squared, 3), "\n")
cat("Training R-squared:", round(summary(baseline_train)$r.squared, 3), "\n")
cat("Test R-squared:", round(test_r2, 3), "\n")
cat("Test RMSE:", round(test_rmse, 3), "\n")
cat("Test MAE:", round(test_mae, 3), "\n")
cat(
  "Viewing change from 10% more IMDb ratings:",
  round(viewing_change_for_10_percent, 2),
 color = "#141414"
  ) +
  labs(
    title = "Predicted vs. Actual Netflix Viewing",
    subtitle = "Predictions for the 30% test sample",
    x = "Predicted Log Viewing Hours",
    y = "Actual Log Viewing Hours"
  ) +
  theme_minimal()  "%\n"
)
# Overall, Established IMDb popularity was a statistically significant positive 
# predictor of Netflix viewing. 
# The similar training and testing results suggest that the relationship
# generalizes reasonably consistently to unseen titles.
# The model does not appear to be substantially overfitting. However, with a 
# test R-squared of approximately 31.2%, established popularity still leaves 
# most of the variation in viewing unexplained.

 

#Creating a clean validation table of results
library(gt)

validation_table <- validation_results |>
  gt() |>
  
  # Add the table title and subtitle
  tab_header(
    title = "Predictive Validation Results",
    subtitle = "Established IMDb popularity predicting Netflix viewing hours"
  ) |>
  
  # Rename the columns
  cols_label(
    Dataset = "Data Sample",
    Titles = "Titles",
    `R-squared` = "R²",
    RMSE = "RMSE",
    MAE = "MAE"
  ) |>
  
  # Center the values
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  
  # Format the numerical results
  fmt_number(
    columns = c(`R-squared`, RMSE, MAE),
    decimals = 3
  ) |>
  
  # Create dark borders around the column headings
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#262626",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) |>
  
  # Create dark borders around every body cell
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#262626",
      weight = px(1)
    ),
    locations = cells_body(
      columns = everything()
    )
  ) |>
  
  # Style the column headings
  tab_style(
    style = list(
      cell_fill(color = "#141414"),
      cell_text(
        color = "white",
        weight = "bold"
      )
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) |>
  
  # Highlight the testing row
  tab_style(
    style = cell_fill(color = "#FDE8E9"),
    locations = cells_body(
      rows = Dataset == "Testing"
    )
  ) |>
  
  # Make the sample labels bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Dataset
    )
  ) |>
  
  # Add explanatory note
  tab_source_note(
    source_note = paste(
      "70/30 train-test split using set.seed(123).",
      "RMSE and MAE are measured using log-transformed viewing hours."
    )
  ) |>
  
  # Adjust the table's general appearance
  tab_options(
    heading.background.color = "#E50914",
    heading.title.font.size = px(20),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(13),
    table.font.size = px(14),
    data_row.padding = px(10),
    table.border.top.color = "#262626",
    table.border.top.width = px(2),
    table.border.bottom.color = "#262626",
    table.border.bottom.width = px(2)
  )

validation_table