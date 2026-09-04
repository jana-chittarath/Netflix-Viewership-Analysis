# Netflix Content Performance Analysis

This project examines how Netflix can evaluate TV-show performance relative to reasonable expectations rather than relying on total viewing hours alone. Five public datasets were cleaned and combined into an analysis-ready catalog of 1,252 TV shows containing 2023 Netflix viewing hours, historical IMDb awareness and ratings, platform information, and content characteristics.

## Methods

- Exploratory data analysis and visualization
- Welch two-sample t-tests
- Simple and multiple linear regression
- 70/30 training-testing model evaluation
- 10-fold cross-validation and out-of-fold predictions
- Performance-gap screening for titles above, near, and below expectations

The selected content-informed model uses IMDb awareness, rating quality, number of seasons, title age, format, and audience category to benchmark expected viewing. Results are intended to prioritize titles for further business review—not automate content decisions.

## Interactive Dashboard

[Open the public R Shiny dashboard](https://janapchi.shinyapps.io/netflix-catalog-benchmark/)

## Presentation

[View the project presentation slides](https://docs.google.com/presentation/d/1hIIv4olhzpDQLJGxhhq1gLNjRUj3zGWayAueF7sWYyM/edit?usp=sharing)

## Tools

R, RStudio, R Shiny, Codex, and PowerPoint
