library(tidyverse)
library(tidymodels)
library(baguette)

# URLs
covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'

# Ingest

data   <- readr::read_csv(covid_url)
census <- co.est2023.alldata

names(census) <- census[1,]
census <- census[-1,]

census <-  census |> 
  filter(COUNTY == "000") |>  # Filter for state-level data only
  mutate(fips = STATE) |>      # Create a new FIPS column for merging
  select(fips, contains("2021"))  # Select relevant columns for 2021 data

# Process COVID-19 Data
state_data <-  data |> 
  group_by(fips) |> 
  mutate(
    new_cases  = pmax(0, cases - lag(cases)),   # Compute new cases, ensuring no negative values
    new_deaths = pmax(0, deaths - lag(deaths))  # Compute new deaths, ensuring no negative values
  ) |> 
  ungroup() |> 
  left_join(census, by = "fips") |>  # Merge with census data
  mutate(
    m = month(date), y = year(date),
    season = case_when(   # Define seasons based on month
      m %in% 3:5 ~ "Spring",
      m %in% 6:8 ~ "Summer",
      m %in% 9:11 ~ "Fall",
      m %in% c(12, 1, 2) ~ "Winter"
    )
  ) |> 
  group_by(state, y, season) |> 
  mutate(
    season_cases  = sum(new_cases, na.rm = TRUE),  # Aggregate seasonal cases
    season_deaths = sum(new_deaths, na.rm = TRUE)  # Aggregate seasonal deaths
  )  |> 
  distinct(state, y, season, .keep_all = TRUE) |>  # Keep only distinct rows by state, year, season
  ungroup() |> 
  select(state, contains('season'), y, POPESTIMATE2021, BIRTHS2021, DEATHS2021) |>  # Select relevant columns
  drop_na() |>  # Remove rows with missing values
  mutate(logC = log(season_cases +1)) |>
  mutate(logD = log(season_deaths +1)) # Log-transform case and death numbers for modeling

#fixing class
state_data$POPESTIMATE2021 <- as.numeric(state_data$POPESTIMATE2021)
state_data$BIRTHS2021 <- as.numeric(state_data$BIRTHS2021)
state_data$DEATHS2021 <- as.numeric(state_data$DEATHS2021)

#data splitting
set.seed(101)
split <- initial_split(state_data, prop = 0.7)

covid_train <- training(split)
covid_test <- testing(split)

covid_cv <- vfold_cv(covid_train, v = 10)

#Recipe building
rec_covid <-  recipe(logD ~ ., data = covid_train) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_rm(state, season_deaths) |>  # Remove non-predictive columns
  step_dummy(all_nominal()) |>  # Convert categorical variables to dummy variables
  step_scale(all_numeric_predictors()) |>  # Scale numeric predictors
  step_center(all_numeric_predictors())  # Center numeric predictors

rf_covid <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wf_covid <- workflow() %>%
  # Add the recipe
  add_recipe(rec_covid) %>%
  # Add the model
  add_model(rf_covid) %>%
  # Fit the model
  fit(data = covid_train) 

lm_covid <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

lm_wf_covid <- workflow() %>%
  add_recipe(rec_covid) %>%
  add_model(lm_covid) %>%
  fit(data = covid_train)

boost_covid <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

boost_wf_covid <- workflow() %>% 
  add_recipe(rec_covid) %>%
  add_model(boost_covid) %>%
  fit(data = covid_train)

wf <- workflow_set(list(rec_covid), list(lm_covid, rf_covid, boost_covid)) %>%
  workflow_map('fit_resamples', resamples = covid_cv) 

autoplot(wf)

rank_results(wf, rank_metric = "rsq", select_best = TRUE)

fit_covid <- workflow() %>% 
  add_recipe(rec_covid) %>% 
  add_model(rf_covid) %>% 
  fit(data = covid_train)

pred_covid <- augment(fit_covid, new_data = covid_test)

metrics(pred_covid, truth = logD, estimate = .pred)

ggplot(pred_covid, aes(x = logD, y = .pred)) +
  geom_point() +
  geom_abline() +
  theme_minimal()+ 
  labs(
    title = "Random Forest Model Performance",
    x = "Actual Deaths (log10)",
    y = "Predicted Deaths (log10)"
  )
