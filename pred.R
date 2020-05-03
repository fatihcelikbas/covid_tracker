# The end date for the observed data is yesterday as today's data is released midnight
refday <- Sys.Date() - 1
cov_dat <- read.csv("input_data/coronavirus.csv", as.is = TRUE)

# Get list of countries that had available data 7 days ago
countries <- cov_dat %>%
  dplyr::filter(date == refday - 7) %>%
  dplyr::select(country) %>%
  unique() %>%
  pull()

# Calculate growth rate
calculate_growth_rate <- function(which.country = "France", today = refday, data = cov_dat, is.cases = TRUE) {
  # What is our time window?
  # 3 days observed (today and yesterday and the day before)
  # 8-day ahead forecast
  days <- today+-2:8
  
  dc <- data %>%
    dplyr::filter(country == which.country) %>%
    dplyr::filter(as.Date(date) %in% days[1:3])
  
  # Calculate growth rate
  country_cum <- if(is.cases) dc$cases else dc$deaths
  if (country_cum[1] == 0) {
    growth_rate <- country_cum[3]
  } 
  else {
    growth_rate <-  sqrt(country_cum[3] / country_cum[1])
  }
  
  return(round(growth_rate, 2))
}

# Generate country predictions
generate_country_predictions <- function(which.country = "Turkey", today = refday, data = cv_cases, is.cases = TRUE) {
  # What is our time window?
  # 3 days observed
  # 8-day ahead forecast
  days <- today + -2:8

  
  dc <- data %>%
    dplyr::filter(country == which.country) %>%
    dplyr::filter(as.Date(date) %in% days[1:3]) %>%
    dplyr::select(date, cases, deaths)

  
  # Not enough data for projection
  if (nrow(dc) < 3) {
    df <- data.frame(
      "date" = dc$date,
      "per.day" = if(is.cases) dc$new_cases else dc$new_deaths,
      "cumulative" = if(is.cases) dc$cases else dc$deaths
    )
    
    df$observed <- "Observed"
    return(df)
  }
  
  # Choose which type of data needed
  country_cum <- if(is.cases) dc$cases else dc$deaths

  train <- data.frame(y=country_cum, x = as.Date(dc$date))
  train <- train[train$y != 0, ] # throw away the initial zero values bcs of log
  
  if(nrow(train) == 0) {
    country_cum_pred <- rep(0, 8)
    country_day_pred <- country_cum_pred
  }
  else {
    
    # Fit the exponential model to the cumulative data and predict
    fit <- lm(data = train, log(y) ~ x)
    country_cum_pred <- exp(as.vector(predict(fit, list(x=tail(days, 8)))))
    
    # Calculate the daily cases
    country_day_pred <- diff(c(country_cum[3], country_cum_pred))
    
    # Set negative values to zero
    country_day_pred[country_day_pred < 0] <- 0
    
    # Recalculate the cumulative cases 
    country_cum_pred <- country_cum[3] + cumsum(country_day_pred)
  }
  
  # Format results as a table
  df <- tibble(
    "date" = today + 1:8,
    "per.day" = round(country_day_pred, 0),
    "cumulative" = round(country_cum_pred, 0),
    "observed" = rep("Predicted", 8)
  )
  return(df)
}

# Final df with observed and predicted
get_country_all_info <-
  function(which.country = "Spain",
           data = cv_cases, is.cases = TRUE) {
    pred <-
      generate_country_predictions(which.country = which.country, is.cases = is.cases) %>%
      mutate(country = which.country) %>%
      mutate(observed = as.character(observed)) # convert from factors
    past <- cv_cases %>%
      dplyr::filter(country == which.country) %>%
      dplyr::filter(date <= as.character(refday)) %>%
      rename(per.day = if(is.cases) "new_cases" else "new_deaths") %>%
      rename(cumulative = if(is.cases) "cases" else "deaths") %>%
      mutate(observed = "Observed")
    m <- merge(
      pred,
      past,
      by = c("country", "date", "per.day", "cumulative", "observed"),
      all = TRUE
    ) %>%
      dplyr::select(date, "country", "date", "per.day", "cumulative", "observed") %>% 
      tibble()
    
    return(m)
}

generate_all_predictions <- function(is.cases =TRUE) {
  l <- lapply(countries, get_country_all_info, is.cases=is.cases)
  return(tibble(rbindlist(l)))
}
generate_all_growth_rates <- function(is.cases = TRUE) {
  l <- lapply(countries, calculate_growth_rate, is.cases=is.cases)
  return(unlist(l))
}

### Write daily predictions to file if it does not exist###
f_pred <- paste0("outputs/predictions_", refday + 1, ".csv")
if (!file.exists(f_pred)) {
  preds <- generate_all_predictions()
  ifelse(!dir.exists(file.path(".", "outputs")), dir.create(file.path(".", "outputs")), FALSE)
  write.csv(preds, f_pred, row.names = FALSE)
}

### Daily death predictions ###
f_pred <- paste0("outputs/predictions_deaths_", refday + 1, ".csv")
if (!file.exists(f_pred)) {
  preds <- generate_all_predictions(FALSE)
  write.csv(preds, f_pred, row.names = FALSE)
}
