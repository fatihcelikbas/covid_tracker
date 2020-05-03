generate_arima_prediction <- function(which.country = "Turkey", today = refday, data = cv_cases, days.ahead = 8, is.cases = TRUE) {
  dc <- data %>%
    dplyr::filter(country == which.country & days_since_death10 > 0)
  
  dat_typ <- if(is.cases) dc$new_cases else dc$new_deaths
  dat_typ <- dat_typ[which(dat_typ != 0)]
  
  if(length(dat_typ) == 0) {
    country_day_pred <- rep(0, 8)
    country_cum_pred <- country_day_pred
  }
  else {
    # Putting the log versions of cumulative cases and deaths into time series format
    cs <- ts(log(dat_typ))
    
    # Fitting the optimal ARIMA Model
    cs.ARIMA <- auto.arima(cs, method = "ML")
    
    # Make the forecast for daily values
    cs.forecasts <- forecast(cs.ARIMA, h = days.ahead, level = c(99.5))
    country_day_pred <- exp(as.numeric(cs.forecasts$mean))
    
    # Set negative values to zero
    country_day_pred[country_day_pred < 0] <- 0
    
    current_cum <- dc %>%
      dplyr::filter(as.Date(date) == today) %>%
      dplyr::select(if(is.cases) "cases" else "deaths") %>%
      as.numeric()
    
    country_cum_pred <- current_cum + cumsum(country_day_pred)
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
get_country_all_info_arima <-
  function(which.country = "Spain",
           data = cv_cases, is.cases = TRUE) {
    pred <-
      generate_arima_prediction(which.country = which.country, is.cases = is.cases) %>%
      mutate(country = which.country) %>%
      mutate(observed = as.character(observed)) # convert from factors
    past <- data %>%
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

generate_all_arima_predictions <- function(is.cases =TRUE) {
  l <- lapply(countries, get_country_all_info_arima, is.cases=is.cases)
  return(tibble(rbindlist(l)))
}

### Write daily predictions to file ###
f_arima <- paste0("outputs/predictions_arima_", refday + 1, ".csv")
if (!file.exists(f_arima)) {
  arima_preds <- generate_all_arima_predictions()
  ifelse(!dir.exists(file.path(".", "outputs")), dir.create(file.path(".", "outputs")), FALSE)
  write.csv(arima_preds, f_arima, row.names = FALSE)
}

### Daily death predictions ###
f_arima <- paste0("outputs/predictions_arima_deaths_", refday + 1, ".csv")
if (!file.exists(f_arima)) {
  arima_preds <- generate_all_arima_predictions(FALSE)
  write.csv(arima_preds, f_arima, row.names = FALSE)
}
