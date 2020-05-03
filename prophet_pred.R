generate_prophet_prediction <- function(which.country = "Turkey", today = refday, data = cv_cases, days.ahead = 8, is.cases = TRUE) {
  dc <- data %>%
    dplyr::filter(country == which.country)
  
  dat_typ <- if(is.cases) dc$new_cases else dc$new_deaths
  
  if(length(dat_typ) == 0) {
    country_day_pred <- rep(0, 8)
    country_cum_pred <- country_day_pred
  }
  else {
    #Putting the Data into a Dataframe Ready for Prophet Package
    prophet.dat <- data.frame(ds = dc$date, y = dat_typ)
    
    # Fitting the Prophet Model
    prophet_model <-  prophet(prophet.dat)
    
    # Choosing Future Forecast Period
    prophet.forecasts <- make_future_dataframe(prophet_model, periods = days.ahead)
    
    # Making daily forecasts
    country_day_pred <- tail(predict(prophet_model, prophet.forecasts)$yhat, days.ahead)
    
    # Set negative values to zero
    country_day_pred[country_day_pred < 0] <- 0
    
    current_cum <- dc %>%
      dplyr::filter(as.Date(date) == today) %>%
      dplyr::select(if(is.cases) "cases" else "deaths") %>%
      as.numeric()
    
    # Get the cumulative results
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
get_country_all_info_prophet <-
  function(which.country = "Spain",
           data = cv_cases, is.cases = TRUE) {
    pred <-
      generate_prophet_prediction(which.country = which.country, is.cases = is.cases) %>%
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

generate_all_prophet_predictions <- function(is.cases =TRUE) {
  l <- lapply(countries, get_country_all_info_prophet, is.cases=is.cases)
  return(tibble(rbindlist(l)))
}

f_prophet <- paste0("outputs/predictions_prophet_", refday + 1, ".csv")
if (!file.exists(f_prophet)) {
  ### Write daily predictions to file ###
  prophet_preds <- generate_all_prophet_predictions()
  ifelse(!dir.exists(file.path(".", "outputs")), dir.create(file.path(".", "outputs")), FALSE)
  write.csv(prophet_preds, paste0("outputs/predictions_prophet_", refday + 1, ".csv"), row.names = FALSE)
}

f_prophet <- paste0("outputs/predictions_prophet_deaths_", refday + 1, ".csv")
if (!file.exists(f_prophet)) {
  ### Daily death predictions ###
  prophet_preds <- generate_all_prophet_predictions(FALSE)
  write.csv(prophet_preds, paste0("outputs/predictions_prophet_deaths_", refday + 1, ".csv"), row.names = FALSE)
}
