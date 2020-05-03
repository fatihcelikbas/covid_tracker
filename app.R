## COVID-2019 interactive mapping tool
### Fatih Celikbas (fatih.celikbas@yale.edu) & Ramiz Colak (ramiz.colak@yale.edu) April 2020

# Import the needed packages
source("loader.R")

# Extract time stamp from cv_cases
cv_cases <- read.csv("input_data/coronavirus.csv", as.is = TRUE)
last_update <- tail(cv_cases$date, 1)
if (as.Date(last_update) != Sys.Date() -1) {
  source("jhu_data_full.R") #  run locally to update numbers
}

# Covid-19 Dataprocessing - needed even if update is unnecesary since it introduces
# names needed later
source("covid_process.R")
country_codes = read.csv("input_data/countries_codes_and_coordinates.csv", as.is = TRUE)

### Prediction tools ###

## Exponential Prediction ##
source("pred.R")

f <- file.path("outputs", paste0("predictions_", refday + 1, ".csv"))
exp_d <- read.csv(f, as.is = TRUE)
f_deaths <- file.path("outputs", paste0("predictions_deaths_", refday + 1, ".csv"))
exp_d_deaths <- read.csv(f_deaths, as.is = TRUE)

get_prediction_range <- function() {
  exp_d %>% 
    dplyr::filter(observed == "Predicted") %>% 
    dplyr::select(date) %>% 
    pull() %>% 
    (function(v) return(c(min(as.Date(v)), max(as.Date(v)))))
}

get_number_countries <- function() {
  exp_d %>% 
    dplyr::select(country) %>% 
    pull() %>% 
    unique() %>% 
    length()
}


## ARIMA & Facebook Prophet##
source("arima_pred.R")
source("prophet_pred.R")

# Get Prophet Datatable
f <- file.path("outputs", paste0("predictions_prophet_", refday + 1, ".csv"))
prophet_d <- read.csv(f, as.is = TRUE)
f_deaths <- file.path("outputs", paste0("predictions_prophet_deaths_", refday + 1, ".csv"))
prophet_d_deaths <- read.csv(f_deaths, as.is = TRUE)

# Get ARIMA Datatable
f <- file.path("outputs", paste0("predictions_arima_", refday + 1, ".csv"))
arima_d <- read.csv(f, as.is = TRUE)
f_deaths <- file.path("outputs", paste0("predictions_arima_deaths_", refday + 1, ".csv"))
arima_d_deaths <- read.csv(f_deaths, as.is = TRUE)

# Create a list of model data frames
choices <- c("Exponential Regression", "ARIMA", "FBProphet")
pred_data <- list(exp_d, arima_d, prophet_d)
pred_death_data <- list(exp_d_deaths, arima_d_deaths, prophet_d_deaths)
names(pred_data) <- choices 
names(pred_death_data) <- choices 

# A dictionary for model selection
model_dict <- 1:length(choices)
names(model_dict) <- choices

## Model dispatch function ##
model_dispatch <- function(which.country = "Spain", is.cases = TRUE, model_typ = "Exponential Regression") {
  if(model_typ == "Exponential Regression") {
    get_country_all_info(which.country = which.country, is.cases = is.cases)
  }
  else if(model_typ == "FBProphet") {
    get_country_all_info_prophet(which.country = which.country, is.cases = is.cases)
  }
  else {
    get_country_all_info_arima(which.country = which.country, is.cases = is.cases)
  }
}


# create plotting parameters for map
bins = c(0,1,10,50,100,500)
cv_pal <- colorBin("Blues", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# create cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Exponential Regression", "ARIMA", "FBProphet"),
    options = layersControlOptions(collapsed = TRUE)) %>% 
  hideGroup(c("ARIMA", "FBProphet"))  %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  fitBounds(~-100,-50,~80,80)
  #addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
   #         title = "<small>Active cases per 100,000</small>")


### SHINY UI ###
# App parameters
max = 8
covid_cols <- c("#FF7F50ff", "#638fB4ff")
names(covid_cols) <- c("Observed", "Predicted")


options(DT.options = list(
  pageLength = 50,
  dom = "t",
  rownames = FALSE,
  language = list(search = "filter:")
))


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("paper"), collapsible = TRUE,
             "COVID-19 Tracker", id="nav",
             tabPanel("Map",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        h3(textOutput("projective_case_count"), align = "right"),
                                        span(h4(textOutput("projective_new_case_count"), align = "right"), style="color:#cc4c02"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        
                                        sliderInput("plot_date",
                                                    label = h5("Select date"),
                                                    min = as.Date("2020-02-01","%Y-%m-%d"),
                                                    max = as.Date(current_date,"%Y-%m-%d") + max,
                                                    value = as.Date(current_date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = 750, loop = FALSE))
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 30, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.yale.edu', tags$img(src='yale-blue@2x.png',height='60',width='80')))
                      )
             ),
        
             tabPanel("Prediction",
               h4(textOutput("prediction_range")),
               fluidRow(
                 box(
                   title = "Filter Parameters",
                   selectInput("country", "Choose a country:", 
                               selected = "Switzerland",
                               countries),
                   selectInput("model_select", "Model:",   
                                selected = "Exp_Lm",
                                choices)
                 ),
                 
                 box(title = "Growth rates",
                     paste("Number of cases:"),
                     textOutput("growth_rate"),
                     "Number of deaths:",
                     textOutput("growth_rate_deaths")
                 )
               ),
               
               fluidRow(box(
                 width = 12,
                 title = "",
                 plotlyOutput("plot_cases_cumulative", height = 250)
               )),
               
               fluidRow(box(
                 width = 12,
                 title = "",
                 plotlyOutput("plot_cases_daily", height = 250)
               )),
               
               fluidRow(box(
                 width = 12,
                 title = "",
                 plotlyOutput("plot_deaths_cumulative", height = 250)
               )),
               
               fluidRow(box(
                 width = 12,
                 title = "",
                 plotlyOutput("plot_deaths_daily", height = 250)
               )),
               
               h2("Number of Cases"),
               fluidRow(fluidRow(column(
                 12,
                 DTOutput('table')
               ))),
               
               h2("Number of Deaths"),
               fluidRow(fluidRow(column(
                 12,
                 DTOutput('table_deaths')
               )))
             ),
             
             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                         "Johns Hopkins Center for Systems Science and Engineering.")
             ),
             
             
             tabPanel("Acknowledgements",  tags$div(
               tags$h2(strong("Acknowledgements")), 
               "When building the shiny app, we were inspired by the recent work of Edward Parker from The Vaccine Center at London School of Hygiene & Tropical Medicine.",tags$br(),
               "When making forecasts with prophet algorithm, we used the R package created by data scientists at Facebook.",tags$br(),
               tags$br(),
               tags$h2(strong("Code")),
               "Code and input data used to generate this Shiny app can be accessed from the linked Github repository:",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
               tags$br(),
               tags$h2(strong("Sources")),
               tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),tags$br(),
               tags$b("US state-level case and testing data: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", "Johns Hopkins Center for Systems Science and Engineering github page,"),tags$br(),
               tags$b("Global testing data: "), tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data/", "Our World in Data"),tags$br(),
               tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),tags$br(),
               tags$br(),
               tags$h2(strong("Authors")),
               "Fatih Celikbas, Yale University",tags$br(),
               "Yavuz Ramiz Colak, Yale University",tags$br(),
               tags$br(),
               tags$h2(strong("Contact")),
               "fatih.celikbas@yale.edu",tags$br(),
               "ramiz.colak@yale.edu",tags$br(),
               tags$img(src = "yale-blue@2x.png", width = "150px", height = "150px")
             ))
    )          
)


### SHINY SERVER ###

server = function(input, output, session) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.Date(input$plot_date),"%d %B %Y")
  })
  
  # reactive databases to use for the map sliced based on input date given
  reactive_db_exp = reactive({
    large_countries <- exp_d %>% 
                       dplyr::filter(date == input$plot_date) %>%
                       merge(., country_codes[, c("country", "alpha3", "latitude", "longitude")],
                             by = "country") %>%
                       dplyr::filter(!is.na(alpha3))
    large_countries$color <- sapply(large_countries$observed, function(x) {covid_cols[x]})
    large_countries
  })
  reactive_db_arima = reactive({
    large_countries <- arima_d %>% 
      dplyr::filter(date == input$plot_date) %>%
      merge(., country_codes[, c("country", "alpha3", "latitude", "longitude")],
            by = "country") %>%
      dplyr::filter(!is.na(alpha3))
    
    large_countries$color <- sapply(large_countries$observed, function(x) {covid_cols[x]})
    large_countries
  })
  reactive_db_prophet = reactive({
    large_countries <- prophet_d %>% 
      dplyr::filter(date == input$plot_date) %>%
      merge(., country_codes[, c("country", "alpha3", "latitude", "longitude")],
            by = "country") %>%
      dplyr::filter(!is.na(alpha3))
    
    large_countries$color <- sapply(large_countries$observed, function(x) {covid_cols[x]})
    large_countries
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  group_selected <-  reactive({ input$mymap_groups[1] })
  
  output$projective_case_count <- renderText({
    db <- reactive_db_exp()
    
    x <- group_selected()
    if (!is.null(x)) {
      if (x == "ARIMA") { db <- reactive_db_arima() }
      else if (x == "FBProphet") { db <- reactive_db_prophet()  }
    }
    
    paste0(prettyNum(sum(db$cumulative), big.mark=","), " total cases")
  })
  
  output$projective_new_case_count <- renderText({
    db <- reactive_db_exp()
    
    x <- group_selected()
    if (!is.null(x)) {
      if (x == "ARIMA") { db <- reactive_db_arima() }
      else if (x == "FBProphet") { db <- reactive_db_prophet()  }
    }
    
    paste0(prettyNum(sum(db$per.day), big.mark=","), " new cases")
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      addCircleMarkers(data = reactive_db_exp(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative)^(1/5), 
                       fillOpacity = 0.25, color = ~ color, group = "Exponential Regression",
                       label = sprintf("<strong>%s </strong><br/>Confirmed cases: %g<br/>Daily cases: %d", reactive_db_exp()$country, reactive_db_exp()$cumulative, reactive_db_exp()$per.day) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db_arima(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative)^(1/5), 
                       fillOpacity = 0.25, color = ~ color, group = "ARIMA",
                       label = sprintf("<strong>%s </strong><br/>Confirmed cases: %g<br/>Daily cases: %d", reactive_db_arima()$country, reactive_db_arima()$cumulative, reactive_db_arima()$per.day) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db_prophet(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative)^(1/5), 
                       fillOpacity = 0.25, color = ~ color, group = "FBProphet",
                       label = sprintf("<strong>%s </strong><br/>Confirmed cases: %g<br/>Daily cases: %d", reactive_db_prophet()$country, reactive_db_prophet()$cumulative, reactive_db_prophet()$per.day) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px", direction = "auto"))
  })
  
  
  # prediction tab
  output$prediction_range <- renderText({
    pred_range <- get_prediction_range()
    s <- paste("Forecasts: from", format(pred_range[1],"%A, %B %d, %Y"),
               "to", format(pred_range[2],"%A, %B %d, %Y"), "for", 
               get_number_countries(), "countries and territories")
    s
  })
  
  output$growth_rate <- renderText({
    ic <- input$country
    rate <- round((calculate_growth_rate(ic) - 1) * 100, 0)
    if (rate <= 15)
      txt <- paste0(rate, "%: SLOWING")
    if (rate > 15 & rate <= 25)
      txt <- paste0(rate, "%: SUSTAINED")
    if (rate > 25)
      txt <- paste0(rate, "%: FAST")
    txt
  })
  
  output$growth_rate_deaths <- renderText({
    ic <- input$country
    rate <- round((calculate_growth_rate(ic, is.cases = TRUE) - 1) * 100, 0)
    if (rate <= 15)
      txt <- paste0(rate, "%: SLOWING")
    if (rate > 15 & rate <= 25)
      txt <- paste0(rate, "%: SUSTAINED")
    if (rate > 25)
      txt <- paste0(rate, "%: FAST")
    txt
  })
  
  output$plot_cases_cumulative <- renderPlotly({
    ic <- input$country
    model <- input$model_select
    past <- model_dispatch(which.country = ic, model_typ = model)
    plot_ly(
      x = past$date,
      y = past$cumulative,
      type = "bar",
      color = factor(past$observed, levels = c(unique(past$observed), "NA"))
    ) %>% layout(title = paste("Cumulative number of cases for", input$country, "on", refday))
    
    
  })
  
  output$plot_cases_daily <- renderPlotly({
    ic <- input$country
    model <- input$model_select
    past <- model_dispatch(which.country = ic, model_typ = model)
    
    plot_ly(
      x = past$date,
      y = past$per.day,
      type = "bar",
      color = factor(past$observed, levels = c(unique(past$observed), "NA"))
    ) %>% layout(title = paste("Daily number of cases for", ic, "on", refday))
    
  })
  
  output$plot_deaths_cumulative <- renderPlotly({
    ic <- input$country
    model <- input$model_select
    past <- model_dispatch(which.country = ic, model_typ = model, is.cases = FALSE)
    
    plot_ly(
      x = past$date,
      y = past$cumulative,
      type = "bar",
      color = factor(past$observed, levels = c(unique(past$observed), "NA"))
    ) %>% layout(title = paste("Cumulative number of deaths for", ic, "on", refday))
  })
  
  output$plot_deaths_daily <- renderPlotly({
    ic <- input$country
    model <- input$model_select
    past <- model_dispatch(which.country = ic, model_typ = model, is.cases = FALSE)
    
    plot_ly(
      x = past$date,
      y = past$per.day,
      type = "bar",
      color = factor(past$observed, levels = c(unique(past$observed), "NA"))
    ) %>% layout(title = paste("Daily number of deaths for", ic, "on", refday))
    
  })
  
  preds <- reactive({
    pred_data[[input$model_select]] %>% 
      dplyr::filter(country == input$country) %>% 
      mutate(date = paste(weekdays(as.Date(date)), date)) %>% 
      dplyr::select(Date = date, `Daily count` = per.day, 
             Cumulative = cumulative, `Observed/Predicted` = observed)
  })
  
  preds_deaths <- reactive({
    pred_death_data[[input$model_select]] %>% 
      dplyr::filter(country == input$country) %>% 
      mutate(date = paste(weekdays(as.Date(date)), date)) %>% 
      dplyr::select(Date = date, `Daily count` = per.day, 
             Cumulative = cumulative, `Observed/Predicted` = observed)
  })
  
  output$table <-  renderDT(
    datatable(tail(preds(), 11), rownames = FALSE)  %>%
      formatStyle(
        "Observed/Predicted",
        target = "row",
        color = styleEqual(c("Observed", "Predicted"), c("green", "darkblue"))
      )
  )
  
  output$table_deaths <-  renderDT(
    datatable(tail(preds_deaths(), 11), rownames = FALSE)  %>%
      formatStyle(
        "Observed/Predicted",
        target = "row",
        color = styleEqual(c("Observed", "Predicted"), c("green", "darkblue"))
      )
  )
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_today$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                      recovered, new_recovered, active_cases, 
                                      per100k, newper100k, activeper100k)), file, row.names = FALSE)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                     recovered, new_recovered, active_cases)), input$maxrows))
    options(orig)
  })
  
}



shinyApp(ui, server)