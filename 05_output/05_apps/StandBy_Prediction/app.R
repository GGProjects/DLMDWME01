#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

require(shiny)
require(shinydashboard)
require(readr)
require(DT)
require(lubridate)
require(fpp3)
require(dplyr)
require(fable.prophet)
require(shinyjs)
require(plotly)

source("../../../03_main/pred_sby.R", local = TRUE)  

# User Interface
ui <- dashboardPage(
    skin = "red",
    
    dashboardHeader(
        title = tags$div(
            style = "display: flex; align-items: center;",
            tags$img(src = "drk_logo.png", height = "40px", style = "margin-right: 10px;"),
            span("Einsatzfahrten", style = "font-family: 'Open Sans', sans-serif; color: white; font-size: 22px; font-weight: bold;")
        )
    ),
    
    dashboardSidebar(
        tags$head(
            tags$style(HTML("
        .main-header .logo {
          background-color: #e60005 !important;
          color: white !important;
          font-family: 'Open Sans', sans-serif;
          font-weight: bold;
        }
        .navbar {
          background-color: #e60005 !important;
        }
        .main-sidebar { background-color: #e60005 !important; }
        .sidebar-menu .menu-item a, .sidebar-menu .menu-item a span {
          color: white !important;
          font-family: 'Open Sans', sans-serif;
          font-weight: bold;
        }
        .sidebar-menu .menu-item a:hover {
          background-color: #b30000 !important;
        }
        body, label, input, button, select {
          font-family: 'Open Sans', sans-serif;
        }
        .btn-danger {
          background-color: #e60005 !important;
          border-color: #e60005 !important;
        }
        .btn-danger:hover {
          background-color: #b30000 !important;
          border-color: #b30000 !important;
        }
      "))
        ),
        sidebarMenu(
            menuItem("Dateneingabe", tabName = "eingabe", icon = icon("pencil-alt")),
            menuItem("Vorhersage StandBy", tabName = "vorhersage", icon = icon("chart-line"))
        )
    ),
    
    dashboardBody(
      useShinyjs(),
        tabItems(
            # Eingabe-Tab
            tabItem(tabName = "eingabe",
                    fluidRow(
                        box(title = span("Neuen Eintrag hinzufügen", style = "color: #ffffff; font-weight: bold;"),
                            status = "danger", solidHeader = TRUE, width = 12,
                            dateInput("date", "Datum", value = Sys.Date()),
                            numericInput("n_duty", "Anzahl diensthabender Einsatzfahrer:innen", value = 1900, min = 0),
                            numericInput("n_sick", "Anzahl der Krankmeldungen", value = 0, min = 0),
                            numericInput("calls", "Anzahl eingegangener Notrufe", value = 0, min = 0),
                            numericInput("n_sby", "Anzahl des eingeteilten Bereitschaftspersonals", value = 0, min = 0),
                            numericInput("sby_need", "Bedarf an Bereitschaftspersonal", value = 0, min = 0),
                            numericInput("dafted", "Zusätzlich aktivierte Fahrer:innen", value = 0, min = 0),
                            actionButton("save_btn", "Eintrag speichern", class = "btn-danger"),
                            verbatimTextOutput("save_status"),
                        )
                    )
            ),
            
            # Vorhersage-Tab
            tabItem(tabName = "vorhersage",
                    fluidRow(
                        box(title = span("Vorhersage-Parameter", style = "color: #ffffff; font-weight: bold;"),
                            status = "danger", solidHeader = TRUE, width = 12,
                            fluidRow(
                                column(6, dateInput("forecast_until", "Vorhersage bis", value = as.Date("2019-05-27") + 62)),
                                column(6, numericInput("n_duty_forecast", "Anzahl diensthabendes Personal", value = 1900, min = 0))
                            ),
                            actionButton("predict_btn", "Vorhersage ausführen", class = "btn-danger")
                        )
                    ),
                    fluidRow(
                        box(title = span("Vorhersage-Ergebnisse", style = "color: #ffffff; font-weight: bold;"),
                            status = "danger", solidHeader = TRUE, width = 6,
                            DTOutput("forecast_table")
                        ),
                        box(title = span("Vorhersage-Plot", style = "color: #e60005; font-weight: bold;"),
                            status = "danger", solidHeader = TRUE, width = 6,
                            plotlyOutput("forecast_plot")
                        )
                    ) #,
                    # fluidRow(
                    #   box(title = span("Debug", style = "color: #ffffff; font-weight: bold;"),
                    #       status = "danger", solidHeader = TRUE, width = 12,
                    #       verbatimTextOutput("debug")
                    #   )
                    # )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
  
  forecast_data <- reactiveVal(NULL)
    
    csv_file <- "../../../01_data/02_warehouse/recorded_personel_daywise.csv"
    
    observeEvent(input$predict_btn, {
      
      showModal(modalDialog(
        title = "Bitte warten!",
        footer = NULL,
        easyClose = FALSE,
        fade = TRUE,
        size = "s",
        tags$div(style = "text-align: center;",
                 tags$img(src = "drk_logo.png", height = "100px"),  # Lege eine spinner.gif in www/
                 tags$p("Vorhersage wird berechnet...", style = "font-weight: bold; color: #e60005;")
        )
      ))
      
     if (file.exists(csv_file)) {
        df <- read_csv(csv_file, show_col_types = FALSE)
        
        fc <- predict_sby(data = df,
                          model = "prophet",
                          fcperiod = as.numeric(input$forecast_until - as.Date(max(df$date)) + 7),
                          onduty = input$n_duty_forecast)
        
        write.csv(fc, 
                  file = paste0("../../02_data/sby_prediction_", 
                                Sys.Date(), 
                                ".csv"),
                  row.names = FALSE)
        forecast_data(fc)
      } else {
        forecast_data(NULL)
      }
      
      removeModal()
    })
    
    output$forecast_table <- renderDT({
      req(forecast_data())
      datatable(select(forecast_data(), 
                       Datum = date,
                       Bedarf = sby),
                rownames = FALSE)
    }, width = "50%")
    
    output$forecast_plot <- renderPlotly({
      req(forecast_data())
      forecast_df <- select(forecast_data(), 
                            Datum = date,
                            Bedarf = sby)
      
      plot_ly(forecast_df, x = ~Datum, y = ~Bedarf, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#e60005'),
              marker = list(color = '#e60005', size = 8)) %>%
        layout(title = list(text = "Bedarf an Bereitschaftspersonal", font = list(color = "#000000", size = 16, family = "Arial Black")),
               xaxis = list(title = "Datum"),
               yaxis = list(title = "Bedarf"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white')
    })
    
    # output$debug <- renderPrint(max(df$date))
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
