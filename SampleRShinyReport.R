library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Homelessness in New Jersey: Weekly Update"),
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1200px;
        margin: 0 auto;
      }
      h2, h3 {
        color: #2e4053;
        font-weight: bold;
      }
      h3 {
        margin-top: 20px;
        margin-bottom: 10px;
      }
      .summary {
        background-color: #f0f3f4;
        padding: 10px 20px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .plot-container {
        border: 1px solid #d5dbdb;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .plot-title {
        color: #2e4053;
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 10px;
      }
    "))
  ),
  
  # Statewide Overview
  fluidRow(
    column(12,
           h2("Statewide Overview"),
           div(class = "summary", textOutput("statewide_stats"))
    )
  ),
  
  # This Week's Data
  fluidRow(
    column(6,
           h3("This Week"),
           div(class = "summary", tableOutput("this_week_stats"))
    ),
    column(6,
           h3("This Week's Housing Outcomes"),
           div(class = "summary", tableOutput("housing_outcomes"))
    )
  ),
  
  # County-Level Statistics
  fluidRow(
    column(6,
           h3("Count of Homeless Individuals by County"),
           div(class = "plot-container", plotlyOutput("county_count_plot"))
    ),
    column(6,
           h3("Homelessness Rate by County"),
           div(class = "plot-container", plotlyOutput("homelessness_rate_plot"))
    )
  ),
  
  # County Statistics
  fluidRow(
    column(12,
           h3("County Statistics"),
           dataTableOutput("county_stats")
    )
  ),
  
  # Data, Methodology, and Limitations
  fluidRow(
    column(12,
           h3("Data, Methodology, and Limitations"),
           div(class = "summary", verbatimTextOutput("methodology"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Statewide Overview
  output$statewide_stats <- renderText({
    paste("In New Jersey, there are currently", nrow(data), "individuals experiencing homelessness, including", veterans, "veterans.")
  })
  
  # This Week's Data
  output$this_week_stats <- renderTable({
    this_week_homeless_summary <- this_week_homeless %>%
      group_by(hmis_project_type) %>%
      count() %>%
      arrange(desc(n))
    this_week_homeless_summary
  })
  
  # This Week's Housing Outcomes
  output$housing_outcomes <- renderTable({
    total_found_housing <- total_found_housing <- current_week_data %>%
      filter(!is.na(discharge_date)) %>%
      group_by(destination_category) %>%
      count() %>%
      filter(destination_category == "Temporary Housing Situations" | destination_category == "Permanent Housing Situations") %>%
      na.omit() %>%
      summarise(total = sum(n))
    total_found_housing
  })
  
  # County-Level Statistics
  output$county_count_plot <- renderPlotly({
    ggplotly(ggplot(count_by_county) +
               aes(x = reorder(program_county, county_total), y = county_total, fill = county_total) +
               geom_col() +
               scale_fill_viridis_c(option = "viridis", direction = 1) +
               labs(x = "County Total", y = "County", title = "Count of Homeless Individuals by County", subtitle = "As measured by individuals reported in HMIS programs") +
               coord_flip() +
               theme_minimal())
  })
  
  output$homelessness_rate_plot <- renderPlotly({
    ggplotly(ggplot(county_count_sf) +
               geom_sf(aes(fill = homelessness_rate)) +
               scale_fill_viridis_c() +
               labs(title = "Homelessness Rate by County", fill = "Total"))
  })
  
  # County Statistics
  output$county_stats <- renderDataTable({
    count_by_county
  })
  
  # Data, Methodology, and Limitations
  output$methodology <- renderPrint({
    "Data was sourced from an HMIS export. Deduplication was handled using a unique personal ID number. Extensive data cleaning was performed to derive county information from program and program division fields."
  })
}

# Run the application
shinyApp(ui = ui, server = server)
