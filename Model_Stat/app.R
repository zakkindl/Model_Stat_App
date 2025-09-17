# Load packages
packages <- c('shiny','tidyverse','googlesheets4','shinycssloaders','DT')

lapply(packages, library, character.only = TRUE)


##### Start UI ####

ui <- fluidPage(
  # App title 
  titlePanel("Model Statistic Dashboard"),
  
  #### Sidebar ####
  
  sidebarLayout(

        sidebarPanel(
      
      selectInput("player_select", 
                  "Choose Player:",
                  choices = "Loading..."),
      
      radioButtons("category_select",
                  "Category:",
                  choices = "Loading..."),
      
      selectInput("year_select",
                   "Year:",
                   choices = "Loading..."),
      
      selectInput("week_one_select",
                  "Select First Week",
                  choices = "Loading..."),
      
      selectInput("week_two_select",
                  "Select Second Week",
                  choices = "Loading...")
      
    ),
    
 #### Main Panel ####   
    mainPanel(
      
      h3("Filtered Data"),
              DT::dataTableOutput("data_table"),
      
      h3("Filtered Data"),
      DT::dataTableOutput("data_table_2")
      
      # Add your outputs here
      
    )
  )
)

#### Server ####

server <- function(input, output, session) {  
  
  # Show loading message while imported data loads from sheet
  showNotification("Loading data...", type = "message", duration = 2, id = "loading")
  
  # Import data from public googlesheet
  sheet_data <- reactive({
    csv_url <- "https://docs.google.com/spreadsheets/d/1br-_Wjw2yhMqsK0qp-DiScOOaO4vd9sPjhgNhXSymnk/export?format=csv&gid=0"
    
    # Some data cleaning/formatting 
    read_csv(csv_url) |> 
      na.omit() |>
      mutate(value = as.numeric(value),
             date = mdy(date),
             month = month(date),
             year = year(date),
             week = week(date),
             athletes = toupper(athletes),
             category = case_when(
               week %in% 3:7 ~ "Pre-Season",
               week %in% 8:41 ~ "In-Season", 
               week %in% c(42:52, 1:2) ~ "Off-Season",
               TRUE ~ NA_character_
             ))
  })
  
  
  # Player choices Select Input
  observe({
    req(sheet_data())  
    
    player_choices <- sheet_data() %>%
      distinct(athletes) %>%  
      pull(athletes) %>%
      sort()
    
    updateSelectInput(session, "player_select", 
                      choices = player_choices)
  })
  
  # Season Category Select Input
  observe({
    req(sheet_data())  
    
    season_category <- sheet_data() %>%
      distinct(category) %>%  
      pull(category)
    
    updateRadioButtons(session, "category_select", 
                       choices = season_category)
  })
  
  # Years Select Input 
  observe({
    req(sheet_data())  
    
    years <- sheet_data() |> 
      distinct(year) |> 
      pull(year) |> 
      sort()
    
    updateSelectInput(session, "year_select", 
                      choices = years, selected = 2021)
  })
  
  # Filter data based on selected category, year
  filtered_data <- reactive({
    req(sheet_data(), input$category_select, input$year_select)
    
    sheet_data() %>%
      filter(category == input$category_select,
             year == input$year_select)
    
  })
  
  # Weeks Select Input based on season category selected
  observe({
    req(filtered_data())  
    
    available_weeks <- filtered_data() |> 
      distinct(week) |> 
      pull(week) |> 
      sort()
    
    updateSelectInput(session, "week_one_select", 
                      choices = available_weeks)
    
    updateSelectInput(session, "week_two_select", 
                      choices = available_weeks)
  })
  
  # Filtered data for comparison weeks selected
  table_data <- reactive({
    req(sheet_data(), input$category_select, input$year_select, 
        input$week_one_select, input$week_two_select)
    
    sheet_data() %>%
      filter(category == input$category_select,
             year == input$year_select,
             athletes == input$player_select,
             week >= min(input$week_one_select, input$week_two_select),
             week <= max(input$week_one_select, input$week_two_select))
  })

  
  timepoint_1 <- reactive({
    req(table_data(), input$week_one_select)
    
    table_data() |>
      filter(week == input$week_one_select,
             assessment == "CMJ (Force Plates)") |>  
      select(athletes, date, assessment, metric, value, week) |>
      group_by(metric, date) |>  
      summarise(mean_value = mean(value, na.rm = TRUE),
                athletes = first(athletes),
                assessment = first(assessment),
                week = first(week),
                .groups = 'drop') 
  })
  
  timepoint_2 <- reactive({
    req(table_data(), input$week_two_select)
    
    table_data() |>
      filter(week == input$week_two_select,
             assessment == "CMJ (Force Plates)") |>  
      select(athletes, date, assessment, metric, value, week) |>
      group_by(metric, date) |>  
      summarise(mean_value = mean(value, na.rm = TRUE),
                athletes = first(athletes),
                assessment = first(assessment),
                week = first(week),
                .groups = 'drop')
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    timepoint_2()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  # Data table output
  output$data_table_2 <- DT::renderDataTable({
    timepoint_2()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  
}

shinyApp(ui = ui, server = server)