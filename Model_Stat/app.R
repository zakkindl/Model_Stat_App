# Load packages
packages <- c('shiny','tidyverse','googlesheets4','shinycssloaders','DT','plotly','gridlayout','bslib')
lapply(packages, library, character.only = TRUE)

##### Grid Layout UI ####
ui <- grid_page(
  theme = bs_theme(base_font = "Helvetica Neue"),
  
  layout = c(
    "header  header   header  ",
    "sidebar plot     plot    ",
    "sidebar table    table   "
  ),
  row_sizes = c(
    "80px",
    "1.5fr",
    "1fr"
  ),
  col_sizes = c(
    "300px",
    "1fr",
    "1fr"
  ),
  gap_size = "1rem",
  
  # Header
  grid_card_text(
    area = "header",
    content = "Model Statistic & CV Dashboard",
    alignment = "center",
    is_title = TRUE
  ),
  
  # Sidebar with controls
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
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
    )
  ),
  
  # Plot area
  grid_card(
    area = "plot",
    card_header("Performance Changes"),
    card_body(
      plotlyOutput("plotly_plot")
    )
  ),
  
  # Table area
  grid_card(
    area = "table",
    card_header("Comparison Data"),
    card_body(
      DTOutput("comparison_data")
    )
  )
)

#### Server ####
server <- function(input, output, session) {  
  
  # Show loading message while imported data loads from sheet
  showNotification("Loading data...", type = "message", duration = 3, id = "loading")
  
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
  
  # Weeks filtered based on season selected
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
  
  # Filtered data for comparing weeks selected
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
  
  # Timepoint 1
  timepoint_1 <- reactive({
    req(table_data(), input$week_one_select)
    
    table_data() |>
      filter(week == input$week_one_select,
             assessment == "CMJ (Force Plates)") |>  
      select(athletes, date, assessment, metric, value, week) |>
      group_by(metric, date) |>  
      summarise(mean_value = mean(value, na.rm = TRUE),
                sd_value = sd(value,na.rm = TRUE),
                athletes = first(athletes),
                assessment = first(assessment),
                week = first(week),
                .groups = 'drop') 
  })
  
  # Timepoint 2
  timepoint_2 <- reactive({
    req(table_data(), input$week_two_select)
    
    table_data() |>
      filter(week == input$week_two_select,
             assessment == "CMJ (Force Plates)") |>  
      select(athletes, date, assessment, metric, value, week) |>
      group_by(metric, date) |>  
      summarise(mean_value = mean(value, na.rm = TRUE),
                sd_value = sd(value,na.rm = TRUE),
                athletes = first(athletes),
                assessment = first(assessment),
                week = first(week),
                .groups = 'drop')
  })
  
  # Comparison Data
  comparison_data <- reactive({
    req(timepoint_1(), timepoint_2())
    
    # Join the two data sets by metric
    timepoint_1() |>
      select(metric, mean_value,sd_value, week, date) |>
      rename(mean_t1 = mean_value,
             sd_t1 = sd_value,
             week_t1 = week, 
             date_t1 = date) |>
      full_join(
        timepoint_2() |>
          select(metric, mean_value,sd_value, week, date) |>
          rename(mean_t2 = mean_value,
                 sd_t2 = sd_value,
                 week_t2 = week,
                 date_t2 = date),
        by = "metric"
      ) |>
      group_by(metric) |> 
      summarise(
        abs_mean_diff = first(abs(mean_t1 - mean_t2)),
        sd_mean = first(((sd_t1^2 + sd_t2^2)/2)^.5),
        percent_change = first(((mean_t2 - mean_t1) / mean_t1) * 100),
        critical_diff = first(sd_mean*1.3733),
        model_stat_significance = case_when(abs_mean_diff > critical_diff ~ "significant_diff",
                                            abs_mean_diff <= critical_diff ~ "non-significant_diff"),
        cv = first(sd_t1/mean_t1)*100,
        cv_significance = case_when(abs(percent_change) > cv ~ "true_difference",
                                    abs(percent_change) <= cv ~ "trivial_difference"),
        .groups = 'drop')
  })
  
  # Data table output
  output$comparison_data <- renderDT({
    comparison_data()
  }, options = list(
    pageLength = 8,
    scrollX = TRUE
  ))
  
  # Plotly output
  output$plotly_plot <- renderPlotly({
    req(comparison_data(), input$week_one_select, input$week_two_select)
    
    plot_data <- comparison_data() %>%
      mutate(color_group = case_when(
        model_stat_significance == "non-significant_diff" ~ "Non-Significant",
        model_stat_significance == "significant_diff" & percent_change < 0 ~ "Significant Decrease",
        model_stat_significance == "significant_diff" & percent_change > 0 ~ "Significant Increase",
        TRUE ~ "Non-Significant"
      ))
    
    p <- plot_data %>%
      plot_ly(x = ~metric, 
              y = ~percent_change,
              color = ~color_group,
              colors = c("Non-Significant" = "grey", 
                         "Significant Decrease" = "red", 
                         "Significant Increase" = "darkgreen"),
              type = "bar",
              hovertemplate = paste0("<b>%{x}</b><br>",
                                     "Change: %{y:.1f}%<br>",
                                     "<extra></extra>")) %>%
      layout(title = paste0("CMJ Metrics Week ",input$week_one_select, " to Week ", input$week_two_select),
             xaxis = list(title = ""),
             yaxis = list(title = "Percent Change (%)"),
             legend = list(title = "Change Type"),
             font = list(family = "Helvetica Neue"))
    p
  })
}

shinyApp(ui = ui, server = server)