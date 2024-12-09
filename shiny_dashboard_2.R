# Interactive dashboard

# Libraries
library_file <- 'libraries'

packages <- readLines(library_file)

lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
})


# Read the file filtered at the previous stage

df <- read.csv(here("data", "cleaned_data.csv"))

df$publication_date <- as.Date(df$publication_date)





ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Surveillance dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "activeTab",
      menuItem("Map overview", tabName = "tab1", icon = icon("globe")),
      menuItem("Facet", tabName = "tab4", icon = icon("chart-line")),
      menuItem("Disease and Syndrome Trends", tabName = "tab2", icon = icon("chart-line")),
      menuItem("Forecast", tabName = "tab3", icon = icon("chart-bar"))
    ),
    
    
    # Widgets for Tab 1
    conditionalPanel(
      condition = "input.activeTab == 'tab1'",
      dateRangeInput(
        inputId = "date_range_tab1",
        label = "Select Date Range:",
        start = max(as.Date(df$publication_date)),
        end = max(df$publication_date),
        min = min(df$publication_date),
        max = max(df$publication_date)
      ),
      radioButtons(
        "selected_country",
        "Select Country:",
        choices = c("All", sort(unique(df$country))),
        selected = "All"),
      checkboxInput("show_cluster", "Enable Marker Clustering", value = TRUE)
    ),
    
    
    # Widgets for Tab 2
    conditionalPanel(
      condition = "input.activeTab == 'tab2'",
      selectInput("country", "Select Country", choices = c("All", unique(df$country)), selected = "All"),
      selectInput("disease", "Select Disease", choices = c("All", unique(df$disease_name)), selected = "All"),
      selectInput("syndromes", "Select Syndromes", choices = unique(df$syndrome_name), multiple = TRUE),
      dateRangeInput("date_range", "Select Date Range",
                     start = min(df$publication_date),
                     end = max(df$publication_date),
                     min = min(df$publication_date),
                     max = max(df$publication_date),
                     format = "yyyy-mm-dd"),
      actionButton("select_all", "Select All Syndromes"),
      actionButton("clear_all", "Clear All Syndromes")
    ),
    
    
    
    # Widgets for Tab 3
    conditionalPanel(
      condition = "input.activeTab == 'tab3'",
      selectInput("disease_selection", "Select Disease:",
                  choices = sort(unique(df$disease_name)),
                  selected = "covid19"),
      radioButtons(
        "select_country", "Select Country:",
        choices = sort(unique(df$country)),
        selected = "United States"
      )
    ),
    
    
    # Widgets fot Tab 4
  conditionalPanel(
  condition = "input.activeTab == 'tab4'",
  selectInput(
    inputId = "selected_disease_tab4",
    label = "Select Disease for Plot:",
    choices = NULL  # Will be updated dynamically
  ),
  dateRangeInput("date_range_tab4", "Select Date Range",
    start = max(df$publication_date) - days(8),
    end = max(df$publication_date),
    min = min(df$publication_date),
    max = max(df$publication_date),
    format = "yyyy-mm-dd"),
 
)
    
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab1",
              leafletOutput("WorldMapPlot"),
              br(),
              textOutput("filtered_count"),
              br(),
              DTOutput("summaryDiseaseTable")
      ),
      tabItem(tabName = "tab2",
              plotOutput("linePlot")
      ),
      tabItem(tabName = "tab3",
              p("ARIMA Forecast"),
              br(),
              plotOutput("forecast_plot")
      ),
      
      tabItem(tabName = "tab4",
              p("Graph"),
              br(),
              plotOutput("tab4_plot")
            
              
    )
  )
)
)

# Server
server <- function(input, output, session) {
  # Filter data for the first tab.
  filteredDataTab1 <- reactive({
    req(input$selected_country, input$date_range_tab1)
    data_filtered <- df %>%
      filter(
        publication_date >= input$date_range_tab1[1] & publication_date <= input$date_range_tab1[2],
        if (input$selected_country != "All") country == input$selected_country else TRUE
      )
    return(data_filtered)
  })
  
  # Show the number of filtered report at the first Tab
  output$filtered_count <- renderText({
    df_filtered <- filteredDataTab1()
    paste("Number of Reports in the chosen period:", nrow(df_filtered))
  })
  
  # Render the leaflet map
  output$WorldMapPlot <- renderLeaflet({
    # Initialize Leaflet map centered globally
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Observe filtered data and update map
  observe({
    df_filtered <- filteredDataTab1()
    
    # Clear existing markers
    leafletProxy("WorldMapPlot") %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (input$show_cluster) {
      # Add clustered markers
      leafletProxy("WorldMapPlot", data = df_filtered) %>%
        addMarkers(
          ~longitude, ~latitude,
          clusterOptions = markerClusterOptions(),
          popup = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Disease or Syndrome:</b> ", ifelse(is.na(disease_name) | disease_name == "",
                                                   syndrome_name, disease_name), "<br>",
            "<b>Number of reports on the day:</b> ", ifelse(is.na(daily_disease_count_country),
                                                            daily_syndrome_count_country, daily_disease_count_country), "<br>",
            "<b>Title:</b> ", title, "<br>",
            "<b>URL:</b> ", "<a href='", url, "' target='_blank'>Click here</a>"
          )
        ) %>%
        fitBounds(
          lng1 = min(df_filtered$longitude),
          lat1 = min(df_filtered$latitude),
          lng2 = max(df_filtered$longitude),
          lat2 = max(df_filtered$latitude)
        )
    } else {
      # Add non-clustered markers
      leafletProxy("WorldMapPlot", data = df_filtered) %>%
        addCircleMarkers(
          ~longitude, ~latitude,
          radius = 5,
          color = "red",
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Disease or Syndrome:</b> ", ifelse(is.na(disease_name) | disease_name == "",
                                                   syndrome_name, disease_name), "<br>",
            "<b>Number of reports on the day:</b> ", ifelse(is.na(daily_disease_count_country), daily_syndrome_count_country, daily_disease_count_country),  "<br>",
            "<b>Title:</b> ", title, "<br>",
            "<b>URL:</b> ", "<a href='", url, "' target='_blank'>Click here</a>"
          )
        ) %>%
        fitBounds(
          lng1 = min(df_filtered$longitude),
          lat1 = min(df_filtered$latitude),
          lng2 = max(df_filtered$longitude),
          lat2 = max(df_filtered$latitude)
        )
    }
  })
  
  # Generate Summary Statistics
  summaryDiseaseData <- reactive({
    df_filtered <- filteredDataTab1()
    # Example: Summarize by Disease
    summary_table <- df_filtered %>%
      filter(!is.na(disease_name)) %>%
      group_by(disease_name) %>%
      summarise(
        Total_Reports = n()
      ) %>%
      arrange(desc(Total_Reports))
    return(summary_table)
  })
  
  # Render Summary Table
  output$summaryDiseaseTable <- renderDT({
    summary_table <- summaryDiseaseData()
    datatable(summary_table,
              options = list(pageLength = 10,
                             order = list(list(1, 'desc'))),
              rownames = FALSE,
              colnames = c("Disease", "Total Reports"))
  })
  
  # Reactive data processing for diseases in Tab 2
  filtered_disease_data <- reactive({
    data_filtered <- df
    
    # Apply country filter if selected
    if (input$country != "All") {
      data_filtered <- data_filtered %>%
        filter(country == input$country)
    }
    
    # Apply disease filter if selected
    if (input$disease != "All") {
      data_filtered <- data_filtered %>%
        filter(disease_name == input$disease)
    }
    
    # Apply date range filter from dateRangeInput
    data_filtered <- data_filtered %>%
      filter(publication_date >= input$date_range[1], publication_date <= input$date_range[2])
    
    return(data_filtered)
  })
  
  # Reactive data processing for syndromes in Tab 2
  filtered_syndrome_data <- reactive({
    data_filtered <- df
    
    # Apply country filter if selected
    if (input$country != "All") {
      data_filtered <- data_filtered %>%
        filter(country == input$country)
    }
    
    # Apply syndrome filter if selected
    if (length(input$syndromes) > 0) {
      data_filtered <- data_filtered %>%
        filter(syndrome_name %in% input$syndromes)
    }
    
    # Apply date range filter from dateRangeInput
    data_filtered <- data_filtered %>%
      filter(publication_date >= input$date_range[1], publication_date <= input$date_range[2])
    
    return(data_filtered)
  })
  
  # Render the plot (Line plot) in Tab 2
  output$linePlot <- renderPlot({
    # Get disease and syndrome data separately
    disease_data <- filtered_disease_data()
    syndrome_data <- filtered_syndrome_data()
    
    # Check if data is not empty
    if (nrow(disease_data) == 0 & nrow(syndrome_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected options", cex = 1.5)
    } else {
      # Get a sequence of all dates in the range to ensure lines are continuous
      all_dates <- seq(from = min(c(min(disease_data$publication_date), min(syndrome_data$publication_date))),
                       to = max(c(max(disease_data$publication_date), max(syndrome_data$publication_date))),
                       by = "day")
      
      # Create data for the disease line, filling in missing dates with 0 counts
      disease_count_data <- disease_data %>%
        count(publication_date = factor(publication_date, levels = all_dates)) %>%
        mutate(disease_count = ifelse(is.na(n), 0, n)) %>%
        select(publication_date, disease_count)
      
      # Create data for the syndrome line, filling in missing dates with 0 counts
      syndrome_count_data <- syndrome_data %>%
        count(publication_date = factor(publication_date, levels = all_dates)) %>%
        mutate(syndrome_count = ifelse(is.na(n), 0, n)) %>%
        select(publication_date, syndrome_count)
      
      # Plot disease and syndrome counts over time
      p <- ggplot() +
        # Disease counts line
        geom_line(data = disease_count_data, aes(x = as.Date(publication_date), y = disease_count), color = "red", size = 1)
      
      # Syndrome counts line (only if syndromes are selected)
      if (length(input$syndromes) > 0 && nrow(syndrome_data) > 0) {
        p <- p + geom_line(data = syndrome_count_data, aes(x = as.Date(publication_date), y = syndrome_count), color = "blue", size = 1)
      }
      
      p <- p + labs(title = "Disease and Syndrome Counts Over Time",
                    x = "Publication Date", y = "Count") +
        theme_minimal() +
        theme(legend.position = "bottom")
      print(p)
    }
  })
  
  # Action to Select All Syndromes
  observeEvent(input$select_all, {
    updateSelectInput(session, "syndromes", choices = unique(df$syndrome_name), selected = unique(df$syndrome_name))
  })
  
  # Action to Clear All Syndromes
  observeEvent(input$clear_all, {
    updateSelectInput(session, "syndromes", choices = unique(df$syndrome_name), selected = character(0))
  })
  
  # Filtering for Country Specific Daily Disease Counts for Tab 3
  filteredDataTab3 <- reactive({
    df %>%
      filter(disease_name == input$disease_selection, country == input$select_country) %>%
      arrange(publication_date) %>%
      select(publication_date, daily_disease_count_country)
  })
  
  # Render forecast plot for Tab 3
  output$forecast_plot <- renderPlot({
    df_filtered <- filteredDataTab3()
    if (nrow(df_filtered) > 1) {
      publication_ts <- ts(df_filtered$daily_disease_count_country,
                           start = c(year(min(df_filtered$publication_date)), yday(min(df_filtered$publication_date))),
                           frequency = 365)
      mod_1 <- auto.arima(publication_ts)
      forecast <- forecast::forecast(mod_1, h = 30)
      autoplot(forecast) +
        labs(title = "ARIMA Forecast",
             x = "Time",
             y = "Daily Disease Count")
    } else {
      plot.new()
      text(0.5, 0.5, "Not Enough Data", cex = 1.5)
    }
  })
  
  
  ################ TAB 4
  
  
  # TAB 4
  
  filteredDataTab4_base <- reactive({
    df %>%
      filter(publication_date >= input$date_range_tab4[1] & 
               publication_date <= input$date_range_tab4[2])
  })
  
  # Observe changes in date range to update disease choices
  observe({
    data <- filteredDataTab4_base()
    
    # Get unique diseases in the filtered data
    diseases <- unique(data$disease_name)
    
    # Handle case when no diseases are available
    if (length(diseases) == 0) {
      diseases <- "No diseases available"
    }
    
    # Update the disease selector choices
    updateSelectInput(session, "selected_disease_tab4",
                      choices = diseases,
                      selected = diseases[1])
  })
  
  filteredDataTab4 <- reactive({
    df <- filteredDataTab4_base()
    if (input$selected_disease_tab4 != "No diseases available") {
      df <- df %>% filter(disease_name == input$selected_disease_tab4)
    } else {
      df <- data.frame()  # Empty data frame
    }
    df  # Return the data frame
  })
  
  output$tab4_plot <- renderPlot({
    df_filtered <- filteredDataTab4()
    ggplot(df_filtered, aes(x = publication_date, y = daily_disease_count_country)) +
      geom_line(color = "blue", size = 1, group=1) +  # Line plot
      facet_wrap(~ country, scales = "free_y") +  
      labs(
        title = paste("Trends in", input$selected_disease_tab4, "Daily Counts by Country"),
        x = "Publication Date",
        y = "Daily Count"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
        axis.text.x = element_text(angle = 45, hjust = 1)    # Rotate x-axis labels
      )
  })
  
  
  
 
  
  
}

# Run the application
shinyApp(ui = ui, server = server)