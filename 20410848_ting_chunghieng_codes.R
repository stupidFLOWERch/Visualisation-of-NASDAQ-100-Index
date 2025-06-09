library(shiny)
library(quantmod)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(DT)
library(RColorBrewer)
library(lubridate)

ui <- fluidPage(
  titlePanel("NASDAQ-100 Index"),
  
  # Tab-based navigation
  tabsetPanel(
    id = "main_tabs",  # Add an ID to the tabset panel for navigation
    
    # Tab 1: NASDAQ-100 INDEX stock prices
    tabPanel(
      "Q1",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("date_range", "Select Date Range:", 
                         start = "2017-01-01", end = Sys.Date()),
          actionButton("update", "Update Chart")
        ),
        mainPanel(
          plotlyOutput("stockPlot", height = "600px") # Output for stock price graph
        )
      )
    ),
    
    # Tab 2: Stock Prices for Each Company 
    tabPanel(
      "Q1a",
      sidebarLayout(
        sidebarPanel(
          selectInput("company_picker", "Select Company:",
                      choices = NULL,  # Placeholder for companies to be populated
                      selected = NULL),
          dateRangeInput("company_date_range", "Select Date Range:", 
                         start = "2017-01-01", end = "2022-12-31"),  # Restrict date range to 2017-2022
          actionButton("update_company", "Update Chart")  # Update button for company stock prices
        ),
        mainPanel(
          plotlyOutput("companyStockPlot", height = "600px")  # Output for stock price chart
        )
      )
    ),
    
    # Tab 3: Sector Distribution 
    tabPanel(
      "Q2",
      fluidRow(
        column(
          width = 12,
          plotlyOutput("sectorPlot", height = "600px") # Pie chart for sector distribution
        )
      )
    ),
    
    # Tab 4: Tab for the Sunburst Graph
    tabPanel(
      "Q2a",
      fluidRow(
        column(
          width = 12,
          plotlyOutput("sunburstPlot", height = "600px")  # Output for sunburst graph
        )
      )
    ),
    
    # Tab 5: Stocks by companies
    tabPanel(
      "Q2b",
      fluidRow(
        column(
          width = 12,
          sliderInput("year_selector", "Year:",
                      min = 2017, max = 2022, value = 2017, step = 1,
                      animate = TRUE),  # Adding the slider for selecting year
          selectInput("company_selector", "Select Companies:",
                      choices = NULL,  # We will populate this dynamically later
                      multiple = TRUE,
                      selectize = TRUE),
          actionButton("select_all", "Select All"),
          actionButton("deselect_all", "Deselect All"),
          plotlyOutput("stocksChart", height = "600px") # Render the bar chart here
        )
      )
    ),
    
    # Tab 6: Scatter Plot
    tabPanel(
      "Q3",
      fluidRow(
        column(
          width = 12,
          sliderInput("earning_yield_range", "Select Earning Yield Range:",
                      min = -10, max = 10, value = c(-5, 5), step = 0.1),
          sliderInput("debt_to_assets_range", "Select Debt-to-Assets Range:",
                      min = 0, max = 2, value = c(0.1, 1), step = 0.05),
          plotlyOutput("scatterPlot", height = "600px")
        )
      )
    ),
    
    # Tab 7: Bubble Chart
    tabPanel(
      "Q3a",
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "color_var",
            label = "Color By:",
            choices = c("sector", "subsector"),
            selected = "sector"
          )
        ),
        column(
          width = 12,
          plotlyOutput("bubbleChart", height = "600px")  # Output for the bubble chart
        )
      )
    )
    
  )
)

# Tab 1
server <- function(input, output, session) {
  csv_path <- "./nasdaq100_metrics_ratios.csv"
  # Reactive event to fetch NASDAQ-100 stock data
  stock_data <- eventReactive(input$update, {
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Fetch NASDAQ-100 index data from Yahoo Finance
    nasdaq_data <- getSymbols("^NDX", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    
    # Convert to a data frame
    data <- data.frame(Date = index(nasdaq_data), Close = coredata(nasdaq_data)[, "NDX.Close"])
    
    # Ensure the Date column is in YYYY-MM-DD format
    data$Date <- ymd(data$Date)
    
    # Handle missing values in 'Close'
    data$Close <- zoo::na.locf(data$Close, na.rm = FALSE)  # Forward-fill missing values
    
    # Return the cleaned data
    data
  })
    
  # Render the stock prices plot
  output$stockPlot <- renderPlotly({
    data <- stock_data()
    
    # Create a ggplot of the closing prices
    plot <- ggplot(data, aes(x = Date, y = Close)) +
      geom_line(color = "green") +
      labs(title = "NASDAQ-100 Stock Price", x = "Date", y = "Closing Price (USD)") +
      theme_minimal()
    
    # Convert the ggplot to an interactive plotly graph
    ggplotly(plot)
  })
  
  # Reactive value to store selected sector
  selected_sector <- reactiveVal(NULL)
  selected_companies <- reactiveVal(NULL)  # Store selected companies for bar chart
  
  # Tab 2
  observe({
    # Fetch list of companies for the "Company Stock Prices" tab
    if (!file.exists(csv_path)) {
      stop("The specified CSV file does not exist.")
    }
    
    data <- read.csv(csv_path)
    all_companies <- data %>% pull(symbol)
    updateSelectInput(session, "company_picker", choices = all_companies)
  })
  
  stock_data1 <- eventReactive(input$update_company, {
    company_symbol <- input$company_picker  # Get selected company symbol
    start_date1 <- input$company_date_range[1]  # Get the start date from the input
    end_date1 <- input$company_date_range[2]    # Get the end date from the input
    
    # Fetch stock data for the selected company from Yahoo Finance
    stock_data <- tryCatch({
      getSymbols(company_symbol, src = "yahoo", from = start_date1, to = end_date1, auto.assign = FALSE)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Unable to retrieve data for", company_symbol, ". Please check the symbol and try again."),
        easyClose = TRUE
      ))
      return(NULL)
    })
    
    # Convert stock data to a data frame
    if (!is.null(stock_data)) {
      data <- data.frame(
        Date = index(stock_data),
        Open = coredata(stock_data)[, paste(company_symbol, "Open", sep = ".")],
        High = coredata(stock_data)[, paste(company_symbol, "High", sep = ".")],
        Low = coredata(stock_data)[, paste(company_symbol, "Low", sep = ".")],
        Close = coredata(stock_data)[, paste(company_symbol, "Close", sep = ".")]
      )
      
      # Data Cleaning and Transformation
      # 1. Ensure the Date column is in YYYY-MM-DD format
      data$Date <- lubridate::ymd(data$Date)
      
      # 2. Handle missing values by forward filling 'Close' column
      data$Close <- zoo::na.locf(data$Close, na.rm = FALSE)  # Forward-fill missing values
      # 3. Remove duplicates (if any)
      data <- data[!duplicated(data$Date), ]
      return(data)
    } else {
      NULL
    }
  })
  
  output$companyStockPlot <- renderPlotly({
    data <- stock_data1()  # Use stock_data1() for the selected company data
    
    # If data is NULL (failed to fetch), return an empty plot
    if (is.null(data)) {
      return(plotly_empty())
    }
    
    # Check if the data has the necessary columns
    if (!all(c("Date", "Open", "High", "Low", "Close") %in% colnames(data))) {
      return(plotly_empty())  # Return an empty plot if columns are missing
    }
    
    # Create the candlestick chart using Plotly
    plot_ly(data, x = ~Date, type = "candlestick",
            open = ~Open, high = ~High, low = ~Low, close = ~Close) %>%
      layout(title = paste("Candlestick Chart for", input$company_picker),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price (USD)"))
  })
  
  # Tab 3
  # Render the sector distribution pie chart
  output$sectorPlot <- renderPlotly({
    # Ensure the CSV file exists
  
    if (!file.exists(csv_path)) {
      stop("The specified CSV file does not exist.")
    }
    
    # Read the CSV file
    data <- read.csv(csv_path)
    
    # Ensure the 'sector' column exists
    if (!"sector" %in% colnames(data)) {
      stop("The 'sector' column is missing from the dataset.")
    }
    
    # Summarize the sector distribution
    sector_summary <- data %>%
      group_by(sector) %>%
      summarize(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    # Create a pie chart using Plotly and register the 'plotly_click' event
    plot <- plot_ly(
      data = sector_summary,
      labels = ~sector,
      values = ~Percentage,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'label+percent',
      marker = list(colors = RColorBrewer::brewer.pal(n = nrow(sector_summary), name = "Set3")),
      source = "sector_plot" # Ensure this source matches
    ) %>%
      layout(
        title = "Sector Distribution of Companies in NASDAQ-100 Index between 2017 and 2022",
        showlegend = TRUE,
        margin = list(l = 20, r = 20, t = 50, b = 20)  # Adjust margins
      )
    
    plot <- event_register(plot, "plotly_click")
    return(plot)
  })
  
  # Handle click event on pie chart
  observeEvent(event_data("plotly_click", source = "sector_plot"), {
    click_data <- event_data("plotly_click", source = "sector_plot")
    if (!is.null(click_data)) {
      # Access pointNumber to get the clicked sector
      point_number <- click_data$pointNumber + 1  # Add 1 because R uses 1-based indexing
      
      # Fetch the sector name from sector_summary
      
      data <- read.csv(csv_path)
      sector_summary <- data %>%
        group_by(sector) %>%
        summarize(Count = n()) %>%
        mutate(Percentage = Count / sum(Count) * 100)
      
      clicked_sector <- sector_summary$sector[point_number]
      
      # Save the clicked sector
      selected_sector(clicked_sector)
      
      # Filter companies by sector
      selected_companies_list <- data %>%
        filter(sector == clicked_sector) %>%
        pull(symbol)
      
      # Get all company names
      all_companies <- data %>% pull(symbol)
      
      # Update the 'Select Companies' dropdown with all companies in the selected sector
      updateSelectInput(session, "company_selector", 
                        choices = all_companies, 
                        selected = selected_companies_list)  # Automatically select all companies
      
      # Update the selected companies reactive value
      selected_companies(selected_companies_list)  # Trigger the bar chart update
      
      # Navigate to the "Stocks by Sector" tab
      updateTabsetPanel(session, "main_tabs", selected = "Q2b")
    }
  })
  
  # Tab 4
  # Prepare data for Sunburst chart
  sunburst_data <- reactive({
    # Read the CSV file
    if (!file.exists(csv_path)) {
      stop("The specified CSV file does not exist.")
    }
    data <- read.csv(csv_path)
    
    # Ensure required columns exist
    required_columns <- c("sector", "subsector", "symbol")
    if (!all(required_columns %in% colnames(data))) {
      stop(paste("The dataset must contain the following columns:", paste(required_columns, collapse = ", ")))
    }
    
    # Prepare hierarchy: Sector > Subsector > Company
    hierarchy_data <- data %>%
      rename(Company = symbol) %>%  # Rename for clarity
      select(sector, subsector, Company) %>%
      pivot_longer(cols = everything(), names_to = "Level", values_to = "Label") %>%
      mutate(Parent = case_when(
        Level == "sector" ~ "",
        Level == "subsector" ~ lag(Label),
        Level == "Company" ~ lag(Label)
      )) %>%
      drop_na(Parent) %>%
      group_by(Label) %>%
      summarize(Parent = first(Parent), Value = n(), .groups = 'drop')
    
    return(hierarchy_data)
  })
  
  # Render Sunburst Chart
  output$sunburstPlot <- renderPlotly({
    data <- sunburst_data()
    
    plot <- plot_ly(
      data = data,
      type = 'sunburst',
      labels = ~Label,
      parents = ~Parent,
      values = ~Value,
      branchvalues = "total"
    ) %>%
      layout(
        title = "Sector Hierarchy of NASDAQ-100 Index",
        margin = list(t = 50, b = 50, l = 50, r = 50)
      )
    return(plot)
  })
  
  # Tab 5
  # Render the bar chart for selected companies
  output$stocksChart <- renderPlotly({
    req(selected_companies())  # Ensure a sector is selected and companies are selected
    
    # Ensure the CSV file exists
    if (!file.exists(csv_path)) {
      stop("The specified CSV file does not exist.")
    }
    
    # Read the CSV file
    data <- read.csv(csv_path)
    
    # Ensure required columns exist
    yoy_column <- paste0("yoy_revenue_growth_", input$year_selector)
    if (!"symbol" %in% colnames(data) || !yoy_column %in% colnames(data)) {
      stop(paste("The dataset must contain 'symbol' and '", yoy_column, "' columns."))
    }
    
    # Filter the data based on the selected companies
    filtered_data <- data %>%
      filter(symbol %in% selected_companies() & !is.na(.data[[yoy_column]]))
    
    # Check if there is any valid data left after filtering
    if (nrow(filtered_data) == 0) {
      return(plotly_empty())  # Return an empty plot if no valid data exists
    }
    
    # Create a bar chart of YOY revenue growth for all selected companies
    plot <- plot_ly(
      data = filtered_data,
      x = ~symbol,  # Company symbols
      y = ~.data[[yoy_column]],  # YOY revenue growth for the selected year
      type = 'bar',
      marker = list(
        color = ifelse(filtered_data[[yoy_column]] < 0, "red", "blue")  # Color bars based on positive/negative growth
      ),
      text = ~paste(symbol, ":", .data[[yoy_column]], "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = ifelse(is.null(selected_sector()), 
                       paste("YOY Revenue Growth of Companies in Year:", input$year_selector), 
                       paste("YOY Revenue Growth of Companies in", selected_sector(), "Sector for Year:", input$year_selector)),
        xaxis = list(title = "Company Symbols", showticklabels = TRUE, tickangle = 45),  # Rotate x-axis labels
        yaxis = list(title = "YOY Revenue Growth (%)"),
        margin = list(l = 20, r = 20, t = 50, b = 100),  # Adjust margins for better visibility
        showlegend = FALSE  # Remove legend as it's not necessary for bar chart
      )
    
    return(plot)
  })
  
  # Action for the Select All button
  observeEvent(input$select_all, {
    # Fetch all companies in the dataset
    data <- read.csv(csv_path)
    all_companies <- data %>% pull(symbol)  # Get all company symbols
    
    # Update the dropdown selection to include all companies
    updateSelectInput(session, "company_selector", selected = all_companies, choices = all_companies)
    
    # Update the selected companies reactive value
    selected_companies(all_companies)  # Trigger the bar chart update
  })
  
  observeEvent(input$deselect_all, {
    # Clear the selected companies in the dropdown
    updateSelectInput(session, "company_selector", 
                      selected = NULL,  # Clear selected companies
                      choices = NULL)   # Temporarily clear choices
    
    # Ensure that we reset the available choices to all companies after clearing
    data <- read.csv(csv_path)
    all_companies <- data %>% pull(symbol)  # Get all company symbols
    
    # Reset the available choices and clear the selected companies list
    updateSelectInput(session, "company_selector", 
                      selected = NULL, 
                      choices = all_companies)
    
    # Clear the reactive value for selected companies (which controls the bar chart)
    selected_companies(NULL)
  })
  

  # Handle company selection change in the dropdown
  observeEvent(input$company_selector, {
    # Get the selected companies from the dropdown
    selected_companies(input$company_selector)
  })
  
  # Populate the company selector dropdown with all company symbols at the start
  observe({
    # Ensure the CSV file exists
    if (!file.exists(csv_path)) {
      stop("The specified CSV file does not exist.")
    }
    
    # Read the CSV file
    data <- read.csv(csv_path)
    
    # Fetch all company symbols
    all_companies <- data %>% pull(symbol)  # Get all company symbols
    
    # Update the dropdown with all companies
    updateSelectInput(session, "company_selector", choices = all_companies)
  })
  
  # Tab 6
  # Step 1: Load the entire dataset initially
  full_data <- reactive({
    if (!file.exists(csv_path)) {
      stop("The specified CSV file does not exist.")
    }
    data <- read.csv(csv_path)
    
    # Ensure required columns exist
    required_columns <- c("earning_yield_greenblatt_latest", "debt_to_assets_latest")
    if (!all(required_columns %in% colnames(data))) {
      stop(paste("The dataset must contain the following columns:", paste(required_columns, collapse = ", ")))
    }
    return(data)
  })
  
  # Step 2: Apply dynamic filtering based on user inputs
  filtered_data <- reactive({
    data <- full_data()  # Use the full dataset
    
    # Apply the filter only if user has set the range
    data %>%
      filter(
        earning_yield_greenblatt_latest >= input$earning_yield_range[1] & 
          earning_yield_greenblatt_latest <= input$earning_yield_range[2],
        debt_to_assets_latest >= input$debt_to_assets_range[1] & 
          debt_to_assets_latest <= input$debt_to_assets_range[2]
      )
  })
  
  # Step 3: Render the scatter plot with filtered data based on slider input
  output$scatterPlot <- renderPlotly({
    data <- filtered_data()  # Get the filtered data
    plot <- plot_ly(
      data = data,  # Use the filtered dataset
      x = ~earning_yield_greenblatt_latest,
      y = ~debt_to_assets_latest,
      type = 'scatter',
      mode = 'markers',
      color = ~sector,  # Group points by sector
      colors = RColorBrewer::brewer.pal(n = length(unique(data$sector)), name = "Set1"),  # Use a valid palette
      marker = list(size = 8, opacity = 0.6),
      text = ~paste(
        "Company:", symbol, 
        "<br>Earning Yield:", earning_yield_greenblatt_latest, 
        "<br>Debt-to-Assets:", debt_to_assets_latest
      ),
      hoverinfo = 'text'  # Show details on hover
    ) %>%
      layout(
        title = "Earning Yield vs Debt-to-Assets Grouped by Sector",
        xaxis = list(title = "Earning Yield (Greenblatt Latest)"),
        yaxis = list(title = "Debt-to-Assets (Latest)"),
        legend = list(title = list(text = "Sector"))  # Add a legend for sectors
      )
    return(plot)
  })
  
  # Step 4: Update sliders dynamically based on the data
  observe({
    data <- full_data()
    updateSliderInput(session, "earning_yield_range",
                      min = min(data$earning_yield_greenblatt_latest, na.rm = TRUE),
                      max = max(data$earning_yield_greenblatt_latest, na.rm = TRUE),
                      value = range(data$earning_yield_greenblatt_latest, na.rm = TRUE))
    
    updateSliderInput(session, "debt_to_assets_range",
                      min = min(data$debt_to_assets_latest, na.rm = TRUE),
                      max = max(data$debt_to_assets_latest, na.rm = TRUE),
                      value = range(data$debt_to_assets_latest, na.rm = TRUE))
  })
  
  # Tab 7
  output$bubbleChart <- renderPlotly({
    data <- filtered_data()  # Assuming filtered_data() is your dataset
    
    # Ensure the data is not empty
    if (nrow(data) == 0) {
      return(plotly_empty())  # Empty plot if no data
    }
    
    # Step 1: Remove rows with missing values for relevant columns
    data <- data %>%
      drop_na(gross_profit_to_assets_latest, inventory_turnover_latest, enterprise_value_to_revenue_latest)
    
    # Step 2: Validate data types and ensure numeric format
    data <- data %>%
      mutate(
        gross_profit_to_assets_latest = as.numeric(gross_profit_to_assets_latest),
        inventory_turnover_latest = as.numeric(inventory_turnover_latest),
        enterprise_value_to_revenue_latest = as.numeric(enterprise_value_to_revenue_latest)
      )
    
    # Dynamically set the color variable
    color_variable <- input$color_var
    
    plot_ly(
      data = data,
      x = ~gross_profit_to_assets_latest,  
      y = ~inventory_turnover_latest,  
      size = ~enterprise_value_to_revenue_latest,  
      sizes = c(50, 1000),
      color = ~get(color_variable),  # Use the selected color variable
      type = 'scatter',
      mode = 'markers',
      marker = list(opacity = 0.6),
      text = ~paste(
        "Company: ", symbol, 
        "<br>Gross Profit to Assets: ", gross_profit_to_assets_latest,
        "<br>Inventory Turnover: ", inventory_turnover_latest,
        "<br>Enterprise Value to Revenue: ", enterprise_value_to_revenue_latest
      ),  # Add x, y, and size info here
      hoverinfo = 'text'  # This ensures that only the 'text' will show on hover
    ) %>%
      layout(
        title = "Bubble Chart of Gross Profit to Assets vs Inventory Turnover with Enterprise Value to Revenue",
        xaxis = list(title = "Gross Profit to Assets"),
        yaxis = list(title = "Inventory Turnover")
      )
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)
