library(shiny)
library(bs4Dash)
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(zoo)
library(ggplot2)
library(scales)
library(bslib)
library(DT)

options(shiny.maxRequestSize = 10000 * 1024^2)  # 100 MB

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1",
  olive = "#4A9094",
  info = "#17a2b8",
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)

# Define the User Interface for the Application
ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = bs4DashNavbar(
    status = "primary",
    skin = "dark",
    title = dashboardBrand(
      title = "UPR & GEP MODEL",
      color = "primary",
      href = "",
      image = ""
    ),
    actionButton("toggleControlbar", "Input Controls", class = "btn btn-primary")
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Overview", tabName = "dataOverview", icon = icon("table")),
      bs4SidebarMenuItem("UPR Summaries", tabName = "uprSummaries", icon = icon("chart-bar")),
      bs4SidebarMenuItem("GEP Results", tabName = "gepResults", icon = icon("chart-line"))
    )
  ),
  controlbar = bs4DashControlbar(
    id = "controlbar",
    skin = "info",
    bs4Card(
      title = "Input Controls",
      background = "info",
      width = 12,
      height = 10,
      style = "max-width: 100%;",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      textInput("valDate", "Input Valuation Date", value = "31/12/2023"),
      numericInput("cutoffYear", "Set the Policy Start Year Threshold", value = 2013, min = 1990, max = 3000),
      actionButton("showData", "Load and View Data", class = "btn btn-primary",
                   style = "background-color: #007bff; color: white; font-size: 12px;"),
      br(),
      br(),
      actionButton("calcClassWiseUPR", "Calculate Class-wise UPR", class = "btn btn-primary",
                   style = "background-color: #007bff; color: white; font-size: 12px;"),
      br(),
      br(),
      actionButton("goButton", "Calculate GEP Summary", class = "btn btn-primary",
                   style = "background-color: #007bff; color: white; font-size: 12px;")
    )
  ),
  body = bs4DashBody(
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
      tags$style(HTML("
        body {font-family: 'Mulish', sans-serif; background-color: #f4f4f4;}
        .shiny-output-error { color: #ff0000;}
        .shiny-output-error:before {content: 'Error: ';}
        .well {background-color: #ffffff; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);}
      "))
    ),
    bs4TabItems(
      bs4TabItem(tabName = "dataOverview",
                 fluidRow(
                   hr(),
                   div(
                     style = "background-color: #17a2b8; padding: 20px; border-radius: 10px; margin-bottom: 20px; color: white;",
                     fileInput("file1", 
                               label = tags$span("Upload Premium Data as a CSV File", style = "color: white;"),
                               accept = c(".csv")
                     ),
                     tags$p(class = "instruction-header", style = "font-weight: bold; font-size: 17px; margin-top: 20px;", "Data Upload Guidelines:"),
                     tags$ul(
                       style = "font-size: 13px;",
                       tags$li("Ensure the data format is CSV. The Required Columns are:"),
                       tags$ul(
                         tags$li(tags$b("BegDate:"), " Representing the policy start date Column."),
                         tags$li(tags$b("EndDate:"), " Representing the policy end date Column."),
                         tags$li(tags$b("AuthDate:"), " Representing the policy underwriting date Column."),
                         tags$li(tags$b("IRA CLASS:"), " Representing the class of Business Column."),
                         tags$li(tags$b("Premium:"), " Representing the Premium Column.")
                       )
                     )
                   ),
                   hr(),
                   br(),
                   bs4Card(
                     title = "Data Overview",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     DTOutput("viewData")
                   )
                 )
      ),
      bs4TabItem(tabName = "uprSummaries",
                 fluidRow(
                   valueBoxOutput("UPRSumBox", width = 6),
                   valueBoxOutput("DACSumBox", width = 6)
                 ),
                 fluidRow(
                   bs4Card(
                     title = "Class-wise Gross UPR Summarization",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     downloadButton("downloadUPR", "Download Gross UPR Summary Table as CSV",class = "btn btn-primary",
                                    style = "background-color: #007bff; color: white; font-size: 14px;"),
                     DTOutput("classWiseUPR")
                   )
                 ),
                 fluidRow(
                   bs4Card(
                     title = "Class-wise Gross UPR Plot",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     plotOutput("classWiseUPRPlot")
                   )
                 )
      ),
      bs4TabItem(tabName = "gepResults",
                 fluidRow(
                   bs4Card(
                     title = "Gross Earned Premiums Results by IRA Class",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     fluidRow(
                       hr(),
                       numericInput("startYear", "Start Year of Analysis", value = 2022, min = 2000, max = 3000),
                       hr(),
                       numericInput("endYear", "End Year of Analysis", value = 2023, min = 2000, max = 3000),
                       hr()),
                     fluidRow(
                       hr(),
                       selectInput("endYearQuarter", "Select Quarter for End Year of Analysis", choices = c("All" = "All", "Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4"), selected = "All"),
                       hr()),
                     DTOutput("summaryData"),
                     downloadButton("downloadData", "Download GEP Summary Table as CSV", class = "btn btn-primary",
                                    style = "background-color: #007bff; color: white; font-size: 14px;")
                   )
                 )
      )
    )
  )
)



# Define the server logic required to read the input and calculate outputs
server <- function(input, output, session) {
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("controlbar")
  })
  # Reactive value for storing the uploaded data with validation
  data <- eventReactive(input$showData, {
    req(input$file1)
    inFile <- input$file1
    
    withProgress(message = 'Reading and validating data...', {
      setProgress(0.2)
      # Attempt to read data
      tryCatch({
        df <- read_csv(inFile$datapath, 
                       col_types = cols(
                         Premium = col_number(), 
                         AuthDate = col_date(format = "%d/%m/%Y"),
                         BegDate = col_date(format = "%d/%m/%Y"), 
                         EndDate = col_date(format = "%d/%m/%Y"), 
                         Commission = col_number()))
        
        # Validate necessary columns
        requiredColumns <- c("Premium", "AuthDate", "BegDate", "EndDate", "Commission")
        if (!all(requiredColumns %in% names(df))) {
          stop("Data must contain the following columns: ", paste(requiredColumns, collapse=", "))
        }
        
        setProgress(1)
        return(df)
      }, error = function(e) {
        # Handle errors in data format
        showModal(modalDialog(
          title = "Error in data format",
          paste("Please check your CSV file for the correct columns and data formats. Details: ", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    })
  })
  
  # Process data when go button is clicked
  processedData <- eventReactive(input$showData, {
    req(data())
    Val_Date <- dmy(input$valDate)
    withProgress(message = 'Processing data...', value = 0, {
      setProgress(0.5)  # Indicate progress at halfway
      processed <- data() %>%
        mutate(
          Auth_year = year(AuthDate),
          Duration = as.numeric(difftime(EndDate, BegDate, units = "days")) + 1,
          Unearned_Duration = ifelse(BegDate <= Val_Date & EndDate >= Val_Date, as.numeric(EndDate - Val_Date), ifelse(BegDate > Val_Date, as.numeric(Duration), ifelse(EndDate <= Val_Date, 0, NA))),
          Earned_Duration = Duration - Unearned_Duration,
          Gross_UPR= ifelse(Auth_year < input$cutoffYear, 0, (as.numeric(Unearned_Duration)/as.numeric(Duration)) * Premium),
          DAC= ifelse(Auth_year < input$cutoffYear, 0, (as.numeric(Unearned_Duration)/as.numeric(Duration)) * -Commission),
          GEP = (as.numeric(Earned_Duration)/as.numeric(Duration))*Premium
        )
      setProgress(1)  # Complete the progress bar when done
      return(processed)
    })
  })
  
  output$viewData <- renderDT({
    req(processedData())
    datatable(processedData(),
              options = list(scrollX = TRUE, 
                             pageLength = 30,
                             autoWidth = TRUE,
                             paging = TRUE,
                             searching = FALSE,
                             info = FALSE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "  $(this.api().table().header()).css({",
                               "    'background-color': '#007BFF',", 
                               "    'color': '#FFFFFF'",  
                               "  });",
                               "}"
                             )))
  })
  
  
  
  #UPR -------------------------------------------------------------------------------------------------------------------------------------------
  # Calculate and display Gross UPR Sum
  output$UPRSumBox <- renderValueBox({
    req(input$calcClassWiseUPR, processedData())
    upr_sum <- sum(processedData()$Gross_UPR, na.rm = TRUE)
    formatted_upr_sum <- comma(upr_sum)
    valueBox(
      value = formatted_upr_sum,
      subtitle = "Gross UPR Sum",
      icon = icon("dollar-sign", class = "fa-2x", style = "color: white;"),
      color = "success"
    )
  })
  
  # Calculate and display DAC Sum
  output$DACSumBox <- renderValueBox({
    req(input$calcClassWiseUPR, processedData())
    dac_sum <- sum(processedData()$DAC, na.rm = TRUE)
    formatted_dac_sum <- comma(dac_sum)
    valueBox(
      value = formatted_dac_sum,
      subtitle = "DAC Sum",
      icon = icon("balance-scale", class = "fa-2x", style = "color: white;"),
      color = "warning"
    )
  })
  
  
  # Reactive function for class-wise UPR summarization
  classWiseUPR <- eventReactive(input$calcClassWiseUPR, {
    req(processedData())
    data <- processedData()
    data %>%
      group_by(`IRA CLASS`) %>%
      summarise(`Class wise Gross UPR Sum` = sum(Gross_UPR, na.rm = TRUE), `Class wise DAC Sum` = sum(Gross_UPR, na.rm = TRUE)) %>%
      mutate(`Class wise Gross UPR Sum` = scales::comma(`Class wise Gross UPR Sum`)) %>%
      mutate(`Class wise DAC Sum` = scales::comma(`Class wise DAC Sum`))  # Format numbers with commas
  })
  
  # Display class-wise UPR summarization with enhanced styling
  output$classWiseUPR <- renderDT({
    req(classWiseUPR())
    datatable(classWiseUPR(), options = list(
      pageLength = 30,
      autoWidth = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      class = 'cell-border stripe',
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).css({",
        "    'background-color': '#007BFF',", 
        "    'color': '#FFFFFF'",  
        "  });",
        "}"
      )
    ))
  })
  
  
  # Define download handler for the UPR table
  output$downloadUPR <- downloadHandler(
    filename = function() {
      paste("Class-wise-Gross-UPR-Summary", Sys.Date(), ".csv", sep = "")  # Construct filename
    },
    content = function(file) {
      req(classWiseUPR())  # Ensure data is ready before download
      write.csv(classWiseUPR(), file, row.names = FALSE)  # Write the data to a CSV file
    }
  )
  
  # Render the bar graph for class-wise UPR
  output$classWiseUPRPlot <- renderPlot({
    req(classWiseUPR())  # Ensure the data is available
    
    # Prepare the data by removing commas for numeric conversion
    data <- classWiseUPR() %>%
      mutate(`Class wise Gross UPR Sum` = as.numeric(gsub(",", "", `Class wise Gross UPR Sum`)))
    
    # Create the bar graph
    ggplot(data, aes(x = `IRA CLASS`, y = `Class wise Gross UPR Sum`, fill = `IRA CLASS`)) +
      geom_bar(stat = "identity", color = "black", fill = "#2575fc") +
      geom_text(aes(label = paste0(round(`Class wise Gross UPR Sum` / 1e6, 0), "M")),  # Add this line
                vjust = -0.3,  # Adjust vertical position to be slightly above the bar
                color = "black", size = 3.7) +
      labs(title = "Class-wise Gross UPR Summary",
           x = "IRA Class",
           y = "Gross UPR Sum") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the plot title
            axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x labels for better readability
            legend.position = "none",
            panel.grid = element_blank(), )  # Hide legend if not necessary
  })
  
  #GEP ------------------------------------------------------------------------------------------------------------------------------
  # Reactive function for summarizing data by IRA Class
  summaryData <- eventReactive(input$goButton, {
    req(processedData())
    withProgress(message = 'Calculating summaries...', {
      setProgress(0)  # Initialize progress
      data <- processedData()
      
      total_operations <- length(input$startYear:input$endYear) * 4  # Assuming up to 4 quarters per year as maximum
      operations_done <- 0
      
      for (yr in input$startYear:input$endYear) {
        year_quarters <- define_quarters(yr)
        
        # Determine quarters to iterate over
        if (yr == input$endYear) {
          if (input$endYearQuarter == "All") {
            quarters_to_iterate <- c("Q1", "Q2", "Q3", "Q4")
          } else {
            # Generate the list of quarters from Q1 to the selected last quarter
            last_quarter_index <- match(input$endYearQuarter, c("Q1", "Q2", "Q3", "Q4"))
            quarters_to_iterate <- c("Q1", "Q2", "Q3", "Q4")[1:last_quarter_index]
          }
        } else {
          quarters_to_iterate <- c("Q1", "Q2", "Q3", "Q4")
        }
        
        for (quarter in quarters_to_iterate) {
          quarter_EP_col <- paste0(quarter, "_", yr, "_EP")
          data[[quarter_EP_col]] <- calculate_EP(data, year_quarters, quarter, yr, input$cutoffYear)
          operations_done <- operations_done + 1
          setProgress(operations_done / total_operations)
        }
      }
      
      
      summarized <- data %>%
        group_by(`IRA CLASS`) %>%
        summarize(across(contains("_EP"), sum, na.rm = TRUE))
      
      # Apply formatting with commas to all summarized columns
      summarized <- summarized %>%
        mutate(across(contains("_EP"), scales::comma))
      return(summarized)
    })
  })
  
  
  output$summaryData <- renderDT({
    req(summaryData())
    datatable(summaryData(),
              options = list(
                paging = TRUE,
                scrollX = TRUE, 
                pageLength = 20,
                autoWidth = TRUE,
                searching = FALSE,
                info = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "  $(this.api().table().header()).css({",
                  "    'background-color': '#007BFF',", 
                  "    'color': '#FFFFFF'",  
                  "  });",
                  "}"
                )
              ))
  })
  
  
  # Add a download handler for the summary data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Earned Premiums Summary-Data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(summaryData())  # Ensure the summary data is ready before attempting to download
      write.csv(summaryData(), file, row.names = FALSE)
    }
  )
  
}


# Define helper functions used in calculations
define_quarters <- function(year) {
  Q1_start <- mdy(paste('1/1/', year))
  Q1_end <- mdy(paste('3/31/', year))
  Q2_start <- mdy(paste('4/1/', year))
  Q2_end <- mdy(paste('6/30/', year))
  Q3_start <- mdy(paste('7/1/', year))
  Q3_end <- mdy(paste('9/30/', year))
  Q4_start <- mdy(paste('10/1/', year))
  Q4_end <- mdy(paste('12/31/', year))
  return(list(Q1_start = Q1_start, Q1_end = Q1_end,
              Q2_start = Q2_start, Q2_end = Q2_end,
              Q3_start = Q3_start, Q3_end = Q3_end,
              Q4_start = Q4_start, Q4_end = Q4_end))
}

calculate_EP <- function(data, year_quarters, quarter, year, cutoff_year) {
  quarter_EP <- ifelse(data$Auth_year < cutoff_year, 0,
                       (pmax(0, pmin(year_quarters[[paste0(quarter, "_end")]], data$EndDate) -
                               pmax(year_quarters[[paste0(quarter, "_start")]], data$BegDate) + 1) /
                          data$Duration) * data$Premium)
  return(quarter_EP)
}

# Run the application
shinyApp(ui = ui, server = server)
