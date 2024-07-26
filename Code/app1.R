library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(zoo)
library(ggplot2)
library(scales)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 100 MB

# Define the User Interface for the Application
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
    tags$style(HTML("
      body {font-family: 'Mulish',
      sans-serif;
      background-color: #f4f4f4;
      }
      .center-content {
          display: flex;
          justify-content: center;
        }
      .card {
          border-radius: 10px;
          background: #2575fc;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1); /* Unchanged shadow */
          padding: 15px; /* Reduced padding */
          margin-top: 20px; /* Unchanged margin-top */
          transition: transform 0.3s ease; /* Unchanged transition */
        }
        
        .card:hover {
          transform: translateY(-5px); /* Unchanged hover effect */
          box-shadow: 0 6px 12px rgba(0,0,0,0.2); /* Unchanged enhanced shadow on hover */
        }
        
        .card-header {
          font-size: 18px; /* Reduced font size */
          color: #fff; /* Unchanged color */
          font-weight: bold; /* Unchanged font-weight */
          text-align: center; /* Unchanged text-align */
          margin-bottom: 15px; /* Slightly reduced margin-bottom */
          padding-bottom: 5px; /* Reduced padding-bottom */
          border-bottom: 2px solid #2196F3; /* Unchanged border-bottom */
        }
        
        .card-content {
          font-size: 14px; /* Reduced font size for content */
          color: #333; /* Unchanged color */
          text-align: center; /* Unchanged text-align */
          background-color: rgba(255, 255, 255, 0.3); /* Unchanged background-color */
          padding: 15px; /* Adjusted padding for content */
          border-radius: 5px; /* Unchanged border-radius */
        }
      .instruction-text {
        color: #2c3e50; /* Dark blue color for text */
        font-size: 9px; /* Slightly larger font size for readability */
        margin-top: 20px; /* Add some space above the text */
      }
      .instruction-header {
        font-size: 12px;
        color: #007bff; /* Red color for headings to grab attention */
        font-weight: bold; /* Make the header bold */
        margin-bottom: 5px; /* Space below the header */
      }
      .shiny-output-error { color: #ff0000;}
      .shiny-output-error:before {content: 'Error: ';}
      .well {
        background-color: #ffffff;  /* White background for sidebar and panels */
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);  /* Soft shadow for depth */
      }
      table {
                width: 100%;
                border-collapse: collapse;
                margin: 20px 0;
                font-size: 0.9em;
                font-family: 'Mulish', sans-serif;
                min-width: 400px;
                box-shadow: 0 0 20px rgba(0, 0, 0, 0.15);
            }

            thead {
                background-color: #2575fc;
                color: #ffffff;
                text-align: left;
            }

            th, td {
                padding: 12px 15px;
            }

            tr {
                border-bottom: 1px solid #dddddd;
            }

            /* Adding zebra striping to rows */
            tr:nth-of-type(even) {
                background-color: #f3f3f3;
            }

            /* Hover effect for rows */
            tr:hover {
                background-color: #f1f1f1;
                cursor: pointer;
            }

            /* Responsive tables */
            @media screen and (max-width: 600px) {
                table {
                    display: block;
                    overflow-x: auto;
                }
            }
      h3 {
        color: #333333;  /* Dark grey color for headings */
        text-align: center;  /* Center-align the headings */
        font-weight: bold; 
      }
      .btn {  /* Styling buttons */
        background-color: #007bff;  /* Bootstrap primary color */
        color: white;
      }
      .btn:hover {
        background-color: #0056b3;
      }
     #header {text-align: center; background: linear-gradient(to right, #6a11cb, #2575fc); color: white; padding: 10px 0; border-radius: 8px; display: flex; align-items: center; justify-content: center;}  
    "))
    
    
    
  ),
  # Custom header with logo and title
  tags$div(id = "header",
           tags$h1("UPR and GEP Model", style = "display: inline;")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = c(".csv")),
      tags$div(
        class = "instruction-text",
        tags$p(class = "instruction-header", "Data Upload Guidelines:"),
        tags$ul(
          tags$li("Ensure the data format is CSV."),
          tags$li("Required Columns:"),
          tags$ul(
            tags$li(tags$b("BegDate:"), " Representing the policy start date Column."),
            tags$li(tags$b("EndDate:"), " Representing the policy end date Column."),
            tags$li(tags$b("AuthDate:"), " Representing the policy underwriting date Column."),
            tags$li(tags$b("IRA CLASS:"), " Representing be the class of Business Column."),
            tags$li(tags$b("Premium:"), " Representing be the Premium Column."),
          )
        )
      ),
      textInput("valDate", "Valuation Date", value = "31/12/2023"),
      numericInput("cutoffYear", "Set Policy Start Year Threshold", value = 2013, min = 1990, max = 3000),
      numericInput("startYear", "Start Year of Analysis", value = 2022, min = 2000, max = 3000),
      numericInput("endYear", "End Year of Analysis", value = 2023, min = 2000, max = 3000),
      selectInput("endYearQuarter", "Select Quarter for End Year of Analysis", choices = c("All" = "All", "Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4"), selected = "All"),
      actionButton("showData", "View Data"),
      br(),
      br(),
      actionButton("calcUPR", "Calculate Sum of Gross UPR"),
      br(),
      br(),
      actionButton("calcDAC", "Calculate Sum of DAC"),
      br(),
      br(),
      actionButton("calcClassWiseUPR", "Calculate Class-wise Gross UPR Sum"),
      br(),
      br(),
      actionButton("goButton", "Calculate Gross Earned Premium"),
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview", tableOutput("viewData")),
        tabPanel("Unearned Premium Reserves Summaries",
                 div(class = "card", 
                     div(class = "card-header", "Gross UPR Sum"),
                     div(class = "card-content", verbatimTextOutput("UPRSum"))
                 ),
                 br(),
                 div(class = "card", 
                     div(class = "card-header", "DAC Sum"),
                     div(class = "card-content", verbatimTextOutput("DACSum"))
                 ),
                 br(),
                 h3("Class-wise Gross UPR Summarization"),
                 br(),
                 div(class = "center-content",downloadButton("downloadUPR", "Download the Gross UPR Summary Table")),
                 div(class = "center-content", tableOutput("classWiseUPR")),
                 br(),
                 plotOutput("classWiseUPRPlot")),
        tabPanel("Gross Earned Premiums Results by IRA Class", tableOutput("summaryData"), downloadButton("downloadData", "Download Earned Premiums Summary by IRA Class as CSV")),
      )
    )
  )
)


# Define the server logic required to read the input and calculate outputs
server <- function(input, output, session) {
  
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
  
  # Display initial or processed data
  output$viewData <- renderTable({
    req(processedData())
    head(processedData(), n=100)
  })
  
  # Calculate and display Gross UPR Sum
  output$UPRSum <- renderText({
    req(input$calcUPR, processedData())
    upr_sum <- sum(processedData()$Gross_UPR, na.rm = TRUE)
    formatted_upr_sum <- comma(upr_sum)
    paste("", formatted_upr_sum)
  })
  
  # Calculate and display DAC Sum
  output$DACSum <- renderText({
    req(input$calcDAC, processedData())
    dac_sum <- sum(processedData()$DAC, na.rm = TRUE)
    formatted_dac_sum <- comma(dac_sum)
    paste("", formatted_dac_sum)
  })
  
  # Reactive function for class-wise UPR summarization
  classWiseUPR <- eventReactive(input$calcClassWiseUPR, {
    req(processedData())
    data <- processedData()
    data %>%
      group_by(`IRA CLASS`) %>%
      summarise(`Class wise Gross UPR Sum` = sum(Gross_UPR, na.rm = TRUE), `Class wise DAC Sum` = sum(Gross_UPR, na.rm = TRUE)) %>%
      mutate(`Class wise Gross UPR Sum` = scales::comma(`Class wise Gross UPR Sum`))  # Format numbers with commas
  })
  
  # Display class-wise UPR summarization with enhanced styling
  output$classWiseUPR <- renderTable({
    req(classWiseUPR())
    classWiseUPR()
  }, sanitize.text.function = function(x) x)
  
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
        quarters_to_iterate <- if (yr == input$endYear && input$endYearQuarter != "All") {
          input$endYearQuarter
        } else if (yr == input$endYear && input$endYearQuarter == "All") {
          c("Q1", "Q2", "Q3", "Q4")
        } else {
          c("Q1", "Q2", "Q3", "Q4")
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
  
  # Display summarized data
  output$summaryData <- renderTable({
    req(summaryData())
    summaryData()
  }, sanitize.text.function = function(x) x)
  
  
  
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
  # Render the bar graph for class-wise UPR
  output$classWiseUPRPlot <- renderPlot({
    req(classWiseUPR())  # Ensure the data is available
    
    # Prepare the data by removing commas for numeric conversion
    data <- classWiseUPR() %>%
      mutate(`Class wise Gross UPR Sum` = as.numeric(gsub(",", "", `Class wise Gross UPR Sum`)))
    
    # Create the bar graph
    ggplot(data, aes(x = `IRA CLASS`, y = `Class wise Gross UPR Sum`, fill = `IRA CLASS`)) +
      geom_bar(stat = "identity", color = "black", fill = "#2575fc") +
      geom_text(aes(label = scales::comma(`Class wise Gross UPR Sum`)),  # Add this line
                vjust = -0.3,  # Adjust vertical position to be slightly above the bar
                color = "black", size = 3.5) +
      labs(title = "Class-wise Gross UPR Summary",
           x = "IRA Class",
           y = "Gross UPR Sum") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center the plot title
            axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for better readability
            legend.position = "none",
            panel.grid = element_blank(), )  # Hide legend if not necessary
  })
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
