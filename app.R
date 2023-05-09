library(ggplot2)
library(dplyr)
library(tidyr)
library(quantmod)
library(shiny)

# Make sure data.table is installed
if(!'data.table' %in% installed.packages()[,1]) install.packages('data.table')

# Function to fetch google stock data
google_stocks <- function(sym, current = TRUE, sy = 2000, sm = 1, sd = 1, ey, em, ed)
{
  # sy, sm, sd, ey, em, ed correspond to
  # start year, start month, start day, end year, end month, and end day
  
  # If TRUE, use the date as the enddate
  if(current){
    system_time <- as.character(Sys.time())
    ey <- as.numeric(substr(system_time, start = 1, stop = 4))
    em <- as.numeric(substr(system_time, start = 6, stop = 7))
    ed <- as.numeric(substr(system_time, start = 9, stop = 10))
  }
  
  require(data.table)
  
  # Fetch data from google
  google_out = tryCatch(
    suppressWarnings(
      fread(paste0("http://www.google.com/finance/historical",
                   "?q=", sym,
                   "&startdate=", paste(sm, sd, sy, sep = "+"),
                   "&enddate=", paste(em, ed, ey, sep = "+"),
                   "&output=csv"), sep = ",")),
    error = function(e) NULL
  )
  
  # If successful, rename first column
  if(!is.null(google_out)){
    names(google_out)[1] = "Date"
  }
  
  return(google_out)
}

# Test it out
apple_data = google_stocks('AAPL')

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("More Widgets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Specify the number of observations to view ----
      numericInput("obs", "Number of observations to view:", 10),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4("Observations"),
      tableOutput("view")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)