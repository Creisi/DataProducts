library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(h1("Coursera Data Products Project"),  
             h3("Donations and Spent - Presidential Candidates 2016")),
  fluidRow(sidebarLayout(
    sidebarPanel( 
      dateRangeInput("dates", label = h4("Select data range to report"), 
                     separator = "until", min = Sys.Date() - 10, max = Sys.Date() + 10),
      selectInput("candidate", label = h4("Select Candidate(s)"), selected = "All",
                  multiple = TRUE, choices =  c("All", "b", "c")),
      
      radioButtons("Radio", h4("Type of report"), c("Donations", "Spent", "Remaining"))
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Geographic", verbatimTextOutput("summary")),
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )),
  fluidRow(
      "Usage : Do something on the left and some stuff will change on the right",
      br(), "Made for ", 
      HTML("<a href=\"https://www.coursera.org/course/devdataprod\">Developing Data Products</a>."),
      "Data from ", 
      HTML("<a href=\"http://www.fec.gov/portal/presidential.shtml\">FEC, 2016 Presidential Elections</a>."),
      "Code available on github", HTML("<a href=\"https://github.com/StartFinishing/DataProducts\">https://github.com/StartFinishing/DataProducts</a>")
    )
    
  )
)
