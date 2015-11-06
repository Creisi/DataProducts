library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(h1("Coursera Data Products Project"),  
             h3("Donations and Spent - Presidential Candidates 2016")),
  fluidRow(sidebarLayout(
    sidebarPanel( 
      dateRangeInput("dates", label = h4("Select data range to report"), 
                     separator = "until", min = Sys.Date() - 10, max = Sys.Date() + 10),
      selectInput("candidate", label = h4("Select Candidate(s)"),
                  multiple = TRUE, choices =  c(levels(combinedData$Candidate))),
      
      radioButtons("Radio", h4("Type of report"), c("Donations", "Spent", "Remaining"))
    ),
    mainPanel(
      p("Some Graph")
    )
  )),
  fluidRow(
    p("Made for ", 
      HTML("<a href=\"https://www.coursera.org/course/devdataprod\">Developing Data Products</a>."),
      "Data from ", 
      HTML("<a href=\"http://www.fec.gov/portal/presidential.shtml\">FEC, 2016 Presidential Elections</a>."),
      "Code available on github", HTML("<a href=\"https://github.com/StartFinishing/DataProducts\">https://github.com/StartFinishing/DataProducts</a>"))
    
  )
))

?p
