library(shiny)

candidateList <- c("All", "Bush, Jeb","Carson, Benjamin S.", "Christie, Christopher J.", 
                   "Clinton, Hillary Rodham", "Cruz, Rafael Edward 'Ted'", 
                   "Fiorina, Carly", "Graham, Lindsey O.",  "Huckabee, Mike", 
                   "Jindal, Bobby",  "Kasich, John R.", "Lessig, Lawrence",  
                   "O'Malley, Martin Joseph", "Pataki, George E.",  "Paul, Rand",  
                   "Perry, James R. (Rick)",  "Rubio, Marco",  "Sanders, Bernard", 
                   "Santorum, Richard J.",  "Trump, Donald J.", "Walker, Scott", 
                   "Webb, James Henry Jr." )

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    h1("Coursera Data Products Project"),  
    h3("Donations and Spent - Presidential Candidates 2016")),
  fluidRow(sidebarLayout(
    sidebarPanel( 
      dateRangeInput("dates", label = h4("Select data range to report"), 
                     separator = "until", min = "2013-10-01", max = "2015-11-07", 
                     start = "2013-10-01", end = "2015-11-07" ),
      selectInput("candidate", label = h4("Select Candidate(s)"), selected = "All",
                  multiple = TRUE, choices =  candidateList),
      
      radioButtons("Radio", h4("Type of report"), c("Donations", "Spent", "Remaining")),
      actionButton(inputId = "goButton",  label = "Genereate!")
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Geographic", fluidRow( 
                    htmlOutput("geomPlot")
                  )
                  ), 
                  tabPanel("Plot", fluidRow( 
                    plotOutput("plotPlot")
                  )),
                  tabPanel("Table",fluidRow( 
                    dataTableOutput("tableTable"))
                  ) 
      )
    )
  )),
  fluidRow(
    verbatimTextOutput("commandTxt"), 
    "Usage : Select stuff on the left, click Generate! and some stuff will change on the right",
    br(), "Made for ", 
    HTML("<a href=\"https://www.coursera.org/course/devdataprod\">Developing Data Products</a>."),
    "Data from ", 
    HTML("<a href=\"http://www.fec.gov/portal/presidential.shtml\">FEC, 2016 Presidential Elections</a>."),
    "Code available on github", HTML("<a href=\"https://github.com/StartFinishing/DataProducts\">https://github.com/StartFinishing/DataProducts</a>")
  )
  
)
)

