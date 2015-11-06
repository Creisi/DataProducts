library(shiny)


cleanupCandidateList <- function(input, output, session) { 
  observe({ 
    selected = NULL
    if(length(input$candidate) == 1) { 
      selected <- input$candidate
    } else if((length(input$candidate) == 2) &  (input$candidate == "All")[1]) { 
      selected <- input$candidate[input$candidate != "All"]
    } else if((length(input$candidate) == 2) &  (input$candidate == "All")[2]) { 
      selected <- "All"
    } else if ("All" %in% input$candidate) {
      selected = "All"
    } else { 
      selected = input$candidate
    }
    
    updateSelectInput(session = session, inputId =  "candidate", selected = selected)
})
}

presidentialData <<- read.csv(file = "../data/combinedData.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output,clientData, session) {
  cleanupCandidateList(input, output, session)
  observe({ 
    switch (input$tabs,
      "Geographic" = show("Geo"),
      "Plot" = show("plot"),
      "Table" = show(getwd())
    )
  })
})
