library(shiny)


doTheSame <- function(input, output, session) { 
  observe({ 
    selected = NULL
    if(length(input$candidate) == 1) { 
      output$summary <- renderPrint(expr = paste(input$candidate))
      selected <- input$candidate
    } else if((length(input$candidate) == 2) & ("All" %in% input$candidate)) { 
      selected <- input$candidate[input$candidate != "All"]
      output$summary <- renderPrint(expr = paste(input$candidate[input$candidate != "All"]))
    } else if ("All" %in% input$candidate) {
      selected = "All"
      output$summary <- renderPrint(expr = "All")
    } else { 
      selected = input$candidate
      output$summary <- renderPrint(input$candidate)
    }
    updateSelectInput(session = session, inputId =  "candidate", selected = selected)
  })
}

# Define server logic required to draw a histogram
shinyServer(function(input, output,clientData, session) {
  doTheSame(input, output, session)
  })
