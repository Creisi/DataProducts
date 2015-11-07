library(shiny)
require(lubridate)

candidatesData <<- read.csv(file = "../data/combinedData.csv", header = T, as.is = T) 
candidatesData <<- candidatesData[-1]
candidatesData$Date <-ymd(candidatesData$Date)


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


setCorrectCandidates <- function(input, output,clientData, session) { 
    updateSelectInput(session = session, inputId =  "candidate", selected = "All")
}


filterCandidates <- function(data, candidates) { 
  if(length(candidates) == 1 & candidates[1] == "All") { 
    filtered <-  data
  } else { 
    filtered <- candidatesData[candidatesData$Candidate %in% input$candidate,]
  }
  filtered
}

filterDates <- function(data, dates)  {
  show(class(dates[1]))
 
  
}

filterReportData <- function(data, report) { 
cat(report)

}

updateGeo <- function(output, selectedCandidates, dates, reportType  ) { 
  show(Sys.time())
  reportData <- filterCandidates(candidatesData, selectedCandidates)
  reportData <- filterDates(candidatesData, dates )
  reportData <- filterReportData(candidatesData, reportType)
  show(reportData)
  output$geomText <- renderText({ paste("Generated ", reportType, " for ", selectedCandidates, 
                                        " between ", dates[1], " and ", dates[2], 
                                        sep = "")
    })

}


shinyServer(function(input, output,clientData, session) {
 # cleanupCandidateList(input, output, session)
 
  show(paste("Client Joined ", Sys.time()))
  

    observeEvent(input$goButton, { isolate(
        switch (input$tabs,
                "Geographic" = updateGeo(output, input$candidate , input$dates, input$Radio),
                "Plot" = show("plot"),
                "Table" = show(getwd())
        )
    )})
    show(paste("client done ", Sys.time()))
   
})
