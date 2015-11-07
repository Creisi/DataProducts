library(shiny)
require(lubridate)
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(dplyr))

candidatesData <<-
  read.csv(file = "../data/combinedData.csv", header = T, as.is = T)
candidatesData <<- candidatesData[-1]
candidatesData$Date <- as.Date(ymd(candidatesData$Date))


cleanupCandidateList <- function(input, output, session) {
  selected = NULL
  if (length(input$candidate) == 1) {
    selected <- input$candidate
  } else if ((length(input$candidate) == 2) &
             (input$candidate == "All")[1]) {
    selected <- input$candidate[input$candidate != "All"]
  } else if ((length(input$candidate) == 2) &
             (input$candidate == "All")[2]) {
    selected <- "All"
  } else if ("All" %in% input$candidate) {
    selected = "All"
  } else {
    selected = input$candidate
  }
  updateSelectInput(session = session, inputId =  "candidate", selected = selected)
  
}


setCorrectCandidates <-
  function(input, output,clientData, session) {
    updateSelectInput(session = session, inputId =  "candidate", selected = "All")
  }


filterCandidates <- function(data, candidates) {
  if (length(candidates) == 1 & candidates[1] == "All") {
    filtered <-  data
  } else {
    filtered <-
      data[data$Candidate %in% candidates,]
  }
  filtered
}

filterDates <- function(data, dates)  {
  data <- data[data$Date >= dates[1],]
  data <- data[data$Date <= dates[2],]
  data
}

filterReportData <- function(data, report) {
  switch(
    report,
    "Donations" = data <- data[data$Amount > 0,],
    "Spent" = data <- data[data$Amount < 0,] ,
    "Remaining" = data <- data
  )
  data
}

getData <-
  function(output, selectedCandidates, dates, reportType) {
    reportData <- filterCandidates(candidatesData, selectedCandidates)
    reportData <- filterDates(candidatesData, dates)
    reportData <- filterReportData(candidatesData, reportType)
    output$commandTxt <-
      renderText({
        paste(
          "Generated ", reportType, " for ", selectedCandidates,
          " between ", dates[1], " and ", dates[2], ". Observations : ", nrow(reportData),
          sep = ""
        )
      })
    reportData
}


getGeomData <-
  function(output, selectedCandidates, dates, reportType)  {
    geomData <- getData(output, selectedCandidates, dates, reportType)
    geomData <- group_by(geomData, State)
    geomData <-  summarise(geomData, Amount = sum(Amount))
    geomData
}

updateGeo <-
  function(output, selectedCandidates, dates, reportType) {
    geomData <- getGeomData(output, selectedCandidates, dates, reportType)
    geomData$State <- paste(geomData$State, ", United States")
    output$geomPlot <- renderGvis({
      gvisGeoChart(
        geomData,
        locationvar = "State", colorvar = "Amount",
        options = list(
          region = "US", displayMode = "regions",
          resolution = "provinces",
          width = 500, height = 400,
          colorAxis = "{colors:['#FFFFFF', '#0000FF']}"
        )
      )
    })
  }



shinyServer(function(input, output, session) {
  observeEvent(input$candidate, {
    isolate(cleanupCandidateList(input, output, session))
  })
  
  
  
  observeEvent(input$goButton, {
    isolate({
      switch (
        input$tabs,
        "Geographic" = updateGeo(output, input$candidate , input$dates, input$Radio),
        "Plot" = show("plot"),
        "Table" = show(getwd())
        
      )
    })
  })
  
  
})
