library(shinydashboard)
library(shiny)
library(ggplot2)

function(input, output) {
  
  bools <- reactive({
    c(input$isFever, input$isDryCough, input$isFatigue, input$isShortBreath, input$isSoreThroat, input$isHeadache, input$isChills, input$isNausea, input$isCongestion, input$isDiarrhea)
  })
  
  # includes values only for the symptoms checked
  c_vals <- reactive({
    c(input$c1, input$c2, input$c3, input$c4, input$c5, input$c6, input$c7, input$c8, input$c9, input$c10)
  })
  
  f_vals <- reactive({
    c(input$f1, input$f2, input$f3, input$f4, input$f5, input$f6, input$f7, input$f8, input$f9, input$f10)
  })
  
  s_vals <- reactive({
    c(input$s1, input$s2, input$s3, input$s4, input$s5, input$s6, input$s7, input$s8, input$s9, input$s10)
  })
  
  covidPrevious <- reactive({ input$covidPrev })
  fluPrevious <- reactive({ input$fluPrev })
  
  results <- reactive({
    c_prop <- prod(c_vals()) * covidPrevious()
    f_prop <- prod(f_vals()) * fluPrevious()
    n_prop <- prod(s_vals()) * (1 - covidPrevious() - fluPrevious())
    
    covid <- c_prop/(c_prop+f_prop+n_prop)
    flu <- f_prop/(c_prop+f_prop+n_prop)
    neither <- n_prop/(c_prop+f_prop+n_prop)
    c(covid, flu, neither)
  })
  
  output$resultC <- renderText({
    results()[1]
  })
  
  output$resultF <- renderText({
    results()[2]
  })
  
  output$resultN <- renderText({
    results()[3]
  })
  
  
  output$most_likely <- renderText({
    final_prob <- ""
    if(results()[1] > results()[2] & results()[1] > results()[3]){
      final_prob <- "COVID-19"
    } else if (results()[2] > results()[3]) {
      final_prob <- "Flu"
    } else {
      final_prob <- "Neither"
    }
  })
  
  output$plot1 <- renderPlot(
    barplot(results(), names.arg=c("COVID-19", "Flu", "Neither"))
  )
  
  
  # Sliders for covid
  output$feversliderC  <- renderUI({
    if(bools()[1]) {
      sliderInput(inputId = "c1", 
                  label = "Fever:", 
                  min = 0, 
                  max = 1, 
                  value = 0.879, 
                  step = 0.001)
    }
  })
  
  output$dryCoughSliderC <- renderUI({
    if(bools()[2]) {
      sliderInput(inputId = "c2", 
                  label = "Dry Cough:", 
                  min = 0, 
                  max = 1, 
                  value = 0.677, 
                  step = 0.001)
    }
  })
  
  output$fatigueSliderC <- renderUI({
    if(bools()[3]) {
      sliderInput(inputId = "c3", 
                  label = "Fatigue", 
                  min = 0, 
                  max = 1, 
                  value = 0.381, 
                  step = 0.001)
    }
  })
  
  output$shortofBreathSliderC <- renderUI({
    if(bools()[4]) {
      sliderInput(inputId = "c4", 
                  label = "Shortness of Breath", 
                  min = 0, 
                  max = 1, 
                  value = 0.186, 
                  step = 0.001)
    }
  })
  
  output$soreThroatSliderC <- renderUI({
    if(bools()[5]) {
      sliderInput(inputId = "c5", 
                  label = "Sore Throat", 
                  min = 0, 
                  max = 1, 
                  value = 0.139, 
                  step = 0.001)
    }
  })
  
  output$headacheSliderC <- renderUI({
    if(bools()[6]) {
      sliderInput(inputId = "c6", 
                  label = "Headache", 
                  min = 0, 
                  max = 1, 
                  value = 0.136, 
                  step = 0.001)
    }
  })
  
  output$chillsSliderC <- renderUI({
    if(bools()[7]) {
      sliderInput(inputId = "c7", 
                  label = "Chills", 
                  min = 0, 
                  max = 1, 
                  value = 0.114, 
                  step = 0.001)
    }
  })
  
  output$nauseaSliderC <- renderUI({
    if(bools()[8]) {
      sliderInput(inputId = "c8", 
                  label = "Nausea or Vomiting", 
                  min = 0, 
                  max = 1, 
                  value = 0.05, 
                  step = 0.001)
    }
  })
  
  output$congestionSliderC <- renderUI({
    if(bools()[9]) {
      sliderInput(inputId = "c9", 
                  label = "Congestion", 
                  min = 0, 
                  max = 1, 
                  value = 0.048, 
                  step = 0.001)
    }
  })
  
  output$diarrheaSliderC <- renderUI({
    if(bools()[10]) {
      sliderInput(inputId = "c10", 
                  label = "Diarrhea", 
                  min = 0, 
                  max = 1, 
                  value = 0.037, 
                  step = 0.001)
    }
  })
  
  # Sliders for flu
  output$feversliderF  <- renderUI({
    if(bools()[1]) {
      sliderInput(inputId = "f1", 
                  label = "Fever:", 
                  min = 0, 
                  max = 1, 
                  value = 0.68, 
                  step = 0.001)
    }
  })
  
  output$dryCoughSliderF <- renderUI({
    if(bools()[2]) {
      sliderInput(inputId = "f2", 
                  label = "Dry Cough:", 
                  min = 0, 
                  max = 1, 
                  value = 0.93, 
                  step = 0.001)
    }
  })
  
  output$fatigueSliderF <- renderUI({
    if(bools()[3]) {
      sliderInput(inputId = "f3", 
                  label = "Fatigue", 
                  min = 0, 
                  max = 1, 
                  value = 0.94, 
                  step = 0.001)
    }
  })
  
  output$shortofBreathSliderF <- renderUI({
    if(bools()[4]) {
      sliderInput(inputId = "f4", 
                  label = "Shortness of Breath", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.001)
    }
  })
  
  output$soreThroatSliderF <- renderUI({
    if(bools()[5]) {
      sliderInput(inputId = "f5", 
                  label = "Sore Throat", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.001)
    }
  })
  
  output$headacheSliderF <- renderUI({
    if(bools()[6]) {
      sliderInput(inputId = "f6", 
                  label = "Headache", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.001)
    }
  })
  
  output$chillsSliderF <- renderUI({
    if(bools()[7]) {
      sliderInput(inputId = "f7", 
                  label = "Chills", 
                  min = 0, 
                  max = 1, 
                  value = 0.5, 
                  step = 0.001)
    }
  })
  
  output$nauseaSliderF <- renderUI({
    if(bools()[8]) {
      sliderInput(inputId = "f8", 
                  label = "Nausea or Vomiting", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.001)
    }
  })
  
  output$congestionSliderF <- renderUI({
    if(bools()[9]) {
      sliderInput(inputId = "f9", 
                  label = "Congestion", 
                  min = 0, 
                  max = 1, 
                  value = 0.5, 
                  step = 0.001)
    }
  })
  
  output$diarrheaSliderF <- renderUI({
    if(bools()[10]) {
      sliderInput(inputId = "f10", 
                  label = "Diarrhea", 
                  min = 0, 
                  max = 1, 
                  value = 0.05, 
                  step = 0.001)
    }
  })
  
  # Sliders for basic symptoms
  output$feversliderS  <- renderUI({
    if(bools()[1]) {
      sliderInput(inputId = "s1", 
                  label = "Fever:", 
                  min = 0, 
                  max = 1, 
                  value = 0.01, 
                  step = 0.001)
    }
  })
  
  output$dryCoughSliderS <- renderUI({
    if(bools()[2]) {
      sliderInput(inputId = "s2", 
                  label = "Dry Cough:", 
                  min = 0, 
                  max = 1, 
                  value = 0.05, 
                  step = 0.001)
    }
  })
  
  output$fatigueSliderS <- renderUI({
    if(bools()[3]) {
      sliderInput(inputId = "s3", 
                  label = "Fatigue", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.001)
    }
  })
  
  output$shortofBreathSliderS <- renderUI({
    if(bools()[4]) {
      sliderInput(inputId = "s4", 
                  label = "Shortness of Breath", 
                  min = 0, 
                  max = 1, 
                  value = 0.03, 
                  step = 0.001)
    }
  })
  
  output$soreThroatSliderS <- renderUI({
    if(bools()[5]) {
      sliderInput(inputId = "s5", 
                  label = "Sore Throat", 
                  min = 0, 
                  max = 1, 
                  value = 0.05, 
                  step = 0.001)
    }
  })
  
  output$headacheSliderS <- renderUI({
    if(bools()[6]) {
      sliderInput(inputId = "s6", 
                  label = "Headache", 
                  min = 0, 
                  max = 1, 
                  value = 0.1, 
                  step = 0.001)
    }
  })
  
  output$chillsSliderS <- renderUI({
    if(bools()[7]) {
      sliderInput(inputId = "s7", 
                  label = "Chills", 
                  min = 0, 
                  max = 1, 
                  value = 0.01, 
                  step = 0.001)
    }
  })
  
  output$nauseaSliderS <- renderUI({
    if(bools()[8]) {
      sliderInput(inputId = "s8", 
                  label = "Nausea or Vomiting", 
                  min = 0, 
                  max = 1, 
                  value = 0.05, 
                  step = 0.001)
    }
  })
  
  output$congestionSliderS <- renderUI({
    if(bools()[9]) {
      sliderInput(inputId = "s9", 
                  label = "Congestion", 
                  min = 0, 
                  max = 1, 
                  value = 0.05, 
                  step = 0.001)
    }
  })
  
  output$diarrheaSliderS <- renderUI({
    if(bools()[10]) {
      sliderInput(inputId = "s10", 
                  label = "Diarrhea", 
                  min = 0, 
                  max = 1, 
                  value = 0.001, 
                  step = 0.001)
    }
  })
}
