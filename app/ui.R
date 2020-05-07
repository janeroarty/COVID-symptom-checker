library(shinydashboard)
library(shiny)
library(ggplot2)

header <- dashboardHeader(title = "COVID-19 Symptom Checker", titleWidth = 300)


sidebar <- dashboardSidebar(
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("info")),  
      menuItem("Symptoms", tabName = "symptoms", icon = icon("thermometer-three-quarters")),
      menuItem("Symptom Probabilities", tabName = "covidprobs", icon = icon("dashboard")),
      menuItem("Disease Prevalence", tabName = "diseaseprev", icon = icon("dashboard")),
      menuItem("Results", tabName = "results", icon = icon("poll-h"))
    )
  )
)

body <- dashboardBody(tabItems(
  
  tabItem(tabName = "welcome",
          tags$h2("Welcome to a Naive Bayesian Model for COVID-19"),
          tags$p("Welcome to a Naive Bayesian Model for COVID-19. In this SHINY app, there are two main features. In the symptoms tab you can check the box of which symptoms you are experiencing. The results tab will then tell you the probability that you have COVID-19, the probability that you have the flu, and the probability that you have neither. In the 'Probabilities' tabs you can adjust the probabilities for each symptom. We made assumptions about default probabilities using data from the following sources:"),
          tags$p(tags$a(href="https://www.cdc.gov/flu/symptoms/coldflu.htm", "CDC")),
          tags$p(tags$a(href="https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf", "WHO")),
          
          tags$p("Since this is a Naive Bayes model we needed to make some assumptions. Our model assumes independence among each symptom and it assumes independence between the flu, COVID-19 and neither. We also limited the number of symptom in our model. We choose 10 symptoms because they are the most common and we were able to find the most information about these symptoms in order to assign default probabilites. Finally, although we did have reaserch for each symptom we made some assumptions about the probabilities for each symptom. We hoped to remedy that to some extent by allowing users to change the probabilities for each symptom in the 'Symptom Probabilities' tab."), 
          
          tags$div(
            tags$b("Theory"),
            tags$h4(withMathJax("$$P(C) + P(F) + P(N) = 1$$")),
            tags$h4(withMathJax("$$P(C|S_{1},S_{2},...,S_{n}) \\propto c = P(C)*\\prod_{i=1}^{n}P(S_{i}|C)$$")),
            tags$h4(withMathJax("$$P(F|S_{1},S_{2},...,S_{n}) \\propto f = P(F)*\\prod_{i=1}^{n}P(S_{i}|F)$$")),
            tags$h4(withMathJax("$$P(N|S_{1},S_{2},...,S_{n}) \\propto n = P(N)*\\prod_{i=1}^{n}P(S_{i}|N)$$")),
            tags$p("Then, we have: "),
            tags$h4(withMathJax("$$P(C|S_{1},S_{2},...,S_{n}) = \\frac{c}{c+f+n}$$")),
            tags$h4(withMathJax("$$P(F|S_{1},S_{2},...,S_{n}) = \\frac{f}{c+f+n}$$")),
            tags$h4(withMathJax("$$P(N|S_{1},S_{2},...,S_{n}) = \\frac{n}{c+f+n}$$")),
            tags$p(withMathJax("Where P(C) = Probability of COVID-19, P(F) = Probability of Flu, and P(N) = Probability of neither. P(C) and P(F) are estimates of the proportions of people with each respective disease at a given time. Each S represents a different symptom that a person may experience with COVID-19. $$P(S_{i}|C)$$ is the probability of having symptom i when a person has COVID-19. This applies to $$P(S_{i}|F)$$ and $$P(S_{i}|N)$$ with the flu and neither, respectively."))
          ),
          
          tags$div(
            HTML("<b>NOTICE:</b> This app should not be used for diagnosis. We used Naive Bayesian Statistics for our model so many assumptions were made in the creation of this app. If you think that you have COVID-19, you should get tested immediately and stay quarantined until you are recovered."))),
  
  
  
  #First tab content
  tabItem(tabName = "symptoms",
          fluidRow(
            box(title = "Instructions:", "Check all of your symptoms. Once finished, either adjust the probabilites in the following two tabs (though they have already been set to approximate values) or proceed to the results tab for the probabilites of each disease.", width = 12)),
          fluidRow(
            box(
              checkboxInput(inputId = "isFever", 
                            label = "Fever",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isDryCough", 
                            label = "Dry Cough",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isFatigue", 
                            label = "Fatigue",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isShortBreath", 
                            label = "Shortness of Breath",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isSoreThroat", 
                            label = "Sore Throat",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isHeadache", 
                            label = "Headache",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isChills", 
                            label = "Chills",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isNausea", 
                            label = "Nausea",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isCongestion", 
                            label = "Congestion",
                            value = FALSE)
            ),
            box(
              checkboxInput(inputId = "isDiarrhea", 
                            label = "Diarrhea",
                            value = FALSE)
            )
            # , 
            # box(
            #   checkboxInput(inputId = "isJointAche", 
            #                 label = "Joint Ache",
            #                 value = FALSE)
            # )
          )
  ),
  
  # Second tab content
  tabItem(tabName = "covidprobs",
          fluidRow(
            box(title = "Instructions:", "Here, you can adjust the probabilites of each symptom given each disease. We set the initial values based on preliminary research, but these values can change as research develops.", width = 12
            )),
          fluidRow(
            box(title = "COVID-19 Probabilities",
                uiOutput("feversliderC"),
                uiOutput("dryCoughSliderC"),
                uiOutput("fatigueSliderC"),
                uiOutput("shortofBreathSliderC"),
                uiOutput("soreThroatSliderC"),
                uiOutput("headacheSliderC"),
                uiOutput("chillsSliderC"),
                uiOutput("nauseaSliderC"),
                uiOutput("congestionSliderC"),
                uiOutput("diarrheaSliderC")
            ),
            box(title = "Flu Probabilities",
                uiOutput("feversliderF"),
                uiOutput("dryCoughSliderF"),
                uiOutput("fatigueSliderF"),
                uiOutput("shortofBreathSliderF"),
                uiOutput("soreThroatSliderF"),
                uiOutput("headacheSliderF"),
                uiOutput("chillsSliderF"),
                uiOutput("nauseaSliderF"),
                uiOutput("congestionSliderF"),
                uiOutput("diarrheaSliderF")
            ),
            box(title = "Neither Probabilites",
                uiOutput("feversliderS"),
                uiOutput("dryCoughSliderS"),
                uiOutput("fatigueSliderS"),
                uiOutput("shortofBreathSliderS"),
                uiOutput("soreThroatSliderS"),
                uiOutput("headacheSliderS"),
                uiOutput("chillsSliderS"),
                uiOutput("nauseaSliderS"),
                uiOutput("congestionSliderS"),
                uiOutput("diarrheaSliderS")
            )
          )
  ),
  
  tabItem(tabName = "diseaseprev",
          fluidRow(
            box(title = "Instructions:", "On this page, you can adjust the probabilities to reflect the infection rate of each disease in your population. This tab can help to account for seasonality including the time of year or the geographical location that are relevant when taking this test. Here you can adjust the probability of having COVID-19 (P(C)) and the Flu (P(F)). The probability of neither (P(N)) is calculated by subtracting P(C) and P(F) from 1.", width = 12)),
          fluidRow(
            box(
              sliderInput(inputId = "covidPrev", 
                          label = "COVID-19 Infection Rate", 
                          min = 0, 
                          max = 1, 
                          value = 0.002, 
                          step = 0.001),
              sliderInput(inputId = "fluPrev", 
                          label = "Flu Infection Rate", 
                          min = 0, 
                          max = 1, 
                          value = 0.01, 
                          step = 0.001)
            )
          )
  ),
  
  tabItem(tabName = "results",
          fluidRow(
            box(
              title = "Results",
              h3("Probability of COVID-19 = ", textOutput("resultC", container = span)),
              h3("Probability of flu = ", textOutput("resultF", container = span)),
              h3("Probability of neither = ", textOutput("resultN", container = span))
            ),
            fluidRow(
              box(h3("You most likely have:", textOutput("most_likely", container = span))),
              box(plotOutput("plot1", width = "100%"))
            ),
            box(title = "WARNING:", "The results from this app should not be used for diagnosis. We used Naive Bayesian Statistics for our model so many assumptions were made in the creation of this app. If you think that you have COVID-19, you should get tested immediately and stay quarantined until you are recovered.", width = 12)
          )
  )
  
))

ui <- dashboardPage(header, sidebar, body)