library(shiny)
library(tm)
library(wordnet)
set.seed(12345)

shinyUI(fluidPage(
    splitLayout(cellWidths = c("20%", "60%", "20%"),
                div(),
                div(titlePanel("Data Science Capstone: Predictive Text Model")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "60%", "20%"),
                div(),
                div(a(em("Click Here to View Documentation"), href = "documentation.html", target = "_blank")),
                div()
    ),
    
    hr(style = "border-color: black"),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(strong("Phrase:")),
                div(textInput("textIn", label = NULL, width = "100%", placeholder = "Enter text in English")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(),
                div(submitButton("Predict")),
                div()
    ),
    
    br(), br(),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(strong("Results:")),
                div(textOutput("textOut1")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(),
                div(textOutput("textOut2")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(),
                div(textOutput("textOut3")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(),
                div(textOutput("textOut4")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(),
                div(textOutput("textOut5")),
                div()
    ),
    
    splitLayout(cellWidths = c("20%", "10%", "50%", "20%"),
                div(),
                div(),
                div(textOutput("textOut6")),
                div()
    ),
    
    br(), br(),
    
    hr(style = "border-color: black"),
    
    div(em("DISCLAIMER: This Shiny Web App is for educational purposes only. The data and prediction accuracies are not guaranteed."), style = "text-align: center")
))
