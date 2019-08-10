library(shiny)
library(shinythemes)
library(dplyr)
require(markdown)
library(tm)

shinyUI(navbarPage("Coursera Data Science Capstone - Text Prediction",
     tabPanel(
              textInput("user_input", "Please enter text for next word prediction:", value =  ""),
              
              tags$hr(),
              
              h4("Prediction"),
              textOutput("guess1")
              
     )
))