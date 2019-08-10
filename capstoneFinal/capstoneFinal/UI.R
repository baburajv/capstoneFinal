library(shiny)
library(markdown)

shinyUI(navbarPage("Capstone: Course Project",
                   tabPanel("Word Prediction",
                            # Sidebar
                              sidebarLayout(
                              sidebarPanel(
                                helpText("Enter a partial sentence here"),
                                textInput("inputString", "Enter a partial sentence here",value = ""),
                                ),
                              mainPanel(
                                  h2("Predicted Next Word"),
                                  verbatimTextOutput("prediction"),
                                  strong("Sentence Input:"),
                                  textOutput('text1'),
                                  br(),
                                  strong("Note:"),
                                  textOutput('text2')
                              )
                              )
                             
                  )
)
)