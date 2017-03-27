# Word Predition Project
# Nick Lukianoff, March 24, 2017
# Coursera Data Science Capstone Project

library(shiny)

shinyUI(
        fluidPage(
                titlePanel("Typed Text Predictor"),
                helpText("This program will accept any typed text of one word or more."),
                helpText("The input should be a short phrase."),
                helpText("No punctuation is necessary."),
                helpText("This amazing app will find the next word as you type !"),
                helpText("And now ... if you're ready ... press TAB to begin."),
                helpText("Note: data load after the first phrase takes up to 20 seconds."),
                
                textInput(
                        "b_text",
                        h3("Type your Text here:"),
                        value = "",
                        width = 400
                ),
                hr(),
                fluidRow(column(3, textOutput("firstguess"))),
                fluidRow(column(3, textOutput("nextguess")))
        )
)
