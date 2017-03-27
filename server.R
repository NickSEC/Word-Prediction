# Word Predition Project
# Nick Lukianoff, March 24, 2017
# Coursera Data Science Capstone Project

library(shiny)

#Initialize variables
bestguess <- ""
secondguess <- ""
matchingRows <-
        data.frame(Gram = character(),
                   Freq = numeric(),
                   stringsAsFactors = FALSE)

#Read in N-grams
# two_df <- read.table("BiGram.txt", header = TRUE, fill = TRUE)
# three_df <- read.table("TriGram.txt", header = TRUE, fill = TRUE)
# four_df <- read.table("QuadGram.txt", header = TRUE, fill = TRUE)
two_df <- read.table(url("https://github.com/NickSEC/Word-Prediction/BiGram.txt"), header = TRUE, fill = TRUE)
three_df <- read.table(url("https://github.com/NickSEC/Word-Prediction/TriGram.txt"), header = TRUE, fill = TRUE)
four_df <- read.table(url("https://github.com/NickSEC/Word-Prediction/QuadGram.txt"), header = TRUE, fill = TRUE)

# Define server logic
shinyServer(function(input, output) {

        guessword <- reactive ({
                basetext <- tolower(input$b_text)
                
                #Select last several words if long sentence
                baselength <- length(unlist(strsplit(as.character(basetext), split = " ")))
                
                if (baselength > 3) {
                        basetext1 <- unlist(strsplit(as.character(basetext), split = " "))
                        basetext <-
                                paste(basetext1[baselength - 2], basetext1[baselength - 1], basetext1[baselength])
                        baselength = 3
                }
                
                #Check quad-grams if we have 3 words entered
                if (baselength == 3) {
                        matchingRows <-
                                four_df[grep(paste("^", basetext, sep = ""),
                                             four_df$QuadGram), ]
                        if (dim(matchingRows)[1] == 1)
                                bestguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$QuadGram[1]),
                                                split = " "
                                        ))[4]
                        if (dim(matchingRows)[1] > 1) {
                                bestguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$QuadGram[1]),
                                                split = " "
                                        ))[4]
                                secondguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$QuadGram[2]),
                                                split = " "
                                        ))[4]
                        }
                        if (dim(matchingRows)[1] == 0) {
                                basetext1 <- unlist(strsplit(
                                        as.character(basetext),
                                        split = " "
                                ))
                                basetext <-
                                        paste(basetext1[baselength - 1], basetext1[baselength])
                                baselength = 2
                        }
                }
                
                #Check tri-grams if we have 2 known words
                if (baselength == 2) {
                        #try TriGrams
                        matchingRows <-
                                three_df[grep(paste("^", basetext, sep = ""),
                                              three_df$TriGram), ]
                        if (dim(matchingRows)[1] == 1)
                                bestguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$TriGram[1]),
                                                split = " "
                                        ))[3]
                        if (dim(matchingRows)[1] > 1) {
                                bestguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$TriGram[1]),
                                                split = " "
                                        ))[3]
                                secondguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$TriGram[2]),
                                                split = " "
                                        ))[3]
                        }
                        if (dim(matchingRows)[1] == 0) {
                                basetext1 <- unlist(strsplit(
                                        as.character(basetext),
                                        split = " "
                                ))
                                basetext <- basetext1[baselength]
                                baselength = 1
                        }
                }
                
                #Check bi-grams if we have 1 word to investigate
                if (baselength == 1) {
                        #try BiGrams
                        matchingRows <-
                                two_df[grep(paste("^", basetext, sep = ""),
                                            two_df$BiGram), ]
                        if (dim(matchingRows)[1] == 1)
                                bestguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$BiGram[1]),
                                                split = " "
                                        ))[2]
                        if (dim(matchingRows)[1] > 1) {
                                bestguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$BiGram[1]),
                                                split = " "
                                        ))[2]
                                secondguess <-
                                        unlist(strsplit(
                                                as.character(matchingRows$BiGram[2]),
                                                split = " "
                                        ))[2]
                        }
                }
                
                #if no match foud - assign default value
                if (bestguess == "")
                        bestguess <- "eh?"
                
                if(bestguess == secondguess)
                        secondguess <- ""
                
                return(c(bestguess, secondguess))

        }) #end of reactive part
        
        #Assign best words
        output$firstguess <- renderText({guessword()[1]})
        output$nextguess <- renderText({guessword()[2]})
})
