# load the model
load("data/train.dfm.rda")

function(input, output) {
    
  # get functions ----
  source("prediction_functions.R")
  
  # get prediction text
  prediction.text <- reactive({input$prediction.text})
  
  # predict top 3 words
  top3 <- reactive({get_word3(prediction.text(), train.dfm)})
  
  # summary of prediction
  prediction.summary <- reactive({predict_next_word(prediction.text(), train.dfm)})
      
  # render results ----
  # top 3 predictions
  output$prediction1 <- renderText(top3()[1])
  output$prediction2 <- renderText(top3()[2])
  output$prediction3 <- renderText(top3()[3])
  
  # summary of predictions
  output$prediction.table <- renderTable(as.data.frame(prediction.summary()$potential_matches))
        
  
}

