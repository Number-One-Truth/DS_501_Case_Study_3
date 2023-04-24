#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(data.table)
library(tree)
library(randomForest)
library(ggcorrplot)
library(gbm)
library(rsconnect)
#deployApp()

gbm_model <- readRDS("gbm_model.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("Calcasola Case Study 3 - Using Generalized Boosted Random Tree Models to Predict Restaurant Origin"),
    
    # Application Theme
    theme = shinytheme("slate"),
    
    #Page Header
    headerPanel("Fast Food Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$label(h3('Item Characteristics')),
            numericInput("cal_fat",
                         label = "Calories From Fat",
                         value = NA),
            numericInput("sodium",
                         label = "Sodium (mg)",
                         value = NA),
            numericInput("total_carb",
                         label = "Total Carb (g)",
                         value = NA),
            numericInput("fiber",
                         label = "Fiber (g)",
                         value = NA),
            numericInput("sugar",
                         label = "Sugar (g)",
                         value = NA),
            numericInput("protein",
                         label = "Protein (g)",
                         value = NA),
            
            actionButton("submitbutton", "Calculate All Probabilities",
                         class = "btn btn-primary")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tags$label(h3('Probability of Restaurant Origin')),
          #verbatimTextOutput('contents'),
          tableOutput('tabledata'),
          tags$label(h3(textOutput('prediction'))),
          
          
          
          tags$div(
          HTML("<h5>This shiny app will predict the fast food restaurant given the input parameters. The dataset used in this project can be found via the link below.</h5>"),
          tags$a(href="https://www.kaggle.com/datasets/ulrikthygepedersen/fastfood-nutrition", "Fast Food Nutrition Dataset From Kaggle")
          ),
          
          tags$div(
            tags$style(
              "p{
              padding: 10px;
              }"
            ),
          HTML("<p>To explore all possible restaurants, I have listed the range of the data below. This is just a suggestion as the model will still predict for values outside these bounds.</p>"),
          HTML("Calories From Fat: 0 - 1300 || "),
          HTML("Sodium: 15 - 6000 || "),
          HTML("Total Carb: 0 - 160 || "),
          HTML("Fiber: 0 - 15 || "),
          HTML("Sugar: 0 - 90 || "),
          HTML("Protein: 5 - 190"),
          HTML("<p> </p>")
          ),
          
          tags$div(
          HTML("This dataset contains information from 8 fast food restaurants; Arbys, Burger King, Chic Fil-A, Dairy Queen, Mcdonalds, Sonic, Subway, and Taco Bell."),
          HTML("I wanted to see if I could predict the restaurant given a set of parameters so I began to explore classification models. I first picked a random forest model because I knew it could provide classification for multiple classes."),
          HTML("I trained this random forest model with 80% of the data and used the remaining 20% to validate this model."),
          HTML("Through exploration of various input parameters, number of trees, predictors to choose at each node, the best models had an accuracy on the test data of 60%-65%"),
          HTML("I wanted to improve the accuracy. I therefore looked at gradient boosting as an alternate model"),
          HTML("The gradient boosted models were better than random forest. By exploring the same parameters I was able to get a model with 73% accuracy on the test data."),
          HTML("While this model was effective, it took close to 2 minutes to be created. The high computational cost was due to the model having 25000 trees with a shrinkage value of 0.001."),
          HTML("To improve this I used the summary function in the GBM package to see the most significant predictors. The six predictors you see in this shiny app were the six most significant predictors."),
          HTML("By reducing the number of predictors I reduced the build time greatly while still maintaining an accuracy greater than 70%."),
          HTML("A new model does not need to be made each time a prediction is made. This shiny app uses a generalized boosted regression model created from all of the data in the dataset."),
          HTML("After a change in parameters, the model makes a prediction based on these parameters and outputs the result."),
          HTML("The prediction is based on the most likely restaurant generated by the model. Clicking the button shows the prediction values for each fast food restaurant.")
          ),
          
          tags$div(
          HTML("<h4>Questions</h4>"),
          ),
          tags$div(
            HTML("<h5>What data did you collect?</h5>"),
            HTML("<h5>Data was collected from Kaggle.com. The link to this dataset is found in the description above.</h5>")
          ),
          tags$div(
            HTML("<h5>Why is this topic interesting?</h5>"),
            HTML("<h5>I thought it would be interesting to build a predictive model. With the combination of more restaurants, you could use this model to determine the healthiness of various foods. If the food is predicted to come from a healthy restaurant then there is a good likelihood that the food is healthy. This model can be used to make smarter food choice for snacks based on how similar the nutrition is to various restaurants.</h5>")
          ),
          tags$div(
            HTML("<h5>How did you analyze the data?</h5>"),
            HTML("<h5>My analysis is in the description above.</h5>")
          ),
          tags$div(
            HTML("<h5>What did you find in the data?</h5>"),
            HTML("<h5>I found that when it comes to predictive analysis, the top six predictors in order or most significant to least significant is, calories from fat, total carbs, sugar, fiber, protein, and sodium. From this analysis, I built the machine learning model. I also found out that GBM is more accurate than simply random tress in this example. I was able to achieve a higher accuracy using the GBM model.</h5>")
          )
          
        )
    )
)

# Define server logic
server <- function(input, output, session) {
  
  # Input Data

  datasetInput <- reactive({
    Value = as.character(c(input$cal_fat, input$sodium, input$total_carb, input$fiber, input$sugar, input$protein))
    
    df <- data.frame(matrix(ncol = 6, nrow = 1))

    colnames(df) <- c("cal_fat", "sodium", "total_carb", "fiber", "sugar", "protein")
    Values = as.character(c(input$cal_fat, input$sodium, input$total_carb, input$fiber, input$sugar, input$protein))

    df <- rbind(df, Values)
    df <- df[-1,]
    output <- predict(gbm_model, newdata = df, type = "response")
    format_output = as.data.frame(output)
    colnames(format_output) <- c("Arbys", "Burger King", "Chick Fil-A", "Dairy Queen", "Mcdonalds", "Sonic", "Subway", "Taco Bell")
    print(format_output)
    
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  output$prediction <- renderText({
    results = datasetInput()
    if (input$submitbutton > 0){
      prediction = which.max(results[1,])
      prediction_name = names(results[prediction])
      sentence = "This item is most likely from " 
      restaurant_name = prediction_name[1]
      return(paste0(sentence, restaurant_name, "."))
    }
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
