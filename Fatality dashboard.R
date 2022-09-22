# Load packages ----
library(shiny)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(MASS)
library(dplyr)
library(DT)

# Loading the data set of interest
fata <- read.csv(file = "fatality data.csv")

# Creating sub data sets by Rate variable 
fatay <- fata[fata$Rate == 1,]
fatan <- fata[fata$Rate == 0,]

# Creating sub data set to use for Data Exploration tab (q4)
fataf <- fata
fataf$Rate[fataf$Rate == "1"] <- "Yes"
fataf$Rate[fataf$Rate == "0"] <- "No"

# Function to change colour of the cell in a table
changeCellColor <- function(row, col){
  c(
    "function(row, data, num, index){",
    sprintf("  if(index == %d){", row-1),
    sprintf("    $('td:eq(' + %d + ')', row)", col),
    "    .css({'background-color': 'yellow'});",
    "  }",
    "}"  
  )
}

# Finding means of the continious variables
beer_mean <- mean(fata[["beertax"]])
miles_mean <- mean(fata[["vmiles"]])
unrate_mean <- mean(fata[["unrate"]])
income_mean <- mean(fata[["perinc"]])

# Function to find mode of a categorical variable
getmode <- function(p) {
  data1 <- unique(p)
  data1[which.max(tabulate(match(p, data1)))]
}

# Finding mode of jaild variable
if(getmode(fata$jaild) == "yes"){
  jail_mode <- 1
}
if(getmode(fata$jaild) == "no"){
  jail_mode <- 0
}


# UI of the app
ui <- navbarPage("Classification Shiny app",
                 
                 ##########################################################################################################################################################################################################
                 ######### Data Exploration panel UI   ###########################################
                 ##########################################################################################################################################################################################################
                 tabPanel("Data Exploration",
                          sidebarLayout(
                            sidebarPanel(
                              
                              # Definitions of every variable of our data set
                              helpText("In this section we explore our dataset related to traffic fatalities in the US. It contains these variables:"),
                              helpText("Beer Tax - tax on a case of beer (in $s),"),
                              helpText("Jail -  Is there a mandatory jail sentence for drink driving? (yes, no),"),
                              helpText("Avg. Miles - average miles driven per driver,"),
                              helpText("Unemployement Rate - unemployment rate of the US state,"),
                              helpText("Income - per capita personal income (in $s),"),
                              helpText("Fatality Rate - Is the states fatality rate above the US average? (0=No, 1=Yes)"),
                              
                              # Input choice for plots with the Rate variable
                              selectInput("raterel",label = "Choose any variable and visualise its relationship with the Fatality Rate variable",
                                          choices = c("Beer Tax (in $s)", "Jail",
                                                      "Avg. Miles (per driver)", "Unemployment Rate", "Income (in $s)"),
                                          selected = "Beer Tax"),
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          ),
                          
                          # For summary statistics of every variable
                          sidebarLayout(
                            sidebarPanel(
                              # Input choice for every variable
                              selectInput("var",label = "Choose a variable to see summary statistics of it",
                                          choices = c("Beer Tax", "Jail",
                                                      "Avg. Miles", "Unemployment Rate", "Income", "Fatality Rate"),
                                          selected = NULL,
                                          multiple = FALSE),
                            ),
                            verbatimTextOutput("summary")
                          ),
                          
                          # For summary statistics of every variable by Rate categroy
                          sidebarLayout(
                            sidebarPanel(
                              # Input choice for every variable by Rate category
                              selectizeInput('mvar', 'Please select a variable from a subgroup of states (containing fatality rate above/below US average) and see summary statistics of it', choices = list(
                                Fatality_rate_above_US_avg = c(`Beer Tax` = 'BT', `Jail` = 'J', `Avg. Miles` = 'AM', `Unemployement Rate` = 'UR', `Income` = 'I'),
                                Fatality_rate_below_US_avg = c(`Beer Tax` = 'BTT', `Jail` = 'JJ',`Avg. Miles` = 'AMM', `Unemployement Rate` = 'URR', `Income` = 'II' )
                              ), multiple = FALSE, selected = NULL),
                            ),
                            verbatimTextOutput("summary2")
                          ),
                 ),
                 ##########################################################################################################################################################################################################
                 ######### Classification Panel UI  ###########################################
                 ##########################################################################################################################################################################################################            
                 tabPanel("Classification tools",
                          sidebarLayout(
                            sidebarPanel(
                              
                              # Slider for proportion of data used for training data
                              sliderInput("slider1", label = "Use a slider input to select the proportion of data used for the training data 
                                    (Please note that data set rows for training data each time are chosen randomly).",
                                          min = 0.4, max = 0.8, value = 0.5, step = 0.1),
                              
                              # Radio buttons for unpruned/pruned tree
                              radioButtons("view", "Classification tree for the training data is displayed on the right. You can also choose to",
                                           c("view unpruned tree" = "unpruned",
                                             "view pruned tree" = "pruned")),
                              em("Note that if unpruned tree does 
                                    not differ from pruned, then it means that unprunned tree already has the best amount of splits 
                                    suggested by cp value."),
                              br(),
                              br(),
                              strong("Would you like to see both the correct-(CCR) and miss-(MCR) classification rates 
                        for the pruned classification tree and LDA?"),
                              
                              # An option to see classification rates presented in a table
                              checkboxInput("checkbox", "Yes", value = TRUE),
                              helpText("Highlighted cell indicates the \"best\" classification method, 
                                i.e. the one with the lowest missclassification rate MCR."),
                              
                            ),
                            mainPanel(
                              plotOutput("class"),
                              DTOutput("table")
                            )
                          ),
                          
                          #  Input options to allow the user to input their desired observed values (continious variables) of unseen state
                          fluidPage(
                            # Input options for numerical variables
                            fluidRow(column(width = 12, helpText(strong("Define a set of observed variable values to make
                      a prediction of the Rate status (i.e. above or below the US average) of an average unseen state using the \"best\" classification method:
                                                                  (you can enter the values manually or use the arrows at the input)"))),
                                     column(width = 3, numericInput("beer", 
                                                                    "Beer Tax (in $s)", 
                                                                    value = beer_mean, min = 0)),
                                     column(width = 3, numericInput("miles", 
                                                                    "Avg. Miles (per driver)", 
                                                                    value = miles_mean, min = 0)),
                                     column(width = 3, numericInput("work", 
                                                                    "Unemployment Rate", 
                                                                    value = unrate_mean, min = 0)),
                                     column(width = 3, numericInput("income", 
                                                                    "Income (in $s)", 
                                                                    value = income_mean, min = 0))
                            ),
                            
                            # Input option for categorical variable
                            fluidRow(column(width = 12, radioButtons("jail", "Is there a mandatory jail sentence for drink driving?",
                                                                     choices = list("Yes" = 1, "No" = 0)
                                                                     ,selected = jail_mode)),
                                     column(width = 12, htmlOutput("prediction"))
                            )
                          ) 
                 )
)

# Server logic ----
server <- function(input, output, session) {
  
  ##########################################################################################################################################################################################################
  ######### Data Exploration panel Server  ###########################################
  ##########################################################################################################################################################################################################
  
  # For visualising variables' relationship with the Rate variable
  output$plot <- renderPlot({
    
    # Plot based on the input choice by the user
    z <- switch(input$raterel, 
                "Beer Tax (in $s)" = fata$beertax,
                "Jail" = fata$jaild,
                "Avg. Miles (per driver)" = fata$vmiles,
                "Unemployment Rate" = fata$unrate,
                "Income (in $s)" = fata$perinc)
    
    # Continious variables plots
    if(input$raterel == "Jail"){
      ggplot(fataf, aes(Rate, ..count..)) + geom_bar(aes(fill = z), position = "dodge") +
        labs(fill='Is there a mandatory \njail sentence \nfor drink driving?') + ylab("Number of US states") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + xlab("Is the states fatality rate above the US average?")
    }
    # Categorical variable plot
    else{ 
      ggplot(data = fata, aes(fataf$Rate, z)) + labs(x = "Is the states fatality rate above the US average?", y = input$raterel) +
        geom_boxplot() + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    }
  })
  
  # For printing summary statistics of any variable
  output$summary <- renderPrint({
    
    # Print based on the input
    x <- switch(input$var, 
                "Beer Tax" = fata$beertax,
                "Jail" = fata$jaild,
                "Avg. Miles" = fata$vmiles,
                "Unemployment Rate" = fata$unrate,
                "Income" = fata$perinc,
                "Fatality Rate" = fata$Rate)
    
    print(summary(x), str(x))
  })
  
  # For printing summary statistics of any variable by Rate category
  output$summary2 <- renderPrint({
    
    # Print based on the input
    y <- switch(input$mvar, 
                'BT' = fatay$beertax,
                'J' = fatay$jaild,
                'AM' = fatay$vmiles,
                'UR' = fatay$unrate,
                'I' = fatay$perinc,
                'BTT' = fatan$beertax,
                'JJ' = fatan$jaild,
                'AMM' = fatan$vmiles,
                'URR' = fatan$unrate,
                'II' = fatan$perinc)
    
    print(summary(y), str(y))
  })
  
  ##########################################################################################################################################################################################################
  ######### Classification Panel Server  ###########################################
  ##########################################################################################################################################################################################################
  
  # Reactive expression to fit classification tree using rpart
  classtree <- reactive({
    
    # Saving slider input
    prop <-input$slider1
    # Randomly choose indices for training data
    ind.train <- sample(1:nrow(fata), round(nrow(fata)*prop))
    # Store training data
    fata.train <- fata[ind.train,]
    # Save validation data as whole data minus training data
    fata.valid <- fata[-c(ind.train),]
    # Fitting classification tree
    tree_class <- rpart(Rate~., data=fata.train, method = "class")
    
    # Returning both training & validation data sets as well as result from rpart function
    return(list(tree_class, fata.train, fata.valid))
  })
  
  # For plotting pruned classification tree
  prunedtree <- reactive({
    # I want to find cp value that corresponds with the smallest xerror
    mydata <- printcp(classtree()[[1]])
    for (i in 1:nrow(mydata)) {
      if (mydata[i,4] == min(mydata[,4]))
        indic = i
    }
    
    # I save my cp value which corresponds to smallest xerror and perform
    # rpart function with it
    cpvalue = mydata[indic, 1]
    tree_class <- rpart(Rate~., data=classtree()[[2]], method = "class", cp=cpvalue)
    
    # Plotting pruned classification tree
    return(tree_class)
  })
  
  # Main output for plotting classification trees
  output$class <- renderPlot({
    
    # Plot unpruned tree if chosen so by the user 
    if(input$view == "unpruned"){
      rpart.plot(classtree()[[1]], roundint = FALSE)
    }
    
    # Plot pruned tree if chosen so by the user 
    else if(input$view == "pruned"){
      rpart.plot(prunedtree())
    }
  })
  
  # For returning various classification calculations based on user's input
  best_classi_rates <- reactive({
    
    # Using our pruned tree to predict classes of our validation data set from classtree() reactive function
    tree_pred_class <- predict(prunedtree(), newdata = classtree()[[3]], type= "class")
    
    # Find rates of correct classification for trees
    class.tree <- (sum(tree_pred_class == classtree()[[3]]$Rate)/length(classtree()[[3]]$Rate))
    
    # Perform LDA classification method
    fata.lda <- lda(formula = Rate~., data = classtree()[[2]])
    
    # Similarly as before, predicting the classes of the validation dataset using LDA method
    lda_pred_class <- predict(fata.lda, classtree()[[3]])
    
    ## Find rates of correct classification for LDA
    class.lda <- sum(lda_pred_class$class == classtree()[[3]]$Rate)/length(classtree()[[3]]$Rate)
    
    # Use an indicator which saves which method had bigger correct classification rates
    # 0 for LDA being bigger
    # 1 for classification trees being bigger
    if(class.lda > class.tree)
      indicator <- 0
    else
      indicator <- 1
    
    # return all quantities of interest
    return(list(class.lda, class.tree, indicator, fata.lda))
  })
  
  # For creating miss and correct classification table
  output$table <- renderDT({
    
    if(input$checkbox == TRUE){
      
      # If LDA is the beter method
      if(best_classi_rates()[[3]] == 0){
        # Save it as data frame and print it with highlighted cell
        data.miss <- data.frame(Method = c("LDA", "TREE"),
                                CCR = c(best_classi_rates()[[1]], best_classi_rates()[[2]]),
                                MCR = 1-c(best_classi_rates()[[1]], best_classi_rates()[[2]]))
        datatable(head(data.miss), options = list(dom = 't', rowCallback = JS(changeCellColor(1, 1)))) 
        
      }
      # If Classifictaion Trees is the better method
      else{
        # Save it as data frame and print it with highlighted cell
        data.miss <- data.frame(Method = c("LDA", "TREE"),
                                CCR = c(best_classi_rates()[[1]], best_classi_rates()[[2]]),
                                MCR = 1-c(best_classi_rates()[[1]], best_classi_rates()[[2]]))
        datatable(head(data.miss), options = list(dom = 't', rowCallback = JS(changeCellColor(2, 1)))) 
      }
    }
    
  })
  
  # For uprediction of unseen new state based on user defined values
  defined_values <- reactive({
    # All variables for prediction
    a <- input$beer
    b <- input$miles
    c <- input$work
    d <- input$income
    e <- input$jail
    
    # Convert jaild variable from numeric to yes/no
    if(e == 0)
      e <- "no"
    else
      e <- "yes"
    
    # Put all user defined varibales into a dataframe containing one row (for a single prediction)
    df <- data.frame(a,e,b,c,d)
    # Change column names so that they would match original data set
    names(df) <- c('beertax', 'jaild', 'vmiles', 'unrate', 'perinc')
    
    # Return user defined dataset for unknown new state
    return(df)
  })
  
  # For printing extrapolation results
  output$prediction <- renderText({
    
    # If LDA was the best method
    if(best_classi_rates()[[3]] == 0){
      
      # Predict Rate variable for new unknow state defined by the user using LDA method
      new_lda_pred_class <- predict(best_classi_rates()[[4]], defined_values())
      
      # Let the user know that fatality rate was Below the US average if prediction result for Rate was 0
      if(as.numeric(as.character(new_lda_pred_class$class)) == 0)
        paste("<font color=\"#FF0000\"> Please note - extrapolation is being done: </font>", "LDA classification method was used for extrapolation. Based on the observed values defined above, the <b>predicted </b> new unseen state's 
              fatality rate is <b>below </b> the US average.")
      # Let the user know that fatality rate was Above the US average if prediction result for Rate was 1
      else
        paste("<font color=\"#FF0000\"> Please note - extrapolation is being done: </font>", "LDA classification method was used for extrapolation. Based on the observed values defined above, the <b>predicted </b> new unseen state's 
              fatality rate is <b>above </b> the US average.")
    }
    # If Classification Trees was the best method
    else{
      
      # Predict Rate variable for new unknow state defined by the user using Classification Trees
      new_tree_pred_class <- predict(classtree()[[1]], newdata = defined_values(), type= "class")
      
      # Let the user know that fatality rate was Below the US average if prediction result for Rate was 0
      if(as.numeric(as.character(new_tree_pred_class)) == 0)
        paste("<font color=\"#FF0000\"> Please note - extrapolation is being done: </font>", "Classification Trees method was used for extrapolation. Based on the observed values defined above, the <b>predicted </b> new unseen state's 
              fatality rate is <b>below </b> the US average.")
      # Let the user know that fatality rate was Above the US average if prediction result for Rate was 1
      else
        paste("<font color=\"#FF0000\"> Please note - extrapolation is being done: </font>", "Classification Trees method was used for extrapolation. Based on the observed values defined above, the <b>predicted </b> new unseen state's 
              fatality rate is <b>above </b> the US average.")
    }
  })
}

# Run the app ----

shinyApp(ui, server)
