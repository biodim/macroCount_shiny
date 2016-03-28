#######################
#Initialization Script
#######################
.initializeModule <- function(){
  if (file.exists("MacroCount.RData")){
    load("MacroCount.RData", envir = .GlobalEnv)
  } else {
    
    ##
    #foodLog
    ##
    
    foodLog <<- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
    names(foodLog) <<- c("Date", "Proteins", "Carbs", "Fats", "Calories", "Current Weight")
    
    ##
    #macroGoals
    ##
    
    macroGoals <<- data.frame(matrix(ncol = 2, nrow = 4), stringsAsFactors = FALSE)
    names(macroGoals) <<- c("Macros", "Percentages")
    rownames(macroGoals)[1] <- "Protein (g)"
    rownames(macroGoals)[2] <- "Carbs (g)"
    rownames(macroGoals)[3] <- "Fats (g)"
    rownames(macroGoals)[4] <- "Calories"
    
    macroGoals <<- macroGoals
    
    
    ##
    #foodDB
    ##
    foodDB <<- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
    names(foodDB) <<- c("Name", "Quantity", "Proteins", "Carbs", "Fats", "Calories")
    
    ##
    #dailySummary
    ##
    dailySummary <<- data.frame(matrix(ncol = 2, nrow = 4), stringsAsFactors = FALSE)
    names(dailySummary) <- c("Current", "Goal")
    rownames(dailySummary)[1] <- "Protein (g)"
    rownames(dailySummary)[2] <- "Carbs (g)"
    rownames(dailySummary)[3] <- "Fats (g)"
    rownames(dailySummary)[4] <- "Calories"
    
    
    dailySummary <<- dailySummary
    
    
    ##
    #Weight
    ##
    
    currentWeight <<- NA
    
  }
}

.initializeModule()


shinyUI(
  navbarPage("macroCount",
    #1
    tabPanel("Add Food",
             sidebarPanel(
               numericInput("package_portion", label = h4("Serving Portion"), value = 1),
               numericInput("protein", label = h4("Protein (g)"), value = 1),
               numericInput("fats", label = h4("Fats (g)"), value = 1),
               numericInput("carbs", label = h4("Carbs (g)"), value = 1),
               numericInput("actual_portion", label = h4("Your Portion"), value = 1),
               actionButton("add_food_button", "Submit")
               
             ),
             mainPanel(
               h2("Daily Summary:"),
               tableOutput("dailysummary"),
               actionButton("reset_button", "Refresh")
               )
             
    ),
    tabPanel("Add From Database",
             sidebarPanel(
               numericInput("row_id", label = h4("What is the Food ID?"), value = 1),
               numericInput("portion_size", label = h4("What is Your Portion Size?"), value = 1),
               actionButton("predict_button", "Predict"),
               actionButton("submit_button", "Submit")
              
               
             ),
             mainPanel(
               h3("Prediction:"),
               tableOutput("prediction"),
               actionButton("reset_button_2", "Refresh"),
               h3("Database:"),

             
             mainPanel(
               style = "overflow-y:scroll; max-height: 300px; position:relative;", 
               tableOutput('foodDB_2'))
             )
               
               
             
    ),
    
    
    
    
    tabPanel("Database",
             sidebarPanel(
               textInput("name_db", label = h4("Name")),
               numericInput("package_portion_db", label = h4("Serving Portion"), value = 1),
               numericInput("protein_db", label = h4("Protein (g)"), value = 1),
               numericInput("fats_db", label = h4("Fats (g)"), value = 1),
               numericInput("carbs_db", label = h4("Carbs (g)"), value = 1),
               actionButton("add_to_database_button", "Add to Database")
             ),
             mainPanel(
               style = "overflow-y:scroll; max-height: 500px; position:relative;", tableOutput('foodDB')
               )
             
             ),
    tabPanel("Food Log",
             style = "overflow-y:scroll; max-height: 400px; position:relative;",tableOutput("foodLog")
             ),
    tabPanel("Macros",
             plotOutput("macro_graph")
    ),
    tabPanel("Calories",
             plotOutput("calorie_graph")
             
    ),
    tabPanel("Set Goals",
             sidebarPanel(
               sliderInput("protein_goal", label = h4("Protein Macro"), value = macroGoals[1,1], min = 0, max = 500, step = 0.1),
               sliderInput("carb_goal", label = h4("Carb Macro"), value = macroGoals[2,1], min = 0, max = 500, step = 0.1),
               sliderInput("fat_goal", label = h4("Fat Macro"), value = macroGoals[3,1], min = 0, max = 300, step = 0.1),
               numericInput("current_weight", label = h4("Current Weight"), value = currentWeight),
               actionButton("add_to_macros_button", "Add Goals")
             ),
             mainPanel(
               tableOutput("macrogoals"))
             
    )
    
             
                            
   
))

