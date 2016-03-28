##Packages

if (!require(plyr, quietly=TRUE)) {
  install.packages("plyr")
  library(plyr)
}

if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}


##########
#AutoSave
##########

.AutoSave <- function(){
  save(foodDB, foodLog, macroGoals, currentWeight, dailySummary, file = "MacroCount.RData")
}


shinyServer(function(input, output) {
  ########################################
  #HELPER FUNCTIONS
  ########################################
  
  prediction_db <- dailySummary
  
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
  
  #######################
  #Summarize Every Change
  #######################
  
  .Summarize <- function(){
    
    todays_indices <- which(foodLog$Date==as.character(Sys.Date()))
    foodLog <- foodLog[todays_indices,]
    
    dailySummary[1,1] <<- sum(as.numeric(foodLog$Proteins))
    dailySummary[2,1] <<- sum(as.numeric(foodLog$Carbs))
    dailySummary[3,1] <<- sum(as.numeric(foodLog$Fats))
    dailySummary[4,1] <<- sum(as.numeric(foodLog$Calories))
    
    dailySummary[1,2] <<- macroGoals[1,1]
    dailySummary[2,2] <<- macroGoals[2,1]
    dailySummary[3,2] <<- macroGoals[3,1]
    dailySummary[4,2] <<- macroGoals[4,1]
    
    
  }
  .Summarize()
  
  
  #The 'Add Food' button
  addData <- observeEvent(input$add_food_button, {

    #ACTION 1 - Update the Food Log
    todays_date <- as.character(Sys.Date())
    
    package_portion <- input$package_portion
    package_protein <- input$protein
    package_carbs <- input$carbs
    package_fats <- input$fats
    package_calories <- round(1.2*(package_protein*4 + package_carbs*4 + package_fats*9), digits =2)
    portion_size <- input$actual_portion
    
    portion_multiplier <- portion_size / package_portion 
    
    portion_protein <- round(portion_multiplier*package_protein, digits = 2)
    portion_carbs <- round(portion_multiplier*package_carbs,digits = 2)
    portion_fats <- round(portion_multiplier*package_fats,digits = 2)
    
    total_cals <- round(1.1*(4*portion_protein + 4*portion_carbs + 9*portion_fats),digits =2)
    
    entry_summary <- c(todays_date, portion_protein, portion_carbs, portion_fats, total_cals,currentWeight)
    
    foodLog[nrow(foodLog)+1,] <<- entry_summary
    .AutoSave()
    
    #Action 2 - Update the Daily Summary
    .Summarize()
    output$dailysummary <- renderTable(dailySummary)
    output$dailysummary_2 <- renderTable(dailySummary)
    
  })
  
  #The 'Add to Database' button
  addData <- observeEvent(input$add_to_database_button, {
    package_name <- input$name_db
    package_portion <- input$package_portion_db
    package_protein <- input$protein_db
    package_carbs <- input$carbs_db
    package_fats <- input$fats_db
    package_calories <- round(1.2*(package_protein*4 + package_carbs*4 + package_fats*9), digits =2)
    
    entry_summary <- c(package_name, package_portion, package_protein, package_carbs, package_fats,package_calories)
    
    foodDB[nrow(foodDB)+1,] <<- entry_summary
    .AutoSave()
    
    output$foodDB<- renderTable(foodDB)
    output$foodDB_2<- renderTable(foodDB)
    
    
  })
  
  #The "Predict" button
  addData <- observeEvent(input$predict_button, {

    
    portion_size <- input$portion_size
    food_id <- input$row_id
    
    db_portion <-as.numeric(foodDB[food_id,2])
    
    pm <- portion_size / db_portion
    
    db_protein <-round(as.numeric(foodDB[food_id,3]) * pm, digits = 2)
    db_carbs <-round(as.numeric(foodDB[food_id,4]) * pm, digits = 2)
    db_fats <-round(as.numeric(foodDB[food_id,5]) * pm,digits = 2)
    db_cals <- round(1.1*(db_protein*4 + db_carbs*4 +db_fats*9) ,digits =2)
    
    
    
    prediction_db[1,1]<- as.numeric(dailySummary[1,1]+db_protein)
    prediction_db[2,1]<- as.numeric(dailySummary[2,1]+db_carbs)
    prediction_db[3,1]<- as.numeric(dailySummary[3,1]+db_fats)
    prediction_db[4,1]<- as.numeric(dailySummary[4,1]+db_cals)
    
    output$prediction <- renderTable(prediction_db)
    

  })
  
  observeEvent(input$submit_button, {
    portion_size <- input$portion_size
    food_id <- input$row_id
    
    db_portion <-as.numeric(foodDB[food_id,2])
    
    pm <- portion_size / db_portion
    
    db_protein <-round(as.numeric(foodDB[food_id,3]) * pm, digits = 2)
    db_carbs <-round(as.numeric(foodDB[food_id,4]) * pm, digits = 2)
    db_fats <-round(as.numeric(foodDB[food_id,5]) * pm,digits = 2)
    db_cals <- round(1.1*(db_protein*4 + db_carbs*4 +db_fats*9) ,digits =2)
    db_cals <- round(1.1*(db_protein*4 + db_carbs*4 +db_fats*9) ,digits =2)
    todays_date <- as.character(Sys.Date())
    entry_summary <- c(todays_date,db_protein,db_carbs,db_fats,db_cals,currentWeight)
    
    foodLog[nrow(foodLog)+1,] <<- entry_summary
    .AutoSave()

    
    output$foodLog <- renderTable(foodLog, include.rownames=F)
    
  })
  
  observeEvent(input$reset_button_2, {
    .Summarize()
    prediction_db[1,1] <<- dailySummary[1,1]
    prediction_db[2,1] <<- dailySummary[2,1]
    prediction_db[3,1] <<- dailySummary[3,1]
    prediction_db[4,1] <<- dailySummary[4,1]
    
    
    output$prediction <- renderTable(prediction_db)
  })
  
  observeEvent(input$reset_button, {
    .Summarize()
    output$dailysummary <- renderTable(dailySummary)

  
  })
  
  observeEvent(input$add_to_macros_button, {
    
    protein_goal <- input$protein_goal
    carb_goal <- input$carb_goal
    fat_goal <- input$fat_goal
    currentWeight <<- input$current_weight
    
    macro_sum <- protein_goal + carb_goal + fat_goal
    
    calories_goal <- round((protein_goal*4+carb_goal*4+fat_goal*9), digits = 2)
    
    protein_percentage <- round((protein_goal / macro_sum)*100, digits =0)
    carb_percentage <- round((carb_goal / macro_sum)*100, digits =0)
    fat_percentage <- round((fat_goal / macro_sum)*100, digits =0)
    
    macroGoals$Macros <<- c(protein_goal, carb_goal, fat_goal,calories_goal)
    macroGoals$Percentages <<-c(protein_percentage,carb_percentage,fat_percentage, NA)
    
    .AutoSave()

    output$macrogoals <- renderTable(macroGoals)
    
    
  })
  
  output$dailysummary <- renderTable(dailySummary)

  
  output$foodLog <- renderTable(foodLog, include.rownames=F)
  output$foodDB<- renderTable(foodDB)
  output$foodDB_2<- renderTable(foodDB)
  output$prediction <- renderTable(prediction_db)
  output$macrogoals <- renderTable(macroGoals)
  
  
  
  
  
  
  #########################
  #PLOTS
  #################
  
  output$macro_graph <- renderPlot({
    #Get Current Month, Year
    date <- Sys.Date()
    date_formatted <- format(date, "%b, %y")
    
    #Subset the Log
    log_subset <- apply(foodLog[1],2, function(x) format(as.Date(x), "%b, %y"))
    filtered_indixes <- which(log_subset %in% date_formatted)
    monthly_log <- (foodLog[filtered_indixes,])
    
    #Get the Weights early on
    start_weight <- monthly_log[1,6]
    end_weight <- monthly_log[nrow(monthly_log),6]
    
    #Group by Days
    monthly_log[1] <-apply(monthly_log[1],2, function(x) (as.numeric(as.Date(x))))
    
    monthly_log[2] <- apply(monthly_log[2],2, function(x) (as.numeric((x))))
    monthly_log[3] <- apply(monthly_log[3],2, function(x) (as.numeric((x))))
    monthly_log[4] <- apply(monthly_log[4],2, function(x) (as.numeric((x))))
    monthly_log[5] <- apply(monthly_log[5],2, function(x) (as.numeric((x))))
    monthly_log[6] <- apply(monthly_log[6],2, function(x) (as.numeric((x))))
    
    monthly_log <-ddply(monthly_log,"Date",numcolwise(sum))
    
    
    #Get the Goals
    goal_protein <- macroGoals[1,1] 
    goal_carbs <- macroGoals[2,1] 
    goal_fats <- macroGoals[3,1] 
    goal_cals <- macroGoals[4,1] 
    
    #Get Current averages
    avg_protein <- round(mean(as.numeric(monthly_log[,2])),digits =2)
    avg_carbs <- round(mean(as.numeric(monthly_log[,3])),digits =2)
    avg_fats <-round(mean(as.numeric(monthly_log[,4])),digits =2)
    avg_cals <-round(mean(as.numeric(monthly_log[,5])),digits =2)
    
    gram_df <- data.frame(matrix(nrow = 3))
    gram_df$macros <- c("protein", "carbs","fats")
    gram_df[1]<-NULL
    gram_df$grams <- c(avg_protein, avg_carbs,avg_fats)
    gram_df$hline <- c(goal_protein,goal_carbs,goal_fats)
    
    bp <- ggplot(gram_df, aes(x=macros, y=grams)) +
      geom_bar(position=position_dodge(), stat="identity")
    bp <- bp + geom_errorbar(width=0.7, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")
    bp <- bp+ geom_text(aes(x=macros, y=grams, label=grams,vjust=-0.5))
    
    print(bp)
    
  })
  
  output$calorie_graph <- renderPlot({
    #Get Current Month, Year
    date <- Sys.Date()
    date_formatted <- format(date, "%b, %y")
    
    #Subset the Log
    log_subset <- apply(foodLog[1],2, function(x) format(as.Date(x), "%b, %y"))
    filtered_indixes <- which(log_subset %in% date_formatted)
    monthly_log <- (foodLog[filtered_indixes,])
    
    #Get the Weights early on
    start_weight <- monthly_log[1,6]
    end_weight <- monthly_log[nrow(monthly_log),6]
    
    #Group by Days
    monthly_log[1] <-apply(monthly_log[1],2, function(x) (as.numeric(as.Date(x))))
    
    monthly_log[2] <- apply(monthly_log[2],2, function(x) (as.numeric((x))))
    monthly_log[3] <- apply(monthly_log[3],2, function(x) (as.numeric((x))))
    monthly_log[4] <- apply(monthly_log[4],2, function(x) (as.numeric((x))))
    monthly_log[5] <- apply(monthly_log[5],2, function(x) (as.numeric((x))))
    monthly_log[6] <- apply(monthly_log[6],2, function(x) (as.numeric((x))))
    
    monthly_log <-ddply(monthly_log,"Date",numcolwise(sum))
    
    
    #Get the Goals
    goal_protein <- macroGoals[1,1] 
    goal_carbs <- macroGoals[2,1] 
    goal_fats <- macroGoals[3,1] 
    goal_cals <- macroGoals[4,1] 
    
    #Get Current averages
    avg_protein <- round(mean(as.numeric(monthly_log[,2])),digits =2)
    avg_carbs <- round(mean(as.numeric(monthly_log[,3])),digits =2)
    avg_fats <-round(mean(as.numeric(monthly_log[,4])),digits =2)
    avg_cals <-round(mean(as.numeric(monthly_log[,5])),digits =2)
    
    cal_df <-data.frame(matrix(nrow = 1))
    cal_df$calories <- c("average")
    cal_df[1]<-NULL
    cal_df$kcal <- c(avg_cals)
    cal_df$hline <- c(goal_cals)
    
    bp2 <- ggplot(cal_df, aes(x=calories, y=kcal)) +
      geom_bar(position=position_dodge(), stat="identity", width =.5)
    bp2 <- bp2 + geom_errorbar(width=0.3, aes(y=hline, ymax=hline, ymin=hline), colour="#AA0000")
    bp2 <- bp2 + geom_text(aes(x=calories, y=kcal, label=kcal,vjust=-0.5))
    
    print(bp2)
    
  })

  
})



