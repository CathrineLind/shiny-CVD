##### Server script #####

datetime <- Sys.time()

# Load data --------------------------------------------------------------------
hungarian <- readRDS("HungarianData1.rds")
switzerland <- readRDS("SwitzerlandData1.rds")
longBeach <- readRDS("LongBeachData1.rds")
combData <- readRDS("SwitzHungLongBeachData1.rds")
largeCombData <- readRDS("mainData1.rds")


# Server start -----------------------------------------------------------------
server <- function(input, output, session){
  
  # Chosen data ----------------------------------------------------------------
  datasetInput <- reactive({
    switch(input$dataFile,
           "Hungarian" = as.data.frame(hungarian),
           "Switzerland" = as.data.frame(switzerland),
           "Long Beach VA" = as.data.frame(longBeach),
           "Combined data (small)" = as.data.frame(combData),
           "Combined data (large)" = as.data.frame(largeCombData))
  })

  # Reactive value for selected dataset in plot panel
  datasetInputPlot <- reactive({
      switch(input$dataFilePlot,
             "Hungarian" = as.data.frame(hungarian),
             "Switzerland" = as.data.frame(switzerland),
             "Long Beach VA" = as.data.frame(longBeach),
             "Combined data (small)" = as.data.frame(combData),
             "Combined data (large)" = as.data.frame(largeCombData))
  })
  
  # Reactive value for selected dataset in the model panel
  datasetInputModel <- reactive({
      switch(input$dataFileModel,
             "Hungarian"=as.data.frame(hungarian), 
             "Switzerland"=as.data.frame(switzerland), 
             "Long Beach VA"=as.data.frame(longBeach),
             "Combined data (small)"=as.data.frame(combData),
             "Combined data (large)"=as.data.frame(largeCombData))
  })

  # Reactive value for selected dataset in the model panel
  datasetInputModelKNN <- reactive({
      switch(input$dataFileModelKNN,
             "Hungarian"=as.data.frame(hungarian), 
             # "Switzerland"=as.data.frame(switzerland),
             "Long Beach VA"=as.data.frame(longBeach),
             "Combined data (small)"=as.data.frame(combData))
             # "Combined data (large)"=as.data.frame(largeCombData))
  })
  
  # Render text to Data panel with unique ID values
  output$uniqueID <- renderText({
    dataset <- datasetInput()
    return(length(unique(dataset$id)))
  })
  
  
  # Summary table for Data panel
  output$summary <- renderDataTable({ #renderPrint
    dataset <- datasetInput()
    if(!is.data.frame(dataset)) stop("df needs to be a dataframe")
    df <- as.data.frame(dataset)
    df <- df[sapply(df, is.logical) | sapply(df, is.numeric)]
    if ((ncol(df) < 1) | (nrow(df) < 2)) stop("Insuitable data frame (does it contain numerical data?)")
    # if (!is.numeric(digits) | length(digits) != 8) stop("digits vector is not numeric or has wrong length (!= 8)")
    
    t <- cbind(apply(df,2,function(x) as.integer(sum(!is.na(x)))),
               apply(df,2,mean, na.rm = TRUE),
               apply(df,2,stats::sd, na.rm = TRUE),
               t(apply(df, 2, function(x) stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))))
    colnames(t) <- c("N", "Mean", "Std. dev.", "Min.", "25 %", "Median", "75 %", "Max.")
    t <- as.data.frame(t)
    t$N <- as.integer(t$N)
    t$Mean <- round(t$Mean, 2)
    t$`Std. dev.` <- round(t$`Std. dev.`, 2)
    
    datatable(t, options = list(pageLength = 20,
                                dom = 't',
                                autoWidth = TRUE,
                                # scrollX = TRUE,
                                width = "100%",
                                columnDefs = list(list(className = 'dt-center',
                                                       targets = "_all"))),
              # rownames = FALSE,
              caption = paste("Table: Summary of ", input$dataFile, " data.", sep = ""))
  })
  
  
  # Table of selected dataset ----
  output$table <- renderDT(
    datatable(datasetInput(), options = list(pageLength = 10,
                                             dom = 't',
                                             autoWidth = TRUE,
                                             # scrollX = TRUE,
                                             width = "100%",
                                             columnDefs = list(list(className = 'dt-center',
                                                                    targets = "_all"))),
              rownames = FALSE, 
              caption = paste("Table: Pre-processed data file (", input$dataFile, ").", sep = "")))
  
  # Amount of NA in data
  output$NAtable <- renderDT(
    datatable(datasetInput() %>% summarise_all(funs(sum(is.na(.)))), 
              options = list(pageLength = 10,
                             dom = 't',
                             ordering = F,
                             autoWidth = TRUE,
                             width = "100%",
                             columnDefs = list(list(className = 'dt-center',
                                                    targets = "_all"))),
              rownames = FALSE)
              # caption = paste("Table: Amount of  in ", input$dataFile, ".", sep = "")
    )
  
  
  # Reactive value for selected dataset when downloading ----
  tableSeparator <- reactive({
    switch(input$separator,
           "Tab ('\t')" = "\t", 
           "Comma (,)" = ",", 
           "Semi-colon (;)" = ";",
           "Colon (:)" = ":",
           "Space" = " ")
  })
  
  
  # Download table button ----
  output$downloadTable <- downloadHandler(
    filename = function(){
      paste0(input$dataFile, input$downloadType)},
    
    content = function(file){
      separator = tableSeparator()
      if(input$downloadType == ".csv") {
        if(is.null(separator)) write_tsv(datasetInput(), file) # NULL == '\t'
        else write.table(datasetInput(), file, row.names = FALSE, sep = separator)
        # write.csv(datasetInput(), file, row.names = FALSE, sep = separator)
      } else if(input$downloadType == ".rds") {
        saveRDS(datasetInput(), file)
      } else if(input$downloadType == ".txt") {
        if(is.null(separator)) write_tsv(datasetInput(), file) # NULL == '\t'
        else write.table(datasetInput(), file, quote = FALSE, sep = separator, row.names = FALSE)
      }
    }
  )
  
  
  # Observe possible names in dataframe to choose from - logistic regression
  observe({
    logRegChoices <- datasetInputModel() %>%
      dplyr::select(-id) 
    
    if(sd(na.omit(logRegChoices$chol)) == 0) logRegChoices = logRegChoices %>% dplyr::select(-chol)
    logRegChoices = logRegChoices %>%
      colnames()
    
    # For boxPlot only continuos vars
    if("ap_hi" %in% logRegChoices){
      logRegChoices = logRegChoices[!logRegChoices %in% c("datasetOrig", "cvdPresent", "gender")]}
    else{
      if("datasetOrig" %in% logRegChoices) logRegChoices = logRegChoices[!logRegChoices %in% c("cvdPresent", "gender", "fbsFactor", "cpFactor", "datasetOrig", "smoke")]
      else  logRegChoices = logRegChoices[!logRegChoices %in% c("cvdPresent", "gender", "fbsFactor", "cpFactor", "smoke")]}
    
    updateCheckboxGroupInput(session, "independentVars", choices = logRegChoices)
  })
  
 
  
  observe(({
    updateCheckboxGroupInput(session, "logScatter", choices = list("Log X", "Log Y"))
  }))
  
  
  observe(({
    updateCheckboxGroupInput(session, "logHist", choices = list("Left plot", "Right plot"))
  }))

  observe({
    df = datasetInputPlot()
    
    # Remove the factor varibale such as 'fbsFactor'
    if("smoke" %in% names(df)){
      df = df %>% 
        dplyr::select(-c(id, gender, fbsFactor, cpFactor, cvdPresent, smoke))}
    else{
      df = df %>% 
        dplyr::select(-c(id, gender, cvdPresent))}
    
    updateSelectInput(session, inputId = 'xcolHist', label = 'Left plot',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ycolHist', label = 'Right plot',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'xcolScatter', label = 'X variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ycolScatter', label = 'Y variable',
                      choices = names(df), selected = names(df)[2])
    updateSliderInput(session, inputId = 'range', label = 'Range',
                      min = round(min(df[, 1]), 2), max = round(max(df[, 1]), 2), step = 1)
    
    # For boxPlot only continuos vars
    if("ap_hi" %in% names(df)){
      df = df %>% 
        dplyr::select(c(age, chol, height, weight, bmi, ap_hi, ap_lo, ap_mean))}
    else{
      df = df %>% 
        dplyr::select(c(age, trestbps, chol))}
    
    updateSelectInput(session, inputId = 'xcolBox', label = 'Variable',
                      choices = names(df), selected = names(df)[1]) #age
    
    updateSelectInput(session, inputId = 'color_scatter', label = 'Select color',
                      choices = c("black", "red", "blue", "green", "yellow"), selected = "blue") 
  })
  

  

  observe({
    df = datasetInputPlot()
    
    # Select all the categorical values
    catChoices <- c("none", "gender", "cvdPresent")
    catChoices <- c(catChoices, ifelse("cp" %in% names(df), "cp (chest pain type)", ""), ifelse("fbs" %in% names(df), "fbs (fasting blood sugar)", ""), ifelse("alco" %in% names(df), "alco (alcohol intake)", ""))
    # catChoices <- c(catChoices, ifelse("cpFactor" %in% names(df), "cpFactor", ""), ifelse("fbsFactor" %in% names(df), "fbsFactor", ""), ifelse("alco" %in% names(df), "alco", ""))
    updateSelectInput(session, inputId = "facetH1", label = "Color by variable",
                      choices = catChoices, selected = catChoices[1])
    updateSelectInput(session, inputId = "facetH2", label = "Color by variable",
                      choices = catChoices, selected = catChoices[1])
  })
  
  getRangeMin <- reactive({
    df = datasetInput()
    if(any(input$logScatter == "Log X")) min1 = round(min(log(df[, input$xcolScatter]), na.rm = TRUE), 2)
    else min1 = round(min(df[, input$xcolScatter], na.rm = TRUE), 2)
  })
  getRangeMax <- reactive({
    df = datasetInput()
    if(any(input$logScatter == "Log X")) max1 = round(max(log(df[, input$xcolScatter]), na.rm = TRUE), 2)
    else max1 = round(max(df[, input$xcolScatter], na.rm = TRUE), 2)
  })
  
  
  # Update range based on variable chosen
  output$range <- renderUI({
    df = datasetInput()
    min1 = getRangeMin()
    max1 = getRangeMax()
    sliderInput("range", "Range (x-axis)",
                min = min1, 
                max = max1, 
                value = c(min1, 
                          max1)) 
  })

  
  histogramPlot1 <- reactive({
    data = datasetInputPlot()
    xColumn = data[, input$xcolHist]
    if(input$facetH1 == "fbs (fasting blood sugar)") facetColumn = data[, "fbsFactor"]
    else if(input$facetH1 == "cp (chest pain type)") facetColumn = data[, "cpFactor"]
    else if(input$facetH1 == "alco (alcohol intake)") facetColumn = data[, "alco"]
    else if(input$facetH1 == "cvdPresent") facetColumn = ifelse(data[, "cvdPresent"] == 0, "No", "Yes")
    else if(input$facetH1 == "none") facetColumn = rep("#5B779A", length(xColumn))
    else facetColumn = data[, input$facetH1] # any other facet variable  
    countMax = as.data.frame(table(xColumn)) %>% dplyr::select(Freq)
    
    # Convert to log-scale
    if(!is.null(input$logHist)){
      if((input$logHist == "Left plot") || sum("Left plot" %in% input$logHist)) xColumn <- log(xColumn)}
    if(sum(is.infinite(xColumn)) > 0) showNotification(paste0("Log-transformation of the variable ", input$xcolHist, " is not optimal due to generation of infinite values.", type = "warning"))

    
    bins <- if(any(is.infinite(c(min(xColumn, na.rm = TRUE), max(xColumn, na.rm = TRUE))))) c(0, 10) else seq(min(xColumn, na.rm = TRUE), max(xColumn, na.rm = TRUE), length.out = input$bins1 + 1)
    verticalLine <- ifelse(max(countMax) <= 1, FALSE, TRUE)
    if(length(unique(xColumn)) < 5) verticalLine = FALSE
    
    # plot
    plot <- ggplot(data, aes(x = xColumn, 
                             fill = facetColumn)) + # geom_bar() +
      geom_histogram(bins = length(bins),
                     color = "#112446",
                     alpha = 0.5) +  #ifelse(!is.null(input$facetH1), as.factor(input$facetH1), "#5B779A")
      labs(#title = paste("Barplot of", input$dataFilePlot, "dataset"),
           fill = ifelse(input$facetH1 != "none", str_to_title(input$facetH1), "")) +
      xlab(input$xcolHist) + 
      theme_minimal() +
      theme(axis.title = element_text(size = 16),
            plot.title = element_text(face = "bold",
                                      size = 20))+ 
      scale_color_manual(values = c("#5B779A", "#112446", "#2974CE", "#3F6654"))+
      scale_fill_manual(values = c("#5B779A", "#112446", "#2974CE", "#3F6654"))
    
    if(input$facetH1 == "none") plot <- plot + theme(legend.position = "none")
    if(verticalLine) plot <- plot + geom_vline(xintercept = mean(xColumn, na.rm = TRUE), linetype = "longdash", size = 1.5) 
    
    print(plot)
  })
  
  histogramPlot2 <- reactive({
    data = datasetInputPlot()
    yColumn = data[, input$ycolHist]
    if(input$facetH2 == "fbs (fasting blood sugar)") facetColumn = data[, "fbsFactor"]
    else if(input$facetH2 == "cp (chest pain type)") facetColumn = data[, "cpFactor"]
    else if(input$facetH2 == "alco (alcohol intake)") facetColumn = data[, "alco"]
    else if(input$facetH2 == "cvdPresent") facetColumn = ifelse(data[, "cvdPresent"] == 0, "No", "Yes")
    else if(input$facetH2 == "none") facetColumn = rep("#5B779A", length(yColumn))
    else facetColumn = data[, input$facetH2] # any other facet variable  
    countMax = as.data.frame(table(yColumn)) %>% dplyr::select(Freq)
    
    # Convert to log-scale
    if(!is.null(input$logHist)){
      if((input$logHist == "Right plot") || sum("Right plot" %in% input$logHist)) yColumn <- log(yColumn)}
    if(sum(is.infinite(yColumn)) > 0) showNotification(paste0("Log-transformation of the variable ", input$ycolHist, " is not optimal due to generation of infinite values.", type = "warning"))
                                                       
    bins <- if(any(is.infinite(c(min(yColumn, na.rm = TRUE), max(yColumn, na.rm = TRUE))))) c(0, 10) else seq(min(yColumn, na.rm = TRUE), max(yColumn, na.rm = TRUE), length.out = input$bins2 + 1)
    verticalLine <- if(max(countMax) <= 1) FALSE else TRUE
    if(length(unique(yColumn)) < 5) verticalLine = FALSE

    # plot
    plot <- ggplot(data, aes(x = yColumn, fill = facetColumn)) + # geom_bar() +
      geom_histogram(bins = length(bins),
                     color = "#112446",
                     alpha=0.5) + # fill = "#5B779A" 
      labs(#title = paste("Barplot of", input$dataFilePlot, "dataset"),
           fill = ifelse(input$facetH2 != "none", str_to_title(input$facetH2), "")) +
      xlab(input$ycolHist) + 
      theme_minimal() + 
      theme(axis.title = element_text(size = 16),
            plot.title = element_text(face = "bold",
                                      size = 20)) + 
      scale_color_manual(values = c("#5B779A", "#112446", "#2974CE", "#3F6654"))+
      scale_fill_manual(values = c("#5B779A", "#112446", "#2974CE", "#3F6654"))
    
    if(input$facetH2 == "none") plot <- plot + theme(legend.position = "none")
    if(verticalLine) plot <- plot + geom_vline(xintercept = mean(yColumn, na.rm = TRUE), linetype = "longdash", size = 1.5) 
    
    print(plot)
  })
  
  scatterPlot <- reactive({
    set.seed(1234)
    data = datasetInputPlot()
    xColumn = data[, input$xcolScatter]
    yColumn = data[, input$ycolScatter]
    
    # Convert to log-scale
    # Convert to log-scale
    if(!is.null(input$logScatter)){
      if((input$logScatter == "Log X") || sum("Log X" %in% input$logScatter)) xColumn <- log(xColumn)
      if((input$logScatter == "Log Y") || sum("Log Y" %in% input$logScatter)) yColumn <- log(yColumn)}
    
    scatterCol = ifelse(input$color_scatter == "red", "#C61D1F", ifelse(input$color_scatter == "blue", "#5B779A", ifelse(input$color_scatter == "green", "#3F6654", ifelse(input$color_scatter == "yellow", "#dec541", "black"))))
    
    # plot
    plot <- ggplot(data, aes(x = xColumn, y = yColumn)) +
      geom_jitter(width = 0.2,
                  color = scatterCol,
                  size = 3) +
      labs(#title = paste("Scatter plot of", input$dataFilePlot, "dataset"),
           color = str_to_title(input$xcolScatter)) +
      xlab(input$xcolScatter) + 
      ylab(input$ycolScatter) +
      xlim(input$range[1], input$range[2]) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 16),
            plot.title = element_text(face = "bold",
                                      size = 20)) 
    print(plot)
  })
  
  observe({
    input$addScatter
  })
  observe({
    input$xcolBox
  })
  
  
  # Box plot for ex. sex med 0 eller 1 viser ikke 0 da det forsvinder naar mansiger factor()
  boxPlot <- reactive({
    data = datasetInputPlot()
    xColumn = data[, input$xcolBox]
    yColumn = data[, "cvdPresent"]
    notPresent = sum(xColumn[which(yColumn == 0)])
    isPresent = sum(xColumn[which(yColumn == 1)])
    yColumn = ifelse(yColumn == 0, paste0("No (N=",  notPresent, ")"), paste0("Yes (N=",  isPresent, ")"))
    
    plot <- ggplot(data, aes(x = yColumn, y = xColumn, group = yColumn)) +
      geom_boxplot(color = "#3F536B", fill = "#5B779A", alpha = 0.75, lwd = 0.8, 
                   outlier.shape = "+", outlier.color = "#3F6654", outlier.size = 5, outlier.alpha = 1) +
      theme_ipsum() +
      theme_minimal() +
      theme(axis.title = element_text(size = 16),
            plot.title = element_text(face = "bold",
                                      size = 20),
            text = element_text(size = 15)) +
      ylab(input$xcolBox) +
      xlab("Presence of cardiovascular disease")
    
    if(input$addScatter) plot = plot + geom_jitter(color = "#112446", size=0.75, alpha=0.9)
    print(plot)
  })

  
  # Call the plot function and observe the variables chosen --------------------
  observeEvent(input$dataFilePlot,
               output$histogramPlot1 <- renderPlot({
                 histogramPlot1()
               }))
  observeEvent(input$dataFilePlot,
               output$histogramPlot2 <- renderPlot({
                 histogramPlot2()
                 }))
  observeEvent(input$xcolHist,
               output$histogramPlot1 <- renderPlot({
                 histogramPlot1()
               }))
  observeEvent(input$ycolHist,
               output$histogramPlot2 <- renderPlot({
                 histogramPlot2()
               }))
  observeEvent(input$facetH1,
               output$histogramPlot1 <- renderPlot({
                 histogramPlot1()
               }))
  observeEvent(input$facetH2,
               output$histogramPlot2 <- renderPlot({
                 histogramPlot2()
               }))
  observeEvent(input$dataFilePlot,
               output$scatter <- renderPlot({
                 scatterPlot()
               }))
  observeEvent(input$dataFilePlot,
               output$box <- renderPlot({
                 boxPlot()
               }))
  
  # Download the plots
  output$downloadPlothist1 <- downloadHandler(
    filename = function() {
      # Generate a dynamic filename based on user input
      paste("histogram1_", Sys.Date(), ".", input$extensionH1, sep = "")},
    
    content = function(file) {
      if (input$extensionH1 == "png") {
        ggsave(file, histogramPlot1(), device = "png") 
      } else if (input$extensionH1 == "pdf") {
        ggsave(file, histogramPlot1(), device = "pdf")
      } else if (input$extensionH1 == "jpeg") {
        ggsave(file, histogramPlot1(), device = "jpeg")
      }
    }
  )
  # Download the plots
  output$downloadPlothist2 <- downloadHandler(
    filename = function() {
      # Generate a dynamic filename based on user input
      paste("histogram2_", Sys.Date(), ".", input$extensionH2, sep = "")},
    
    content = function(file) {
      if (input$extensionH2 == "png") {
        png(file = file)
        histogramPlot2()
        dev.off()
      } else if (input$extensionH2 == "pdf") {
        ggsave(file, histogramPlot2(), device = "pdf")
      } else if (input$extensionH2 == "jpeg") {
        ggsave(file, histogramPlot2(), device = "jpeg")
      }
    }
  )
  
  # Download the plots
  output$downloadPlotscatter <- downloadHandler(
    filename = function() {
      # Generate a dynamic filename based on user input
      paste("scatterplot_", Sys.Date(), ".", input$extensionS, sep = "")},
    
    content = function(file) {
      if (input$extensionS == "png") {
        ggsave(file, scatterPlot(), device = "png")
      } else if (input$extensionS == "pdf") {
        ggsave(file, scatterPlot(), device = "pdf")
      } else if (input$extensionS == "jpeg") {
        ggsave(file, scatterPlot(), device = "jpeg")
      }
    }
  )
  
  # Download the plots
  output$downloadPlotbox <- downloadHandler(
    filename = function() {
      # Generate a dynamic filename based on user input
      paste("boxplot_", Sys.Date(), ".", input$extensionB, sep = "")},
    
    content = function(file) {
      if (input$extensionB == "png") {
        ggsave(file, boxPlot(), device = "png")
      } else if (input$extensionB == "pdf") {
        ggsave(file, boxPlot(), device = "pdf")
      } else if (input$extensionB == "jpeg") {
        ggsave(file, boxPlot(), device = "jpeg")
      }
    })
  
  
  

  # Logistic regression -----------------------------------------------------
  model <- reactive({
    set.seed(1234)
    
    # Remove smoke and NA
    data = datasetInputModel()
    
    if("smoke" %in% names(data)){
      data = data %>%
        dplyr::select(-c(smoke, gender, fbsFactor, cpFactor)) %>%
        tidyr::drop_na()}
    else{
      data = data %>%
        tidyr::drop_na() %>%
        dplyr::select(-gender)}
    
    # Remove datasetOrig
    if("datasetOrig" %in% names(data)) data = data %>% dplyr::select(-datasetOrig)
    
    # Convert alco to integers
    if("alco" %in% names(data)) data <- data %>% mutate(alco = as.numeric(alco))
    
    # Remove chol is variance is zero
    if(sd(na.omit(data$chol)) == 0) data <- data %>% dplyr::select(-chol)
        
    train <- data %>% dplyr::sample_frac(0.70)
    test <- dplyr::anti_join(data, train, by = 'id') 
    
    # Normalize makes a difference
    cvdTrain <- train %>% dplyr::select(cvdPresent) 
    cvdTest <- test %>% dplyr::select(cvdPresent)
    train = train %>% dplyr::select(-c(id, cvdPresent)) %>% scale() %>% as.data.frame() %>% bind_cols(cvdTrain)
    test = test %>% dplyr::select(-c(id, cvdPresent)) %>% scale() %>% as.data.frame() %>% bind_cols(cvdTest)
    
    # Create the glm formula
    if(is.null(input$independentVars)) vars = "~."
    else vars = paste("~", paste(input$independentVars, collapse = " + "), sep = "")
    
    formula <- as.formula(paste("cvdPresent", vars, collapse = " + "))
    model_glm <- glm(formula, data = train, family = "binomial")
    
    # Predictions on the training set
    predictTrain = predict(model_glm, data = train, type = "response")
    
    # Confusion matrix on training data
    CMtrain = table(train$cvdPresent, predictTrain >= 0.5) 
    
    #Predictions on the test set
    predictTest = predict(model_glm, newdata = test, type = "response")
    
    # Confusion matrix on test set
    CMtest = table(test$cvdPresent, predictTest >= 0.5) 
    
    return(list(glmOutput = model_glm,
                CMtrain = train$cvdPresent, 
                CMtest = test$cvdPresent,   
                predTrain = predictTrain,
                predTest = predictTest)) 
  })
  
  output$accuracyTrain <- renderText({
    if (!is.null(model())) {
      info = model()
      TrainTable = table(info$CMtrain, info$predTrain >= 0.5)
      if(length(TrainTable) == 2) return(round((TrainTable[1,1] + TrainTable[2,1])/length(info$predTrain), 2))
      else return(round((TrainTable[1,1] + TrainTable[2,2])/length(info$predTrain), 2))
    }
  })
  output$accuracyTest <- renderText({
    if (!is.null(model())) {
      info = model()
      TestTable = table(info$CMtest, info$predTest >= 0.5)
      if((dim(TestTable)[1] == 2) & (dim(TestTable)[2] == 1)) return(round((TestTable[1,1] + TestTable[2,1])/length(info$predTest), 2))
      else if((dim(TestTable)[1] == 1) & (dim(TestTable)[2] == 1)) return(round(TestTable[1,1]/length(info$predTest), 2))
      else if((dim(TestTable)[1] == 2) & (dim(TestTable)[2] == 2)) return(round((TestTable[1,1] + TestTable[2,2])/length(info$predTest), 2))
      else if((dim(TestTable)[1] == 1) & (dim(TestTable)[2] == 2)) return(round((TestTable[1,1] + TestTable[1,2])/length(info$predTest), 2))
    }
  })
  
  
  output$logregSummary <- renderPrint({
    if (!is.null(model())) {
      coef_summary <- coef(summary(model()$glmOutput))
      print(coef_summary)
    }
  })

  

  # KNN ---------------------------------------------------------------------
  observe({
    choices <- colnames(datasetInputModelKNN())
    
    # only some variables
    if("ap_hi" %in% choices){
      choices = choices[!choices %in% c("id", "datasetOrig", "cvdPresent", "gender", "sex")]}
    else{
      if("datasetOrig" %in% choices) choices = choices[!choices %in% c("id", "cvdPresent", "gender", "fbsFactor", "cpFactor", "datasetOrig", "smoke", "sex")]
      else  choices = choices[!choices %in% c("id", "cvdPresent", "gender", "fbsFactor", "cpFactor", "smoke", "sex")]}

    updateSelectInput(session, "targetVar1", choices = choices, selected = choices[1])
    updateSelectInput(session, "targetVar2", choices = choices, selected = choices[2])
    
  })
  
  observe({
    # Notification is select same variables
    if (!is.null(input$targetVar1) & !is.null(input$targetVar2)){
      if((input$targetVar1 == input$targetVar2) & (input$targetVar1 != "")){
        showNotification("The selected variables cannot be the same. Select two different.", type = "error")}}
  })

  
  observe({
    input$k
    
    if(input$k > 30) {
      showNotification("K-values above 30 is not supported in this shiny app.", type = "warning")
      updateNumericInput(session, "k", value = 30)}
  })
  
  
  
  knnModel <- reactive({
  
    set.seed(1234)
    
    # Split data into train and test ---------------------------------
    data = datasetInputModelKNN()
    if("smoke" %in% names(data)){
      data = data %>%
        dplyr::select(-c(smoke, gender, fbsFactor, cpFactor, sex)) %>%
        tidyr::drop_na()}
    else{
      data = data %>%
        tidyr::drop_na() %>%
        dplyr::select(-c(gender, sex))}
    
    # Remove datasetOrig
    if("datasetOrig" %in% names(data)) data = data %>% dplyr::select(-datasetOrig)
    
    # Convert alco to integers
    if("alco" %in% names(data)) data <- data %>% mutate(alco = as.numeric(alco))
    
    train <- data %>% dplyr::sample_frac(0.80)
    test <- dplyr::anti_join(data, train, by = 'id') 
   
    # Normalize makes a difference
    cvdTrain <- train %>% dplyr::select(cvdPresent) 
    cvdTest <- test %>% dplyr::select(cvdPresent)
    # sexVar = test %>% dplyr::select(sex)
    cholVarTrain = train %>% dplyr::select(chol) %>% scale() %>% as.data.frame()
    cholVarTest = test %>% dplyr::select(chol) %>% scale() %>% as.data.frame()
    train = train %>% dplyr::select(-c(id, cvdPresent, chol)) %>% scale() %>% as.data.frame() %>% bind_cols(c(cholVarTrain, cvdTrain))
    test = test %>% dplyr::select(-c(id, cvdPresent, chol)) %>% scale() %>% as.data.frame() %>% bind_cols(c(cholVarTest, cvdTest))

    # Prediction error on training data set for K = 1 to 30
    error.train <- replicate(0, 30)
    for(k in 1:30) {
      predCVD <- knn(train = train[, c(input$targetVar1, input$targetVar2)], test = train[, c(input$targetVar1, input$targetVar2)], cl = cvdTrain$cvdPresent, k)
      error.train[k ]<- 1-mean(predCVD == cvdTrain$cvdPresent)}
    error.train <- unlist(error.train, use.names = FALSE)
    
    # Prediction error on testing data set for K = 1 to 30
    error.test <- replicate(0, 30)
    for(k in 1:30) {
      predCVD <- knn(train = train[, c(input$targetVar1, input$targetVar2)], test = test[, c(input$targetVar1, input$targetVar2)], cl = cvdTrain$cvdPresent, k)
      error.test[k] <- 1-mean(predCVD == cvdTest$cvdPresent)}
    error.test <- unlist(error.test, use.names = FALSE)
    
    # Confusion matrix
    cm <- table(as.data.frame(test)$cvdPresent, predCVD)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))} #* 100
    acc = accuracy(cm)
    
    confusionMatrix = data.frame("True" = c(cm[1], cm[3]), 
                                 "False" = c(cm[2], cm[4]))
    row.names(confusionMatrix) <- c("True", "False")
    return(list(error.train = error.train, 
                error.test = error.test,
                accuracy = acc))
  })

  
  knnErrorPlot <- reactive({
    if(!is.null(knnModel())){
      error.rates = knnModel()
      error.train.value = round(error.rates$error.train, 3)
      error.test.value = round(error.rates$error.test, 3)
      k.value = 1:30
      
      blue.color = rep("Training error", length(error.train.value))
      red.color = rep("Testing error", length(error.test.value))
      
      plot <- ggplot() + 
        geom_line(aes(x = k.value, y = error.train.value, color = blue.color)) +#, color = "blue") + 
        geom_line(aes(x = k.value, y = error.test.value, color = red.color)) + #, color = "red") + 
        theme_minimal() + 
        labs(title = paste0("Classification of cvdPresent based on ", input$targetVar1, " and ", input$targetVar2),
             color = "") +
        ylab("Misclassification errors (%)") +
        xlab("K values") + 
        scale_color_manual(values = c("Training error" = "#2974CE", "Testing error" = "#C61D1F")) +
        # scale_color_manual(labels = c("Training error", "Testing error"), values = c("blue", "red")) +
        scale_x_continuous(breaks = seq(1, 31, 2)) + 
        theme(axis.title = element_text(size = 16),
              plot.title = element_text(size = 20)) 
      # print(plot)
      plotly::ggplotly(plot)
    }
  })
  
  output$knnEPlot <- renderPlotly({ # renderPlot
    if (!is.null(knnErrorPlot())) {
      knnErrorPlot()
    }
  })
  
  
  output$knnInfo <- renderUI({
    if(!is.null(knnModel())){
      error.rates = knnModel()
      error.train = error.rates$error.train
      error.test = error.rates$error.test
      
      # The best K-value
      K.test = c(1:30)[which.min(error.test)]
      K.train = c(1:30)[which.min(error.train)]
      
      HTML(paste0("The best K-value to classify the presence of cardiovascular disease based on the selected variables: ", 
          input$targetVar1, " and ", input$targetVar2, ", is ", K.test, ". By choosing this K this would give an accuracy (when run of training data (80%)) of ", round(error.rates$accuracy, 2), ".", sep = ""))
    }
  })
  

    
  ## KNN plot
  knnPlot1 <- reactive({
    set.seed(1234)
    
    data = datasetInputModelKNN()
    if("smoke" %in% names(data)){
      data = data %>%
        dplyr::select(-c(smoke, gender, fbsFactor, cpFactor, sex)) %>%
        tidyr::drop_na()}
    else{
      data = data %>%
        tidyr::drop_na() %>%
        dplyr::select(-sex)}
    
    # Remove datasetOrig
    if("datasetOrig" %in% names(data)) data = data %>% dplyr::select(-datasetOrig)
    
    # Convert alco to integers
    if("alco" %in% names(data)) data <- data %>% mutate(alco = as.numeric(alco))
    
    train <- data %>% dplyr::sample_frac(0.80) 
    test <- dplyr::anti_join(data, train, by = 'id') 
    
    # Normalize makes a difference
    cvdTrain <- train %>% dplyr::select(cvdPresent) 
    cvdTest <- test %>% dplyr::select(cvdPresent)
    # sexVar = test %>% dplyr::select(sex)
    train = train %>% dplyr::select(-c(id, cvdPresent)) %>% scale() %>% as.data.frame() %>% bind_cols(cvdTrain)
    test = test %>% dplyr::select(-c(id, cvdPresent)) %>% scale() %>% as.data.frame() %>% bind_cols(cvdTest)
    
    k = input$k
    predCVD <- knn(train = train[, c(input$targetVar1, input$targetVar2)], test = test[, c(input$targetVar1, input$targetVar2)], cl = cvdTrain$cvdPresent, k)
    result <- cbind(test[, c(input$targetVar1, input$targetVar2)], predCVD)
    combinetest <- cbind(test[, c(input$targetVar1, input$targetVar2)], cvdTest)
    
    # Print accuracy
    cm <- table(as.data.frame(test)$cvdPresent, predCVD)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))} #* 100
    acc = accuracy(cm)
    print(acc)

    # The plot
    plot <- ggplot(as.data.frame(result), aes(x = result[, 1], y = result[, 2], color = as.factor(predCVD))) +
      geom_point(size = 3) + 
      xlab(input$targetVar1) + 
      ylab(input$targetVar2) + 
      theme_minimal() +
      labs(title = "Prediction of cardiovascular disease",
           color = "cvdPresent") +
      scale_color_manual(labels = c("False", "True"), values = c("#2974CE", "#C61D1F")) +
      theme(axis.title = element_text(size = 16),
            plot.title = element_text(size = 20))
    
    return(list(plot = plot,
                accuracy = acc))
  })
  
  ## KNN plot
  knnPlot2 <- reactive({
    set.seed(1234)
    
    data = datasetInputModelKNN()
    if("smoke" %in% names(data)){
      data = data %>%
        dplyr::select(-c(smoke, gender, fbsFactor, cpFactor, sex)) %>%
        tidyr::drop_na()}
    else{
      data = data %>%
        tidyr::drop_na() %>%
        dplyr::select(-sex)}
    
    # Remove datasetOrig
    if("datasetOrig" %in% names(data)) data = data %>% dplyr::select(-datasetOrig)
    
    # Convert alco to integers
    if("alco" %in% names(data)) data <- data %>% mutate(alco = as.numeric(alco))
    
    train <- data %>% dplyr::sample_frac(0.80) %>% tidyr::drop_na()
    test <- dplyr::anti_join(data, train, by = 'id') %>% tidyr::drop_na()
    
    # Normalize makes a difference
    cvdTrain <- train %>% dplyr::select(cvdPresent) 
    cvdTest <- test %>% dplyr::select(cvdPresent)
    # sexVar = test %>% dplyr::select(sex)
    train = train %>% dplyr::select(-c(id, cvdPresent)) %>% scale() %>% as.data.frame() %>% bind_cols(cvdTrain)
    test = test %>% dplyr::select(-c(id, cvdPresent)) %>% scale() %>% as.data.frame() %>% bind_cols(cvdTest)
    
    k = input$k
    predCVD <- knn(train = train[, c(input$targetVar1, input$targetVar2)], test = test[, c(input$targetVar1, input$targetVar2)], cl = cvdTrain$cvdPresent, k)
    result <- cbind(test[, c(input$targetVar1, input$targetVar2)], predCVD)
    combinetest <- cbind(test[, c(input$targetVar1, input$targetVar2)], cvdTest)
    
    plot <- ggplot(as.data.frame(combinetest), aes(x = combinetest[, 1], y = combinetest[, 2], color = as.factor(cvdPresent))) +
      geom_point(size = 3) + 
      xlab(input$targetVar1) + 
      ylab(input$targetVar2) + 
      theme_minimal() +
      labs(title = "Real classes of cardiovascular disease",
           color = "cvdPresent") +
      scale_color_manual(labels = c("False", "True"), values = c("#2974CE", "#C61D1F")) +
      theme(axis.title = element_text(size = 16),
            plot.title = element_text(size = 20)) 
    return(plot)
  })
  
  
  output$knnPredictionPlot1 = renderPlot({
    if(!is.null(knnPlot1())){
      knnplot1 <- knnPlot1()
      print(knnplot1$plot)
      }
    })
  output$knnPredictionPlot2 = renderPlot({if(!is.null(knnPlot2())) knnPlot2()})
  
  output$accuracyTestKNN <- renderText({
    if (!is.null(knnPlot1())) {
      info = knnPlot1()
      return(round(info$accuracy, 2))
    }
  })
}