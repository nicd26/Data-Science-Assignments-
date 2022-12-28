

shinyServer(function(input, output, session) {
  
  
  
  output$mytable = renderDataTable({dat})
  output$SummaryA2 <- renderPrint({
    if(input$sum_clean == FALSE){
      sumdata <- as.data.frame(dat)
    }
    else{
      sumdata <- as.data.frame(getCleanData2())
    }
    print(dfSummary(sumdata))
  })
  
  react <- reactiveValues(cleanData1=NULL, cleanData2=NULL, recipe=NULL, model=NULL)
  
  getCleanData1 <- reactive({
    vRatio <- apply(X = cleandat, MARGIN = 2, FUN = pMiss)
    cleandat[, vRatio < input$VarThreshold]
  })  
  
  getCleanData2 <- reactive({
    data <- getCleanData1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })  
  

  
## RAW DATA
  
  
  output$RAWMissing <- renderPlot({
    visdata <- dat
    vis_miss(visdata, cluster = input$Rcluster) +
      labs(title = "Missingness of Raw 2019 COVID-19 Data")
  })
  
  output$RAWBoxplot <- renderPlot({
    boxdata <- as.matrix(dat[input$RVariablesB])
    boxdata <- scale(boxdata, center = input$Rstandardise, scale = input$Rstandardise)
    Boxplot(y = boxdata, xlab = "Variables", ylab = "Values", use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
            horizontal = FALSE, outline = input$Routliers, 
            col = turbo(n = length(input$RVariablesB)),
            range = input$Rrange, main = "Boxplots of Raw 2019 COVID-19 Data",
            id = FALSE)
  })
  output$RAWCorrgram <- renderPlot({
    Rcorrdata <- as.data.frame(dat[input$RVariablesC])
    corrgram(Rcorrdata, 
             order = input$RGroup2, 
             abs = input$Rabs2, 
             cor.method = input$RCorrMeth2,
             text.panel = panel.txt,
             main = "Correlation of Raw 2019 COVID-19 Data")
  })
  output$RAWMosaic <- renderPlot({
    Rformula <- as.formula(paste("~",paste(namesmosaic, collapse = " + ")))
    mosaic(Rformula, data = dat[,c(2, 12)],
           main = "Mosiac of Raw 2019 COVID-19 Data", shade = TRUE, legend = TRUE, rot_labels=c(0,90,0,0), abbreviate_labs = c(3, 9))
  }, height=600, width=1000)
  
  output$RAWValues <- renderPlot({
    RAWvaluedata <- as.data.frame(dat[input$RVariablesR])
    RAWvaluedata <- scale(RAWvaluedata, center = input$Rstandardise1, scale = input$Rstandardise1)
    Rn = nrow(dat)
    Rchartcol <- turbo(n = length(input$RVariablesR))
    plot((1:length(sort(RAWvaluedata[,1])))/Rn, sort(RAWvaluedata[,1]), type="l", lwd = 1.5,
         main = "Visualizing Percentiles of Raw 2019 COVID-19 Data",
         xlab = "Percentile",
         ylab = "Value",
         xlim=c(0,1),
         ylim=c(min(RAWvaluedata, na.rm = TRUE), max(RAWvaluedata, na.rm = TRUE)),
         col=Rchartcol[1])
    if(length(input$RVariablesR) > 1) {
      for(x in 2:length(input$RVariablesR)){
        lines(1:length(sort(RAWvaluedata[,x]))/Rn, sort(RAWvaluedata[,x]), col=Rchartcol[x], lwd = 1.5)
        if(input$Rshow_legend == TRUE){
          legend(x = "bottomright", input$RVariablesR, lwd = 2, col=Rchartcol[1:x])
        }
        
      }
    }
  })
  output$RAWpairGraph <- renderPlot({
    Rcolourchoice <- input$RColours
    Rpairdata <- as.data.frame(dat[input$RVariablesP])
    ggpairs(Rpairdata, mapping = aes(colour = as.matrix(dat[Rcolourchoice])), title = "Correlation between Raw 2019 COVID-19 Data")
  })

  
## CLEAN DATA 
 
     
    output$Missing <- renderPlot({
      data <- getCleanData2()
      req(data)
      vis_dat(data) +
        labs(title = paste("Missingness of 2019 COVID-19 Data VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
    }, width = 500)
    
    observe({
      data <- getCleanData2()
      
      req(data)
      rec <- recipe(DEATH_RATE ~ ., data)
      if (input$ImpMethod == "KNN") {
        rec <- step_impute_knn(rec, neighbors = 5)
      } else if (input$ImpMethod == "Median") {
        rec <- step_impute_mode(rec)
      } else if (input$ImpMethod == "Partial Del") {
        rec <- step_naomit(rec, all_predictors(), skip = TRUE)
      }
      react$recipe <- rec
    })  
    
    observe({
      req(input$Go)
      isolate({
        set.seed(12)
        req(react$recipe, getCleanData2())
        mod <- caret::train(react$recipe, 
                            data = getCleanData2(), 
                            method = "rpart", 
                            na.action = na.rpart)
      })
      react$model <- mod
    })
    
    output$Summary <- renderPrint({
      mod <- react$model
      req(mod)
      print(mod)
      
    })
    
    observeEvent(input$VarThreshold, {
      datac <- getCleanData2()
      dataf <- subset(datac, select=-c(CODE, OBS_TYPE, POLITICS, HEALTHCARE_BASIS))
      datap <- subset(datac, select=-c(CODE, OBS_TYPE))
      columnsf <- colnames(dataf)
      columnsp <- colnames(datap)
      updateSelectizeInput(session, "VariablesB",
                           choices = columnsf,
                           selected = input$VariablesB)
      updateSelectizeInput(session, "VariablesC",
                           choices = columnsf,
                           selected = input$VariablesC)
      updateSelectizeInput(session, "VariablesR",
                           choices = columnsf,
                           selected = input$VariablesR)
      updateSelectizeInput(session, "VariablesP",
                           choices = columnsp,
                           selected = input$VariablesP)
    })
    
    output$Boxplot <- renderPlot({
      boxdata <- as.data.frame(getCleanData2())
      data <- as.matrix(boxdata[input$VariablesB])
      data <- scale(data, center = input$standardise, scale = input$standardise)
      Boxplot(y = data, xlab = "Variables", ylab = "Values", use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
              horizontal = FALSE, outline = input$outliers, 
              col = turbo(n = length(input$VariablesB)),
              range = input$range, main = "Boxplots of 2019 COVID-19 Data",
              id = FALSE)
      
    })
    
    output$Corrgram <- renderPlot({
      corrdata <- as.data.frame(getCleanData2())
      corrdata <- as.data.frame(corrdata[input$VariablesC])
      corrgram(corrdata, 
               order = input$Group2, 
               abs = input$abs2, 
               cor.method = input$CorrMeth2,
               text.panel = panel.txt,
               main = "Correlation of 2019 COVID-19 Data")
    })
    
    output$Mosaic <- renderPlot({
      mosaicdat <- getCleanData2()
      formula <- as.formula(paste("~",paste(c("POLITICS", "HEALTHCARE_BASIS" ), collapse = " + ")))
      mosaic(formula, data = mosaicdat[,c("POLITICS", "HEALTHCARE_BASIS")],
             main = "Mosiac of 2019 COVID-19 Data", shade = TRUE, legend = TRUE, rot_labels=c(0,90,0,0), abbreviate_labs = c(3, 9))
    }, height=600, width=1000)
    
    output$Values <- renderPlot({
      valuedata <- as.data.frame(getCleanData2())
      valuedata <- as.data.frame(valuedata[input$VariablesR])
      valuedata <- scale(valuedata, center = input$standardise1, scale = input$standardise1)
      n = nrow(valuedata)
      chartcol <- turbo(n = length(input$VariablesR))
      plot((1:length(sort(valuedata[,1])))/n, sort(valuedata[,1]), type="l", lwd = 1.5,
           main = "Visualizing Percentiles of 2019 COVID-19 Data",
           xlab = "Percentile",
           ylab = "Value",
           xlim=c(0,1),
           ylim=c(min(valuedata, na.rm = TRUE), max(valuedata, na.rm = TRUE)),
           col=chartcol[1])
      if(length(input$VariablesR) > 1) {
        for(x in 2:length(input$VariablesR)){
          lines(1:length(sort(valuedata[,x]))/n, sort(valuedata[,x]), col=chartcol[x], lwd = 1.5)
          if(input$show_legend == TRUE){
            legend(x = "bottomright", input$VariablesR, lwd = 2, col=chartcol[1:x])
          }
        }
      }
    })
    output$pairGraph <- renderPlot({
      colourchoice <- input$Colours
      pairdata1 <- as.data.frame(getCleanData2())
      pairdata2 <- as.data.frame(pairdata1[input$VariablesP])
      print(as.matrix(pairdata1[colourchoice]))
      ggpairs(pairdata2, mapping = aes(colour = as.matrix(pairdata1[colourchoice])), title = "Correlation between 2019 COVID-19 Data")
    })
    
    output$treeGraph <- renderPlot({
      cleandat2 <- getCleanData2()
      cleandat2$MISSINGNESS <- apply(X = is.na(cleandat2), MARGIN = 1,
                                    FUN = sum)
      tree <- train(MISSINGNESS ~ . -CODE -OBS_TYPE, data =
                      cleandat2, method = "rpart", na.action = na.rpart)
      rpart.plot(tree$finalModel, main = "TUNED: Predicting
      the number of missing variables in an observation",
                 roundint = TRUE, clip.facs = TRUE)
    })
    
    output$glmResults <- renderUI({
      set.seed(12)
      glmdata <- getCleanData2()
      train <- glmdata[glmdata$OBS_TYPE == "Train",]
      test <- glmdata[glmdata$OBS_TYPE == "Test",]
      ogtest <- dat[dat$OBS_TYPE =="Test",]
      
      rec <- recipes::recipe(DEATH_RATE ~., data = glmdata) %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_impute_knn(all_predictors(), neighbors = 5) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())
    
      
      train_cont <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 3,
                                 search = "random",
                                 selectionFunction = "best",
                                 verboseIter = TRUE)
      
      model <- caret::train(rec, data = train, method = "glmnet",tuneLength = 3, trControl = train_cont)
      
      output$coefText <- renderPrint({
        nums <- coef(model$finalModel, model$bestTune$lambda)
        print(nums)
      })
      
      predictions_train <- predict(model, train)
      
      eval_results <- function(true, predicted, df) {
        SSE <- sum((predicted - true)^2)
        SST <- sum((true - mean(true))^2)
        R_square <- 1 - SSE / SST
        RMSE = sqrt(SSE/nrow(df))
        data.frame(
          RMSE = RMSE,
          Rsquare = R_square)
      }
      
      eval_results(train$DEATH_RATE, predictions_train, train) 
      
      predictions_test <- predict(model, test)
      results <- eval_results(test$DEATH_RATE, predictions_test, test)
      
      output$TestResults <- renderUI({
        
        HTML(paste(paste("RMSE (based on test data) =", round(results[1],2)),
             paste("RSquared (based on test data) =", round(results[2],2)),
             paste("Best Tuning Parameters: Alpha =", round(model$bestTune[1], 7), "Lambda =", round(model$bestTune[2], 7)), sep="<br/>"))
        })

      output$testBox <- renderPlot({
        testres <- test$DEATH_RATE - predictions_test
        trainres <- train$DEATH_RATE - predictions_train
        bothresids <- c(testres, trainres)
        stateID <- c(test$CODE, train$CODE)
        boxdf <- data.frame(testres = c(testres, rep(NA, length(bothresids) - length(testres))), 
                            trainres = c(trainres, rep(NA, length(bothresids) - length(trainres))),
                            bothresids = bothresids)
        rownames(boxdf) <- stateID
        
        Boxplot(y = boxdf, ylab = "Residuals", use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                horizontal = FALSE, outline = input$Boutliers, 
                col = turbo(n = 3),
                range = input$Brange, main = "Residual Test Data",
                id = ifelse(input$Boutliers, list(n = Inf, location = "avoid", id = rownames(boxdf)),  FALSE))
        
        output$residframe <- renderDataTable({
          boxdf
        })
      })
      
      output$glmGraph <- renderPlot({
        plot(test$DEATH_RATE, predictions_test, main= "Death Rate Predictions of COVID19 Test Data", xlab = "Actual", ylab = "Predicted", xlim = c(5,35), ylim = c(5,35))
        lines(0:100, 0:100, col="red")
      }, height=700, width=700) 
    })
})
