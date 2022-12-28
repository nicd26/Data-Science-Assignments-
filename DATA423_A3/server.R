shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "cubist")  ## change the value parameter to your best method
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # # METHOD * nnet ---------------------------------------------------------------------------------------------------------------------------
  
  library(nnet)
  
  # reactive getnnetRecipe ----
  getnnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$nnet_Preprocess)
  })
  
  # observeEvent nnet_Go ----
  observeEvent(
    input$nnet_Go,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getnnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont,
                              tuneLength = 5,  na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$nnet_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$nnet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()
    }
  )
  
  # output nnet_ModelSummary0 (text) ----
  output$nnet_ModelSummary0 <- renderText({
    description("nnet") 
  })
  
  # output nnet_Metrics (table) ----
  output$nnet_Metrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })
  
  # output nnet_ModelPlots (plot) ----
  output$nnet_ModelPlots <- renderPlot({
    req(models$nnet)
    plot(models$nnet)
  })
  
  # output nnet_Recipe (print) ----
  output$nnet_Recipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })
  
  # output nnet_ModelSummary2 (print) ----
  output$nnet_ModelSummary2 <- renderPrint({
    req(models$nnet)
    summary(models$nnet$finalModel)
  })
  
  # output nnet_ModelTree (plot) ----
  
  output$nnet_ModelTree <- renderPlot({
    library(devtools)
    source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
    req(models$nnet)
    plot.nnet(models$nnet$finalModel)
  }) 

  
  # METHOD * avNNet ---------------------------------------------------------------------------------------------------------------------------
  
  library(nnet)
  
  # reactive getavNNetRecipe ----
  getavNNetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$avNNet_Preprocess)
  })
  
  # observeEvent avNNet_Go ----
  observeEvent(
    input$avNNet_Go,
    {
      method <- "avNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getavNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, 
                              tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$avNNet_Load,
    {
      method  <- "avNNet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$avNNet_Delete,
    {
      models[["avNNet"]] <- NULL
      gc()
    }
  )
  
  # output avNNet_ModelSummary0 (text) ----
  output$avNNet_ModelSummary0 <- renderText({
    description("avNNet")   
  })
  
  # output avNNet_Metrics (table) ----
  output$avNNet_Metrics <- renderTable({
    req(models$avNNet)
    models$avNNet$results[ which.min(models$avNNet$results[, "RMSE"]), ]
  })
  
  # output avNNet_ModelPlots (plot) ----
  output$avNNet_ModelPlots <- renderPlot({
    req(models$avNNet)
    plot(models$avNNet)
  })     
  
  # output avNNet_Recipe (print) ----
  output$avNNet_Recipe <- renderPrint({
    req(models$avNNet)
    models$avNNet$recipe
  })  
  
  # output avNNet_ModelSummary2 (print) ----
  output$avNNet_ModelSummary2 <- renderPrint({
    req(models$avNNet)
    summary(models$avNNet$finalModel)
  })
  
  
  # METHOD * qrf ---------------------------------------------------------------------------------------------------------------------------
  library(quantregForest)
  
  # reactive getqrfRecipe ----
  getqrfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$qrf_Preprocess)
  })
  
  # observeEvent qrf_Go ----
  observeEvent(
    input$qrf_Go,
    {
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.pass)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$qrf_Load,
    {
      method  <- "qrf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$qrf_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output qrf_ModelSummary0 (print) ----
  output$qrf_ModelSummary0 <- renderText({
    description("qrf")  
  })
  
  # output qrf_Metrics (table) ----
  output$qrf_Metrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  # output qrf_Recipe (print) ----
  output$qrf_Recipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  # output qrf_ModelPlots (plot) ----
  output$qrf_ModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })
  
  
  # output qrf_ModelTree (plot) ----
  output$qrf_ModelTree <- renderPlot({
    req(models$qrf)
    randomForest::varImpPlot(models$qrf$finalModel)
  })
  
  
  
  # METHOD * glm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getglmRecipe ----
  getglmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glm_Preprocess)
  })
  
  # observeEvent glm_Go ----
  observeEvent(
    input$glm_Go,
    {
      method <- "glm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$glm_Load,
    {
      method  <- "glm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glm_Delete,
    {
      models[["glm"]] <- NULL
      gc()
    }
  )
  
  # output glm_ModelSummary0 (text) ----
  output$glm_ModelSummary0 <- renderText({
    description("glm")
  })
  
  # output glm_Metrics (table) ----
  output$glm_Metrics <- renderTable({
    req(models$glm)
    models$glm$results[ which.min(models$glm$results[, "RMSE"]), ]
  })
  
  # output glm_ModelPlots (plot) ----
  output$glm_ModelPlots <- renderPlot({
    req(models$glm)
    plot(models$glm)
  })     
  
  # output glm_Recipe (print) ----
  output$glm_Recipe <- renderPrint({
    req(models$glm)
    models$glm$recipe
  })
  
  # output glm_Coef (print) ----
  output$glm_Coef <- renderTable({
    req(models$glm)
    co <- coef(models$glm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * bayesglm ---------------------------------------------------------------------------------------------------------------------------
  
  library(arm)  
  
  # reactive getbayesglmRecipe ----
  getbayesglmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bayesglm_Preprocess)
  })
  
  # observeEvent bayesglm_Go ----
  observeEvent(
    input$bayesglm_Go,
    {
      method <- "bayesglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getbayesglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bayesglm_Load,
    {
      method  <- "bayesglm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bayesglm_Delete,
    {
      models[["bayesglm"]] <- NULL
      gc()
    }
  )
  
  # output bayesglm_ModelSummary0 (text) ----
  output$bayesglm_ModelSummary0 <- renderText({
    description("bayesglm") 
  })
  
  # output bayesglm_Metrics (table) ----
  output$bayesglm_Metrics <- renderTable({
    req(models$bayesglm)
    models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
  })
  
  # output bayesglm_Recipe (print) ----
  output$bayesglm_Recipe <- renderPrint({
    req(models$bayesglm)
    models$bayesglm$recipe
  })  
  
  # # output bayesglm_ModelSummary2 (print) ----
  output$bayesglm_ModelSummary2 <- renderPrint({
    req(models$bayesglm)
    print(models$bayesglm)
  })
  
  # output bayesglm_Coef (print) ----
  output$bayesglm_Coef <- renderTable({
    req(models$bayesglm)
    co <- coef(models$bayesglm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * leapSeq ---------------------------------------------------------------------------------------------------------------------------
 
   library(leaps)  
  
  # reactive getleapSeqRecipe ----
  getleapSeqRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$leapSeq_Preprocess) %>%  
      step_rm(has_type("date")) 
  })
  
  # observeEvent leapSeq_Go ----
  observeEvent(
    input$leapSeq_Go,
    {
      method <- "leapSeq"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getleapSeqRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$leapSeq_Load,
    {
      method  <- "leapSeq"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$leapSeq_Delete,
    {
      models[["leapSeq"]] <- NULL
      gc()
    }
  )
  
  # output leapSeq_ModelSummary0 (text) ----
  output$leapSeq_ModelSummary0 <- renderText({
    description("leapSeq")  
  })
  
  # output leapSeq_Metrics (table) ----
  output$leapSeq_Metrics <- renderTable({
    req(models$leapSeq)
    models$leapSeq$results[ which.min(models$leapSeq$results[, "RMSE"]), ]
  })
  
  # output leapSeq_ModelPlots (plot) ----
  output$leapSeq_ModelPlots <- renderPlot({
    req(models$leapSeq)
    plot(models$leapSeq)
  })     
  
  # output leapSeq_Recipe (print) ----
  output$leapSeq_Recipe <- renderPrint({
    req(models$leapSeq)
    models$leapSeq$recipe
  })  
  
  # output leapSeq_ModelSummary2 (print) ----
  output$leapSeq_ModelSummary2 <- renderPrint({
    req(models$leapSeq)
    summary(models$leapSeq$finalModel)
  })
  
  
  
  # METHOD * bagEarth ---------------------------------------------------------------------------------------------------------------------------
  library(earth)  
  
  # reactive getbagEarthRecipe ----
  getbagEarthRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bagEarth_Preprocess) %>%   
      step_rm(has_type("date"))  
  })
  
  # observeEvent bagEarth_Go ----
  observeEvent(
    input$bagEarth_Go,
    {
      method <- "bagEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getbagEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.fail)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bagEarth_Load,
    {
      method  <- "bagEarth"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bagEarth_Delete,
    {
      models[["bagEarth"]] <- NULL
      gc()
    }
  )
  
  # output bagEarth_ModelSummary0 (text) ----
  output$bagEarth_ModelSummary0 <- renderText({
    description("bagEarth") 
  })
  
  # output bagEarth_Metrics (table) ----
  output$bagEarth_Metrics <- renderTable({
    req(models$bagEarth)
    models$bagEarth$results[ which.min(models$bagEarth$results[, "RMSE"]), ]
  })
  
  # output bagEarth_ModelPlots (plot) ----
  output$bagEarth_ModelPlots <- renderPlot({
    req(models$bagEarth)
    plot(models$bagEarth)
  })     
  
  # output bagEarth_Recipe (print) ----
  output$bagEarth_Recipe <- renderPrint({
    req(models$bagEarth)
    models$bagEarth$recipe
  })  
  
  # output bagEarth_ModelSummary2 (print) ----
  output$bagEarth_ModelSummary2 <- renderPrint({
    req(models$bagEarth)
    summary(models$bagEarth$finalModel)
  })
  
  # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
 
   library(Cubist)  
  
  # reactive getcubistRecipe ----
  getcubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%  
      step_rm(has_type("date")) 
  })
  
  # observeEvent cubist_Go ----
  observeEvent(
    input$cubist_Go,
    {
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.fail)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$cubist_Load,
    {
      method  <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$cubist_Delete,
    {
      models[["cubist"]] <- NULL
      gc()
    }
  )
  
  # output cubist_ModelSummary0 (text) ----
  output$cubist_ModelSummary0 <- renderText({
    description("cubist") 
  })
  
  # output cubist_Metrics (table) ----
  output$cubist_Metrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output cubist_ModelPlots (plot) ----
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })     
  
  # output cubist_Recipe (print) ----
  output$cubist_Recipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output cubist_ModelSummary2 (print) ----
  output$cubist_ModelSummary2 <- renderPrint({
    req(models$cubist)
    summary(models$cubist$finalModel)
  })
  
  
  # METHOD * knn ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getknnRecipe ----
  getknnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$knn_Preprocess) %>%   
      step_rm(has_type("date"))   
  })
  
  # observeEvent knn_Go ----
  observeEvent(
    input$knn_Go,
    {
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.fail)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$knn_Load,
    {
      method  <- "knn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$knn_Delete,
    {
      models[["knn"]] <- NULL
      gc()
    }
  )
  
  # output knn_ModelSummary0 (text) ----
  output$knn_ModelSummary0 <- renderText({
    description("knn")   
  })
  
  # output knn_Metrics (table) ----
  output$knn_Metrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  # output knn_ModelPlots (plot) ----
  output$knn_ModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })     
  
  # output knn_Recipe (print) ----
  output$knn_Recipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  # output knn_ModelSummary2 (print) ----
  output$knn_ModelSummary2 <- renderPrint({
    req(models$knn)
    summary(models$knn$finalModel)
  })
  
  
  # METHOD * spikeslab ---------------------------------------------------------------------------------------------------------------------------
  library(spikeslab)
  
  # reactive getspikeslabRecipe ----
  getspikeslabRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$spikeslab_Preprocess) %>%  
      step_rm(has_type("date")) 
  })
  
  # observeEvent spikeslab_Go ----
  observeEvent(
    input$spikeslab_Go,
    {
      method <- "spikeslab"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getspikeslabRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$spikeslab_Load,
    {
      method  <- "spikeslab"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$spikeslab_Delete,
    {
      models[["spikeslab"]] <- NULL
      gc()
    }
  )
  
  # output spikeslab_ModelSummary0 (text) ----
  output$spikeslab_ModelSummary0 <- renderText({
    description("spikeslab")  
  })
  
  # output spikeslab_Metrics (table) ----
  output$spikeslab_Metrics <- renderTable({
    req(models$spikeslab)
    models$spikeslab$results[ which.min(models$spikeslab$results[, "RMSE"]), ]
  })
  
  # output spikeslab_ModelPlots (plot) ----
  output$spikeslab_ModelPlots <- renderPlot({
    req(models$spikeslab)
    plot(models$spikeslab)
  })     
  
  # output spikeslab_Recipe (print) ----
  output$spikeslab_Recipe <- renderPrint({
    req(models$spikeslab)
    models$spikeslab$recipe
  })  
  
  # output spikeslab_ModelSummary2 (print) ----
  output$spikeslab_ModelSummary2 <- renderPrint({
    req(models$spikeslab)
    summary(models$spikeslab$finalModel)
  })
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
})
