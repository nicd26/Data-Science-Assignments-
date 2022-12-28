shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Nicole Dunn"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         tabPanel("NNet Model",
                                  verbatimTextOutput(outputId = "nnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "nnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(nnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = nnet_initial), 
                                           bsTooltip(id = "nnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Go", label = "Train", icon = icon("play")), 
                                           bsTooltip(id = "nnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "nnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "nnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "nnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "nnet_ModelTree"),
                                  verbatimTextOutput(outputId = "nnet_Recipe"),
                                  verbatimTextOutput(outputId = "nnet_ModelSummary2")
                                  
                         ),
                         tabPanel("avNNet Model",
                                  verbatimTextOutput(outputId = "avNNet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "avNNet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(avNNet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = avNNet_initial), 
                                           bsTooltip(id = "avNNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "avNNet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "avNNet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "avNNet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "avNNet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "avNNet_ModelPlots"),
                                  verbatimTextOutput(outputId = "avNNet_Recipe"),
                                  verbatimTextOutput(outputId = "avNNet_ModelSummary2")
                         ),
                         
                         tabPanel("QRF Model",
                                  verbatimTextOutput(outputId = "qrf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "qrf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(qrf_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = qrf_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "qrf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "qrf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "qrf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "qrf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "qrf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "qrf_ModelPlots"),
                                  plotOutput(outputId = "qrf_ModelTree"),
                                  verbatimTextOutput(outputId = "qrf_Recipe")

                                  
                         ),
                         tabPanel("GLM Model",
                                  verbatimTextOutput(outputId = "glm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "glm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glm_initial), 
                                           bsTooltip(id = "glm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glm_Go", label = "Train", icon = icon("play")), 
                                           bsTooltip(id = "glm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glm_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "glm_Recipe"),
                                  verbatimTextOutput(outputId = "glm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glm_Coef")  
                                  )
                                  
                         ),
                         
                         tabPanel("BayesGLM Model",
                                  verbatimTextOutput(outputId = "bayesglm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "bayesglm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bayesglm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bayesglm_initial), 
                                           bsTooltip(id = "bayesglm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bayesglm_Go", label = "Train", icon = icon("play")), 
                                           bsTooltip(id = "bayesglm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bayesglm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bayesglm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bayesglm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bayesglm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bayesglm_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "bayesglm_Recipe"),
                                  verbatimTextOutput(outputId = "bayesglm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "bayesglm_Coef")  
                                  )
                                  
                         ),
                         
                         tabPanel("LeapSeq Model",
                                  verbatimTextOutput(outputId = "leapSeq_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "leapSeq_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(leapSeq_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = leapSeq_initial), 
                                           bsTooltip(id = "leapSeq_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "leapSeq_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "leapSeq_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "leapSeq_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "leapSeq_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "leapSeq_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "leapSeq_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "leapSeq_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "leapSeq_ModelPlots"),
                                  verbatimTextOutput(outputId = "leapSeq_Recipe"),
                                  verbatimTextOutput(outputId = "leapSeq_ModelSummary2")
                                  
                                  
                         ),
                         tabPanel("bagEarth Model",
                                  verbatimTextOutput(outputId = "bagEarth_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "bagEarth_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bagEarth_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bagEarth_initial), 
                                           bsTooltip(id = "bagEarth_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarth_Go", label = "Train", icon = icon("play")), 
                                           bsTooltip(id = "bagEarth_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarth_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bagEarth_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarth_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bagEarth_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bagEarth_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "bagEarth_ModelPlots"),
                                  verbatimTextOutput(outputId = "bagEarth_Recipe")
                                  
                         ),
                         
                         tabPanel("Cubist Model",
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "cubist_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(cubist_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = cubist_initial), 
                                           bsTooltip(id = "cubist_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "cubist_ModelPlots"),
                                  verbatimTextOutput(outputId = "cubist_Recipe")
                                  
                         ),
                         
                         tabPanel("kNN Model",
                                  verbatimTextOutput(outputId = "knn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "knn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(knn_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = knn_initial), 
                                           bsTooltip(id = "knn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Go", label = "Train", icon = icon("play")), 
                                           bsTooltip(id = "knn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "knn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "knn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "knn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "knn_ModelPlots"),
                                  verbatimTextOutput(outputId = "knn_Recipe")
                                  
                         ),
                         
                         tabPanel("Spikeslab Model",
                                  verbatimTextOutput(outputId = "spikeslab_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           
                                           selectizeInput(inputId = "spikeslab_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(spikeslab_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = spikeslab_initial), 
                                           bsTooltip(id = "spikeslab_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spikeslab_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "spikeslab_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spikeslab_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "spikeslab_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spikeslab_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "spikeslab_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "spikeslab_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "spikeslab_ModelPlots"),
                                  verbatimTextOutput(outputId = "spikeslab_Recipe")
                                  
                         )
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = TRUE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
