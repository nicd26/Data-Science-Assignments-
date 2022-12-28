shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Assignment 2 - Nicole Dunn"),

      tabsetPanel(
        tabPanel("Overall Summary and Data",
               h3("Assignment 2 Dataset"),
               tabsetPanel(
                 tabPanel("Summary",
                          checkboxInput(inputId = "sum_clean", label = "Clean Data", value = FALSE),
                          verbatimTextOutput(outputId = "SummaryA2")
                 ),
                 tabPanel("Raw Data",
                          dataTableOutput(outputId = "mytable")
                 ))),
        tabPanel("Exploratory Data Analysis",
              h3("Assignment 2 Raw Data Graphs"),
              tabsetPanel(
                 tabPanel("Missing Data Graph",
                          withSpinner(
                            plotOutput(outputId = "RAWMissing")
                          ),
                          checkboxInput(inputId = "Rcluster", label = "Cluster missingness", value = FALSE),
                          hr()),
                 tabPanel("Box Plot",
                          selectizeInput(inputId = "RVariablesB", label = "Show variables:", choices = boxnames, multiple = TRUE, selected = boxchoices),
                          withSpinner(
                            plotOutput(outputId = "RAWBoxplot")
                          ),
                          checkboxInput(inputId = "Rstandardise", label = "Show standardized", value = FALSE),
                          checkboxInput(inputId = "Routliers", label = "Show outliers", value = TRUE),
                          sliderInput(inputId = "Rrange", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                          hr()),
                 tabPanel("Correlation Graph",
                          selectizeInput(inputId = "RVariablesC", label = "Show variables:", choices = namescorr, multiple = TRUE, selected = corrchoices),
                          withSpinner(
                            plotOutput(outputId = "RAWCorrgram")
                          ),
                          checkboxInput(inputId = "Rabs2", label = "Uses absolute correlation", value = TRUE),
                          selectInput(inputId = "RCorrMeth2", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                          selectInput(inputId = "RGroup2", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                          ),
                 tabPanel("Mosaic",
                          withSpinner(column(8, align="center",
                            plotOutput(outputId = "RAWMosaic"))
                          )),
                 tabPanel("Rising Value Chart",
                          selectizeInput(inputId = "RVariablesR", label = "Show variables:", choices = boxnames, multiple = TRUE, selected = valuechoices),
                          withSpinner(
                            plotOutput(outputId = "RAWValues")
                          ),
                          checkboxInput(inputId = "Rstandardise1", label = "Show standardized", value = FALSE),
                          checkboxInput(inputId = "Rshow_legend", label = "Legend", value = FALSE),
                          ),
                 tabPanel("Pairs Chart",
                          selectizeInput(inputId = "RVariablesP", label = "Show variables:", choices = pairsnames, multiple = TRUE, selected = pairschoices),
                          withSpinner(
                            plotOutput(outputId = "RAWpairGraph")
                          ),
                          selectInput(inputId = "RColours", label = "Colouring", choices = morecolours), selected = "POLITICS"
                          ))),

                 
         tabPanel("Cleaned Data Analysis",
                  h3("Assignment 2 Cleaned Data Graphs"),
                  tabsetPanel(
                    tabPanel("Missing Data Graph",
                             sidebarPanel("Thresholds will change all graphs within the 'Cleaned Data Graphs' tab",
                               sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                                           min = 1, max = 100, value = 50, post = "%"),
                               sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness", 
                                           min = 1, max = 100, value = 50, post = "%"),
                               selectInput(inputId = "ImpMethod", label = "Imputation method", 
                                           choices = c("None", "KNN", "Partial Del","Median"), selected = "None"),
                               actionButton(inputId = "Go", label = "Train", icon = icon("play"))
                             ),
                             mainPanel(
                               withSpinner(
                                 plotOutput(outputId = "Missing")
                               ),
                               withSpinner(
                                 verbatimTextOutput(outputId = "Summary")
                               ))),

                    tabPanel("Box Plot", 
                             selectizeInput(inputId = "VariablesB", label = "Show variables:", choices = boxnames, multiple = TRUE, selected = boxchoices),
                             withSpinner(
                               plotOutput(outputId = "Boxplot")
                             ),
                             checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                             checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                             sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                             hr()),
                    
                    tabPanel("Correlation Graph",
                             selectizeInput(inputId = "VariablesC", label = "Show variables:", choices = namescorr, multiple = TRUE, selected = corrchoices),
                             withSpinner(
                               plotOutput(outputId = "Corrgram")
                             ),
                             checkboxInput(inputId = "abs2", label = "Uses absolute correlation", value = TRUE),
                             selectInput(inputId = "CorrMeth2", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                             selectInput(inputId = "Group2", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                    ),
                    tabPanel("Mosaic",
                             withSpinner(column(8, align="center",
                               plotOutput(outputId = "Mosaic"))
                             )),
                    tabPanel("Rising Value Chart",
                             selectizeInput(inputId = "VariablesR", label = "Show variables:", choices = boxnames, multiple = TRUE, selected = valuechoices),
                             withSpinner(
                               plotOutput(outputId = "Values")
                             ),
                             checkboxInput(inputId = "standardise1", label = "Show standardized", value = FALSE),
                             checkboxInput(inputId = "show_legend", label = "Legend", value = FALSE)
                    ),
                    tabPanel("Pairs Chart",
                             selectizeInput(inputId = "VariablesP", label = "Show variables:", choices = pairsnames, multiple = TRUE, selected = pairschoices),
                             withSpinner(
                               plotOutput(outputId = "pairGraph")
                             ),
                             selectInput(inputId = "Colours", label = "Colouring", choices = morecolours), selected = "POLITICS"
                    ),
                    tabPanel("Regression Tree",
                             withSpinner(
                               plotOutput(outputId = "treeGraph")
                             )),
                    tabPanel("GLMNET Results",
                             
                             withSpinner(
                               uiOutput(outputId = "glmResults")
                             ),
                             tabsetPanel(
                               tabPanel("Test Results",
                             htmlOutput(outputId = "TestResults")),
                             tabPanel("Coefficients",
                             verbatimTextOutput(outputId = "coefText"))),
                             tabsetPanel(
                              tabPanel("Residual Box Plots",
                                       withSpinner(
                                         plotOutput(outputId = "testBox")),
                                         checkboxInput(inputId = "Boutliers", label = "Show outliers", value = TRUE),
                                         sliderInput(inputId = "Brange", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                                         hr()
                                       ),
                              tabPanel("Residual Data Frame",
                                       withSpinner(
                                         dataTableOutput(outputId = "residframe")
                                       ))))
                 )
                 )
        )
    )
  )
    


