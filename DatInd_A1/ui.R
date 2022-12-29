shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Assignment 1 - Nicole Dunn"),

      tabsetPanel(
        tabPanel("Overall Summary and Data",
               h3("Assignment 1 Dataset"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryA2")
                 ),
                 tabPanel("Raw Data",
                          checkboxInput(inputId = "numericals", label = "Numerical", value = TRUE),
                          checkboxInput(inputId = "categoricals", label = "Categorical", value = TRUE),
                          dataTableOutput(outputId = "mytable")
                 ))),
        tabPanel("Exploratory Data Analysis",
              h3("Assignment 1 Graphs"),
              tabsetPanel(
                 tabPanel("Box Plot", 
                          selectizeInput(inputId = "VariablesB", label = "Show variables:", choices = boxnames, multiple = TRUE, selected = boxchoices),
                          withSpinner(
                            plotOutput(outputId = "Boxplot")
                          ),
                          checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                          checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                          sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                          hr()),
                 tabPanel("Missing Data Graph", 
                          selectizeInput(inputId = "VariablesM", label = "Show variables:", choices = choicesA, multiple = TRUE, selected = mischoices),
                          withSpinner(
                            plotOutput(outputId = "Missing")
                          ),
                          checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE),
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
                          selectizeInput(inputId = "VariablesA", label = "Show variables:", choices = namesmosaic, multiple = TRUE, selected = mosaicchoices),
                          withSpinner(
                            plotOutput(outputId = "Mosaic")
                          )),
                 tabPanel("Rising Value Chart",
                          selectizeInput(inputId = "VariablesR", label = "Show variables:", choices = boxnames, multiple = TRUE, selected = valuechoices),
                          withSpinner(
                            plotOutput(outputId = "Values")
                          ),
                          checkboxInput(inputId = "standardise1", label = "Show standardized", value = FALSE),
                          ),
                 tabPanel("Pairs Chart",
                          selectizeInput(inputId = "VariablesP", label = "Show variables:", choices = pairsnames, multiple = TRUE, selected = pairschoices),
                          withSpinner(
                            plotOutput(outputId = "pairGraph")
                          ),
                          selectInput(inputId = "Colours", label = "Colouring", choices = morecolours), selected = "Operator"),

                          
                    
                          
                 ))
        )
    )
    )


