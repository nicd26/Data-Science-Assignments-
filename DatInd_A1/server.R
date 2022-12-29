shinyServer(function(input, output) {

    output$pairGraph <- renderPlot({
      colourchoice <- input$Colours
      pairdata <- as.data.frame(dat[input$VariablesP])
        ggpairs(pairdata, mapping = aes(colour = as.matrix(dat[colourchoice])), title = "Correlation between Data Points")
    })
    output$mytable = renderDataTable({
      if(input$numericals == TRUE){
        if(input$categoricals == TRUE){
          dat
        }else{
          dat[,c(1, 15:44)]
        }
      }else if(input$categoricals == TRUE){
        dat[,c(2:14)]
      }else{
        dat
      }
    })
    output$SummaryA2 <- renderPrint({
      print(dfSummary(dat))
    })
    output$Boxplot <- renderPlot({
      data <- as.matrix(dat[input$VariablesB])
      data <- scale(data, center = input$standardise, scale = input$standardise)
      Boxplot(y = data, xlab = "Variables", ylab = "Values", use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                   horizontal = FALSE, outline = input$outliers, 
                   col = turbo(n = length(input$VariablesB)),
                   range = input$range, main = "Boxplots of Assignment 1 data",
                   id = FALSE)
    
    })
    output$Missing <- renderPlot({
      missdata <- as.data.frame(dat[input$VariablesM])
      vis_miss(missdata, cluster = input$cluster) +
        labs(title = "Missingness of Assignment 1 data")
    })
    output$Corrgram <- renderPlot({
      corrdata <- as.data.frame(dat[input$VariablesC])
      corrgram(corrdata, 
               order = input$Group2, 
               abs = input$abs2, 
               cor.method = input$CorrMeth2,
               text.panel = panel.txt,
               main = "Correlation of Assignment 1 data")
    })
    output$Mosaic <- renderPlot({
      formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
      mosaic(formula, data = dat[,c(3, 5:14)],
                  main = "Mosiac of Data", shade = TRUE, legend = TRUE)
    })
    output$Values <- renderPlot({
      valuedata <- as.data.frame(dat[input$VariablesR])
      valuedata <- scale(valuedata, center = input$standardise1, scale = input$standardise1)
      n = nrow(dat)
      chartcol <- turbo(n = length(input$VariablesR))
      plot((1:length(sort(valuedata[,1])))/n, sort(valuedata[,1]), type="l", lwd = 1.5,
           main = "Visualizing Percentiles",
           xlab = "Percentile",
           ylab = "Value",
           xlim=c(0,1),
           ylim=c(min(valuedata, na.rm = TRUE), max(valuedata, na.rm = TRUE)),
           col=chartcol[1])
      if(length(input$VariablesR) > 1) {
        for(x in 2:length(input$VariablesR)){
          lines(1:length(sort(valuedata[,x]))/n, sort(valuedata[,x]), col=chartcol[x], lwd = 1.5)
          legend(x = "topleft", input$VariablesR, lwd = 2, col=chartcol[1:x])
        }
      }
    })

})


