library(shiny)
library(lmtest)

server <- shinyServer(function(input, output) {
    
    datasetInput <- reactive({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote)
    })
    
    output$sum <- renderPrint({
      summaryOutput <- summary(datasetInput())
      if(length(summaryOutput)==3){invisible()} else {summaryOutput}
      
    })
    
    output$varselector <- renderUI({
      tagList(
        selectInput(inputId="yvar", label="dependent variable (y axis):", 
                    choices = names(datasetInput()), selected=names(datasetInput())[[2]]),
        selectInput(inputId="xvar", label="independent variable (x axis):", 
                    choices = names(datasetInput()), selected=names(datasetInput())[[1]])        
      )
    })
    
    PlotType <- reactive({
      if (is.numeric(datasetInput()[[input$yvar]]) & is.factor(datasetInput()[[input$xvar]])){return("box plot:")}
      if (is.numeric(datasetInput()[[input$yvar]]) & is.numeric(datasetInput()[[input$xvar]])){return("scatter plot:")}
      if (is.factor(datasetInput()[[input$yvar]]) & is.factor(datasetInput()[[input$xvar]])){return("bar plot:")}
    })
    
	XTitle <- reactive({paste(input$xvar)})
	YTitle <- reactive({paste(input$yvar)})

    output$Xsettings <- renderUI({
	if(is.numeric(datasetInput()[[input$xvar]]) != TRUE) {invisible()} else {
   		 sliderInput("inXSlider", "Range of values in histogram", min=range(datasetInput()[[input$xvar]])[1], max=range(datasetInput()[[input$xvar]])[2], value=range(datasetInput()[[input$xvar]]))
			}
	})

    output$Ysettings <- renderUI({
	if(is.numeric(datasetInput()[[input$yvar]]) != TRUE) {invisible()} else {
    		sliderInput("inYSlider", "Range of values in histogram", min=range(datasetInput()[[input$yvar]])[1], max=range(datasetInput()[[input$yvar]])[2], value=range(datasetInput()[[input$yvar]]))
			}
	})

	SummaryPlotX <- function(){
		if (is.numeric(datasetInput()[[input$xvar]])){hist(datasetInput()[[input$xvar]], xlab=XTitle(), xlim=input$inXSlider)}
		if (is.factor(datasetInput()[[input$xvar]])){plot(datasetInput()[[input$xvar]], xlab=XTitle())}
		}
		
	SummaryPlotY <- function(){
		if (is.numeric(datasetInput()[[input$yvar]])){hist(datasetInput()[[input$yvar]], xlab=YTitle(), xlim=input$inYSlider)}
		if (is.factor(datasetInput()[[input$yvar]])){plot(datasetInput()[[input$yvar]], xlab=YTitle())}
		}
	
    output$PlotType <- renderText({
        PlotType()
    })
    
    dataobject <- reactive({
      if (PlotType()=="bar plot:"){
        datatabletemp <- table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]])
        if(input$propCheck=="relative"){
          datatabletemp <- prop.table(datatabletemp, 2)
        }
        datatabletemp
      }
    })
    output$TEST <- renderTable(datatable())
    
    Plotcode <- function(){
      if (!(is.null(PlotType()))){
        # create a boxplot if y variable is numeric and x variable is categorical
        if (PlotType()=="box plot:"){
          boxplot(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]],
                  xlab=input$xvar,
                  ylab=input$yvar,
                  main = input$titleInput,
                  notch=input$notchCheck)
        }
        # create a scatterplot if y variable is numeric and x variable is numeric
        if (PlotType()=="scatter plot:"){
          plot(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]],
               xlab=input$xvar,
               ylab=input$yvar,
                  main = input$titleInput)
          if (input$regrCheck){
            abline(lm(datasetInput()[[input$yvar]]~datasetInput()[[input$xvar]]))
          }
        }
        # create a barplot if y variable is categorical and x variable is categorical
        if (PlotType()=="bar plot:"){
          barplot(dataobject(),
                  xlab = input$xvar,
                  ylab = input$yvar,
                  main = input$titleInput,
                  beside = input$besideCheck,
                  legend = input$leg)
        }
      }
    }
    
    output$Plot <- renderPlot({
      Plotcode()
    })

	output$SumPlotX <- renderPlot({
      SummaryPlotX()
    })

	output$SumPlotY <- renderPlot({
      SummaryPlotY()
    })
	
    output$downloadData <- downloadHandler(
      filename = function(){
        paste(input$yvar, "By", input$xvar, '.png', sep='')
      },
      content = function(file) {
        png(file)
        Plotcode()
        dev.off()
      },
      contentType = "image/png"
    )
    
    Testcode <- reactive({
      if (!(is.null(PlotType()))){
        # t-test logic for numeric-categorial (two levels): if parameters fulfilled, do t-test, if not, do Wilcoxon-Mann-Whitney
        if (PlotType()=="box plot:" & length(levels(datasetInput()[[input$xvar]]))==2 ){
		  normality <- aggregate(formula = datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]], FUN = function(x) {y <- shapiro.test(x); c(y$statistic, y$p.value)})
		  variance <- var.test(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]], ratio = 1, alternative = "two.sided", conf.level = 0.95,)
		  # t-test parameters fulfilled
		  if (range(normality[,2][,2])[1] >= 0.05 & variance$p.value >= 0.05){
          results <- t.test(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]])
          output$Signalert <- renderText({
            ifelse(results$p.value<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
          })}
		  # t-test parameters notfulfilled
		  if (range(normality[,2][,2])[1] < 0.05 | variance$p.value < 0.05) {
          results <- wilcox.test(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]])
          output$Signalert <- renderText({
            ifelse(results$p.value<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
          })}
        }
        # linear regression for numeric-categorial (more levels)
        if (PlotType()=="box plot:" & length(levels(datasetInput()[[input$xvar]]))>2 ){
          results <- summary(lm(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]]))
          pvalue <- pf(results$fstatistic[1], results$fstatistic[2], results$fstatistic[3], lower.tail =F)
          output$Signalert <- renderText({
            ifelse(pvalue<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(pvalue<0.001, "< 0.001", paste("= ", (round(pvalue,3)), sep="")), sep="")
          })
        }
		
        # correlation test for numeric-numeric: if parameters fulfilled, do Pearson-r, if not, do Kendall-tau
        if (PlotType()=="scatter plot:"){
		  scedasticity <- bptest(lm(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]]))
		  normality <- lapply(data.frame(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]) , function(x) {y <- shapiro.test(x); c(y$p.value)})
          
		  if (range(normality)[1] >= 0.05 & scedasticity$p.value >= 0.05) {
		  results <- cor.test(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]], method="pearson")
          output$Signalert <- renderText({
            ifelse(results$p.value<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
          })}
		  if (range(normality)[1] < 0.05 | scedasticity$p.value <0.05) {
		  results <- cor.test(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]], method="kendall")
          output$Signalert <- renderText({
            ifelse(results$p.value<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
          })}
        }
        # categorial-categorial: if parameters fulfilled, do a chi square test, if not do fisher's exact
        if (PlotType()=="bar plot:"){
		  if (range(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))[1] > 5) {		
          results <- chisq.test(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))
          output$Signalert <- renderText({
            ifelse(results$p.value<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
          })}
		if (range(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))[1] <= 5) {		
          results <- fisher.test(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))
          output$Signalert <- renderText({
            ifelse(results$p.value<=0.05, "significant", "not significant")
          })
          output$Pvalue <- renderText({
            paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
          })}
        }
        return(results)
      }
    })
    
    output$Testresults <- renderPrint({
      Testcode()
    })
})


shinyApp(ui, server)