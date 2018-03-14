library(shiny)
library(lmtest)
library(ggplot2)

shinyServer(function(input, output) {
    
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
	if(is.numeric(datasetInput()[[input$xvar]]) != TRUE) {invisible()} else {tagList(
   		 sliderInput("inXSlider", "Range of values in histogram", min=range(datasetInput()[[input$xvar]])[1], max=range(datasetInput()[[input$xvar]])[2], value=range(datasetInput()[[input$xvar]])),
		 sliderInput("inXBinSlider", "Number of bars displayed in histogram", min=5, max=25, value=10)
			)}
	})

    output$Ysettings <- renderUI({
	if(is.numeric(datasetInput()[[input$yvar]]) != TRUE) {invisible()} else {tagList(
    	sliderInput("inYSlider", "Range of values in histogram", min=range(datasetInput()[[input$yvar]])[1], max=range(datasetInput()[[input$yvar]])[2], value=range(datasetInput()[[input$yvar]])),
		sliderInput("inYBinSlider", "Number of bars displayed in histogram", min=5, max=25, value=10)

			)}
	})

	SummaryPlotX <- function(){
		if (is.numeric(datasetInput()[[input$xvar]])){
			Xdata <- datasetInput()[[input$xvar]]
			Xdata <- Xdata[Xdata>input$inXSlider[1] & Xdata<input$inXSlider[2]]
			hist(Xdata, xlab=XTitle(), breaks=seq(min(Xdata), max(Xdata), l=input$inXBinSlider+1), main=XTitle())}
		if (is.factor(datasetInput()[[input$xvar]])){plot(datasetInput()[[input$xvar]], xlab=XTitle())}
		}
		
	SummaryPlotY <- function(){
		if (is.numeric(datasetInput()[[input$yvar]])){
			Ydata <- datasetInput()[[input$yvar]]
			Ydata <- Ydata[Ydata>input$inYSlider[1] & Ydata<input$inYSlider[2]]
			hist(Ydata, xlab=YTitle(),breaks=seq(min(Ydata), max(Ydata), l=input$inYBinSlider+1), main=YTitle())}
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
    
	output$PlotSettings <- renderUI({
		if (PlotType()=="bar plot:"){tagList(
				checkboxInput("leg", "Legend", TRUE),
				checkboxInput("besideCheck", "Do not stack", FALSE),
				radioButtons("propCheck", "",	choices = c("absolute", "relative"), selected = "absolute")
				)
			}
		else if (PlotType()=="box plot:"){tagList(
				checkboxInput("notchCheck", "Notches", FALSE)
				)
			}
		else if (PlotType()=="scatter plot:"){tagList(
				checkboxInput("regrCheck", "Regression line", FALSE),
				checkboxInput("regrSE", "Display standard error", FALSE)
				)
			}
		 
		else {invisible()}
	})	
	
    Plotcode <- function(){
      if (!(is.null(PlotType()))){
        # create a boxplot if y variable is numeric and x variable is categorical
        if (PlotType()=="box plot:"){
			outPlot <- ggplot(datasetInput(), aes(datasetInput()[[input$xvar]], datasetInput()[[input$yvar]])) +
				geom_boxplot(notch=input$notchCheck) +
				scale_x_discrete(name=input$xvar) + 
				scale_y_continuous(name=input$yvar)+
				ggtitle(input$titleInput)+
				theme_bw()+
				theme(plot.title = element_text(hjust=0.5, face="bold", size=14))
			
			return(outPlot)
			
        }
        # create a scatterplot if y variable is numeric and x variable is numeric
        if (PlotType()=="scatter plot:"){
			outPlot <- ggplot(datasetInput(), aes(datasetInput()[[input$xvar]], datasetInput()[[input$yvar]])) +
				geom_point() +
				scale_x_continuous(name=input$xvar) + 
				scale_y_continuous(name=input$yvar)+
				ggtitle(input$titleInput)+
				theme_bw()+
				theme(plot.title = element_text(hjust=0.5, face="bold", size=14))
			
			if (input$regrCheck==TRUE){
				outPlot <- outPlot + geom_smooth(method=lm, se=input$regrSE)
				}
			return(outPlot)		

        }
        # create a barplot if y variable is categorical and x variable is categorical
        if (PlotType()=="bar plot:"){
			outPlot <- ggplot(datasetInput(), aes(datasetInput()[[input$xvar]], fill=datasetInput()[[input$yvar]])) +
				scale_x_discrete(name=input$xvar) + 
				scale_fill_discrete(name = input$yvar) +
				ggtitle(input$titleInput)+
				theme_bw()+
				scale_fill_grey(start=0, end=0.9) +
				theme(plot.title = element_text(hjust=0.5, face="bold", size=14))
			
			if (input$besideCheck==TRUE){
				outPlot <- outPlot + geom_bar(position=position_dodge())
				}	
			else {outPlot <- outPlot + geom_bar(position=position_stack())}
			if (input$leg==TRUE){
				outPlot <- outPlot + theme(legend.position="right")
				}	
			else {outPlot <- outPlot + theme(legend.position="none")}			
			
			return(outPlot)
			
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
   

# DETERMINE THE TYPE OF TEST USED   
	TestType <- reactive({
	if (!(is.null(PlotType()))){
		if (PlotType()=="box plot:" & length(levels(datasetInput()[[input$xvar]]))==2 ){
			normality <- aggregate(formula = datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]], FUN = function(x) {y <- shapiro.test(x); c(y$statistic, y$p.value)})
			variance <- var.test(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]], ratio = 1, alternative = "two.sided", conf.level = 0.95,)
			if (range(normality[,2][,2])[1] >= 0.05 & variance$p.value >= 0.05){
				return("t")} #TTEST
			if (range(normality[,2][,2])[1] < 0.05 | variance$p.value < 0.05) {
				return("wilcox")}#WILCOX TEST
			}
		if (PlotType()=="box plot:" & length(levels(datasetInput()[[input$xvar]]))>2 ){
			return("linreg")}#LINEAR REGRESSION
			
		if (PlotType()=="scatter plot:"){
			scedasticity <- bptest(lm(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]]))
			normality <- lapply(data.frame(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]) , function(x) {y <- shapiro.test(x); c(y$p.value)})	
			if (range(normality)[1] >= 0.05 & scedasticity$p.value >= 0.05) {
				return("pearson")}#PEARSON CORELATION
			if (range(normality)[1] < 0.05 | scedasticity$p.value <0.05) {
				return("kendall")}#KENDALL
			}
        if (PlotType()=="bar plot:"){			
			if (range(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))[1] > 5) {	
				return("chisq")}#CHISQ
			if (range(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))[1] <= 5) {	
				return("fisher")}#FISHER
			}}
	else {return("none")}
	})
	

# CHOOSE SETTINGS TO DISPLAY DEPENDING ON THE TYPE OF TEST USED
	
	output$TestSettings <- renderUI({
		CurrentTestType <- TestType()
		if (CurrentTestType == "t") {tagList(
			h2("test settings:"),
			checkboxInput("Paired", "Paired data (for example before/after an experiment)", FALSE)
			)}
		else if (CurrentTestType == "wilcox") {tagList(
			h2("test settings:"),
			checkboxInput("Paired", "Paired data (for example before/after an experiment)", FALSE)
			)}
		else if (CurrentTestType == "linreg") {invisible()}
		else if (CurrentTestType == "pearson") {tagList(
			h2("test settings:"),
			checkboxInput("Linear", "The relationship between the data is linear (the plot displays a line, not a curve)", TRUE),
			checkboxInput("Continuous", "None of the variables is ordinal (like ranking: 1st, 2nd, etc.)", TRUE),
			checkboxInput("Outliers", "There are no outliers in the data", TRUE)
			)}
		else if (CurrentTestType == "spearman") {invisible()}
		else if (CurrentTestType == "chisq") {invisible()}
		else if (CurrentTestType == "fisher") {invisible()}
		else {invisible()}
		})

###TESTCODE NEEDS TO BE SPLIT INTO TEST-DEFINING CODE AND TEXT EXECUTING CODE TO AVOID ISSUES WITH RESETTING
    Testcode <- reactive({
      if (!(is.null(PlotType()))){
	  	CurrentTestType <- TestType()
        # t-test logic for numeric-categorial (two levels): if parameters fulfilled, do t-test, if not, do Wilcoxon-Mann-Whitney
        
		if (CurrentTestType  == "t") {
			output$TestDescription <- renderText({
			"The assumptions required for a t-test (normal distribution of variables, variance) seem to be fulfilled. However, you need to verify whether your data is a random representative selection from the whole data (or the whole data)." 
			 })
			results <- t.test(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]], paired=input$Paired)
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
			})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
			})
		}

		# t-test parameters not fulfilled
		if (CurrentTestType  == "wilcox") {
			output$TestDescription <- renderText({
				"Some of the assumptions required for a t-test (normal distribution of variables, variance) seem not fulfilled. A non-parametric test will thus be performed." 
				})
			results <- wilcox.test(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]], paired=input$Paired)
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
				})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
			})
		}
		
        # linear regression for numeric-categorial (more levels)
        if (CurrentTestType  == "linreg"){
			output$TestDescription <- renderText({"Linear regression was selected for the type of variables chosen"})
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
        if (CurrentTestType  == "pearson") {
			output$TestDescription <- renderText({"Automatically tested assumptions made by Pearson's r are met. Some assumptions need to be verified manually."})
			if (input$Linear==TRUE & input$Continuous==TRUE & input$Outliers==TRUE){
				results <- cor.test(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]], method="pearson")}
			else if (input$Outliers==TRUE & input$Linear==FALSE | input$Continuous==FALSE){
				results <- cor.test(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]], method="spearman")}
			else {
				results <- "none"}
			
			if (results != "none"){
				output$Signalert <- renderText({
					ifelse(results$p.value<=0.05, "significant", "not significant")
					})
				output$Pvalue <- renderText({
					paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
					})
				}
			else {
				output$Signalert <- renderText({"Testing not possible"})
				output$Pvalue <- renderText({"Please remove outliers before uploading data."})
				}
			}
		if (CurrentTestType  == "spearman") {
			output$TestDescription <- renderText({"Automatically tested assumptions made by Pearson's r are not met. Measuring by Spearman's rho"})
			results <- cor.test(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]], method="spearman")
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
				})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
				})
			}
        
        # categorial-categorial: if parameters fulfilled, do a chi square test, if not do fisher's exact
        if (CurrentTestType  == "chisq"){
			output$TestDescription <- renderText({"Assumptions of chi-squared test are met."})	
			results <- chisq.test(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
				})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
				})
			}
		if (CurrentTestType  == "fisher") {
			output$TestDescription <- renderText({"Assumptions of chi-squared test are not met. Performing Fisher's exact test instead."})		
			results <- fisher.test(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]))
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
				})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
				})
			}
			
		if (CurrentTestType  == "none") {
			output$TestDescription <- renderText({"There is currently no test for this combination of variables"})		
			output$Signalert <- renderText({""})
			output$Pvalue <- renderText({""})
			}
        return(results)
		}
		
    })

    output$Testresults <- renderPrint({
      Testcode()
    })
})

