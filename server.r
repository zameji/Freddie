library(shiny)
library(lmtest)
library(ggplot2)
library(Cairo)
shinyServer(function(input, output) {
	datafileInput <- reactive({
		  inFile <- input$file1
		  if (is.null(inFile))
			return(NULL)
		  read.csv(inFile$datapath, header=input$header, sep=input$sep, 
				   quote=input$quote)
		})
	datasetInput <- reactive({
		if (is.null(datafileInput()) == FALSE) {
			dataset <- data.frame(lapply(names(datafileInput()), function (variable) {
				if (is.factor(datafileInput()[[variable]]) == TRUE & input[[paste(variable, "type", sep="_")]] == "category") {datafileInput()[[variable]]}
				else if (is.numeric(datafileInput()[[variable]]) == TRUE & input[[paste(variable, "type", sep="_")]] == "number") {datafileInput()[[variable]]}
				else if (is.factor(datafileInput()[[variable]]) == TRUE & input[[paste(variable, "type", sep="_")]] == "number") {as.numeric(as.character(datafileInput()[[variable]]))}
				else if (is.numeric(datafileInput()[[variable]]) == TRUE & input[[paste(variable, "type", sep="_")]] == "category") {as.factor(datafileInput()[[variable]])}
				}			
				)
			)
			colnames(dataset) <- names(datafileInput())
			return(dataset)
		}
		else {NULL}
		})
	
    output$sum <- renderPrint({
		if(is.null(datafileInput()) == FALSE){
			summaryOutput <- summary(datasetInput())
			if(length(summaryOutput)==3){invisible()} 
			else {summaryOutput}}
		else {invisible()}
    })
	output$overrider <- renderUI({
		if (is.null(datafileInput()) == FALSE){
			tagList(
				lapply (names(datafileInput()), function (variable) {
						if (is.factor(datafileInput()[[variable]])) {radioButtons(paste(variable, "type", sep="_"), variable,	choices = c("category", "number"), selected = "category")}
						else {radioButtons(paste(variable, "type", sep="_"), variable,	choices = c("category", "number"), selected = "number")} 
						}
					)
				)
			}
		else {invisible()}
		})
	vals <- reactiveValues(keeprows=TRUE, markcases=FALSE)

	output$help <- renderUI({
		if (is.null(datafileInput()) == TRUE) {
				tagList(h3("Welcome to FREDDIE Shiny!"),
							h4("How to use it"),
							HTML('FREDDIE Shiny is an interface to allow you simple quantitative analyses of your data. In order to use it, you will need a dataset in the CSV format.
								For practice you can get some data <a href="https://1drv.ms/u/s!AiIxNMjRmvCLj-cpj0yUHcS9uDSLCQ">here</a>.'),
							p("The interface loads after you uploaded the dataset. If it displays errors, make sure that you select the correct settings for 'Separator', 'Quote sign' and 'Header' with respect to the CSV file you uploaded."),
							p("Once everything is ready, use the 'Data summary' tab to check that the individual variables were correctly recognized as categories or numbers."),
							h4("Creating plots"),
							p("Plots can be created in the tabs 'Variable summary' and 'Plot', depending whether you want to plot the distribution of one of your variables on its own or the relationship between your dependent and independent variables.
								The plot type is selected automatically and depends on the type of variable used. Still you have some options to determine the exact visuals of most of the plots. The plots used include:"),
							HTML('<ul>
								<li><a href="https://en.wikipedia.org/wiki/Histogram">Histogram</a></li>
								<li><a href="https://en.wikipedia.org/wiki/Box_plot">Boxplot</a></li>
								<li><a href="https://en.wikipedia.org/wiki/Scatter_plot">Scatterplot</a></li>
								<li><a href="https://en.wikipedia.org/wiki/Bar_chart">Bar chart</a></li>
								</ul>'),
							h4("Statistical testing"),
							p("Statistical tests are automated as far as possible, so you should not make any grave mistakes. Still, some of them require some input from you. This is always described together with the test in the correspondig tab"),
							h4("Procedure"),
							HTML('<ol>
								<li>Load your data in CSV format</li>
								<li>Verify that FREDDIE imported the data correctly, if not adjust the necessary settings.</li>
								<li>Choose your <b>independent (predicting) variable</b> - usually something like "age" or "gender"</li>
								<li>Choose your <b>dependent (predicted) variable</b> - usually something like "points" or "frequency"</li>
								<li>Explore the distribution of the variables on their own</li>
								<li>Look at their relationship</li>
								<li>If there are <a href="https://en.wikipedia.org/wiki/Outlier">outliers</a> in the data, use the outlier removal function to exclude them. <b> Only click the "Apply" button once!</b></li>
								<li>Adjust the settings of the selected statistical test, if needed.</li>
								<li>View the results of the test. Is there a significant relationship/difference?</li>
								</ol>')
				)
			}
		else {invisible()}
		})
    output$varselector <- renderUI({
		if (is.null(datafileInput()) == FALSE){
		  tagList(
			selectInput(inputId="yvar", label="dependent variable (y axis):", 
						choices = names(datafileInput()), selected=names(datafileInput())[[2]]),
			selectInput(inputId="xvar", label="independent variable (x axis):", 
						choices = names(datafileInput()), selected=names(datafileInput())[[1]])        
		  )}
		else {invisible()}
    })
    PlotType <- reactive({
      if (is.numeric(datasetInput()[[input$yvar]]) & is.factor(datasetInput()[[input$xvar]])){return("Box plot:")}
      if (is.numeric(datasetInput()[[input$yvar]]) & is.numeric(datasetInput()[[input$xvar]])){return("Scatter plot:")}
      if (is.factor(datasetInput()[[input$yvar]]) & is.factor(datasetInput()[[input$xvar]])){return("Bar plot:")}
	  if (is.factor(datasetInput()[[input$yvar]]) & length(levels(datasetInput()[[input$yvar]]))==2 & is.numeric(datasetInput()[[input$xvar]])){return("LogReg plot:")}
    })
	XTitle <- reactive({if(is.null(datasetInput()) == FALSE){paste(input$xvar)}})
	YTitle <- reactive({if(is.null(datasetInput()) == FALSE){paste(input$yvar)}})
    output$Xsettings <- renderUI({
		if(is.null(datasetInput()) == FALSE){
			if(is.numeric(datasetInput()[[input$xvar]]) != TRUE) {invisible()} else {tagList(
				 sliderInput("inXSlider", "Range of values in histogram", min=range(datasetInput()[[input$xvar]], na.rm=TRUE)[1], max=range(datasetInput()[[input$xvar]], na.rm=TRUE)[2], value=range(datasetInput()[[input$xvar]], na.rm=TRUE)),
				 sliderInput("inXBinSlider", "Number of bars displayed in histogram", min=5, max=25, value=10)
					)}
		}
	})
    output$Ysettings <- renderUI({
		if(is.null(datasetInput()) == FALSE){
			if(is.numeric(datasetInput()[[input$yvar]]) != TRUE) {invisible()} else {tagList(
				sliderInput("inYSlider", "Range of values in histogram", min=range(datasetInput()[[input$yvar]], na.rm=TRUE)[1], max=range(datasetInput()[[input$yvar]], na.rm=TRUE)[2], value=range(datasetInput()[[input$yvar]], na.rm=TRUE)),
				sliderInput("inYBinSlider", "Number of bars displayed in histogram", min=5, max=25, value=10)
					)}
		}
	})
	SummaryPlotX <- function(){
		if (!is.null(datasetInput()==TRUE)){
			if (is.numeric(datasetInput()[[input$xvar]])){
				Xdata <- datasetInput()[[input$xvar]]
				Xdata <- Xdata[Xdata>input$inXSlider[1] & Xdata<input$inXSlider[2]]
				g <- ggplot() +
					aes(Xdata) +
					geom_histogram(aes(fill=..count..), col="black", alpha=.5, breaks=seq(min(Xdata, na.rm=TRUE), max(Xdata, na.rm=TRUE), l=input$inXBinSlider+1)) +
					theme_bw() +
					theme(plot.title = element_text(hjust = 0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize)) +
					scale_fill_gradient("Count", low = "grey100", high = "grey50") +
					labs(title=paste("Distribution of ", tolower(XTitle()), sep="")) +
					labs(x=XTitle(),y="Frequency")
				return(g)
				}
			if (is.factor(datasetInput()[[input$xvar]])){
				ggplot(datasetInput(), aes(datasetInput()[[input$xvar]])) +
						scale_x_discrete(name=XTitle()) + 
						labs(title=paste("Distribution of ", tolower(XTitle()), sep="")) +
						theme_bw() +
						theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), legend.position="none", text=element_text(family=input$serif, size=input$fontSize)) +
						geom_bar(position=position_dodge(), aes(fill=..count..)) +
						scale_fill_gradient("Count", low = "grey75", high = "grey50")
				}
			}
		}
	SummaryPlotY <- function(){
		if (!is.null(datasetInput()==TRUE)){
			if (is.numeric(datasetInput()[[input$yvar]])){
				Ydata <- datasetInput()[[input$yvar]]
				Ydata <- Ydata[Ydata>input$inYSlider[1] & Ydata<input$inYSlider[2]]
				g <- ggplot() +
					aes(Ydata) +
					geom_histogram(aes(fill=..count..), col="black", alpha=.5, breaks=seq(min(Ydata, na.rm=TRUE), max(Ydata, na.rm=TRUE), l=input$inYBinSlider+1)) +
					theme_bw() +
					theme(plot.title = element_text(hjust = 0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize)) +
					scale_fill_gradient("Count", low = "grey100", high = "grey50") +
					labs(title=paste("Distribution of ", tolower(YTitle()), sep="")) +
					labs(x=YTitle(),y="Frequency")
				return(g)
				}
			if (is.factor(datasetInput()[[input$yvar]])){
				ggplot(datasetInput(), aes(datasetInput()[[input$yvar]])) +
					scale_x_discrete(name=YTitle()) + 
					labs(title=paste("Distribution of ", tolower(YTitle()), sep="")) +
					theme_bw() +
					theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), legend.position="none", text=element_text(family=input$serif, size=input$fontSize)) +
					geom_bar(position=position_dodge(), aes(fill=..count..)) +
					scale_fill_gradient("Count", low = "grey75", high = "grey50")
				}
			}
		else {invisible()}
		}
	output$PlotType <- renderText({
			PlotType()
    })
	output$PlotSettings <- renderUI({
		if (is.null(datasetInput()) == TRUE) {invisible()}
		else{
			if (PlotType()=="Bar plot:"){tagList(
					checkboxInput("leg", "Legend", TRUE),
					checkboxInput("besideCheck", "Do not stack", FALSE),
					radioButtons("propCheck", "",	choices = c("absolute", "relative"), selected = "absolute")
					)
				}
			else if (PlotType()=="Box plot:"){tagList(
					checkboxInput("notchCheck", "Notches", FALSE)
					)
				}
			else if (PlotType()=="Scatter plot:"){tagList(
					checkboxInput("regrCheck", "Regression line", FALSE),
					checkboxInput("regrSE", "Display standard error", FALSE)
					)
				}
			else if (PlotType()=="LogReg plot:") {tagList(
					checkboxInput("regrCheck", "Regression curve", FALSE),
					checkboxInput("regrSE", "Display standard error", FALSE)
					)
				}				
			else {invisible()}
		}
	})	
	output$OutlierFilter <- renderUI({
	if (is.null(datasetInput()) == TRUE) {invisible()}
	else {
			if (PlotType()=="Scatter plot:") 
					{tagList(
						h3("Outlier removal"),
						HTML('<p>If there are any <a href="https://en.wikipedia.org/wiki/Outlier">outliers</a> that you believe to be due to measurement error (their value is too large to be realist), 
							remove them by clicking on them or dragging a box and then clicking the "Select" button. To remove them from the statistical tests, click "Apply".</p>'),
						p("You should select ALL outliers before hitting the 'Apply' button. "),
						fluidRow(
							column(1, actionButton("exclude_toggle", "Select")),
							column(1, actionButton("exclude_reset", "Reset")),
							column(1, actionButton("apply_removal", "Apply", style="color: #fff; background-color: #009933;")),
							column(9, invisible())
							)
						)
					}	
			else if (PlotType()=="Box plot:") 
					{tagList(
						h3("Outlier removal"),
						HTML('<p>The boxplot chart is showing <a href="https://en.wikipedia.org/wiki/Outlier">outliers</a> as individual dots. Mark those outliers 
							that are too extreme to be realistic by clicking on them or dragging a box and then clicking the "Select" button. To remove them from the statistical tests, click "Apply".</p>'),
						p("You should select ALL outliers before hitting the 'Apply' button. If the redrawn chart shows new outlier-like cases, do not remove them anymore."),
						fluidRow(
							column(1, actionButton("exclude_toggle", "Select")),
							column(1, actionButton("exclude_reset", "Reset")),
							column(1, actionButton("apply_removal", "Apply", style="color: #fff; background-color: #009933;")),
							column(9, invisible())
							)
						)
					}
			else {invisible()}
			}
		})
    # dataobject <- reactive({
      # if (PlotType()=="Bar plot:"){
        # datatabletemp <- table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]])
        # if(input$propCheck=="relative"){
          # datatabletemp <- prop.table(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]), 2)
        # }
        # datatabletemp
      # }
    # })	
    Plotcode <- function(){
      if (!(is.null(PlotType()))){
        # create a boxplot if y variable is numeric and x variable is categorical
        if (PlotType()=="Box plot:"){
			keep    <- datasetInput()[ vals$keeprows, , drop = FALSE]
			exclude <- datasetInput()[!vals$keeprows, , drop = FALSE]
			mark <- datasetInput()[ vals$markcases, , drop = FALSE]
			outPlot <- ggplot(keep, aes(keep[[input$xvar]], keep[[input$yvar]])) +
				geom_boxplot(notch=input$notchCheck) +
				geom_point(data = mark, aes(mark[[input$xvar]], mark[[input$yvar]]), shape = 4, color = "red", size=3, stroke=1.5, alpha = 0.75) +
				geom_point(data = exclude, aes(exclude[[input$xvar]], exclude[[input$yvar]]), shape = 4, color = "black", size=2, alpha = 0.15) +
				scale_x_discrete(name=input$xvar) + 
				scale_y_continuous(name=input$yvar)+
				ggtitle(input$titleInput)+
				theme_bw()+
				theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize))
			return(outPlot)
        }
        # create a scatterplot if y variable is numeric and x variable is numeric
        if (PlotType()=="Scatter plot:"){
			keep    <- datasetInput()[ vals$keeprows, , drop = FALSE]
			exclude <- datasetInput()[!vals$keeprows, , drop = FALSE]	
			mark <- datasetInput()[ vals$markcases, , drop = FALSE]
			outPlot <- ggplot(keep, aes(keep[[input$xvar]], keep[[input$yvar]])) +
				geom_point(shape = 16) +
				geom_point(data = mark, aes(mark[[input$xvar]], mark[[input$yvar]]), shape = 4, color = "red", size=3, alpha = 0.75) +
				geom_point(data = exclude, aes(exclude[[input$xvar]], exclude[[input$yvar]]), shape = 4, color = "black", size=2, alpha = 0.15) +
				scale_x_continuous(name=input$xvar) + 
				scale_y_continuous(name=input$yvar)+
				ggtitle(input$titleInput)+
				theme_bw()+
				theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize))
			if (input$regrCheck==TRUE){
				outPlot <- outPlot + geom_smooth(method=lm, se=input$regrSE)
				}
			return(outPlot)		
        }
        # create a barplot if y variable is categorical and x variable is categorical
        if (PlotType()=="Bar plot:"){
			if (input$propCheck=="absolute"){
				outPlot <- ggplot(datasetInput(), aes(datasetInput()[[input$xvar]], fill=datasetInput()[[input$yvar]])) +
					scale_x_discrete(name=XTitle()) + 
					ggtitle(input$titleInput)+
					theme_bw()+
					scale_fill_grey(start=0.2, end=0.9, name = YTitle()) +
					theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize))
				if (input$besideCheck==TRUE){
					outPlot <- outPlot + geom_bar(position=position_dodge())
					}	
				else {outPlot <- outPlot + geom_bar(position=position_stack())}
				if (input$leg==TRUE){
					outPlot <- outPlot + theme(legend.position="right")
					}	
				else {outPlot <- outPlot + theme(legend.position="none")}}
			else {
				outPlot <- ggplot(datasetInput(), aes(datasetInput()[[input$xvar]], fill=datasetInput()[[input$yvar]])) +
					scale_x_discrete(name=XTitle()) + 
					ggtitle(input$titleInput) +
					theme_bw()+
					scale_fill_grey(start=0.2, end=0.9, name = YTitle()) +
					theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize))
				if (input$besideCheck==TRUE){
					outPlot <- outPlot + geom_bar(position=position_dodge())
					}	
				else {outPlot <- outPlot + geom_bar(position="fill")}
				if (input$leg==TRUE){
					outPlot <- outPlot + theme(legend.position="right")
					}	
				else {outPlot <- outPlot + theme(legend.position="none")}}
				return(outPlot)	
				}
		if (PlotType()=="LogReg plot:") {
			outPlot <- ggplot(datasetInput(),aes(datasetInput()[[input$xvar]],as.numeric(datasetInput()[[input$yvar]])-1)) +
				geom_point(position="identity") +
				scale_x_continuous(name=XTitle()) + 
				scale_y_continuous(name = YTitle(), expand=c(0.1,0.1), breaks=c(0,1), labels=levels(datasetInput()[[input$yvar]])) +
				ggtitle(input$titleInput) +
				theme_bw()+
				theme(plot.title = element_text(hjust=0.5, face="bold", size=round(input$fontSize*1.15)), text=element_text(family=input$serif, size=input$fontSize))				
			if (input$regrCheck==TRUE){
				outPlot <- outPlot + stat_smooth(method="glm", method.args = list(family="binomial"),formula=y~x, se=input$regrSE)
				}
			return(outPlot)	
			}
		else {outPlot <- NULL}	
			return(outPlot)
        }
      }
	# Toggle points that are clicked
	observeEvent(input$plot1_click, {
		res <- nearPoints(datasetInput(), input$plot1_click, xvar=input$xvar, yvar=input$yvar, allRows = TRUE)
		vals$markcases <- vals$markcases | res$selected_
	})
	observeEvent(input$exclude_toggle, {
		res <- brushedPoints(datasetInput(), input$plot1_brush, xvar=input$xvar, yvar=input$yvar, allRows = TRUE)
		vals$markcases <- vals$markcases | res$selected_
	})
	observeEvent(input$apply_removal, {
		vals$keeprows <- vals$keeprows & !vals$markcases
		vals$markcases <- FALSE
	})
	# Reset all points
	observeEvent(input$exclude_reset, {
		vals$keeprows <- TRUE
		vals$markcases <- FALSE
		})
	observeEvent(input$xvar, {
		vals$keeprows <- TRUE
		vals$markcases <- FALSE
		})
	observeEvent(input$yvar, {
		vals$keeprows <- TRUE
		vals$markcases <- FALSE
		})
    output$Plot <- renderPlot({
      if (is.null(Plotcode())!= TRUE) {Plotcode()
	  } else {plot(datasetInput()[[input$yvar]] ~ datasetInput()[[input$xvar]])}
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
		keep    <- datasetInput()[ vals$keeprows, , drop = FALSE]
		if (PlotType()=="Box plot:" & length(levels(keep[[input$xvar]]))==2 ){
			return("t")} #TTEST
		if (PlotType()=="Box plot:" & length(levels(keep[[input$xvar]]))>2 ){
			return("linreg")}#LINEAR REGRESSION
		if (PlotType()=="Scatter plot:"){
			scedasticity <- bptest(lm(keep[[input$yvar]] ~ keep[[input$xvar]]))
			normality <- lapply(data.frame(keep[[input$yvar]], keep[[input$xvar]]) , function(x) {y <- shapiro.test(x); c(y$p.value)})	
			if (range(normality)[1] >= 0.05 & scedasticity$p.value >= 0.05) {
				return("pearson")}#PEARSON CORELATION
			if (range(normality)[1] < 0.05 | scedasticity$p.value <0.05) {
				return("spearman")}#SPEARMAN
			}
        if (PlotType()=="Bar plot:"){			
			if (range(table(keep[[input$yvar]], keep[[input$xvar]]))[1] > 5) {	
				return("chisq")}#CHISQ
			if (range(table(keep[[input$yvar]], keep[[input$xvar]]))[1] <= 5) {	
				return("fisher")}#FISHER
			}}
	else {return("none")}
	})
# CHOOSE SETTINGS TO DISPLAY DEPENDING ON THE TYPE OF TEST USED
	output$TestSettings <- renderUI({
		CurrentTestType <- TestType()
		if (CurrentTestType == "t") {tagList(
			h3("Test settings:"),
			checkboxInput("Paired", "Paired data (for example before/after an experiment)", FALSE)
			)}
		else if (CurrentTestType == "linreg") {invisible()}
		else if (CurrentTestType == "pearson") {tagList(
			h3("Test settings:"),
			checkboxInput("Linear", "The relationship between the data is linear (the plot displays a line, not a curve)", TRUE),
			checkboxInput("Continuous", "None of the variables is ordinal (like ranking: 1st, 2nd, etc.)", TRUE),
			checkboxInput("Outliers", "There are no outliers in the data", TRUE)
			)}
		else if (CurrentTestType == "spearman") {tagList(
			h3("Test settings:"),
			checkboxInput("Outliers", "There are no outliers in the data", TRUE)
			)}
		else if (CurrentTestType == "chisq") {invisible()}
		else if (CurrentTestType == "fisher") {invisible()}
		else {invisible()}
		})
###TESTCODE NEEDS TO BE SPLIT INTO TEST-DEFINING CODE AND TEXT EXECUTING CODE TO AVOID ISSUES WITH RESETTING
    Testcode <- reactive({
      if (!(is.null(PlotType()))){
		keep    <- datasetInput()[ vals$keeprows, , drop = FALSE]
	  	CurrentTestType <- TestType()
        # t-test logic for numeric-categorial (two levels): if parameters fulfilled, do t-test, if not, do Wilcoxon-Mann-Whitney
		if (CurrentTestType  == "t") {
			output$TestDescription <- renderUI({
				HTML(
				"The variables you selected could be used for testing through a <b>two-sample t-test</b> or <b>Wilcoxon rank-sum test</b>.
				This test takes the two groups in your data (e.g. male/female) and compares their values.
				It determines the probability, with which the population from which you took the samples have different means.
				For example, if you study the number of different words (type count) in essays by beginner and advanced students, it will tell you - on the base of your samples - how likely is is, that beginner and advanced students use the same number of types in their essays.<br/><br/>
				If your values are <b>paired</b> - for example they contain the test scores of a group of students before a class and after it, select the corresponding option.")
			 })
			if (input$Paired==TRUE){
					normality <- aggregate(formula = keep[[input$yvar]] ~ keep[[input$xvar]], FUN = function(x) {y <- shapiro.test(x); c(y$statistic, y$p.value)})
					variance <- var.test(keep[[input$yvar]] ~ keep[[input$xvar]], ratio = 1, alternative = "two.sided", conf.level = 0.95,)
					if (range(normality[,2][,2])[1] >= 0.05 & variance$p.value >= 0.05){
						results <- t.test(keep[[input$yvar]] ~ keep[[input$xvar]], paired=input$Paired)} #TTEST
					if (range(normality[,2][,2])[1] < 0.05 | variance$p.value < 0.05) {
						results <- wilcox.test(keep[[input$yvar]] ~ keep[[input$xvar]], paired=input$Paired)}#WILCOX TEST}
						}
			if (input$Paired==FALSE){
					normality <- shapiro.test(keep[[input$yvar]])
					variance <- var.test(keep[[input$yvar]] ~ keep[[input$xvar]], ratio = 1, alternative = "two.sided", conf.level = 0.95,)
					if (normality >= 0.05 & variance$p.value >= 0.05){
						results <- t.test(keep[[input$yvar]] ~ keep[[input$xvar]], paired=input$Paired)} #TTEST
					if (normality < 0.05 | variance$p.value < 0.05) {
						results <- wilcox.test(keep[[input$yvar]] ~ keep[[input$xvar]], paired=input$Paired)}#WILCOX TEST}
						}
				output$Signalert <- renderText({
					ifelse(results$p.value<=0.05, "significant", "not significant")
					})
				output$Pvalue <- renderText({
					paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
					})
		}
        # linear regression for numeric-categorial (more levels)
        if (CurrentTestType  == "linreg"){
			output$TestDescription <- renderUI({
				HTML(
				"You selected a combination of variables that does not allow for a simple test. Because of that, <b>linear regression</b> a slightly more advanced model is used.
				If you do not understand the output presented below, it would be safer not to use such variables for statistic testing.")
				})
			results <- summary(lm(keep[[input$yvar]] ~ keep[[input$xvar]]))
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
			output$TestDescription <- renderUI({
				HTML(
				"The variables you selected could be used for testing through a <b>correlation test</b>.
				This test takes the two groups of 'scores' (e.g. age/first formant) and compares their values.
				It determines the strength of the relationship between them - how well does knowing the value of X allow us to predict Y - and determines whether the relationship may be purely 
				coincidental (e.g. if you have few examples).<br/><br/>
				Some of the prerequisites of this test were tested automatically by FREDDIE Shiny (normality, heteroscedasticity), but some of them need to be decided by you.<br/>
				<ul>
					<li>Is the relationship between the two variables approximately linear? (if you look at their plot, do the datapoints plot a line and not a curve) </li>
					<li>Are both of the variables continuous? (none of them is a ranking, like 1st, 2nd etc.</li>
					<li>Are there any outliers in the data? (no values so extreme that they are unrealistic or influencing the whole data too much)</li>
				</ul>
				Depending on your answers, a fitting correlation method will be used.")
				})
			if (input$Linear==TRUE & input$Continuous==TRUE & input$Outliers==TRUE){
				results <- cor.test(keep[[input$yvar]], keep[[input$xvar]], method="pearson")
				}
			else if (input$Outliers==TRUE & input$Linear==FALSE | input$Continuous==FALSE){
				results <- cor.test(keep[[input$yvar]], keep[[input$xvar]], method="spearman")
				}
			else {results <- "none"}
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
				output$Pvalue <- renderText({"Please remove outliers in the Plot view."})
				}
			}
		if (CurrentTestType  == "spearman") {
			output$TestDescription <- renderUI({
				HTML(
				"The variables you selected could be used for testing through a <b>correlation test</b>.
				This test takes the two groups of 'scores' (e.g. age/first formant) and compares their values.
				It determines the strength of the relationship between them - how well does knowing the value of X allow us to predict Y - and determines whether the relationship may be purely 
				coincidental (e.g. if you have few examples).<br/><br/>
				Some of the prerequisites of this test were tested automatically by FREDDIE Shiny (normality, heteroscedasticity), but some of them need to be decided by you.<br/>
				<ul>
					<li>Are there any outliers in the data? (no values so extreme that they are unrealistic or influencing the whole data too much)</li>
				</ul>
				Depending on your answers, a fitting correlation method will be used.")
				})
			if (input$Outliers==TRUE) {results <- cor.test(keep[[input$yvar]], keep[[input$xvar]], method="spearman")}
			else {results <- "none"}
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
				output$Pvalue <- renderText({"Please remove outliers in the Plot view"})
				}			
			}
        # categorial-categorial: if parameters fulfilled, do a chi square test, if not do fisher's exact
        if (CurrentTestType  == "chisq"){
			output$TestDescription <- renderUI({
				HTML(
				"The variables you selected could be used for testing through a <b>chi-squared test</b>.
				This test takes the counts of the various combinations in your data (e.g. sex/education) and compares their distribution.
				It determines whether there is a significant difference between the expected and observed frequencies. For example if you want to see whether the various education levels are represented approximately equally with respect to the individual genders in your data,
				you could use a chi-squared test to see whether this is the case or not.<br><br/>
				FREDDIE Shiny has atomatically checked the requirements that the chi-squared test has on your data.")
				})
			results <- chisq.test(table(keep[[input$yvar]], keep[[input$xvar]]))
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
				})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
				})
			}
		if (CurrentTestType  == "fisher") {
			output$TestDescription <- renderUI({
				HTML(
				"The variables you selected could be used for testing through a chi-squared test.
				This test takes the counts of the various combinations in your data (e.g. sex/education) and compares their distribution.
				It determines whether there is a significant difference between the expected and observed frequencies. For example if you want to see whether the various education levels are represented approximately equally with respect to the individual genders in your data,
				you could use a chi-squared test to see whether this is the case or not.<br><br/>
				FREDDIE Shiny has atomatically checked the requirements that the chi-squared test has on your data. Some of them were not fulfilled, which is why <b>Fisher's exact test</b> is performed instead")
				})	
			results <- fisher.test(table(keep[[input$yvar]], keep[[input$xvar]]))
			output$Signalert <- renderText({
				ifelse(results$p.value<=0.05, "significant", "not significant")
				})
			output$Pvalue <- renderText({
				paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
				})
			}
		if (CurrentTestType  == "none") {
			output$TestDescription <- renderUI({
				HTML(
				"There is currently no test for the variables you selected. If you think support for your variable type is necessary, please contact us <a href='mailto: jiri.zamecnik@anglistik.uni-freiburg.de'>jiri.zamecnik@anglistik.uni-freiburg.de</a>.
					")
				})
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