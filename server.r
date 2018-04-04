library(shiny)
library(lmtest)
library(ggplot2)
library(Cairo)

shinyServer(function(input, output) {
    
	datafileInput <- reactive({
      
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
		else {return(NULL)}
		})
		
	output$overrider <- renderUI({
		tagList(
			lapply (names(datafileInput()), function (variable) {
					if (is.factor(datafileInput()[[variable]])) {radioButtons(paste(variable, "type", sep="_"), variable,	choices = c("category", "number"), selected = "category")}
					else {radioButtons(paste(variable, "type", sep="_"), variable,	choices = c("category", "number"), selected = "number")} 
					}
				)
		
			)
		})
    
	vals <- reactiveValues(keeprows=TRUE, markcases=FALSE)
	
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
      if (is.numeric(datasetInput()[[input$yvar]]) & is.factor(datasetInput()[[input$xvar]])){return("Box plot:")}
      if (is.numeric(datasetInput()[[input$yvar]]) & is.numeric(datasetInput()[[input$xvar]])){return("Scatter plot:")}
      if (is.factor(datasetInput()[[input$yvar]]) & is.factor(datasetInput()[[input$xvar]])){return("Bar plot:")}
    })
    
	XTitle <- reactive({paste(input$xvar)})
	YTitle <- reactive({paste(input$yvar)})

    output$Xsettings <- renderUI({

	if(is.numeric(datasetInput()[[input$xvar]]) != TRUE) {invisible()} else {tagList(
   		 sliderInput("inXSlider", "Range of values in histogram", min=range(datasetInput()[[input$xvar]], na.rm=TRUE)[1], max=range(datasetInput()[[input$xvar]], na.rm=TRUE)[2], value=range(datasetInput()[[input$xvar]], na.rm=TRUE)),
		 sliderInput("inXBinSlider", "Number of bars displayed in histogram", min=5, max=25, value=10)
			)}
	})

    output$Ysettings <- renderUI({
	if(is.numeric(datasetInput()[[input$yvar]]) != TRUE) {invisible()} else {tagList(
    	sliderInput("inYSlider", "Range of values in histogram", min=range(datasetInput()[[input$yvar]], na.rm=TRUE)[1], max=range(datasetInput()[[input$yvar]], na.rm=TRUE)[2], value=range(datasetInput()[[input$yvar]], na.rm=TRUE)),
		sliderInput("inYBinSlider", "Number of bars displayed in histogram", min=5, max=25, value=10)
			)}
	})

	SummaryPlotX <- function(){
		if (is.numeric(datasetInput()[[input$xvar]])){
			Xdata <- datasetInput()[[input$xvar]]
			Xdata <- Xdata[Xdata>input$inXSlider[1] & Xdata<input$inXSlider[2]]
			hist(Xdata, xlab=XTitle(), breaks=seq(min(Xdata, na.rm=TRUE), max(Xdata, na.rm=TRUE), l=input$inXBinSlider+1), main=paste("Distribution of ", XTitle(), sep=""))}
		if (is.factor(datasetInput()[[input$xvar]])){plot(datasetInput()[[input$xvar]], xlab=XTitle(), main=paste("Distribution of ", XTitle(), sep=""))}
		}
		
	SummaryPlotY <- function(){
		if (is.numeric(datasetInput()[[input$yvar]])){
			Ydata <- datasetInput()[[input$yvar]]
			Ydata <- Ydata[Ydata>input$inYSlider[1] & Ydata<input$inYSlider[2]]
			hist(Ydata, xlab=YTitle(),breaks=seq(min(Ydata, na.rm=TRUE), max(Ydata, na.rm=TRUE), l=input$inYBinSlider+1), main=paste("Distribution of ", YTitle(), sep=""))}
		if (is.factor(datasetInput()[[input$yvar]])){plot(datasetInput()[[input$yvar]], xlab=YTitle(), main=paste("Distribution of ", YTitle(), sep=""))}

		}
	
    output$PlotType <- renderText({
        PlotType()
    })
    

    
	output$PlotSettings <- renderUI({
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
		 
		else {invisible()}
	})	
	
	output$OutlierFilter <- renderUI({
		if (PlotType()=="Scatter plot:") 
				{tagList(
					h3("Outlier removal"),
					p("Mark outliers (cases that are too extreme to be realistic) by clicking 
						or dragging a box and then clicking the 'Select' button. To remove them from the statistical tests, click 'Apply'."),
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
					p("Mark outliers (cases that are too extreme to be realistic) by clicking 
						or dragging a box and then clicking the 'Select' button.  To remove them from the statistical tests, click 'Apply'."),
					p("You should select ALL outliers before hitting the 'Apply' button. If the redrawn chart shows new outlier-like cases, do not remove them anymore."),
					fluidRow(
						column(1, actionButton("exclude_toggle", "Select")),
						column(1, actionButton("exclude_reset", "Reset")),
						column(1, actionButton("apply_removal", "Apply", style="color: #fff; background-color: #009933;")),
						column(9, invisible())
						)
					)
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
				geom_point(data = mark, aes(mark[[input$xvar]], mark[[input$yvar]]), shape = 4, color = "black", size=3, alpha = 0.75) +
				geom_point(data = exclude, aes(exclude[[input$xvar]], exclude[[input$yvar]]), shape = 4, color = "black", size=3, alpha = 0.25) +
				scale_x_discrete(name=input$xvar) + 
				scale_y_continuous(name=input$yvar)+
				ggtitle(input$titleInput)+
				theme_bw()+
				theme(plot.title = element_text(hjust=0.5, face="bold", size=14))
			
			return(outPlot)
			
        }
        # create a scatterplot if y variable is numeric and x variable is numeric
        if (PlotType()=="Scatter plot:"){
			keep    <- datasetInput()[ vals$keeprows, , drop = FALSE]
			exclude <- datasetInput()[!vals$keeprows, , drop = FALSE]	
			mark <- datasetInput()[ vals$markcases, , drop = FALSE]
			outPlot <- ggplot(keep, aes(keep[[input$xvar]], keep[[input$yvar]])) +
				geom_point(shape = 16) +
				geom_point(data = mark, aes(mark[[input$xvar]], mark[[input$yvar]]), shape = 4, color = "black", size=3, alpha = 0.75) +
				geom_point(data = exclude, aes(exclude[[input$xvar]], exclude[[input$yvar]]), shape = 4, color = "black", size=3, alpha = 0.25) +
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
        if (PlotType()=="Bar plot:"){
			if (input$propCheck=="absolute"){
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
				else {outPlot <- outPlot + theme(legend.position="none")}}
			else {
				datatabletemp <- prop.table(table(datasetInput()[[input$yvar]], datasetInput()[[input$xvar]]), 2)
				outPlot <- barplot(datatabletemp, beside=input$besideCheck, col=gray.colors(length(levels(datasetInput()[[input$yvar]])), start = 0.0, end = 0.9))
				}
			
			return(outPlot)
			
        }
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
		print(vals$markcases)
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
		keep    <- datasetInput()[ vals$keeprows, , drop = FALSE]
		if (PlotType()=="Box plot:" & length(levels(keep[[input$xvar]]))==2 ){
			normality <- aggregate(formula = keep[[input$yvar]] ~ keep[[input$xvar]], FUN = function(x) {y <- shapiro.test(x); c(y$statistic, y$p.value)})
			variance <- var.test(keep[[input$yvar]] ~ keep[[input$xvar]], ratio = 1, alternative = "two.sided", conf.level = 0.95,)
			if (range(normality[,2][,2])[1] >= 0.05 & variance$p.value >= 0.05){
				return("t")} #TTEST
			if (range(normality[,2][,2])[1] < 0.05 | variance$p.value < 0.05) {
				return("wilcox")}#WILCOX TEST
			}
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
			checkboxInput("Paired", "Paired data (for example before/after an experiment)", FALSE),
			checkboxInput("Independent", "Independent data collection (the two samples are independent on each other)", TRUE)
			)}
		else if (CurrentTestType == "wilcox") {tagList(
			h3("Test settings:"),
			checkboxInput("Paired", "Paired data (for example before/after an experiment)", FALSE),
			checkboxInput("Independent", "Independent data collection (the two samples are independent on each other)", TRUE)
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
				"The variables you selected could be used for testing through a <b>two-sample t-test</b>.
				This test takes the two groups in your data (e.g. male/female) and compares their values.
				It determines the probability, with which the population from which you took the samples have different means.
				For example, if you study the number of different words (type count) in essays by beginner and advanced students, it will tell you - on the base of your samples - how likely is is, that beginner and advanced students use the same number of types in their essays.<br/><br/>
				If your values are <b>paired</b> - for example they contain the test scores of a group of students before a class and after it, select the corresponding option.
				You need to verify that the sampling was <b>independent</b>, e.g. you did not ask participants in group A to bring their siblings as participants in group B.
				If this was not the case, the result presented here is unreliable.")
			 })
			if (input$Independent==TRUE){results <- t.test(keep[[input$yvar]] ~ keep[[input$xvar]], paired=input$Paired)}
			else {results <- "dependent"}
			
			if (results != "dependent"){
				output$Signalert <- renderText({
					ifelse(results$p.value<=0.05, "significant", "not significant")
					})
				output$Pvalue <- renderText({
					paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
					})
				}
			else {
				output$Signalert <- renderText({"Testing not possible"})
				output$Pvalue <- renderText({"There is currently no test for clustered sampling in FREDDIE Shiny. If you think support for your variable type is necessary, please contact jiri.zamecnik@anglistik.uni-freiburg.de"})
				}
		}

		# t-test parameters not fulfilled
		if (CurrentTestType  == "wilcox") {
			output$TestDescription <- renderUI({
				HTML(
				"The variables you selected could be used for testing through a <b>Wilcoxon rank-sum test</b>.
				This test takes the two groups in your data (e.g. male/female) and compares their values.
				It determines the probability, with which the population from which you took the samples have different means.
				For example, if you study the number of different words (type count) in essays by beginner and advanced students, it will tell you - on the base of your samples - how likely is is, that beginner and advanced students use the same number of types in their essays.<br/><br/>
				If your values are <b>paired</b> - for example they contain the test scores of a group of students before a class and after it, select the corresponding option.
				You need to verify that the sampling was <b>independent</b>, e.g. you did not ask participants in group A to bring their siblings as participants in group B.
				If this was not the case, the result presented here is unreliable.")
				})
			if (input$Independent==TRUE){results <- wilcox.test(keep[[input$yvar]] ~ keep[[input$xvar]], paired=input$Paired)}
			else {results <- "dependent"}
			
			if (results != "dependent"){
				output$Signalert <- renderText({
					ifelse(results$p.value<=0.05, "significant", "not significant")
					})
				output$Pvalue <- renderText({
					paste("p ", ifelse(results$p.value<0.001, "< 0.001", paste("= ", (round(results$p.value,3)), sep="")), sep="")
					})
				}
			else {
				output$Signalert <- renderText({"Testing not possible"})
				output$Pvalue <- renderText({"There is currently no test for clustered sampling in FREDDIE Shiny. If you think support for your variable type is necessary, please contact jiri.zamecnik@anglistik.uni-freiburg.de"})
				}
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

