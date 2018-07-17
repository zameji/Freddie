library(shiny)
library(shinyLP)
library(shinyjqui)
library(dplyr)
library(lmtest)
library(ggplot2)
options(shiny.usecairo=FALSE)

bx.stat <- function(inp){return(boxplot.stats(inp)$stats[c(1,5)])}
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
fl_cl <- function(inp){return(c(floor(inp[1]),ceiling(inp[2])))}
splitVec <- function(vec){nvec <- c()
	temp <- c()
	while (length(vec) > 2) {
		while (length(temp) < 3){
		
			}
		}
	}

shinyServer(function(input, output, session) {
	rawData <- reactive({
		  inFile <- input$file1
		  if (is.null(inFile))
			return(NULL)
		  read.csv(inFile$datapath, header=input$header, sep=input$sep, 
				   quote=input$quote)
		})

	cookedData <- reactiveValues(cats=NULL, nums=NULL, cooked=NULL)				
	plotType <- reactiveValues(current=NULL)
	
	cookData <- function() {
		req(rawData())
		cookedData$cooked <- rawData()
		for (label in colnames(rawData())) {
			if (label %in% cookedData$cats) {
				if (!is.factor(cookedData$cooked[[label]])){cookedData$cooked[[label]] <- as.factor(cookedData$cooked[[label]])}
				}
			if (label %in% cookedData$nums) {
				if (is.factor(cookedData$cooked[[label]])){cookedData$cooked[[label]] <- as.numeric(cookedData$cooked[[label]])}
				}
			}
		}
	
	# Observer waiting for the upload of a file --> once its done the page Data check is activated and selected
    observe({
			if (is.null(rawData()) == FALSE) {
				session$sendCustomMessage('activeNavs', 'Data check')
				updateNavbarPage(session, 'mainnavbar', selected = 'Data check')
			}
		})
		
	observeEvent(input$bigFriendlyButton, {
		session$sendCustomMessage('activeNavs', 'Data summary')
		session$sendCustomMessage('activeNavs', 'Visualisation')
		session$sendCustomMessage('activeNavs', 'Statistics')
		
		cookedData$cats <- input$cats_order
		cookedData$nums <- input$nums_order

		cookData()
		updateNavbarPage(session, 'mainnavbar', selected = 'Data summary')	
		})
		
	plotinput <- eventReactive(input$doPlot, {	
		if (length(input$outcome_order)!=1|length(input$pred_order)>2) {
			showModal(
				modalDialog(
					title="ERROR",
					"You need one outcome and 0-2 predictors",
					footer=NULL,
					easyClose=TRUE
					)
				)
			}
		list(outcome=input$outcome_order, pred=input$pred_order)		
		})
	

	output$outplot <- renderPlot({

		req(cookedData$cooked, plotinput())
		pars <- plotinput()	
		
		req(length(pars$outcome)==1)
		req(length(pars$pred)<3)
		
		# There are no predictors, plot just the outcome
		if (length(pars$pred)<1) {
			
			# Outcome is a factor
			if (pars$outcome %in% cookedData$cats) {
				plotType$current <- "bar_dist"
				p <- ggplot(cookedData$cooked, aes_string(pars$outcome))
				p <- p + geom_bar()}
				
			# Outcome is numeric	
			else if (pars$outcome %in% cookedData$nums) {
				req(input$histRange, input$histBins)
				plotType$current <- "histogram"
				p <- ggplot(cookedData$cooked[between(cookedData$cooked[[pars$outcome]],input$histRange[1],input$histRange[2]),], aes_string(pars$outcome))
				p <- p + geom_histogram(bins=input$histBins)}
			}
		
		# There is one predictor
		else if (length(pars$pred)==1){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictor is a factor --> boxplot
				if (pars$pred %in% cookedData$cats){
					p <- ggplot(cookedData$cooked, aes_string(pars$pred, pars$outcome))
					p <- p + geom_boxplot()
					}
				# The predictor is numeric --> scatterplot
				else {
					p <- ggplot(cookedData$cooked, aes_string(pars$pred, pars$outcome))
					p <- p + geom_point()
					}			
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictor is a factor --> barplot
				if (pars$pred %in% cookedData$cats){
					p <- ggplot(cookedData$cooked, aes_string(pars$pred, fill=pars$outcome))
					p <- p + geom_bar()
					}
				# The predictor is numeric --> logistic regression?
				else {			
					p <- ggplot(cookedData$cooked, aes_string(pars$pred, pars$outcome))
					p <- p + geom_point()				
					}			
				}
			}
		
		# There are two predictors
		else if (length(pars$pred)==2){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictor is are factors --> boxplot with subcategories
				if (all(pars$pred %in% cookedData$cats)){
					p <- ggplot(cookedData$cooked, aes_string(x=pars$pred[1], y=pars$outcome, fill=pars$pred[2]))
					p <- p + geom_boxplot()
					}
				# The predictors is numeric --> scatterplot with color gradient
				else if (all(pars$pred %in% cookedData$nums)){
					p <- ggplot(cookedData$cooked, aes_string(x=pars$pred[1], y=pars$outcome, color=pars$pred[2]))
					p <- p + geom_point()
					}

				# Mixed predictors --> scatterplot with character mapping
				else {
					fil <- pars$pred %in% cookedData$nums
					p <- ggplot(cookedData$cooked, aes_string(x=pars$pred[fil], y=pars$outcome, pch=pars$pred[!fil]))
					p <- p + geom_point()

					###
					# ADD FACET_WRAP
					###
					
					}
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictors are a factor --> faceted barplot
				if (all(pars$pred %in% cookedData$cats)){
					p <- ggplot(cookedData$cooked, aes_string(x=pars$pred[1], fill=pars$outcome))
					p <- p + geom_bar() + facet_wrap(pars$pred[2])
					}
				
				# The predictors are all numeric --> logistic regression with color
				else if (all(pars$pred %in% cookedData$nums)){
					p <- ggplot(cookedData$cooked, aes_string(x=pars$pred[1], y=pars$outcome, color=pars$pred[2]))
					p <- p + geom_point()
					}					
				# Mixed predictors --> logistic regression with pch
				else {
					fil <- pars$pred %in% cookedData$nums			
					p <- ggplot(cookedData$cooked, aes_string(x=pars$pred[fil], y=pars$outcome))
					p <- p + geom_point() + facet_wrap(pars$pred[!fil])

					}			
				}
						
			}
	
	p <- p + theme_bw()
	return(p)
	})
	
	output$plotChoices <- renderUI({
		req(cookedData$cooked, plotinput())
		
		pars <- plotinput()	
		
		req(length(pars$outcome)==1)
		req(length(pars$pred)<3)
		
		if (length(pars$pred)<1) {
			
			# Outcome is a factor
			if (pars$outcome %in% cookedData$cats) {}
				
			# Outcome is numeric	
			else if (pars$outcome %in% cookedData$nums) {
				dataSpan <- range(cookedData$cooked[[pars$outcome]], na.rm=T)
				dataSpan[1] <- floor(dataSpan[1])
				dataSpan[2] <- ceiling(dataSpan[2])
				tagList(column(6,
						sliderInput("histRange", "Range of values in histogram", min=dataSpan[1], max=dataSpan[2], value=dataSpan)
						),
					column(6,
						sliderInput("histBins", "Number of bars displayed in histogram", min=5, max=25, value=10)
						)
					)
				}
			}
		
		# There is one predictor
		else if (length(pars$pred)==1){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictor is a factor --> boxplot
				if (pars$pred %in% cookedData$cats){}
				# The predictor is numeric --> scatterplot
				else {}			
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictor is a factor --> barplot
				if (pars$pred %in% cookedData$cats){}
				# The predictor is numeric --> logistic regression?
				else {}			
				}
			}
		
		# There are two predictors
		else if (length(pars$pred)==2){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictor is are factors --> boxplot with subcategories
				if (all(pars$pred %in% cookedData$cats)){}
				# The predictors is numeric --> scatterplot with color gradient
				else if (all(pars$pred %in% cookedData$nums)){}

				# Mixed predictors --> scatterplot with character mapping
				else {}
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictors are a factor --> faceted barplot
				if (all(pars$pred %in% cookedData$cats)){}
				
				# The predictors are all numeric --> logistic regression with color
				else if (all(pars$pred %in% cookedData$nums)){}					
				# Mixed predictors --> logistic regression with pch
				else {}			
				}
			
			
			}		
		})
	
	output$summaryOutput <- renderUI({
		req(cookedData$cooked)
		
		wells <- '<h3 style="text-align: center">Categories</h3>'
		
		# Create rows to display
		rows <- list()
		cRow <- c()
		ind <- 1
		cRowInd <- 1
		rest <- length(cookedData$cats)
		cats <- cookedData$cats[order(seq(-1,-rest))]
		
		for (label in cats) {
			while(rest >5) {
				while (ind <=5){
					cRow <- c(cRow, as.character(cats[rest]))
					rest <- rest - 1
					ind <- ind + 1
					}
				ind <- 1
				rows[[cRowInd]] <- cRow
				cRowInd <- cRowInd + 1
				cRow <- c()
				}
				
				cRow <- as.character(cats[seq(rest,1)])
				rows[[cRowInd]] <- cRow
			}		
		
		
		for (row in rows) {
			if (length(row) == 5){
				wells <- paste(wells, '<div class="row"><div class=col-sm-1></div>', sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<p>", sep="")
						for (level in levels(cookedData$cooked[[label]])) {
						summarize <- paste(summarize,
							level,
							"\t",
							sum(cookedData$cooked[[label]]==level),
							"<br/>",
							sep=""
								)
							}
					summarize <- paste(summarize, "</p>", sep="")
					wells <- paste(wells, paste('<div class=col-sm-2><div class="well" style="text-align:center">', summarize, "</div></div>", sep=""))
					}
				
				wells <- paste(wells, "<div class=col-sm-1></div></div>", sep="")
			}
			
			if (length(row)!=5){
				wells <- paste(wells, '<div class="row">','<div class=col-sm-',6-length(row),'></div>', sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<p>", sep="")
						for (level in levels(cookedData$cooked[[label]])) {
						summarize <- paste(summarize,
							level,
							"\t",
							sum(cookedData$cooked[[label]]==level),
							"<br/>",
							sep=""
								)
							}
					summarize <- paste(summarize, "</p>", sep="")
					wells <- paste(wells, paste('<div class=col-sm-2><div class="well" style="text-align:center">', summarize, "</div></div>", sep=""))
					}
				
				wells <- paste(wells, "<div class=col-sm-",6-length(row),"></div></div>", sep="")			

				}

			}
			
		wells <- paste(wells, '<h3 style="text-align: center">Numbers</h3>', sep="")	

		rows <- list()
		cRow <- c()
		ind <- 1
		cRowInd <- 1
		rest <- length(cookedData$nums)
		nums <- cookedData$nums[order(seq(-1,-rest))]
		
		for (label in nums) {
			while(rest >5) {
				while (ind <=5){
					cRow <- c(cRow, as.character(nums[rest]))
					rest <- rest - 1
					ind <- ind + 1
					}
				ind <- 1
				rows[[cRowInd]] <- cRow
				cRowInd <- cRowInd + 1
				cRow <- c()
				}
				
				cRow <- as.character(nums[seq(rest,1)])
				rows[[cRowInd]] <- cRow
			}		
		
		for (row in rows) {
			if (length(row) == 5){
				wells <- paste(wells, '<div class="row"><div class=col-sm-1></div>', sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<p>", sep="")
					mn = round(mean(cookedData$cooked[[label]], na.rm=T),3)
					quants <-  round(quantile(cookedData$cooked[[label]], c(0,0.25,0.5,0.75,1), na.rm=T),3)
					nas <- sum(is.na(cookedData$cooked[[label]])|is.nan(cookedData$cooked[[label]]))
					summarize <- paste(
						"<h4>",
						label,
						"</h4>",
						"<p>",
							"Min.:\t",
							quants[1],
							"<br/>25%.:\t",
							quants[2],
							"<br/>Median.:\t",
							quants[3],
							"<br/>Mean.:\t",
							mn,
							"<br/>75%.:\t",
							quants[4],
							"<br/>Max.:\t",
							quants[5],							
							ifelse(nas>1, 
								paste("<br/>NAs.:\t",nas),
								""),
						"</p>",
						sep=""
						)
							summarize <- paste(summarize, "</p>", sep="")
							wells <- paste(wells, paste('<div class=col-sm-2><div class="well" style="text-align:center">', summarize, "</div></div>", sep=""))
							}
				
				wells <- paste(wells, "<div class=col-sm-1></div></div>", sep="")
			}
			
			if (length(row)!=5){
				wells <- paste(wells, '<div class="row">','<div class=col-sm-',6-length(row),'></div>', sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<p>", sep="")
					mn = round(mean(cookedData$cooked[[label]], na.rm=T),3)
					quants <-  round(quantile(cookedData$cooked[[label]], c(0,0.25,0.5,0.75,1), na.rm=T),3)
					nas <- sum(is.na(cookedData$cooked[[label]])|is.nan(cookedData$cooked[[label]]))
					summarize <- paste(
						"<h4>",
						label,
						"</h4>",
						"<p>",
							"Min.:\t",
							quants[1],
							"<br/>25%.:\t",
							quants[2],
							"<br/>Median.:\t",
							quants[3],
							"<br/>Mean.:\t",
							mn,
							"<br/>75%.:\t",
							quants[4],
							"<br/>Max.:\t",
							quants[5],							
							ifelse(nas>1, 
								paste("<br/>NAs.:\t",nas),
								""),
						"</p>",
						sep=""
						)
					summarize <- paste(summarize, "</p>", sep="")
					wells <- paste(wells, paste('<div class=col-sm-2><div class="well" style="text-align:center">', summarize, "</div></div>", sep=""))
					}
				
				wells <- paste(wells, "<div class=col-sm-",6-length(row),"></div></div>", sep="")			

			}	
			
		}		
				
		HTML(wells)
		})

	output$vartype <- renderUI({
		fil <- sapply(rawData(), class)=="factor"
		fil2 <- sapply(rawData(), is.numeric)
		tagList(
			orderInput('cats', 'Categories', items = colnames(rawData())[fil],
						as_source = FALSE, connect = c('nums', 'none'), width="100%", item_class="primary"),
			orderInput('nums', 'Numbers', items = colnames(rawData())[fil2],
						as_source = FALSE, connect = c('cats', 'none'), width="100%", item_class="primary"),
			orderInput('none', 'Not analysed', items = colnames(rawData())[!(fil|fil2)],
						as_source = FALSE, connect = c('cats', 'nums'), width="100%", item_class="default", placeholder="e.g. Names")						
			)
		})	
		
	output$plotselect <- renderUI({
	
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		
		tagList(
			orderInput('outcome', 'Outcome', items=c(),
						as_source = FALSE, connect = c('pred', 'source'), width="100%", item_class="primary", placeholder="Choose only the outcome to visualise its distribution"),
			orderInput('pred', 'Predictors', items = c(),
						as_source = FALSE, connect = c('outcome', 'source'), width="100%", item_class="primary", placeholder="Up to two predictors"),
			orderInput('source', 'Not displayed', items = c(cookedData$cats, cookedData$nums),
						as_source = FALSE, connect = c('outcome', 'pred'), width="100%", item_class="primary")						
			
			
			)
		})		
		
	output$columnNumber <- renderText({
		req(rawData())
		as.character(ncol(rawData()))
		})
		
	output$rowNumber <- renderText({
		req(rawData())
		as.character(nrow(rawData()))
		})		
	
})
