library(shiny)
library(shinyLP)
library(shinyBS)
library(shinyjqui)
library(dplyr)
library(tools)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(RColorBrewer)	

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
doughnut <-
function (x0, edges = 200, labels=NULL, outer.radius = 1, 
          inner.radius=0.75, clockwise = FALSE,
          init.angle = ifelse (clockwise, 90, 0), density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, inside=NULL, ...){
	if (!is.factor(x0)){stop("'x' values must be factors.")}
	if (is.null(labels)){labels <- as.character(levels(x0))}
	
	nas <- sum(is.na(x0))
	
	x <- as.vector(table(x0[!is.na(x0)]))
	
	col = colorRampPalette(brewer.pal(9,"Spectral"))(length(labels))
	
	x <- c(0, cumsum(x)/sum(x))
	dx <- diff(x)
	nx <- length(dx)
	par(mar=c(2,2,2,2))
	plot.new()
	pin <- par("pin")
	xlim <- ylim <- c(-1, 1)
	if (pin[1L] > pin[2L])
		{xlim <- (pin[1L]/pin[2L]) * xlim}
	else {ylim <- (pin[2L]/pin[1L]) * ylim}
	plot.window(xlim, ylim, "", asp = 1)	
	
	twopi <- if (clockwise)
		-2 * pi
	else 2 * pi
	t2xy <- function(t, radius) {
		t2p <- twopi * t + init.angle * pi/180
		list(x = radius * cos(t2p), 
			 y = radius * sin(t2p))
	}
	for (i in 1L:nx) {
		n <- max(2, floor(edges * dx[i]))
		P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
				  outer.radius)
		polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
				angle = angle[i], border = border[i], 
				col = col[i], lty = lty[i])
		Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
		lab <- as.character(labels[i])
		if (!is.na(lab) && nzchar(lab)) {
			text(1.3 * Pout$x, 1.3 * Pout$y, labels[i], family="serif",
				 xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
				 ...)
		}
		## Add white disc          
		Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
				  inner.radius)
		polygon(Pin$x, Pin$y, density = density[i], 
				angle = angle[i], border = border[i], 
				col = "white", lty = lty[i])
	if (nas > 0)
		text(0,0,paste(nas, ifelse(nas>1,"\nNAs", "\nNA")), family="serif", cex=1.25)
	}
 
	title(main = main, ...)
	invisible(NULL)
	}

bend <-
function (nas, tot, edges = 100, labels=NULL, outer.radius = 1, 
          inner.radius=0.75,
          init.angle = 0, density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, inside=NULL, ...){
	if (!all(is.numeric(c(nas, tot)))){stop("'x' values must be factors.")}
	if (is.null(labels)){labels <- c("", "")}
	
	col <- c("grey90", "red")
	x <- c(0, (tot-nas)/tot, 1)
	dx <- diff(x)
	nx <- length(dx)
	par(mar=c(0,2,1,2))
	plot.new()
	pin <- par("pin")
	xlim <- c(-1, 1)
	ylim <- c(0,1)
	if (pin[1L] > pin[2L])
		{xlim <- (pin[1L]/pin[2L]) * xlim
	} else {ylim <- (pin[2L]/pin[1L]) * ylim}
	plot.window(xlim, ylim, "", asp = 1)	

	t2xy <- function(t, radius) {
		t2p <- pi * t + init.angle * pi/180
		list(x = radius * cos(t2p), 
			 y = radius * sin(t2p))
	}
	for (i in 1L:nx) {
		n <- max(50, floor(edges * dx[i]))
		P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
				  outer.radius)
		polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
				angle = angle[i], border = border[i], 
				col = col[i], lty = lty[i])
		Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
		lab <- as.character(labels[i])
		if (!is.na(lab) && nzchar(lab)) {
			text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], family="serif",
				 xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0))
		}
	}

		## Add white disc          
		Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
				  inner.radius)
		polygon(Pin$x, Pin$y, density = density[i], 
				angle = angle[i], border = border[i], 
				col = "white", lty = lty[i])
	
	text(0,0.2,paste(round((1-x[2])*100, 1), "% missing", sep=""), family="serif", cex=1.25)
 
	title(main = main, ...)
	invisible(NULL)
	}
	
getna <- function(cc) {
	cc[ sample(c(TRUE, NA), prob = c(0.95, 0.05), size = length(cc), replace = TRUE) ]
	}
	
	
randdata <- function(){
	rd <- data.frame(
		gender=c(sample(c(rep("female",10),rep("male",8)), 50, replace=T), 
			sample(c(rep("female",8),rep("male",10)), 50, replace=T)),
		age=c(round(runif(50,min=10,max=30),0), round(runif(50,min=20,max=45),0)),
		education=c(sample(c(rep("university",7),rep("high",5),rep("elementary",2),rep("doctorate",10)),50,replace=T),
			sample(c(rep("university",5),rep("high",7),rep("elementary",10),rep("doctorate",2)),50,replace=T)),
		residence=c(sample(c(rep("E",7),rep("M",5),rep("W",1)),50,replace=T),
			sample(c(rep("E",2),rep("M",7),rep("W",10)),50,replace=T)),
		birthplace=c(sample(c(rep("E",5),rep("M",4),rep("W",1)),50,replace=T),
			sample(c(rep("E",3),rep("M",8),rep("W",15)),50,replace=T)),	
		rhoticity=c(abs(round(rnorm(50,0.2, 0.1),2)),abs(round(rnorm(50,0.5, 0.1),2))),
		glottalisation=c(sample(c(rep("preconsonantal",7), rep("preconsonantal+initial",1), rep("none",10)),50,replace=T),
			sample(c(rep("preconsonantal",7), rep("preconsonantal+initial",3), rep("none",1)),50,replace=T))
		)
		
	rd <- as.data.frame(lapply(rd, getna))
	
	return(rd)

	}

getExp <- function (cdata) {
	expDat <- data.frame()
	for (i in 1:length(rowSums(cdata))){
		for (j in 1:length(colSums(cdata))) {
			expDat[i,j] <- (sum(cdata[i,]) * sum(cdata[,j])) / sum(cdata)
			expDat[i,j] <- (sum(cdata[i,]) * sum(cdata[,j])) / sum(cdata)
			}
		}
	colnames(expDat) <- colnames(cdata)
	return(expDat)
	}
	
renderCatPlots <- function(cats, dats, input, output) {
		for (i in cats) {
			local({
				ii <- i  
				# need i evaluated here
				output[[sprintf('%s_%s', "plot", ii)]] <- renderPlot({
					doughnut(dats[[ii]])
				})
			})
		}
	}

renderNumPlots <- function(nums, dats, input, output) {
		for (i in nums) {
			local({
				ii <- i
				nas <- sum(is.na(dats[[ii]]))
				
				# need i evaluated here
				output[[sprintf('%s_%s', "plot", ii)]] <- renderPlot({
					ggplot(dats[!is.na(dats[[ii]]),], aes_string(ii))+ 
						geom_histogram(aes(fill=..count..), bins=10) + 
						theme_minimal() + xlab("") + ylab("") + 
						theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none") +
						scale_y_continuous(labels=c()) + 
						scale_fill_distiller(palette="Oranges")+
						annotate("text", Inf, Inf, label = ifelse(nas>0, paste(nas, ifelse(nas>1,"NAs", "NA")), ""), hjust = 1, vjust = 1, size=6, family="serif")
						
				})
			})
		}
	}	
	
shinyServer(function(input, output, session) {
	cookedData <- reactiveValues(cats=NULL, nums=NULL, cooked=NULL)				
	plotType <- reactiveValues(current=NULL)
	importSettings <- reactiveValues(header=TRUE, sep="\t", quoter='"')	
	testSet <- reactiveValues(settings=NULL, vals=NULL, test=NULL)
	settings <- reactiveValues(na.ignore="ignore", mod="real")

	
	observeEvent(input$upFile, {
		settings$mod <- "real"
		showModal(
			modalDialog(size="s",
				fileInput(inputId='file1', label='Choose a CSV file',
									accept=c('text/csv', 
											 'text/comma-separated-values', 
											 'text/plain',
											 'text/tab-separated-values',
											 'csv',
											 'tsv')),
				hr(),
				p(a("Download sample data", href="https://onedrive.live.com/download?cid=8BF09AD1C8343122&resid=8BF09AD1C8343122%21258985&authkey=ACWhDNRScGBzKwk"),  style='text-align:center'),
				easyClose = TRUE
			)
		)
	})
	
	observeEvent(input$startDemo, {
		settings$mod <- "demo"
		session$sendCustomMessage('activeNavs', 'Check')
		updateNavbarPage(session, 'mainnavbar', selected = 'Check')
	})
	
	observeEvent(input$file1, {
	
		if (is.null(rawData())){
			showModal(modalDialog(
				size="s",
				title="Invalid file",
				h4("This file is not supported", style="color: #ff0000; font-face: bold"),
				fileInput(inputId='file1', label='Choose a CSV file',
									accept=c('text/csv', 
											 'text/comma-separated-values', 
											 'text/plain',
											 'text/tab-separated-values',
											 'csv',
											 'tsv')
					),
				hr(),
				p(a("Download sample data", href="https://onedrive.live.com/download?cid=8BF09AD1C8343122&resid=8BF09AD1C8343122%21258985&authkey=ACWhDNRScGBzKwk"),  style='text-align:center'),
				easyClose = TRUE,		
				fade=FALSE))
			}
		else {removeModal()}
	})	
	
	rawData <- reactive({
		if (settings$mod == "demo"){randdata()}
		else {
			inFile <- input$file1
				if (is.null(inFile))
					return(NULL)
				if (file_ext(inFile$name) %in% c(
					'text/csv',
					'text/comma-separated-values',
					'text/tab-separated-values',
					'text/plain',
					'csv',
					'tsv'))	{read.csv(inFile$datapath, header=importSettings$header, sep=importSettings$sep, quote=importSettings$quoter)}
				else {return(NULL)}}
		})
	
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
				session$sendCustomMessage('activeNavs', 'Check')
				updateNavbarPage(session, 'mainnavbar', selected = 'Check')
			}
		})
		
	observeEvent(input$bigFriendlyButton, {
		session$sendCustomMessage('activeNavs', 'Summarize')
		session$sendCustomMessage('activeNavs', 'Explore')
		session$sendCustomMessage('activeNavs', 'Analyze')
		cookedData$cats <- if(!is.null(input$cats_order)){input$cats_order} else {TRUE}
		cookedData$nums <- if(!is.null(input$nums_order)){input$nums_order} else {TRUE}

		cookData()
		
		req(cookedData$cooked)
		renderCatPlots(cookedData$cats, cookedData$cooked, input, output)
		renderNumPlots(cookedData$nums, cookedData$cooked, input, output)
		
		updateNavbarPage(session, 'mainnavbar', selected = 'Summarize')	
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
	

	
	observeEvent(input$bigUnfriendlyButton, {
		showModal(
			modalDialog(
				title="Tell us more about your data",
				tagList(
					#FOR SOME REASON, THIS DOES NOT WORK WITH T/F AND NEEDS THIS KIND OF WORKAROUND
					selectInput(inputId='header', label='Header',
							   c("Header row in the data"="h",
								 "No header in the data"="nh"),
							   ifelse(importSettings$header, "h", "nh")),
					selectInput(inputId='sep', label='Separator',
							   c(Comma=',',
								 Semicolon=';',
								 Tab='\t'),
							   importSettings$sep),
					selectInput(inputId='quoter', label='Quote sign',
							   c(None='',
								 'Double Quote'='"',
								 'Single Quote'="'"),
							   importSettings$quoter)
							  ),
				footer=	actionButton("ok", "OK")
				)
			)
		})	
		
	observeEvent(input$ok, {		
		importSettings$header <- ifelse(input$header=="h", T, F)
		importSettings$quoter <- input$quoter
		importSettings$sep <- input$sep	
		removeModal()
		})
	
	observeEvent(input$compSel, {
		showModal(
			modalDialog(
				wellPanel(
					h4("Columns"),
					p("Take several columns from your data and compare them to each other"),
					actionButton("colComp", "Proceed", class="btn btn-secondary")						
					),
				wellPanel(
					h4("Groups"),
					p("Take one column from your data and compare several groups in it"),
					actionButton("grComp", "Proceed", class="btn btn-secondary")							
					)
				)
			)
		})
		
	observeEvent(input$predSel, {
		showModal(
			modalDialog(
				wellPanel(
					h4("Columns"),
					p("Take several columns from your data and predict the values in one by the values in the others"),
					actionButton("colPred", "Proceed", class="btn btn-secondary")				
					),
				wellPanel(
					h4("Groups"),
					p("Take one column from your data predict the values in one part by the values in the rest"),
					actionButton("grPred", "Proceed", class="btn btn-secondary")							
					)
				)
			)
		})		

	observeEvent(input$colComp, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Columns ')
		testSet$settings <- "colComp"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('sels', 'Columns to compare', items=c(),
								as_source = FALSE, connect = c('ns', 'cs'), width="100%", item_class="success", placeholder="Select two or more columns to compare"),
					orderInput('ns', 'Numbers', items =  cookedData$nums,
								as_source = FALSE, connect = c('cs', 'sels'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cookedData$cats,
								as_source = FALSE, connect = c('ns', 'sels'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_cols", "OK"), fade = FALSE
				)
			)
		})

	observeEvent(input$colCChange, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Columns ')
		testSet$settings <- "colComp"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		labs <- colnames(testSet$vals)
		cats <- cookedData$cats[!(cookedData$cats %in% labs)]
		nums <- cookedData$nums[!(cookedData$nums %in% labs)]
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('sels', 'Columns to compare', items=labs,
								as_source = FALSE, connect = c('ns', 'cs'), width="100%", item_class="success", placeholder="Select two or more columns to compare"),
					orderInput('ns', 'Numbers', items =  nums,
								as_source = FALSE, connect = c('cs', 'sels'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cats,
								as_source = FALSE, connect = c('ns', 'sels'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_cols", "OK"), fade = FALSE
				)
			)
		})
		
	observeEvent(input$grComp, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Groups ')
		removeModal()
		})
		
	observeEvent(input$colPred, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Columns')		
		testSet$settings <- "colPred"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('sels', 'Columns to compare', items=c(),
								as_source = FALSE, connect = c('ns', 'cs'), width="100%", item_class="success", placeholder="Select two or more columns to compare"),
					orderInput('ns', 'Numbers', items =  cookedData$nums,
								as_source = FALSE, connect = c('cs', 'sels'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cookedData$cats,
								as_source = FALSE, connect = c('ns', 'sels'), width="100%", item_class="primary", placeholder="Categoric variables"),							
				footer=actionButton("ok_cols", "OK"), fade = FALSE
				)
			)
		})	

	observeEvent(input$colPChange, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Columns ')
		testSet$settings <- "colComp"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		labs <- colnames(testSet$vals)
		cats <- cookedData$cats[(!cookedData$cats %in% labs)]
		nums <- cookedData$nums[(!cookedData$nums %in% labs)]
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('sels', 'Columns to compare', items=labs,
								as_source = FALSE, connect = c('ns', 'cs'), width="100%", item_class="success", placeholder="Select two or more columns to compare"),
					orderInput('ns', 'Numbers', items =  nums,
								as_source = FALSE, connect = c('cs', 'sels'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cats,
								as_source = FALSE, connect = c('ns', 'sels'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_cols", "OK"), fade = FALSE
				)
			)
		})
		
	observeEvent(input$grPred, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Groups')
		removeModal()
		})
	
	observeEvent(input$ok_cols, {
		if (length(input$sels_order)>1 & (all(input$sels_order %in% cookedData$cats)|all(input$sels_order %in% cookedData$nums))) {
			testSet$vals <- cookedData$cooked[,input$sels_order]
			testSet$vals <- testSet$vals[!is.null(testSet$vals)]
			removeModal()
			}
			
		else if (length(input$sels_order)<2) {testSet$vals <- NULL
			showModal(modalDialog(
				title="ERROR",
					h4("Not enough data - select two or more columns", style="font-face: bold; color: red"),
					orderInput('sels', 'Columns to compare', items=c(),
								as_source = FALSE, connect = c('ns', 'cs'), width="100%", item_class="primary", placeholder="Select two or more columns to compare"),
					orderInput('ns', 'Numbers', items =  cookedData$nums,
								as_source = FALSE, connect = c('cs', 'sels'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cookedData$cats,
								as_source = FALSE, connect = c('ns', 'sels'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_cols", "OK")			
				))
			}
			
		else {testSet$vals <- NULL
			showModal(modalDialog(
				title="ERROR",
					h4("Can't compare numbers to categories", style="font-face: bold; color: red"),
					orderInput('sels', 'Columns to compare', items=c(),
								as_source = FALSE, connect = c('ns', 'cs'), width="100%", item_class="primary", placeholder="Select two or more columns to compare"),
					orderInput('ns', 'Numbers', items =  cookedData$nums,
								as_source = FALSE, connect = c('cs', 'sels'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cookedData$cats,
								as_source = FALSE, connect = c('ns', 'sels'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_cols", "OK")			
				))
			}

		})
		
		
		
	observeEvent(input$selGrs, {
		# toggleModal(session, "grSel", toggle="open")
		})
		
	output$fulltab <- renderDataTable({
		req(cookedData$cooked, input$selGrs)
		cookedData$cooked
		})
	
	observeEvent(input$handleNAs, {
		req(cookedData$cooked, settings)
		showModal(
			modalDialog(
				title="Handle NAs",
				p("Choose what to do with the missing values. Some tests will not work if they are kept."),
				selectInput("naAction", label="",
					c("Ignore missing values"="ignore",
					"Display in plots, ignore in tests"="plots",
					"Keep missing values"="keep"
					), settings$na.ignore),
				footer=actionButton("ok_nas", "OK"),
				easyClose=T
				)
			)
	
		})
		
	observeEvent(input$ok_nas, {
		settings$na.ignore <- input$naAction
		removeModal()
		
		})
		
	output$outplot <- renderPlot({

		req(cookedData$cooked, plotinput())		
		
		plotSet <- cookedData$cooked
		
		pars <- plotinput()	
		
		req(length(pars$outcome)==1)
		req(length(pars$pred)<3)
		
		if (settings$na.ignore=="ignore") {plotSet <- plotSet %>% drop_na(c(pars$outcome, pars$pred))}
		
		# There are no predictors, plot just the outcome
		if (length(pars$pred)<1) {
			
			# Outcome is a factor
			if (pars$outcome %in% cookedData$cats) {
				plotType$current <- "bar_dist"
				p <- ggplot(plotSet, aes_string(pars$outcome))
				p <- p + geom_bar()}
				
			# Outcome is numeric	
			else if (pars$outcome %in% cookedData$nums) {
				req(input$histRange, input$histBins)
				plotType$current <- "histogram"
				p <- ggplot(plotSet[between(plotSet[[pars$outcome]],input$histRange[1],input$histRange[2]),], aes_string(pars$outcome))
				p <- p + geom_histogram(bins=input$histBins)}
			}
		
		# There is one predictor
		else if (length(pars$pred)==1){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictor is a factor --> boxplot
				if (pars$pred %in% cookedData$cats){
					p <- ggplot(plotSet, aes_string(pars$pred, pars$outcome))
					p <- p + geom_boxplot()
					}
				# The predictor is numeric --> scatterplot
				else {
					p <- ggplot(plotSet, aes_string(pars$pred, pars$outcome))
					p <- p + geom_point()
					}			
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictor is a factor --> barplot
				if (pars$pred %in% cookedData$cats){
					p <- ggplot(plotSet, aes_string(pars$pred, fill=pars$outcome))
					p <- p + geom_bar()
					}
				# The predictor is numeric --> logistic regression?
				else {			
					p <- ggplot(plotSet, aes_string(pars$pred, pars$outcome))
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
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], y=pars$outcome, fill=pars$pred[2]))
					p <- p + geom_boxplot()
					}
				# The predictors is numeric --> scatterplot with color gradient
				else if (all(pars$pred %in% cookedData$nums)){
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], y=pars$outcome, color=pars$pred[2]))
					p <- p + geom_point()
					}

				# Mixed predictors --> scatterplot with character mapping
				else {
					fil <- pars$pred %in% cookedData$nums
					p <- ggplot(plotSet, aes_string(x=pars$pred[fil], y=pars$outcome, pch=pars$pred[!fil]))
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
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], fill=pars$outcome))
					p <- p + geom_bar() + facet_wrap(pars$pred[2])
					}
				
				# The predictors are all numeric --> logistic regression with color
				else if (all(pars$pred %in% cookedData$nums)){
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], y=pars$outcome, color=pars$pred[2]))
					p <- p + geom_point()
					}					
				# Mixed predictors --> logistic regression with pch
				else {
					fil <- pars$pred %in% cookedData$nums			
					p <- ggplot(plotSet, aes_string(x=pars$pred[fil], y=pars$outcome))
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
	
	
	output$settings <- renderUI({
		req(settings$na.ignore)
		
		tagList(
			fluidRow(
				column(6,
					wellPanel(align="center",
						selectInput("naSetting", label="NA action",
								c("Ignore missing values"="ignore",
								"Display in plots, ignore in tests"="plots",
								"Keep missing values"="keep"),
								settings$na.ignore
								),
						hr()	
							)
						),
				column(6,
					wellPanel()
					)
				)
			)
		})
	
	output$naPlot <- renderPlot({
		req(cookedData$cooked)
		nas <- sum(is.na(cookedData$cooked))
		
		if (nas > 0) {bend(nas,nrow(cookedData$cooked)*ncol(cookedData$cooked))}
		else {return(NULL)}
		})
	
	output$naCount <- renderUI({
		req(cookedData$cooked)

			if (sum(is.na(cookedData$cooked))>0){
				tagList(
					h3("Overall", style="text-align: center"),
					fluidRow(
						column(3),
						column(6,
							wellPanel(align="center", 
								plotOutput("naPlot" ,height="200px", width="100%"),
								hr(),
								actionButton("handleNAs","What to do with missing values?")
								)
							),
						column(3)
						)
					)
				}
			else {invisible()}


		# <- paste('<h3 style="text-align: center">Overall</h3>', 
			# "<div class=row><div class=col-sm-3></div>", 
			# '<div class=col-sm-6><div class=well style="text-align:center>"',
				# plotOutput("naPlot" ,height="200px", width="100%"),
			# "</div><div class=col-sm-3></div></div></div>")
		
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
			while(rest >4) {
				while (ind <=4){
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
			if (length(row) == 4){
				wells <- paste(wells, '<div class="row">', sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<hr>",
						plotOutput(sprintf('%s_%s', "plot", label), height="200px", width="100%"),
						"<hr><p>", sep="")
						for (level in levels(cookedData$cooked[[label]])) {
						summarize <- paste(summarize,
							level,
							"\t",
							sum(cookedData$cooked[[label]]==level, na.rm=T),
							"<br/>",
							sep=""
								)
							}
					summarize <- paste(summarize, "</p>", sep="")
					wells <- paste(wells, '<div class=col-sm-3><div class="well" style="text-align:center">', summarize, "</div></div>", sep="")
					}
				
				wells <- paste(wells, "</div>", sep="")
			}
			
			if (length(row)!=4){
				wells <- paste(wells, '<div class="row">',ifelse(length(row)!=3, paste('<div class=col-sm-',5-length(row), "></div>", sep=""), ""), sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<hr>",
						plotOutput(sprintf('%s_%s', "plot", label), height="200px", width="100%"),
						"<hr><p>", sep="")
						for (level in levels(cookedData$cooked[[label]])) {
						summarize <- paste(summarize,
							level,
							"\t",
							sum(cookedData$cooked[[label]]==level, na.rm=T),
							"<br/>",
							sep=""
								)
							}
					summarize <- paste(summarize, "</p>", sep="")
					wells <- paste(wells, '<div class=col-sm-', ifelse(length(row)==2,3,4),'><div class="well" style="text-align:center">', summarize, "</div></div>", sep="")
					}
				
				wells <- paste(wells, ifelse(length(row)!=3, paste('<div class=col-sm-',5-length(row), "></div>", sep=""), ""),"</div>", sep="")			

				}

			}
			
		wells <- paste(wells, '<div class=row><h3 style="text-align: center">Numbers</h3></div>', sep="")	

		rows <- list()
		cRow <- c()
		ind <- 1
		cRowInd <- 1
		rest <- length(cookedData$nums)
		nums <- cookedData$nums[order(seq(-1,-rest))]
		
		for (label in nums) {
			while(rest >4) {
				while (ind <=4){
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
			if (length(row) == 4){
				wells <- paste(wells, '<div class="row">', sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<p>", sep="")
					mn = round(mean(cookedData$cooked[[label]], na.rm=T),3)
					quants <-  round(quantile(cookedData$cooked[[label]], c(0,0.25,0.5,0.75,1), na.rm=T),3)
					nas <- sum(is.na(cookedData$cooked[[label]])|is.nan(cookedData$cooked[[label]]))
					summarize <- paste(
						"<h4>",
						label,
						"</h4>",
						"<hr>",
						plotOutput(sprintf('%s_%s', "plot", label), height="200px", width="100%"),
						"<hr>",
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
							wells <- paste(wells, '<div class=col-sm-3><div class="well" style="text-align:center">', summarize, "</div></div>", sep="")
							}
				
				wells <- paste(wells, "</div>", sep="")
			}
			
			if (length(row)!=4){
				wells <- paste(wells, '<div class="row">',ifelse(length(row)!=3, paste('<div class=col-sm-',5-length(row), "></div>", sep=""), ""), sep="")
				for (label in row)	{
					summarize <- paste("<h4>",label, "</h4>","<p>", sep="")
					mn = round(mean(cookedData$cooked[[label]], na.rm=T),3)
					quants <-  round(quantile(cookedData$cooked[[label]], c(0,0.25,0.5,0.75,1), na.rm=T),3)
					nas <- sum(is.na(cookedData$cooked[[label]])|is.nan(cookedData$cooked[[label]]))
					summarize <- paste(
						"<h4>",
						label,						
						"</h4>",
						"<hr>",
						plotOutput(sprintf('%s_%s', "plot", label), height="200px", width="100%"),
						"<hr>",					
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
					wells <- paste(wells, '<div class=col-sm-', ifelse(length(row)==2,3,4),'><div class="well" style="text-align:center">', summarize, "</div></div>", sep="")
					}
				
				wells <- paste(wells, ifelse(length(row)!=3, paste('<div class=col-sm-',5-length(row), "></div>"), ""),"</div>", sep="")			

			}	
			
		}		
				
		HTML(wells)
		})

	output$vartype <- renderUI({
		req(rawData())
		
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
	
	output$testSettings <- renderUI({
		req(testSet$settings)
		if (testSet$settings %in% c("numbers2", "ttest", "wilcox")){
			tagList(
				fluidRow(
					column(3),
					column(6,
						wellPanel(align="center",
							h3("Test setup"),
							hr()
							h4("Data"),
							checkboxInput("paired", "Individual rows form pairs", FALSE),
							h4("Hypothesis"),
							radioButtons("tails", paste("The mean in", colnames(testSet$vals)[1], "is:"), c(
								"Lower"="less",
								"Different"="two.sided",
								"Higher"="greater"),
								selected="two.sided"
								),
							p(style="face:bold", paste("in comparison to", colnames(testSet$vals)[2]))
							)
						),
					column(3)
					)
				)
			}
			})
	
	output$testOutput <- renderUI({

		req(testSet$settings, testSet$vals)
		
		if (testSet$settings == "colComp") {
			if (ncol(testSet$vals)==2){
				if (all(colnames(testSet$vals) %in% cookedData$cats)) {
					# Check that we can do X-squared:
					#1. Check that there is an overlap in the levels
					c1 <- testSet$vals[,1]
					c2 <- testSet$vals[,2]
					
					if (!any(levels(c1) %in% levels(c2))){t.obj <- "applesToOranges"
						testSet$test <- "none"
						}
					else {
						# Merge the levels in both columns, so that levels not present in col A equal to 0 rather than error, remove NAs
						levs <- sort(unique(c(levels(c1), levels(c2))))
						levs <- levs[!is.na(levs)]

						c1 <- c1[!is.na(c1)]
						c2 <- c2[!is.na(c2)]
						
						c1 <- factor(c1, levels=levs)
						c2 <- factor(c2, levels=levs)	
						
						t1 <- as.vector(table(c1))
						t2 <- as.vector(table(c2))
						
						gt1 <- t1 > t2
						gt2 <- t2 > t1
						
						tab <- data.frame(t1,t2)
						
						#There should not be any zeros or expected values below 5 
						cond1 <- sum(tab==0)
						cond2 <- getExp(tab)
						
						if (cond1 == 0 & !any(cond2 < 5)){
							t.obj <- chisq.test(t1, t2)
							testSet$test <- "chisq2"
							}
							
						else {		
							t.obj <- fisher.test(t1, t2)
							testSet$test <- "fisher2"
							}
							
						}

					}
				if (all(colnames(testSet$vals) %in% cookedData$nums)) {
					# Check that we can do t-test
					c1 <- testSet$vals[,1]
					c2 <- testSet$vals[,2]
					testSet$settings <- "numbers2"
					
					req(input$tails, input$paired)
					
					print(input$tails)
					print(input$paired)
				
					}
				}
			else {}
			
			}
	
		})
		

})
