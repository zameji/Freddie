library(shiny)
library(shinyLP)
library(shinyBS)
library(shinyjqui)
library(dplyr)
library(tools)
library(tidyverse)
library(lmtest)
library(lm.beta)
library(ggplot2)
library(RColorBrewer)	
library(car)
library(data.table)
options(shiny.reactlog = TRUE)
options(shiny.usecairo=FALSE)
colMax <- function(data) sapply(data, max, na.rm = TRUE)
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
	
normRes <- function(vals){
	m <- mean(vals)
	res <- vals-m
	shapiro.test(res)$p.value > 0.05
	}	

getColNames <- function(ct){
	colLabs <- unlist(lapply(1:ncol(ct), function (i){
			paste(
				paste(rownames(ct)[ct[,i]>0], collapse=", "),
				"vs.",
				paste(rownames(ct)[ct[,i]<0], collapse=", ")
				)
		}))

	return(colLabs)
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


effPlot <-
function (eff, type, boundary=2, edges = 100, labels=NULL, outer.radius = 1, 
          inner.radius=0.75,
          init.angle = 0, density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, inside=NULL, ...){
	if (!all(is.numeric(c(eff, boundary)))){stop("'x' values must be factors.")}
	if (is.null(labels)){labels <- c("", "")}
	effb <- eff
	eff <- min(abs(eff), 2)

	x <- abs(c(0, (boundary-eff)/boundary, 1))
	rat <- abs(0.5*eff/boundary)
	efcol <- hsv(max(0.5-rat, 0),1,1)
	col <- c("grey90", efcol)
	
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
	
	text(0,0.2,paste(type, ": ", abs(round(effb, 2)), sep=""), family="serif", cex=1.25)
 	text(-1,-0.2, "Weak", family="serif", cex=1.25)
	text(1,-0.2,"Strong", family="serif", cex=1.25)
	title(main = main, ...)
	invisible(NULL)
	}

corPlot <-
function (eff, type="spearman", boundary=1, edges = 100, labels=NULL, outer.radius = 1, 
          inner.radius=0.75,
          init.angle = 0, density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, inside=NULL, ...){

	if (type == "spearman") {type <- "Spearman's rho"} else {type <- "Pearson's r"}

	if (!all(is.numeric(c(eff, boundary)))){stop("'x' values must be factors.")}
	if (is.null(labels)){labels <- c("", "")}
	effb <- eff
	#eff <- min(abs(eff), 2)

	x <- c(-1, max(eff-0.02,-1), min(eff+0.02,1), 1)
	x <- (x/-2)+0.5
	rat <- abs(0.4*eff)
	efcol <- hsv(max(rat, 0),1,1)
	col <- c("grey90", efcol, "grey90")
	
	dx <- diff(x)
	print(dx)
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
	
	text(0,0.2,paste(type, ": ", abs(round(effb, 2)), sep=""), family="serif", cex=1.25)
 	text(-1,-0.2, "Strong negative", family="serif", cex=1.25)
	text(1,-0.2,"Strong positive", family="serif", cex=1.25)
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
	
getEffLab <- function(eff) {
	eff <- abs(eff)
	if (eff <0.8){
		if (eff < 0.2) {return("very small")}
		else if (eff < 0.5) {return("small")}
		else {return("medium")}
		}
	else {
		if (eff < 1.2) {return("large")}
		else if (eff < 2) {return("very large")}
		else {return("huge")}	
		}
	}

getCorLab <- function(eff, rel = T) {
	eff <- abs(eff)
	if (rel == T){if (eff <0.5){
			if (eff < 0.1) {return("very weak")}
			else if (eff < 0.3) {return("weak")}
			else {return("medium")}
		}
		else {
			return("strong")
			}
		}
		
	else {
		if (eff <0.5){
			if (eff < 0.1) {return("at all")}
			else if (eff < 0.3) {return("to a limited degree")}
			else {return("reasonably well")}
			}
		else {
			return("well")
			}
		}
	}
	
randdata <- function(){
	seed0 <- round(rnorm(100, 100, 25))
	rd <- data.frame(
		gender=c(sample(c(rep("female",10),rep("male",8)), 50, replace=T), 
			sample(c(rep("female",8),rep("male",10)), 50, replace=T)),
		age=c(round(runif(50,min=10,max=30),0), round(runif(50,min=20,max=45),0)),
		education=c(sample(c(rep("3_university",7),rep("2_high",5),rep("1_elementary",2),rep("4_doctorate",10)),50,replace=T),
			sample(c(rep("3_university",5),rep("2_high",7),rep("1_elementary",10),rep("4_doctorate",2)),50,replace=T)),
		residence=c(sample(c(rep("East",7),rep("Mid",5),rep("West",1)),50,replace=T),
			sample(c(rep("East",2),rep("Mid",7),rep("West",10)),50,replace=T)),
		birthplace=c(sample(c(rep("East",5),rep("Mid",4),rep("West",1)),50,replace=T),
			sample(c(rep("East",3),rep("Mid",8),rep("West",15)),50,replace=T)),	
		rhoticity=c(abs(round(rnorm(50,0.2, 0.1),2)),abs(round(rnorm(50,0.5, 0.1),2))),
		glottalisation=c(sample(c(rep("2_preconsonantal",7), rep("3_preconsonantal+initial",1), rep("1_none",10)),50,replace=T),
			sample(c(rep("2_preconsonantal",7), rep("3_preconsonantal+initial",3), rep("1_none",1)),50,replace=T)),
		before_class=seed0,
		mid_class=seed0+rnorm(100,10,26),
		after_class=seed0+rnorm(100,20,25)
		)
		
	# rd <- as.data.frame(lapply(rd, getna))
	
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

getSummary <- function(dt.i){
	smr <- lapply(levels(dt.i[,2]), function(i){
		 m <- mean(dt.i[dt.i[,2]==i,1], na.rm=T)
		 s <- sd(dt.i[dt.i[,2]==i,1], na.rm=T)/sqrt(length(dt.i[dt.i[,2]==i,1]))
   		 return(c(i, m,s))
		})
	smr <- do.call("rbind",smr)	
	colnames(smr) <- c("l", "m", "s")
	smr <- as.data.frame(smr)
	smr$m <- as.numeric(as.character(smr$m))
	smr$s <- as.numeric(as.character(smr$s))
	return(smr)
	}
	
shinyServer(function(input, output, session) {
	cookedData <- reactiveValues(cats=NULL, nums=NULL, cooked=NULL)				
	plotType <- reactiveValues(current=NULL)
	importSettings <- reactiveValues(header=TRUE, sep="\t", quoter='"')	
	testSet <- reactiveValues(settings=NULL, vals=NULL, manConts=NULL)
	settings <- reactiveValues(na.ignore="ignore", mod="real", cols="b&w", serif="serif")
	summaryVals <- reactiveValues(freqs=NULL)
	anovaGroups <- reactiveValues(level=NULL, finished=FALSE, results=NULL)
	
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
	
	observeEvent(input$saveSettings, {
		settings$na.ignore <- input$naSetting
		settings$cols <- input$plotCol
		settings$serif <- input$serif
		
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
			
		# colnames(cookedData$cooked) <- gsub("_", " ", colnames(cookedData$cooked))
		# names(cookedData$cats) <- gsub("_", " ", names(cookedData$cats))
		# names(cookedData$nums) <- gsub("_", " ", names(cookedData$nums))
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
		
	observeEvent(input$colComp, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Columns ')
		testSet$settings <- "colComp"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		anovaGroups$finished <- FALSE
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
		labs <- labs[labs %in% c(cookedData$cats, cookedData$nums)]
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
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('ind', 'Column to evaluate', items=c(),
								as_source = FALSE, connect = c('ns', 'pred', 'cs'), width="100%", item_class="success", placeholder="Select the measured variable"),
					orderInput('pred', 'Column with groups', items=c(),
								as_source = FALSE, connect = c('ns', 'cs', 'ind'), width="100%", item_class="success", placeholder="Select the column defining the groups"),					
					orderInput('ns', 'Numbers', items =  cookedData$nums,
								as_source = FALSE, connect = c('cs', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cookedData$cats,
								as_source = FALSE, connect = c('ns', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_grs", "OK"), fade = FALSE
				)		
			)
	
		})	
	
	observeEvent (input$ok_grs, {
		req(input$ind_order, input$pred_order)
		testSet$manConts <- NULL
		if (length(input$ind_order) == 1 & length(input$pred_order)==1 & input$pred_order[1] %in% cookedData$cats){
		
			testSet$settings <- "grComp"
			c1 <- input$ind_order
			c2 <- input$pred_order
			testSet$vals <- cookedData$cooked[,c(input$ind_order, input$pred_order)]
			updateNavbarPage(session, 'mainnavbar', selected = 'Groups ')
			removeModal()			
			}
			
		else {
			if (input$pred_order[1] %in% cookedData$nums){
				showModal(
				modalDialog(
					title="ERROR",
						p("The grouping variable must be a category, not a number",style="font-face: bold; color: red"),
						orderInput('ind', 'Column to evaluate', items=input$ind_order,
									as_source = FALSE, connect = c('ns', 'cs', 'pred'), width="100%", item_class="success", placeholder="Select the measured variable"),
						orderInput('pred', 'Column with groups', items=c(),
									as_source = FALSE, connect = c('ns', 'cs', 'ind'), width="100%", item_class="success", placeholder="Select the column defining the groups"),					
						orderInput('ns', 'Numbers', items =  cookedData$nums,
									as_source = FALSE, connect = c('cs', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Numeric variables"),
						orderInput('cs', 'Categories', items = cookedData$cats,
									as_source = FALSE, connect = c('ns', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Categoric variables"),						
						
					footer=actionButton("ok_grs", "OK"), fade = FALSE
					)		
				)
				}
			else{
				showModal(
				modalDialog(
					title="ERROR",
						p("One measured variable and one grouping variable needed",style="font-face: bold; color: red"),
						orderInput('ind', 'Column to evaluate', items=c(),
									as_source = FALSE, connect = c('ns', 'cs', 'pred'), width="100%", item_class="success", placeholder="Select the measured variable"),
						orderInput('pred', 'Column with groups', items=c(),
									as_source = FALSE, connect = c('ns', 'cs', 'ind'), width="100%", item_class="success", placeholder="Select the column defining the groups"),					
						orderInput('ns', 'Numbers', items =  cookedData$nums,
									as_source = FALSE, connect = c('cs', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Numeric variables"),
						orderInput('cs', 'Categories', items = cookedData$cats,
									as_source = FALSE, connect = c('ns', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Categoric variables"),						
						
					footer=actionButton("ok_grs", "OK"), fade = FALSE
					)		
				)			
				}
			}
	})
	
	observeEvent(input$grCChange, {
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		outp <- input$ind_order
		preds <- input$pred_order
		labs <- unique(c(outp, preds))
		cats <- cookedData$cats[(!cookedData$cats %in% labs)]
		nums <- cookedData$nums[(!cookedData$nums %in% labs)]	
		
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('ind', 'Column to evaluate', items=input$ind_order,
								as_source = FALSE, connect = c('ns', 'pred', 'cs'), width="100%", item_class="success", placeholder="Select the measured variable"),
					orderInput('pred', 'Column with groups', items=input$pred_order,
								as_source = FALSE, connect = c('ns', 'cs', 'ind'), width="100%", item_class="success", placeholder="Select the column defining the groups"),					
					orderInput('ns', 'Numbers', items =  nums,
								as_source = FALSE, connect = c('cs', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cats,
								as_source = FALSE, connect = c('ns', 'pred', 'ind'), width="100%", item_class="primary", placeholder="Categoric variables"),						
					
				footer=actionButton("ok_grs", "OK"), fade = FALSE
				)		
			)
		})	
		
	observeEvent(input$modSet, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Model data')		
		testSet$settings <- "mod"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('outp', 'Column to predict', items=c(),
								as_source = FALSE, connect = c('ns', 'cs', 'pred'), width="100%", item_class="success", placeholder="Select the column to predict"),
					orderInput('pred', 'Columns with predictors', items=c(),
								as_source = FALSE, connect = c('ns', 'cs', 'outp'), width="100%", item_class="success", placeholder="Select one or more columns as predictors"),								
					orderInput('ns', 'Numbers', items =  cookedData$nums,
								as_source = FALSE, connect = c('cs', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cookedData$cats,
								as_source = FALSE, connect = c('ns', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Categoric variables"),							
				footer=actionButton("ok_mod", "OK"), fade = FALSE
				)
			)
		})	

	observeEvent(input$modChange, {
		updateNavbarPage(session, 'mainnavbar', selected = 'Model data')
		testSet$settings <- "mod"
		req(cookedData$cooked, cookedData$cats, cookedData$nums)
		outp <- input$outp_order
		preds <- input$pred_order
		labs <- unique(c(outp, preds))
		cats <- cookedData$cats[(!cookedData$cats %in% labs)]
		nums <- cookedData$nums[(!cookedData$nums %in% labs)]
		showModal(
			modalDialog(
				title="Select columns",
					orderInput('outp', 'Column to predict', items=outp,
								as_source = FALSE, connect = c('ns', 'cs', 'pred'), width="100%", item_class="success", placeholder="Select the column to predict"),
					orderInput('pred', 'Columns with predictors', items=preds,
								as_source = FALSE, connect = c('ns', 'cs', 'outp'), width="100%", item_class="info", placeholder="Select one or more columns as predictors"),								
					orderInput('ns', 'Numbers', items =  nums,
								as_source = FALSE, connect = c('cs', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cats,
								as_source = FALSE, connect = c('ns', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Categoric variables"),							
				footer=actionButton("ok_mod", "OK"), fade = FALSE
				)
			)
		})

	observeEvent(input$ok_cols, {
		if (length(input$sels_order)>1 & (all(input$sels_order %in% cookedData$cats)|all(input$sels_order %in% cookedData$nums))) {
			testSet$vals <- cookedData$cooked[,input$sels_order]
			testSet$vals <- testSet$vals[!is.null(testSet$vals)]
			testSet$manConts <- NULL
			testSet$twoCol <- NULL
			if (length(input$sels_order) == 2 & all(colnames(testSet$vals) %in% cookedData$nums)) {
				showModal(modalDialog(
					title="Test type",
					h4("Decide what to do with these columns"),
					p("Compare distribution - e.g. for counts from a corpus, where each row is a different option"),
					actionButton("compCount", "Proceed"),
					p("Compare means - e.g. for measurements, where each row is one participant"),
					actionButton("compMeans", "Proceed"),
					p("Compare values - to determine the strength of relationship between two numeric variables"),
					actionButton("correlate", "Proceed"),
					fade=FALSE,
					easyClose=FALSE
					))
				}
				
			else {removeModal()}
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
	
	observeEvent(input$compCount, {
		testSet$twoCol <- "compCount"
		showModal(modalDialog(
			title="Set rownames",
			p("Select the column which contains the names of the categories that the counts represent"),
			selectInput("namesCol", label="Columns with labels",
				choices=c(cookedData$cats),
				),
			actionButton("ok_namesCol", "OK"),
			fade=FALSE,
			easyClose=FALSE
			))
		})
	observeEvent(input$ok_namesCol, {
		testSet$namesCol <- input$namesCol
		removeModal()
		})
	observeEvent(input$compMeans, {
		testSet$twoCol <- "compMeans"
		removeModal()
		})
	observeEvent(input$correlate, {
		testSet$twoCol <- "correlate"
		removeModal()
		})
	
	observeEvent(input$ok_mod, {
		anovaGroups$finished <- NULL
		if (length(input$outp_order)==1 & length(input$pred_order)>0) {
			testSet$vals <- cookedData$cooked[,c(input$outp_order, input$pred_order)]
			testSet$manConts <- NULL
			removeModal()
			}
			
		else if (length(input$outp_order)!=1) {testSet$vals <- NULL
			if (length(input$outp_order)!=0 | length(input$outp_order)!=0){
				outp <- input$outp_order
				preds <- input$pred_order
				labs <- unique(c(outp, preds))
				cats <- cookedData$cats[(!cookedData$cats %in% labs)]
				nums <- cookedData$nums[(!cookedData$nums %in% labs)]
				}
			else {
				if(length(input$outp_order)==0) {outp <- c()} else {outp <- input$outp_order}
				if(length(input$pred_order)==0) {preds <- c()} else {preds <- input$pred_order}
				labs <- unique(c(outp, preds))
				cats <- cookedData$cats[(!cookedData$cats %in% labs)]
				nums <- cookedData$nums[(!cookedData$nums %in% labs)]
				}
			showModal(modalDialog(
				title="ERROR",
					h4("Select one column to be predicted", style="font-face: bold; color: red"),
					orderInput('outp', 'Column to predict', items=outp,
								as_source = FALSE, connect = c('ns', 'cs', 'pred'), width="100%", item_class="success", placeholder="Select the column to predict"),
					orderInput('pred', 'Columns with predictors', items=preds,
								as_source = FALSE, connect = c('ns', 'cs', 'outp'), width="100%", item_class="info", placeholder="Select one or more columns as predictors"),								
					orderInput('ns', 'Numbers', items =  nums,
								as_source = FALSE, connect = c('cs', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cats,
								as_source = FALSE, connect = c('ns', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Categoric variables"),							
				footer=actionButton("ok_mod", "OK")			
				))
			}
			
		else {testSet$vals <- NULL
			if (length(input$outp_order)!=0 | length(input$outp_order)!=0){
				outp <- input$outp_order
				preds <- input$pred_order
				labs <- unique(c(outp, preds))
				cats <- cookedData$cats[(!cookedData$cats %in% labs)]
				nums <- cookedData$nums[(!cookedData$nums %in% labs)]
				}
			else {
				if(length(input$outp_order)==0) {outp <- c()} else {outp <- input$outp_order}
				if(length(input$pred_order)==0) {preds <- c()} else {preds <- input$pred_order}
				labs <- unique(c(outp, preds))
				cats <- cookedData$cats[(!cookedData$cats %in% labs)]
				nums <- cookedData$nums[(!cookedData$nums %in% labs)]
				}
			showModal(modalDialog(
				title="ERROR",
					h4("Select some predictors", style="font-face: bold; color: red"),
					orderInput('outp', 'Column to predict', items=outp,
								as_source = FALSE, connect = c('ns', 'cs', 'pred'), width="100%", item_class="success", placeholder="Select the column to predict"),
					orderInput('pred', 'Columns with predictors', items=preds,
								as_source = FALSE, connect = c('ns', 'cs', 'outp'), width="100%", item_class="info", placeholder="Select one or more columns as predictors"),								
					orderInput('ns', 'Numbers', items =  nums,
								as_source = FALSE, connect = c('cs', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Numeric variables"),
					orderInput('cs', 'Categories', items = cats,
								as_source = FALSE, connect = c('ns', 'outp', 'pred'), width="100%", item_class="primary", placeholder="Categoric variables"),							
				footer=actionButton("ok_mod", "OK")			
				))
			}

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
		
	observeEvent(input$setContsCol,{
		
		req(testSet$vals)
		
		anovaGroups$finished <- NULL
				
		if (testSet$settings=="colComp") {
			dat0 <- testSet$vals
			ids <- colnames(dat0)
			dat <- data.frame(values__=c(), id__=c())
			for (i in ids) {
				dat <- rbind(dat, data.frame(values__=dat0[, i], id__=rep(i, length(dat0[, i]))))
				}
			
			dat$id__ <- factor(dat$id__)			
			if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}		
		
			groups <- levels(dat[,2])
			}
			
		else if (testSet$settings=="grComp"){
			dat <- testSet$vals
			colnames(dat) <- c("values__", "id__")			
			dat$id__ <- factor(dat$id__)
			
			if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}		
				
			groups <- levels(dat[,2])
			}
		
		if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}
		
		print("DOING")		
		anovaGroups$groupMatrix <- data.frame(matrix(ncol=length(groups), nrow=10))
		colnames(anovaGroups$groupMatrix) <- groups
				
		anovaGroups$groupMatrix[1,] <- 1
		anovaGroups$level <- 1		
		anovaGroups$groupnos <- c(2,3)
						
		showModal(
			modalDialog(
				title="Set contrasts",
				p("You can now divide the labels into groups. To do so, draw the labels into the appropriate bin. Labels left aside will still be used in the overall evaluation."),
				p(style="col: 'red'; font-weight: bold", "Important: To make sure that the results reported are correct, only draw labels from ONE LINE in each round of contrast set up. Contrasting labels from different rows may lead to incorrect results."),
				fluidRow(
					column(6,
						wellPanel(align="center",
							orderInput('gr1', 'Group 1', items=c(),
							as_source = FALSE, connect = c("gr2", "inp1"), width="100%", item_class="primary", placeholder="")
							)),
					column(6,
						wellPanel(align="center",
							orderInput('gr2', 'Group 2', items=c(),
							as_source = FALSE, connect =  c("gr1", "inp1"), width="100%", item_class="primary", placeholder="")				
							))
					),
				fluidRow(						
					column(12,
						wellPanel(align="center",
							orderInput('anovaInp1', 'Labels available', items=groups,
							as_source = FALSE, connect = c("gr1", "gr2"), width="100%", item_class="primary", placeholder="")				
							)
						)					
					),
				footer=tagList(
					actionButton("nextConts", "Next contrast"),
					actionButton("saveConts", "Done")
					),
				fade=FALSE
				)
			)
		})
	observeEvent(input$setContsGr,{
		
		req(testSet$vals)
		
		anovaGroups$finished <- NULL
				
		if (testSet$settings=="colComp") {
			dat0 <- testSet$vals
			ids <- colnames(dat0)
			dat <- data.frame(values__=c(), id__=c())
			for (i in ids) {
				dat <- rbind(dat, data.frame(values__=dat0[, i], id__=rep(i, length(dat0[, i]))))
				}
			
			dat$id__ <- factor(dat$id__)			
			if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}		
		
			groups <- levels(dat[,2])
			}
			
		else if (testSet$settings=="grComp"){
			dat <- testSet$vals
			colnames(dat) <- c("values__", "id__")			
			dat$id__ <- factor(dat$id__)
			
			if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}		
				
			groups <- levels(dat[,2])
			}
		
		if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}
		
		print("DOING")		
		anovaGroups$groupMatrix <- data.frame(matrix(ncol=length(groups), nrow=10))
		colnames(anovaGroups$groupMatrix) <- groups
				
		anovaGroups$groupMatrix[1,] <- 1
		anovaGroups$level <- 1		
		anovaGroups$groupnos <- c(2,3)
						
		showModal(
			modalDialog(
				title="Set contrasts",
				p("You can now divide the labels into groups. To do so, draw the labels into the appropriate bin. Labels left aside will still be used in the overall evaluation."),
				p(style="col: 'red'; font-weight: bold", "Important: To make sure that the results reported are correct, only draw labels from ONE LINE in each round of contrast set up. Contrasting labels from different rows may lead to incorrect results."),
				fluidRow(
					column(6,
						wellPanel(align="center",
							orderInput('gr1', 'Group 1', items=c(),
							as_source = FALSE, connect = c("gr2", "inp1"), width="100%", item_class="primary", placeholder="")
							)),
					column(6,
						wellPanel(align="center",
							orderInput('gr2', 'Group 2', items=c(),
							as_source = FALSE, connect =  c("gr1", "inp1"), width="100%", item_class="primary", placeholder="")				
							))
					),
				fluidRow(						
					column(12,
						wellPanel(align="center",
							orderInput('anovaInp1', 'Labels available', items=groups,
							as_source = FALSE, connect = c("gr1", "gr2"), width="100%", item_class="primary", placeholder="")				
							)
						)					
					),
				footer=tagList(
					actionButton("nextConts", "Next contrast"),
					actionButton("saveConts", "Done")
					),
				fade=FALSE
				)
			)
		})
	observeEvent(input$saveConts, {
		anovaGroups$level <- anovaGroups$level + 1
		if (length(input$gr1_order)>0){anovaGroups$groupMatrix[anovaGroups$level, input$gr1_order] <- anovaGroups$groupnos[1]}
		if (length(input$gr2_order)>0){anovaGroups$groupMatrix[anovaGroups$level, input$gr2_order] <- anovaGroups$groupnos[2]}
		
		if (!is.null(anovaGroups$finished)){anovaGroups$finished <- anovaGroups$finished + 1}
		else {anovaGroups$finished <- 1}
		testSet$manConts <- TRUE
		
		removeModal()
	})	
	observeEvent(input$nextConts, { 
		anovaGroups$level <- anovaGroups$level + 1	
		anovaGroups$groupMatrix[anovaGroups$level, input$gr1_order] <- anovaGroups$groupnos[1]
		anovaGroups$groupMatrix[anovaGroups$level, input$gr2_order] <- anovaGroups$groupnos[2]
		if (testSet$settings=="colComp"|  (testSet$settings == "grComp" & is.numeric(testSet$vals[,1]))){
			inps <- c("gr1", "gr2", paste("anovaInp", seq(1,max(anovaGroups$groupnos))))
			inps <- gsub(" ", "", inps)
			
			anovaGroups$groupnos <- anovaGroups$groupnos + 2
			groups	<- colnames(anovaGroups$groupMatrix)
			
			groupItems <- list("initial" = c(1,2,3))
			
			for (inp in inps[-c(1,2)]) {
				inpNo <- as.integer(gsub("anovaInp", "", inp))
				# print(inpNo)
				fil <- colMax(anovaGroups$groupMatrix)==inpNo
				its <- groups[fil]
				# print(its)
				if (length(its) > 0){groupItems[[inp]] <- its}
				}		
			}					

		else if (testSet$settings == "grComp" & is.factor(testSet$vals[,1])){
			anovaGroups$groupnos <- anovaGroups$groupnos + 2
			groups	<- colnames(anovaGroups$groupMatrix)		
			inps <- c("gr1", "gr2", "i")
			}
		
		# str(groupItems)
		showModal(
			modalDialog(
				title="Set contrast",
				p("You can now divide the labels into groups. To do so, draw the labels into the appropriate bin. Labels left aside will still be used in the overall evaluation."),
				p(style="col: 'red'; font-weight: bold", "Important: To make sure that the results reported are correct, only draw labels from ONE LINE in each round of contrast set up. Contrasting labels from different rows may lead to incorrect results."),
				fluidRow(
					column(6,
						wellPanel(align="center",
							orderInput('gr1', 'Group 1', items=c(),
							as_source = FALSE, connect = inps[inps!="gr1"], width="100%", item_class="primary", placeholder="")
							)),
					column(6,
						wellPanel(align="center",
							orderInput('gr2', 'Group 2', items=c(),
							as_source = FALSE, connect =  inps[inps!="gr2"], width="100%", item_class="primary", placeholder="")				
							))
					),
				tagList(
					if (testSet$settings=="colComp"|  (testSet$settings == "grComp" & is.numeric(testSet$vals[,1]))){
						lapply(1:length(names(groupItems)[names(groupItems)!="initial"]), function(x) {
							i <- names(groupItems)[names(groupItems)!="initial"][x]
							# print(i)
							# print(groupItems[i])
							# print(inps[inps!=i])
								wellPanel(align="center",
									orderInput(i, paste('Comparison ', gsub("anovaInp", "", i, fixed=T), sep="", collapse=""), items=groupItems[[i]],
									as_source = FALSE, connect = inps[inps!=i], width="100%", item_class="primary", placeholder="")								
									)
							})					
						}
					else if (testSet$settings == "grComp" & is.factor(testSet$vals[,1])){

						wellPanel(align="center",
							orderInput("i", 'Groups', items=levels(testSet$vals[,2]),
							as_source = FALSE, connect = inps[inps!="i"], width="100%", item_class="primary", placeholder="")								
							)
						}						
					),
				footer=tagList(
					actionButton("nextConts", "Next contrast"),
					actionButton("saveConts", "Done")
					),
				fade=FALSE
				)				
			)
	
		})
	observeEvent(input$doATestCol, {
		print(input$doATestCol)
		print(is.null(testRes()))})
	observeEvent(input$doATestGr, {})
	
	aovConts <- reactive({
		# reactlog::listDependencies()	
		req(testSet$vals)
		req(!is.null(input$doATestCol)|!is.null(input$doATestGr))
		
		if (!is.null(input$doATestCol)) {req(input$doATestCol > 0)}
		else {
			req(input$doATestGr > 0)
			}

		if (testSet$settings == "colComp"){
			dat0 <- testSet$vals
			ids <- colnames(dat0)
			dat <- data.frame(values__=c(), id__=c())
			for (i in ids) {
				dat <- rbind(dat, data.frame(values__=dat0[[i]], id__=rep(i, length(dat0[[i]]))))
				}
			dat$id__ <- factor(dat$id__)					
			}
		else if (testSet$settings=="grComp"){
			dat <- testSet$vals
			}
		
		if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}		
								
		
		if (!is.null(input$setContsGr)){
			if (input$setContsGr==0) {manualContrasts <- "GrFALSE"}
			else {manualContrasts <- "GrTRUE"}
			}
			
		if (!is.null(input$setContsCol)){
			if (input$setContsCol==0) {manualContrasts <- "ColFALSE"}
			else {manualContrasts <- "ColTRUE"}
			}

		if (manualContrasts %in% c("ColFALSE","GrFALSE")){
			if (testSet$settings=="colComp"){
				print("ContsNO")
				return(contrasts(dat$id__))}
			else if (testSet$settings=="grComp"){
				print("ContsNO")
				return(contrasts(dat[,2]))}		
			}
			
		else{
			req(!is.null(input$setContsGr)|!is.null(input$setContsCol))
			
			if (!is.null(input$setContsGr)) {req(input$setContsGr > 0)}
			else {
				req(input$setContsCol > 0)
				}			

			req(anovaGroups$groupMatrix, anovaGroups$finished)
									
			conts <- anovaGroups$groupMatrix[-1,]
			conts <- conts[1:((anovaGroups$groupnos[2]-1)/2),]
			contTable <- as.data.frame(t(conts))
			contTable <- sapply(1:ncol(contTable), function(i){
				iCol <- contTable[,i]
				iCol[iCol==0] <- NA
				ifelse(!is.na(iCol),ifelse(iCol%%2==0, -1/sum(iCol%%2==0, na.rm=T), 1/sum(iCol%%2!=0, na.rm=T)),0)
				})
				
			inps <- c(paste("anovaInp", seq(1,max(anovaGroups$groupnos))))
			inps <- gsub(" ", "", inps)
			
			# anovaGroups$groupnos <- anovaGroups$groupnos + 2
			groups	<- colnames(anovaGroups$groupMatrix)
			
			groupItems <- list("initial" = c(1,2,3))
			
			for (inp in inps) {
				inpNo <- as.integer(gsub("anovaInp", "", inp))
				fil <- colMax(anovaGroups$groupMatrix)==inpNo
				its <- groups[fil]
				if (length(its) > 0){groupItems[[inp]] <- its}
				}
				
			groupItems <- groupItems[!(names(groupItems) %in% c("initial", "anovaInp1"))]
			
			rownames(contTable) <- levels(dat[,2])
			colnames(contTable) <- getColNames(contTable)
			contrasts(dat[,2]) <- as.matrix(contTable)				
			# print(contTable)
			print("ContsYES")
			return(contTable)
			}

		})


	output$outplot <- renderPlot({

		req(cookedData$cooked, plotinput())		
		
		plotSet <- cookedData$cooked
		
		pars <- plotinput()	
		
		req(length(pars$outcome)==1)
		req(length(pars$pred)<3)
		
		if (settings$na.ignore=="ignore") {plotSet <- plotSet %>% drop_na(c(pars$outcome, pars$pred))}
		colScale <- "none"
		
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
				p <- p + geom_histogram(bins=input$histBins, fill="grey90")}
			}
		
		# There is one predictor
		else if (length(pars$pred)==1){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictor is a factor --> boxplot
				if (pars$pred %in% cookedData$cats){
					req(input$catVis)
					p <- ggplot(plotSet, aes_string(pars$pred, pars$outcome))
					if (input$catVis == "violin"){p <- p + geom_violin(draw_quantiles=c(0.25,0.5,0.75))}
					else {p <- p + geom_boxplot()}	
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
					colScale <- "fill_discrete"
					}
				# The predictor is numeric --> logistic regression?
				else {
					req(input$catVis)
					p <- ggplot(plotSet, aes_string(pars$outcome, pars$pred))
					if (input$catVis == "violin"){p <- p + geom_violin(draw_quantiles=c(0.25,0.5,0.75)) + coord_flip()}
					else {p <- p + geom_boxplot() + coord_flip()}					
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
					colScale <- "fill_discrete"
					}
				# The predictors is numeric --> scatterplot with color gradient
				else if (all(pars$pred %in% cookedData$nums)){
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], y=pars$outcome, color=pars$pred[2]))
					p <- p + geom_point()
					colScale <- "fill_gradient"
					}

				# Mixed predictors --> scatterplot with character mapping
				else {
					fil <- pars$pred %in% cookedData$nums
					p <- ggplot(plotSet, aes_string(x=pars$pred[fil], y=pars$outcome))
					req(input$catVis)
					if (input$catVis == "facet"){p <- p + geom_point() + facet_wrap(pars$pred[!fil])}
					else {p <- p + geom_point(aes_string(shape=pars$pred[!fil]))+scale_shape_manual(values=c(1,3,5,15,17,0,2,4,6,7,8,9,10,11,12,13,14,16,18,19,20,21,22,23,24,25))}
					}
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictors are a factor --> faceted barplot
				if (all(pars$pred %in% cookedData$cats)){
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], fill=pars$outcome))
					p <- p + geom_bar() + facet_wrap(pars$pred[2])
					colScale <- "fill_discrete"
					}
				
				# The predictors are all numeric --> logistic regression with color
				else if (all(pars$pred %in% cookedData$nums)){
					p <- ggplot(plotSet, aes_string(x=pars$pred[1], y=pars$outcome, color=pars$pred[2]))
					p <- p + geom_point()
					colScale <- "color_gradient"
					}					
				# Mixed predictors --> logistic regression with pch
				else {
					req(input$catVis)
					fil <- pars$pred %in% cookedData$nums			
					p <- ggplot(plotSet, aes_string(x=pars$outcome, y=pars$pred[fil]))
					# p <- p + geom_point()
					if (input$catVis == "violin_col"){p <- p + geom_violin(aes_string(fill=pars$pred[!fil]), draw_quantiles=c(0.25,0.5,0.75)) + coord_flip()
						colScale <- "fill_discrete"}
					else if (input$catVis == "violin_facet"){p <- p + geom_violin(draw_quantiles=c(0.25,0.5,0.75)) + coord_flip() + facet_wrap(pars$pred[!fil])}	
					else if (input$catVis == "boxplot_col"){p <- p + geom_boxplot(aes_string(fill=pars$pred[!fil])) + coord_flip()
						colScale <- "fill_discrete"}
					else if (input$catVis == "boxplot_facet"){p <- p + geom_boxplot() + coord_flip()+facet_wrap(pars$pred[!fil])}
					}			
				}
						
			}
	
		p <- p + theme_bw()
		if (settings$cols == "b&w"){
			if (colScale == "fill_discrete"){
				p <- p + scale_fill_grey(start=0.4, end=0.8)
				}
			else if (colScale == "fill_gradient"){
				p <- p + scale_fill_grey(start=0.4, end=0.8)
				}
			else if (colScale == "color_gradient"){
				p <- p + scale_color_grey(start=0.4, end=0.8)
				}	
			else if (colScale == "color_discrete"){		
				p <- p + scale_color_grey(start=0.4, end=0.8)
				}
			}
		else if (settings$cols == "colored"){
			getPalette <- colorRampPalette(brewer.pal(10,"Spectral"))
			if (colScale == "fill_discrete"){
				p <- p + discrete_scale("fill", "manual", getPalette)
				}
			else if (colScale == "fill_gradient"){
				p <- p + scale_fill_gradient(low="whitesmoke", high="midnightblue")
				}
			else if (colScale == "color_gradient"){
				p <- p + scale_color_gradient(low="whitesmoke", high="midnightblue")
				}	
			else if (colScale == "color_discrete"){		
				p <- p + discrete_scale("color", "manual", getPalette )
				}				
			}
		
		if (settings$serif == "serif"){
			p <- p + theme(text=element_text(family="serif"))
			}		
		else if (settings$serif == "noserif") {
			p <- p + theme(text=element_text(family="sans"))		
			}
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
				if (pars$pred %in% cookedData$cats){
					tagList(
							radioButtons("catVis", "Select visualisation style", choices=c("Boxplot"="boxplot", "Violin plot"="violin"), selected="violin")
						)					
					}
				# The predictor is numeric --> scatterplot
				else {}			
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictor is a factor --> barplot
				if (pars$pred %in% cookedData$cats){}
				# The predictor is numeric --> logistic regression?
				else {
					tagList(
							radioButtons("catVis", "Select visualisation style", choices=c("Boxplot"="boxplot", "Violin plot"="violin"), selected="violin")
						)				
					}			
				}
			}
		
		# There are two predictors
		else if (length(pars$pred)==2){
			
			# the outcome is numeric
			if (pars$outcome %in% cookedData$nums){
				# The predictors are factors --> boxplot with subcategories
				if (all(pars$pred %in% cookedData$cats)){}
				# The predictors are numeric --> scatterplot with color gradient
				else if (all(pars$pred %in% cookedData$nums)){				
					}

				# Mixed predictors --> scatterplot with character mapping
				else {
					tagList(
							radioButtons("catVis", "Select visualisation style", choices=c("Facet plot"="facet", "Character mapping"="charmap"), selected="charmap")
						)					
					}
				}
			
			# The outcome is categorical
			else if (pars$outcome %in% cookedData$cats){
			
				# The predictors are a factor --> faceted barplot
				if (all(pars$pred %in% cookedData$cats)){}
				
				# The predictors are all numeric --> logistic regression with color
				else if (all(pars$pred %in% cookedData$nums)){}					
				# Mixed predictors --> logistic regression with pch
				else {
					tagList(
							selectInput("catVis", "Select visualisation style", choices=c("Facetted boxplot"="boxplot_facet",
																						"Facetted violin"="violin_facet",
																						"Colored boxplot"="boxplot_col", 
																						"Colored violin"="violin_col"), 
								selected="violin_col")
						)									
					}			
				}
			
			
			}		
		})
		
	output$settings <- renderUI({

		tagList(
			fluidRow(
				column(6,
					wellPanel(align="center",
						selectInput("naSetting", label="NA action",
								c("Ignore missing values"="ignore",
								"Keep missing values"="keep"),
								settings$na.ignore
								),
						hr()	
							)
						),
				column(6,
					wellPanel(align="center",
						h3("Plotting options"),
						selectInput(
							"plotCol",
							"Colors",
							choices=c("Black and white"="b&w",
								"Colored"="colored"),
							settings$cols),
						selectInput(
							"serif",
							"Font",
							choices=c("Serif"="serif",
								"Sans serif"="noserif"
								),
							selected=settings$serif	
							)
						)
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
	
	output$testSettingsColComp <- renderUI({
		req(testSettings(), testSet$settings)
		if(testSet$settings=="colComp"){
			testSettings()
			}
		else {invisible()}
		})

	output$testSettingsGrComp <- renderUI({
		req(testSettings(), testSet$settings)
		if(testSet$settings=="grComp"){
			testSettings()
			}
		else {invisible()}
		})

	output$testSettingsMod <- renderUI({
		req(testSettings(), testSet$settings)
		if(testSet$settings=="mod"){
			testSettings()
			}
		else {invisible()}
		})
		
	testSettings <- reactive({
		req(testSet$settings, testSet$vals)
		if (testSet$settings == "colComp"){
			if (ncol(testSet$vals)==2){
				if (testSet$twoCol == "compMeans") {
					if (ncol(testSet$vals)==2 & all(colnames(testSet$vals) %in% cookedData$nums)){
						tagList(
							fluidRow(
								column(3),
								column(6,
									wellPanel(align="center",
										h3("Test setup"),
										hr(),
										h4("Data"),
										checkboxInput("paired", "Individual rows form pairs", FALSE),
										hr(),
										h4("Hypothesis"),
										selectInput("tails", paste("The mean in", colnames(testSet$vals)[1], "is:"), c(
											"Lower"="less",
											"Different"="two.sided",
											"Higher"="greater"),
											selected="two.sided"
											),
										p(style="font-weight:700; font-size: 14px ", paste("in comparison to", colnames(testSet$vals)[2])),
										actionButton("doTestCol", "Save", style="background-color: green; color: white")
										)
									),
								column(3)
								)
							)
						}
						
					}
					
					else if (testSet$twoCol=="correlate"){
						tagList(
							fluidRow(
								column(3),
								column(6,
									wellPanel(align="center",
										h3("Test setup"),
										hr(),							
										selectInput("method", paste("The relationship between", colnames(testSet$vals)[1], "and", colnames(testSet$vals)[2], "is:"), c(
											"Linear"="pearson",
											"Monotonic"="spearman"),
											selected="spearman"
											),
										p("Linear relationships can be visualised as a simple line, connecting the dots on a scatterplot. Monotonic relationships can be represented by a curve or a line."),
										actionButton("doCorTest", "Proceed", style="background-color: green; color: white")
										)
									),
								column(3)
								)
							)				
						}
					
					else {invisible()}			
				}
			
			else if (ncol(testSet$vals)>2 & all(colnames(testSet$vals) %in% cookedData$nums)){
				tagList(
					fluidRow(
						column(3),
						column(6,
							wellPanel(align="center",
								h3("Test setup"),
								hr(),							
								h4("Hypothesis"),
								hr(),
								p("If your data contains several groups which you planned beforehand, set them up here."),
								p("Example case: Your data contains non-native (French, German) and native speakers. 
									You hypothesized that natives differ from non-natives, and French differ from Germans.
								"),
								actionButton("setContsCol", "Planned contrasts", class="btn btn-info"),	
								actionButton("doATestCol", "Proceed", style="background-color: green; color: white")
								)
							),
						column(3)
						)
					)
				}
			
			else {invisible()}
			}
			
		else if (testSet$settings == "grComp"){
			if (length(levels(testSet$vals[,2]))==2 & colnames(testSet$vals)[1] %in% cookedData$nums){
				tagList(
					fluidRow(
						column(3),
						column(6,
							wellPanel(align="center",
								h3("Test setup"),
								hr(),
								h4("Data"),
								checkboxInput("paired", "Individual rows form pairs", FALSE),
								hr(),
								h4("Hypothesis"),
								selectInput("tails", paste("The mean in", colnames(testSet$vals)[1], "is:"), c(
									"Lower"="less",
									"Different"="two.sided",
									"Higher"="greater"),
									selected="two.sided"
									),
								p(style="font-weight:700; font-size: 14px ", paste("in comparison to", colnames(testSet$vals)[2])),
								actionButton("doTestGr", "Save", style="background-color: green; color: white")
								)
							),
						column(3)
						)
					)
				}
				
			else if (length(levels(testSet$vals[,2]))>2 & colnames(testSet$vals)[1] %in% cookedData$nums){
				tagList(
					fluidRow(
						column(3),
						column(6,
							wellPanel(align="center",
								h3("Test setup"),
								hr(),							
								h4("Hypothesis"),
								hr(),
								p("If your data contains several groups which you planned beforehand, set them up here."),
								p("Example case: Your data contains non-native (French, German) and native speakers. 
									You hypothesized that natives differ from non-natives, and French differ from Germans.
								"),
								actionButton("setContsGr", "Plannned contrasts", class="btn btn-info"),	
								actionButton("doATestGr", "Proceed", style="background-color: green; color: white")
								)
							),
						column(3)
						)
					)
				}			
				
			
			}			

		else if (testSet$settings == "mod"){
			invisible()		
			}

			})
			
	doChisq <- function(dat, type="raw") {
		if (type=="raw"){
			# Check that we can do X-squared:
			#1. Check that there is an overlap in the levels
			c1 <- dat[,1]
			c2 <- dat[,2]
			grLabs <- colnames(dat)			
			if (testSet$settings == "grComp"){
				grLabs <- levels(c2)
				c1.x <- c1[c2==levels(c2)[1]]
				c2.x <- c1[c2==levels(c2)[2]]
				c1 <- c1.x
				c2 <- c2.x	
				}
			
			if (!any(levels(c1) %in% levels(c2))){
				return(list(results=NULL, type="applesToOranges"))
				}
			
			else { 
				# Merge the levels in both columns, so that levels not present in col A equal to 0 rather than error, remove NAs
				levs <- sort(unique(c(levels(c1), levels(c2))))
				levs <- levs[!is.na(levs)]
				if (settings$na.ignore == "ignore"){
					c1 <- c1[!is.na(c1)]
					c2 <- c2[!is.na(c2)]
					}
					
				c1 <- factor(c1, levels=levs)
				c2 <- factor(c2, levels=levs)	
				
				t1 <- as.vector(table(c1))
				t2 <- as.vector(table(c2))
				
				comp <- t1 < t2
				
				tab <- data.frame(t1,t2)		
			}
		
			forplot <- factor(c(as.character(c1), as.character(c2)))
			forplot <- data.frame(id=c(rep(grLabs[1], length(c1)),rep(grLabs[2], length(c2))) ,values=forplot)			
			p.obj <- ggplot(forplot, aes(id, fill=values)) + geom_bar(position=position_dodge()) + theme_bw()		
		
		}

		else if (type=="count") {
			req(testSet$namesCol)
			catnames <- factor(cookedData$cooked[[testSet$namesCol]])
			# Check that we can do X-squared:
			#1. Check that there is an overlap in the levels
			c1 <- dat[,1]
			c2 <- dat[,2]
			grLabs <- colnames(dat)			

			# Merge the levels in both columns, so that levels not present in col A equal to 0 rather than error, remove NAs			
			if (settings$na.ignore == "ignore"){
				fil = !is.na(c1) & !is.na(c2)
				c1 <- c1[fil]
				c2 <- c2[fil]
				}
			levs <- as.character(catnames)
			
			dat <- data.frame(c1,c2)	

			forplot <- data.frame(c(c1,c2), c(rep(grLabs[1],length(c1)), rep(grLabs[2],length(c2))), rep(factor(catnames),2))
			colnames(forplot) <- c("Frequency", "Data", "Category")
			p.obj <- ggplot(forplot, aes(Data,y=Frequency, fill=Category))+
				geom_col() + 
				theme_bw()		
			comp <- c1 < c2			
			tab <- data.frame(c1,c2)
		
		}
			
			
			

		#There should not be any zeros or expected values below 5 
		cond1 <- sum(tab==0)
		cond2 <- getExp(tab)
		
		if (cond1 == 0 & !any(cond2 < 5)){
			t.obj <- suppressWarnings(chisq.test(tab))
			rownames(t.obj$observed) <- levs
			colnames(t.obj$observed) <- grLabs
			return(list(results=t.obj, type="chisq2", larger=levs[comp], smaller=levs[!comp], plot=p.obj))
			}
			
		else {		
			t.obj <- suppressWarnings(fisher.test(tab))
			t.obj$observed <- tab
			rownames(t.obj$observed) <- levs
			colnames(t.obj$observed) <- grLabs	
			return(list(results=t.obj, type="fisher2", larger=levs[comp], smaller=levs[!comp], plot=p.obj))
			}
			
		}			
	
	doTtest <- function(dat){
		# Check that we can do t-test
		c1 <- dat[,1]
		c2 <- dat[,2]
		grLabs <- colnames(dat)
		
		if (testSet$settings=="grComp"){
			grLabs <- levels(c2)
			c1.x <- c1[c2==levels(c2)[1]]
			c2.x <- c1[c2==levels(c2)[2]]
			c1 <- c1.x
			c2 <- c2.x						
			}
		
		if(testSet$settings=="colComp"){req(input$doTestCol > 0)}
		
		if(testSet$settings=="grComp"){req(input$doTestGr > 0)}
		
		pars <- isolate(list(tails=input$tails, paired=input$paired))
		
		if (settings$na.ignore == "ignore"){
			if (pars$paired) {
				mask <- !(is.na(c1)|is.na(c2))
				c1 <- c1[mask]
				c2 <- c2[mask]
				}
				
			else {
				c1 <- c1[!is.na(c1)]
				c2 <- c2[!is.na(c2)]
				}				
		}

		forplot <- data.frame(
			group=grLabs,
			means=c(mean(c1), mean(c2)),
			sds=c(sd(c1), sd(c2))
			)
		
		lc1 <- length(c1)
		lc2 <- length(c2)
		forplot$se <- forplot$sds/sqrt(c(lc1, lc2))

		p.obj <- ggplot(forplot, aes(x=group, y=means, fill=group))+
			geom_bar(stat="identity")+
			geom_errorbar(aes(ymin=means-2*se, ymax=means+2*se),
						  width=.2,                    # Width of the error bars
						  position=position_dodge(.9))+
			theme_bw()+
			scale_fill_brewer(palette="Accent")	+
			ylab("mean")
			
		larger <- forplot[forplot$means==max(forplot$means), "means"]
		names(larger) <- forplot[forplot$means==max(forplot$means), "group"]
		smaller <- forplot[forplot$means==min(forplot$means), "means"]
		names(smaller) <- forplot[forplot$means==min(forplot$means), "group"]	
		
		## TO DO: 
		# EFF SIZE: For equal samples, use Cohen's d
		if (lc1==lc2) {
			eff <- (forplot$means[1] - forplot$means[2])/
				(sum(forplot$sds*(c(lc1, lc2)-1))/
					(lc1+lc2-2))
			names(eff) <- "Cohen's d"		
			}
						
		# For unequal samples, use Hedge's g	
		else {eff <- (1-(3/((4*(lc1+lc2))-9)))*((forplot$means[1] - forplot$means[2])/
				(sum(forplot$sds*(c(lc1, lc2)-1))/
					(lc1+lc2-2)))
			names(eff) <- "Hedge's d"		
					}
		
		sumTab <- forplot[, c("means", "sds", "se")]
		colnames(sumTab) <- c("Mean", "SD", "SE")
		rownames(sumTab) <- forplot$group
		
		if (pars$paired==TRUE) {

			cond1 <- shapiro.test(c1-c2)$p.value >= 0.05
			if (cond1) {
				t.obj <- suppressWarnings(t.test(c1, c2, paired=T, alternative=pars$tails))
				return(list(results=t.obj, type="ttest2", plot=p.obj, larger=larger, smaller=smaller, sumTab=sumTab, eff=eff))
				}
			else {
				t.obj <- suppressWarnings(wilcox.test(c1, c2, paired=T, alternative=pars$tails))
				return(list(results=t.obj, type="wilcox2", plot=p.obj, larger=larger, smaller=smaller, sumTab=sumTab, eff=eff))
				}
			
			}

		else {
			cond1 <- shapiro.test(c1)$p.value >= 0.05
			cond2 <- shapiro.test(c2)$p.value >= 0.05
			cond3 <- var.test(c1, c2)$p.value >= 0.05
			
			if (all(cond1, cond2, cond3)) {
				t.obj <- suppressWarnings(t.test(c1, c2, paired=F, alternative=pars$tails))
				return(list(results=t.obj, type="ttest2", plot=p.obj, larger=larger, smaller=smaller, sumTab=sumTab, eff=eff))
				}
				
			else {
				t.obj <- suppressWarnings(wilcox.test(c1, c2, paired=F, alternative=pars$tails))
				return(list(results=t.obj, type="wilcox2", plot=p.obj, larger=larger, smaller=smaller, sumTab=sumTab, eff=eff))
				}						
			}	
		}

	doAOV <- function(dat0, conts){
		# str(dat0)
		
		req(!is.null(input$doATestCol)|!is.null(input$doATestGr))
		if (!is.null(input$doATestCol)) {
			temp <- input$doATestCol
			req(input$doATestCol > 0)}
		else {
			temp <- input$doATestGr
			req(input$doATestGr > 0)
			}
		
		if (testSet$settings == "colComp") {
			ids <- colnames(dat0)
			dat <- data.frame(values__=c(), id__=c())
			for (i in ids) {
				dat <- rbind(dat, data.frame(values__=dat0[[i]], id__=rep(i, length(dat0[[i]]))))
				}
			
			dat$id__ <- factor(dat$id__)
			}
			
		else if (testSet$settings == "grComp"){
			dat <- dat0
			}
		
		colnames(dat) <- c("value__", "id__")
		contrasts(dat$id__) <- conts
		
		
		if (settings$na.ignore=="ignore"){dat <- dat[rowSums(is.na(dat))==0,]}
	
		ordering <- as.vector(by(dat[,1], dat[,2], mean))
		names(ordering) <- levels(dat[,2])
		ordering <- sort(ordering, decreasing = TRUE)
				
		cond1 <- all(as.vector(by(dat[,1], dat[,2], normRes)))
		cond2 <- leveneTest(dat[,1]~dat[,2])$"Pr(>F)"[1]>0.05
		
		if (!is.null(testSet$manConts)) {
			conts <- colnames(as.data.frame(conts))
			}
		
		if (cond1 & cond2){
			t.obj <- aov(dat[,1] ~ dat[,2])
			sm <- summary.lm(t.obj)
			
			p.data <- getSummary(dat)
			p.obj <- ggplot(p.data , aes(x=l, y=m, fill=l))+
				geom_bar(stat="identity")+
				geom_errorbar(aes(ymin=m-2*s, ymax=m+2*s),
							  width=.2,                    # Width of the error bars
							  position=position_dodge(.9))+
				theme_bw()+
				scale_fill_brewer("Group", palette="Accent")	+
				xlab("Group")+
				ylab("Mean")
			print("AOV-DONE")
			return(list(results=t.obj, sm=sm, type="anova", plot=p.obj, conts=conts, ordering=ordering))
			}
		
		#Welch
		# else if (cond1 & !cond2){
			# t.obj <- summary.lm(oneway(dat[,2] ~ dat[,1]))
			# }
		
		#Kruskal Wallis <-- done as ANOVA on ranks
		else {
			dat[,1] <- rank(dat[,1], ties.method="average")
			t.obj <- aov(dat[,1] ~ dat[,2])
			sm <- summary.lm(t.obj)

			p.data <- getSummary(dat)
			p.obj <- ggplot(p.data , aes(x=l, y=m, fill=l))+
				geom_bar(stat="identity")+
				geom_errorbar(aes(ymin=m-2*s, ymax=m+2*s),
							  width=.2,                    # Width of the error bars
							  position=position_dodge(.9))+
				theme_bw()+
				scale_fill_brewer("Group", palette="Accent")	+
				xlab("Group") +
				ylab("Mean")			
			print("KW-DONE")
			return(list(results=t.obj, sm=sm, type="anovaOnRanks", plot=p.obj,  conts=conts, ordering=ordering))
			}		
		}
	
	doBigChisq <- function(dat, conts) {
		print("BIGCHISQ")
		if (testSet$settings=="colComp"){
			dat <- as.data.frame(rbindlist(
			lapply(
				colnames(dat),
				function(i){
					data.frame(
						observations=dat[[i]],
						groups=rep(i, length(dat[,i]))
							)}
				)
			))
			}
		# else if (testSet$settings=="grComp"){
			# }
		
		# print(conts)
		
		c1 <- dat[,1]
		c2 <- dat[,2]
		
		
		
		levs <- sort(levels(c1))
		levs <- levs[!is.na(levs)]
		
		grNames <- sort(levels(c2))
		grNames <- grNames[!is.na(grNames)]
		
		if (settings$na.ignore == "ignore"){
			fil <- !(is.na(c1)|is.na(c2))
			c1 <- c1[fil]
			c2 <- c2[fil]
			}
			
		dat <- data.frame(values=c1, id=c2)	
		
		tab <- table(dat)
		p.obj <- ggplot(dat, aes(id, fill=values)) + geom_bar(position=position_dodge()) + theme_bw()
		
		#There should not be any zeros or expected values below 5 
		cond1 <- sum(tab==0)
		cond2 <- getExp(tab)

		if (!is.null(testSet$manConts)) {
			conts <- colnames(as.data.frame(conts))
			}
		
		if (cond1 == 0 & !any(cond2 < 5)){
			t.obj <- suppressWarnings(chisq.test(tab))
			rownames(t.obj$observed) <- levs
			colnames(t.obj$observed) <- grNames
			print("BIGCHISQ1")
			return(list(results=t.obj, type="chisq", plot=p.obj, conts=conts))
			}
			
		else {		
			t.obj <- suppressWarnings(fisher.test(tab))
			t.obj$observed <- tab
			rownames(t.obj$observed) <- levs
			colnames(t.obj$observed) <- grNames
			print("BIGCHISQ2")
			return(list(results=t.obj, type="fisher", plot=p.obj, conts=conts))
			}						
		}
	
	doCor <- function(dat){
	
		c1 <- dat[,1]
		c2 <- dat[,2]
		grLabs <- colnames(dat)
		
		cond1 <- shapiro.test(c1)$p.value >= 0.05
		cond2 <- shapiro.test(c2)$p.value >= 0.05	
		
		req(input$doCorTest > 0)
		
		if (settings$na.ignore == "ignore"){
			fil <- !(is.na(c1) | is.na(c2))
			c1 <- c1[fil]
			c2 <- c2[fil]
			}
		
		if (cond1 & cond2){
			c.obj <- suppressWarnings(cor.test(c1, c2, method=input$method))
			method <- input$method
			}
			
		else {c.obj <- suppressWarnings(cor.test(c1, c2, method="spearman"))
			method <- "spearman"
			}
		
		forplot <- data.frame(x=c1, y=c2)
		p.obj <- ggplot(forplot, aes(x, y))+
			geom_point()+
			theme_bw()+
			xlab(grLabs[1])+
			ylab(grLabs[2])			
		
		return(list(results=c.obj, type=method, plot=p.obj))
		}
	
	testRes <- reactive({
		req(testSet$settings, testSet$vals)
		
		if (testSet$settings == "colComp") {
			if (ncol(testSet$vals)==2){
				if (all(colnames(testSet$vals) %in% cookedData$cats)) {
						return(doChisq(testSet$vals, type="raw"))
					}
				else if (all(colnames(testSet$vals) %in% cookedData$nums)) {
				
					if (testSet$twoCol == "compMeans"){
						return(doTtest(testSet$vals))
						}
					else if (testSet$twoCol == "compCount"){
						return(doChisq(testSet$vals, type="count"))
						}
					else if (testSet$twoCol == "correlate"){
						return(doCor(testSet$vals))
						}					
					}
				}
				
			else if (ncol(testSet$vals)>2){
				if (all(colnames(testSet$vals) %in% cookedData$cats)) {
					# ids <- colnames(testSet$vals)
					# dat <- data.frame(id__=c(), values__=c())
					# for (i in ids) {
						# dat <- rbind(dat, data.frame(id__=rep(i, length(testSet$vals[, i])), values__=testSet$vals[, i]))
						# }
					
					# dat$id__ <- factor(dat$id__)
					# testSet$vals <- dat							
					return(doBigChisq(testSet$vals, aovConts()))
					}

				else if (all(colnames(testSet$vals) %in% cookedData$nums)) {
					return(doAOV(testSet$vals, aovConts()))					
					}				
				}		
			}
		
		else if (testSet$settings == "grComp") {
			if (length(levels(testSet$vals[,2]))==2){
				if (all(colnames(testSet$vals) %in% cookedData$cats)) {
						return(doChisq(testSet$vals))
					}
				else if (colnames(testSet$vals)[1] %in% cookedData$nums) {
						return(doTtest(testSet$vals))
					}
				}
				
			else {
				if (all(colnames(testSet$vals) %in% cookedData$cats)) {
					return(doBigChisq(testSet$vals, aovConts()))
					}
				else if (colnames(testSet$vals)[1] %in% cookedData$nums){
					return(doAOV(testSet$vals, aovConts()))
					}
				}				
			}			

		else if (testSet$settings == "mod") {

			outp <- input$outp_order
			preds <- input$pred_order
			
			
			dat <- cookedData$cooked[,c(outp, preds)]
			if (settings$na.ignore == "ignore"){
				mask <- rowSums(is.na(dat))==0
				dat <- dat[mask,]
				}
			
			# LINEAR REGRESSION
			if (outp %in% cookedData$nums){
				cord <- dat[colnames(dat)[colnames(dat) %in% cookedData$nums]]
				corpreds <- preds[preds %in% cookedData$nums]
				coroutp <- outp[outp %in% cookedData$nums]

				lin <- cor(cord, method="pearson")[corpreds,coroutp]-cor(cord, method="spearman")[corpreds,coroutp]
				names(lin) <- corpreds
				# print(lin)
				lin <- names(lin)[lin < -0.1]
								
				mod <- lm(reformulate(termlabels = preds, response = outp), dat)
				if (length(preds)>1){mColl <- vif(mod)
					if (class(mColl)=="matrix"){						
						mColl <- as.data.frame(mColl)
						# print(mColl)
						mColl <- mColl[3]**2
						mColl <- rownames(mColl)[mColl[,1]>10]
						}
					else {
						mColl <- names(mColl)[mColl>10]
						}
					}
				else {mColl <- NULL}
				
				modplot <- data.frame(rlv=mod$model[[outp]], predicted=mod$fitted)
				p.obj <- ggplot(modplot, aes(rlv, predicted))+
					geom_point()+
					theme_bw()+
					#geom_line(aes(x=rlv, y=rlv))+
					xlab("Real value in the data") +
					ylab("Value predicted by the model")

				return(list(results=mod, sm=summary(lm.beta(mod)), type="linreg", lin=lin, mColl=mColl, plot=p.obj))
				# create a summary for factors?

				}
				
			# LOGISTIC REGRESSION/DECISION TREES?	
			else if (outp %in% cookedData$cats) {
				
				mod <- glm(reformulate(termlabels = preds, response = outp), dat, family="binomial")
				return(list(results=mod, type="logreg"))
				# create a summary for factors?
				
				}	
				
			}

		})
		
	# CREATE THE SUMMARY
	output$testOutputColComp <- renderUI({
		req(testSet$settings)
		req(testRes())
		if (testSet$settings == "colComp"){
					
			req(testRes()$type, testSet$vals)
			a <- anovaGroups$finished		
			
			inp <- testRes()
			print(inp)
			
			if (inp$type %in% c("chisq2", "fisher2")) {
					n <- gsub("_", " ", colnames(testSet$vals))
					tagList(
						fluidRow(
							column(2),			
							column(4,
								#Summarize the test
								wellPanel(align="center",
									h4("Test"),
									p(inp$results$method),
									br(),
									p(paste("p-value:",  max(round(inp$results$p.value, 4), 0.0001))),
									if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
										} else {p(style="color: green; font-weight:400", "not significant")}
									)
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Comparison"),
									uiOutput("freqComparisonColComp")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										column(3, tableOutput("testTable")),
										column(1),
										column(8, plotOutput("testPlotColComp"))								
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The distribution of the variables ", paste(sort(c(inp$larger, inp$smaller)), collapse=", "), " was evaluated with ", 
											inp$results$method,". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level ", 
											ifelse(inp$type=="chisq2", 
												sprintf("(p< %g, chi-squared= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												""),
											"suggests that the distribution of these variables ",ifelse(inp$results$p.value<0.05, "differs", "does not substantially differ"), " between ",
											n[1], " and ", n[2], ".",
											sep=""),
										fixed=T)							
										)							
									)
								),
							column(2)			
							)
						)
					}			
				
			else if (inp$type %in% c("chisq", "fisher")) {
					n <- gsub("_", " ", colnames(testSet$vals))
					tagList(
						fluidRow(
							column(2),			
							column(4,
								#Summarize the test
								wellPanel(align="center",
									h4("Test"),
									p(inp$results$method),
									br(),
									p(paste("p-value:",  max(round(inp$results$p.value, 4), 0.0001))),
									if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
										} else {p(style="color: green; font-weight:400", "not significant")}
									)
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Comparison"),
									uiOutput("freqComparisonColComp")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										column(3, tableOutput("testTable")),
										column(1),
										column(8, plotOutput("testPlotColComp"))								
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The distribution of the variables ", paste(n, collapse=", "), " was evaluated with ", 
											inp$results$method,". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level ", 
											ifelse(inp$type=="chisq", 
												sprintf("(p< %g, chi-squared= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												""),
											"suggests that the distribution of these variables ",ifelse(inp$results$p.value<0.05, "differs", "does not substantially differ"), " between ",
											n[1], " and ", n[2], ".",
											sep=""),
										fixed=T)							
										)							
									)
								),
							column(2)			
							)
						)
					}			
																
			else if (inp$type == "applesToOranges") {
				tagList(
					fluidRow(
						column(3),
						column(6,
							wellPanel(align="center",
								h4("ERROR"),
								p("You are probably comparing columns which contain different data. It is not possible to compare the distribution in the columns as they do not contain the same variables."),
								p("Change the analysis type, or use the button in the top left corner to select different data."),
								img(src="aToO.jpg", width="100%")						
								)
							),
						column(3)
						)
					)
				}

				
			else if (inp$type %in% c("pearson", "spearman")) {
					n <- gsub("_", " ", colnames(testSet$vals))
					print(inp)
					tagList(
						fluidRow(
							column(2),						
							column(4,
								fluidRow(
									#Summarize the test
									wellPanel(align="center",
										h4("Test"),
										p(inp$results$method),
										br(),
										p(sprintf("correlation coefficient: %g", max(round(inp$results$estimate, 4), 0.0001))),
										if (inp$results$p.value>0) {p(style="font-weight:400", "positive")
											} else {p(style="font-weight:400", "negative")}	,
										br(),
										p(sprintf("p-value: %g", max(round(inp$results$p.value, 4), 0.0001))),
										if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
											} else {p(style="color: red; font-weight:400", "not significant")}
										)
									)		
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Effect size"),
									plotOutput("effectCol", height="200px")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										plotOutput("testPlotColComp")
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The relationship between the variables '", n[1], "' and '", n[2], "' was evaluated with ", 
											inp$results$method,". The correlation coefficient between these two variables was ", ifelse(inp$type=="pearson", "r=", "rho="), round(inp$results$estimate,3),
											". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level of 0.05 ", 
											ifelse(inp$type=="pearson", 
												sprintf("(p= %g, t= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												sprintf("(p= %g, S= %g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3))
												),
											"suggests that the correlation coefficient in the population ",ifelse(inp$results$p.value<0.05, "is", "may not be"), " different from zero. In other words, it is",
											ifelse(inp$results$p.value<0.05, " likely", " unlikely"), " that knowing the value of '", n[1], "' allows to predict '", n[2], "' and vice versa. ",
											"The size of the correlation coefficient suggests a ", ifelse(inp$results$estimate<0, "negative", "positive"), " correlation, i.e. as one of the variables increases, the other ",
											ifelse(inp$results$estimate<0, "decreases.", "increases as well."), " Additionally, the relationship is ", getCorLab(inp$results$estimate), 
											"; thus, by knowing the value of one of the variables, it is ", ifelse(abs(inp$results$estimate>0.1),"", "not "), "possible to predict the value of the other ", getCorLab(inp$results$estimate, rel=F), ".",
											sep=""),
										fixed=T)
									
										)							
									)
								),
							column(2)			
							)
						)}			
			
				
			else if (inp$type %in% c("ttest2", "wilcox2")) {
					n <- gsub("_", " ", colnames(testSet$vals))
					tagList(
						fluidRow(
							column(2),						
							column(4,
								fluidRow(
									#Summarize the test
									wellPanel(align="center",
										h4("Test"),
										p(inp$results$method),
										br(),
										p(sprintf("p-value: %g", max(round(inp$results$p.value, 4), 0.0001))),
										if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
											} else {p(style="color: green; font-weight:400", "not significant")}
										)
									),
								fluidRow(
									wellPanel(align="center",							
										h4("Mean comparison"),
										uiOutput("freqComparisonColComp")							
										)
									)		
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Effect size"),
									plotOutput("effectCol", height="200px")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										column(3, tableOutput("testTable")),
										column(1),
										column(8, plotOutput("testPlotColComp"))								
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The distribution of the values associated with the groups '", n[1], "' and '", n[2], "' was evaluated with ", 
											inp$results$method,". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level of 0.05 ", 
											ifelse(inp$type=="ttest2", 
												sprintf("(p= %g, t= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												sprintf("(p= %g, W= %g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3))
												),
											"suggests that these groups ",ifelse(inp$results$p.value<0.05, "do not belong", "belong"), " to the same population, that means that there is ",
											ifelse(inp$results$p.value<0.05, "a", "no"), " reason to distinguish between '", n[1], "' and '", n[2], "' with respect to their value. ",
											"The effect size as measured by ", names(inp$eff), " was ", abs(round(inp$eff,2)), " corresponding to a ", getEffLab(inp$eff), " effect; in other words the difference between '", n[1], "' and '", n[2], "' is", getEffLab(inp$eff), ".",
											sep=""),
										fixed=T)
									
										)							
									)
								),
							column(2)			
							)
						)}			
			
			else if (inp$type == "anova"){

				if (testSet$settings == "colComp"){n <- gsub("_", " ", colnames(testSet$vals))}
				else if (testSet$settings == "grComp"){n <- levels(testSet$Vals[,2])}
				pval <- 1-pf(inp$sm$fstatistic[1], inp$sm$fstatistic[2], inp$sm$fstatistic[3])
				print(inp$sm)
				tagList(
					fluidRow(
						column(2),			
						column(4,
							#Summarize the test
							wellPanel(align="center",
								h4("Test"),
								p("One-way analysis of variance (ANOVA)"),
								br(),
								p("Overall result"),
								p(sprintf("p-value: %g",  max(round(pval, 4), 0.0001))),
								if (pval<0.05) {p(style="color: green; font-weight:400", "significant")
									} else {p(style="color: green; font-weight:400", "not significant")}
								)
							),
						column(4,
							#Summarize the data
							wellPanel(align="center",
								h4("Comparison of means"),
								uiOutput("freqComparisonColComp")
								)
							),						
						column(2)
						),
					
				##############WIP CONTINUE HERE		
					fluidRow(
						column(2),
						column(8,
							wellPanel(align="center",
								fluidRow(
									column(3, tableOutput("testTable")),
									column(1),
									column(8, plotOutput("testPlotColComp"))								
									)
								)
							),
						column(2)
						),
						
					fluidRow(
						column(2),
						column(8,
							wellPanel(
								h4("Summary"),
								br(),
								p(gsub(" .", ".",
									paste("The distribution of the scores within the groups ", paste(names(inp$ordering), collapse=", "), " was evaluated with a One-way ANOVA", 
										". The fact that the p-value yielded by the test was ", ifelse(pval<0.05, "below", "above"), 
										" the significance level ", 
										sprintf("(p<%g, F= %g, df= %g and %g) ",  max(round(pval, 4), 0.0001), round(inp$sm$fstatistic[1],3), inp$sm$fstatistic[2], inp$sm$fstatistic[3]),
										"suggests that ",ifelse(pval<0.05, "at least one", "none"), " of these groups",ifelse(pval<0.05, "differs", "does not substantially differ"), " from the rest.",
										ifelse(input$setContsCol==0, "", 
											ifelse(sum(inp$sm$coefficients[-1,4]<0.05)>0, paste(" Additionally, the following manually preset contrasts were identified as significant (at p<0.05):"), "None of the manually preset contrasts were identified as significant (at p<0.05)")), sep=""),
									fixed=T)										
									), 
								{
								if (input$setContsCol>0) {
									if (sum(inp$sm$coefficients[-1,4]<0.05)>0){
										o2 <- "<p><ul>"								
										for (i in 2:nrow(inp$sm$coefficients)){
											if (inp$sm$coefficients[i,][4]<0.05){
												o2 <- paste(o2,"<li>", 
													inp$conts[i-1],
													"</li>"
													)
												}
											}
										o2 <- paste(o2, "</ul></p>")
										HTML(o2)																			
										}
									}
								
								}								
								)
							),
						column(2)			
						)
					)
				}
				
			
			else if (inp$type == "anovaOnRanks"){
				print("AOVonRanks")
				if (testSet$settings == "colComp"){n <- gsub("_", " ", colnames(testSet$vals))}
				else if (testSet$settings == "grComp"){n <- levels(testSet$Vals[,2])}
				pval <- 1-pf(inp$sm$fstatistic[1], inp$sm$fstatistic[2], inp$sm$fstatistic[3])
				tagList(
					fluidRow(
						column(2),			
						column(4,
							#Summarize the test
							wellPanel(align="center",
								h4("Test"),
								p("Kruskal-Wallis test"),
								br(),
								p("Overall result"),
								p(sprintf("p-value: %g",  max(round(pval, 4), 0.0001))),
								if (pval<0.05) {p(style="color: green; font-weight:400", "significant")
									} else {p(style="color: green; font-weight:400", "not significant")}
								)
							),
						column(4,
							#Summarize the data
							wellPanel(align="center",
								h4("Comparison of means"),
								uiOutput("freqComparisonColComp")
								)
							),						
						column(2)
						),
					
				##############WIP CONTINUE HERE		
					fluidRow(
						column(2),
						column(8,
							wellPanel(align="center",
								fluidRow(
									column(3, tableOutput("testTable")),
									column(1),
									column(8, plotOutput("testPlotColComp"))								
									)
								)
							),
						column(2)
						),
						
					fluidRow(
						column(2),
						column(8,
							wellPanel(
								h4("Summary"),
								br(),
								p(gsub(" .", ".",
									paste("The distribution of the scores within the groups ", paste(names(inp$ordering), collapse=", "), " was evaluated with the Kruskal-Wallis test as the data did not allow for a parametric test", 
										". The fact that the p-value yielded by the test was ", ifelse(pval<0.05, "below", "above"), 
										" the significance level ", 
										sprintf("(p<%g, F= %g, df= %g and %g) ",  max(round(pval, 4), 0.0001), round(inp$sm$fstatistic[1],3), inp$sm$fstatistic[2], inp$sm$fstatistic[3]),
										"suggests that ",ifelse(pval<0.05, "at least one", "none"), " of these groups",ifelse(pval<0.05, "differs", "does not substantially differ"), " from the rest.",
										ifelse(input$setContsCol==0, "", 
											ifelse(sum(inp$sm$coefficients[-1,4]<0.05)>0, paste(" Additionally, the following manually preset contrasts were identified as significant (at p<0.05):"), "None of the manually preset contrasts were identified as significant (at p<0.05)")), sep=""),
									fixed=T)										
									), 
								{
								if (input$setContsCol>0) {
									if (sum(inp$sm$coefficients[-1,4]<0.05)>0){
										o2 <- "<p><ul>"								
										for (i in 2:nrow(inp$sm$coefficients)){
											if (inp$sm$coefficients[i,][4]<0.05){
												o2 <- paste(o2,"<li>", 
													inp$conts[i-1],
													"</li>"
													)
												}
											}
										o2 <- paste(o2, "</ul></p>")
										HTML(o2)																			
										}
									}
								
								}									
								)
							),
						column(2)			
						)
					)
				}						
			}
		else {invisible()}			
		})
		
	output$testOutputGrComp <- renderUI({
		req(testSet$settings)
		if (testSet$settings=="grComp"){
				
			req(testRes()$type, testSet$vals)
			# a <- anovaGroups$finished		
			
			inp <- testRes()
			# print
			print(inp)
			
			if (inp$type %in% c("chisq2", "fisher2")) {

					n <- gsub("_", " ", colnames(testSet$vals))
					tagList(
						fluidRow(
							column(2),			
							column(4,
								#Summarize the test
								wellPanel(align="center",
									h4("Test"),
									p(inp$results$method),
									br(),
									p(paste("p-value:",  max(round(inp$results$p.value, 4), 0.0001))),
									if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
										} else {p(style="color: green; font-weight:400", "not significant")}
									)
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Comparison"),
									uiOutput("freqComparisonGrComp")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										column(3, tableOutput("testTable")),
										column(1),
										column(8, plotOutput("testPlotGrComp"))								
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The distribution of the variables ", paste(sort(c(inp$larger, inp$smaller)), collapse=", "), " was evaluated with ", 
											inp$results$method,". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level ", 
											ifelse(inp$type=="chisq2", 
												sprintf("(p< %g, chi-squared= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												""),
											"suggests that the distribution of these variables ",ifelse(inp$results$p.value<0.05, "differs", "does not substantially differ"), " between ",
											n[1], " and ", n[2], ".",
											sep=""),
										fixed=T)							
										)							
									)
								),
							column(2)			
							)
						)
				}
				
			else if (inp$type %in% c("chisq", "fisher")) {
					n <- levels(testSet$vals[,2])
					print("Drawinng....")
					tagList(
						fluidRow(
							column(2),			
							column(4,
								#Summarize the test
								wellPanel(align="center",
									h4("Test"),
									p(inp$results$method),
									br(),
									p(paste("p-value:",  max(round(inp$results$p.value, 4), 0.0001))),
									if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
										} else {p(style="color: green; font-weight:400", "not significant")}
									)
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Comparison"),
									uiOutput("freqComparisonGrComp")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										column(3, tableOutput("testTable")),
										column(1),
										column(8, plotOutput("testPlotGrComp"))								
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The distribution of the variables ", paste(n, collapse=", "), " was evaluated with ", 
											inp$results$method,". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level ", 
											ifelse(inp$type=="chisq", 
												sprintf("(p< %g, chi-squared= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												""),
											"suggests that the distribution of these variables ",ifelse(inp$results$p.value<0.05, "differs", "does not substantially differ"), " between ",
											n[1], " and ", n[2], ".",
											sep=""),
										fixed=T)							
										)							
									)
								),
							column(2)			
							),
						if (!is.null(testSet$manConts)){
							fluidRow(
								column(2),
								column(8,
									wellPanel(
										h4("Manual contrasts"),
										br(),
										p("The following manually set contrasts were significant:",											
											),
										HTML(paste(
											"<ul>",
											paste(collapse="</li><li>")
											,"</ul>"
											))
										)
									),
								column(2)			
								)							
							}																					
						)
					}			
									
			else if (inp$type == "applesToOranges") {
				tagList(
					fluidRow(
						column(3),
						column(6,
							wellPanel(align="center",
								h4("ERROR"),
								p("You are probably comparing columns which contain different data. It is not possible to compare the distribution in the columns as they do not contain the same variables."),
								p("Change the analysis type, or use the button in the top left corner to select different data."),
								img(src="aToO.jpg", width="100%")						
								)
							),
						column(3)
						)
					)
				}
				
			else if (inp$type %in% c("ttest2", "wilcox2")) {

					n <- gsub("_", " ", levels(testSet$vals[,2]))
					tagList(
						fluidRow(
							column(2),						
							column(4,
								fluidRow(
									#Summarize the test
									wellPanel(align="center",
										h4("Test"),
										p(inp$results$method),
										br(),
										p(sprintf("p-value: %g", max(round(inp$results$p.value, 4), 0.0001))),
										if (inp$results$p.value<0.05) {p(style="color: green; font-weight:400", "significant")
											} else {p(style="color: green; font-weight:400", "not significant")}
										)
									),
								fluidRow(
									wellPanel(align="center",							
										h4("Mean comparison"),
										uiOutput("freqComparisonGrComp")							
										)
									)		
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Effect size"),
									plotOutput("effectGr", height="200px")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									fluidRow(
										column(3, tableOutput("testTable")),
										column(1),
										column(8, plotOutput("testPlotGrComp"))								
										)
									)
								),
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									br(),
									p(gsub(" .", ".",
										paste("The distribution of the values associated with the groups ", n[1], " and ", n[2], " was evaluated with ", 
											inp$results$method,". The fact that the p-value yielded by the test was ", ifelse(inp$results$p.value<0.05, "below", "above"), 
											" the significance level of 0.05 ", 
											ifelse(inp$type=="ttest2", 
												sprintf("(p= %g, t= %g, df=%g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3), inp$results$parameter),
												sprintf("(p= %g, W= %g) ",  max(round(inp$results$p.value, 4), 0.0001), round(inp$results$statistic, 3))
												),
											"suggests that these groups ",ifelse(inp$results$p.value<0.05, "do not belong", "belong"), " to the same population, that means that there is ",
											ifelse(inp$results$p.value<0.05, "a", "no"), " reason to distinguish between ", n[1], " and ", n[2], "with respect to their value. ",
											"The effect size as measured by ", names(inp$eff), " was ", abs(round(inp$eff,2)), " corresponding to a ", getEffLab(inp$eff), " effect; in other words the difference between ", n[1], " and ", n[2], "is", getEffLab(inp$eff), ".",
											sep=""),
										fixed=T)
									
										)							
									)
								),
							column(2)			
							)
						)			

					}
				
			
			else if (inp$type == "anova"){

				n <- levels(testSet$Vals[,2])
				pval <- 1-pf(inp$sm$fstatistic[1], inp$sm$fstatistic[2], inp$sm$fstatistic[3])
				tagList(
					fluidRow(
						column(2),			
						column(4,
							#Summarize the test
							wellPanel(align="center",
								h4("Test"),
								p("One-way analysis of variance (ANOVA)"),
								br(),
								p("Overall result"),
								p(sprintf("p-value: %g",  max(round(pval, 4), 0.0001))),
								if (pval<0.05) {p(style="color: green; font-weight:400", "significant")
									} else {p(style="color: green; font-weight:400", "not significant")}
								)
							),
						column(4,
							#Summarize the data
							wellPanel(align="center",
								h4("Comparison of means"),
								uiOutput("freqComparisonGrComp")
								)
							),						
						column(2)
						),
					
				##############WIP CONTINUE HERE		
					fluidRow(
						column(2),
						column(8,
							wellPanel(align="center",
								fluidRow(
									column(3, tableOutput("testTable")),
									column(1),
									column(8, plotOutput("testPlotGrComp"))								
									)
								)
							),
						column(2)
						),
						
					fluidRow(
						column(2),
						column(8,
							wellPanel(
								h4("Summary"),
								br(),
								p(gsub(" .", ".",
									paste("The distribution of the scores within the groups ", paste(names(inp$ordering), collapse=", "), " was evaluated with a One-way ANOVA", 
										". The fact that the p-value yielded by the test was ", ifelse(pval<0.05, "below", "above"), 
										" the significance level ", 
										sprintf("(p<%g, F= %g, df= %g and %g) ",  max(round(pval, 4), 0.0001), round(inp$sm$fstatistic[1],3), inp$sm$fstatistic[2], inp$sm$fstatistic[3]),
										"suggests that ",ifelse(pval<0.05, "at least one", "none"), " of these groups ",ifelse(pval<0.05, "differs", "substantially differs"), " from the rest.",
										ifelse(input$setContsGr==0, "", 
											ifelse(sum(inp$sm$coefficients[-1,4]<0.05)>0, paste(" Additionally, the following manually preset contrasts were identified as significant (at p<0.05):"), "None of the manually preset contrasts were identified as significant (at p<0.05)")), sep=""),
									fixed=T)										
									), 
								{
								if (input$setContsCol>0) {
									if (sum(inp$sm$coefficients[-1,4]<0.05)>0){
										o2 <- "<p><ul>"								
										for (i in 2:nrow(inp$sm$coefficients)){
											if (inp$sm$coefficients[i,][4]<0.05){
												o2 <- paste(o2,"<li>", 
													inp$conts[i-1],
													"</li>"
													)
												}
											}
										o2 <- paste(o2, "</ul></p>")
										HTML(o2)																			
										}
									}
								
								}		
								)
							),
						column(2)			
						)
					)
				}
				
			
			else if (inp$type == "anovaOnRanks"){

				n <- levels(testSet$Vals[,2])
				pval <- 1-pf(inp$sm$fstatistic[1], inp$sm$fstatistic[2], inp$sm$fstatistic[3])
				tagList(
					fluidRow(
						column(2),			
						column(4,
							#Summarize the test
							wellPanel(align="center",
								h4("Test"),
								p("Kruskal-Wallis test"),
								br(),
								p("Overall result"),
								p(sprintf("p-value: %g",  max(round(pval, 4), 0.0001))),
								if (pval<0.05) {p(style="color: green; font-weight:400", "significant")
									} else {p(style="color: green; font-weight:400", "not significant")}
								)
							),
						column(4,
							#Summarize the data
							wellPanel(align="center",
								h4("Comparison of means"),
								uiOutput("freqComparisonGrComp")
								)
							),						
						column(2)
						),
					
				#############WIP CONTINUE HERE		
					fluidRow(
						column(2),
						column(8,
							wellPanel(align="center",
								fluidRow(
									column(3, tableOutput("testTable")),
									column(1),
									column(8, plotOutput("testPlotGrComp"))								
									)
								)
							),
						column(2)
						),
						
					fluidRow(
						column(2),
						column(8,
							wellPanel(
								h4("Summary"),
								br(),
								p(gsub(" .", ".",
									paste("The distribution of the scores within the groups ", paste(names(inp$ordering), collapse=", "), " was evaluated with the Kruskal-Wallis test as the data did not allow for a parametric test", 
										". The fact that the p-value yielded by the test was ", ifelse(pval<0.05, "below", "above"), 
										" the significance level ", 
										sprintf("(p<%g, F= %g, df= %g and %g) ",  max(round(pval, 4), 0.0001), round(inp$sm$fstatistic[1],3), inp$sm$fstatistic[2], inp$sm$fstatistic[3]),
										"suggests that ",ifelse(pval<0.05, "at least one", "none"), " of these groups",ifelse(pval<0.05, "differs", "does not substantially differ"), " from the rest.",
										ifelse(input$setContsGr==0, "", 
											ifelse(sum(inp$sm$coefficients[-1,4]<0.05)>0, paste(" Additionally, the following manually preset contrasts were identified as significant (at p<0.05):"), "None of the manually preset contrasts were identified as significant (at p<0.05)")), sep=""),
									fixed=T)										
									), 
								{
								if (input$setContsCol>0) {
									if (sum(inp$sm$coefficients[-1,4]<0.05)>0){
										o2 <- "<p><ul>"								
										for (i in 2:nrow(inp$sm$coefficients)){
											if (inp$sm$coefficients[i,][4]<0.05){
												o2 <- paste(o2,"<li>", 
													inp$conts[i-1],
													"</li>"
													)
												}
											}
										o2 <- paste(o2, "</ul></p>")
										HTML(o2)																			
										}
									}
								
								}							
								)
							),
						column(2)			
						)
					)
				}			

			}
		else {invisible()}
		})

	output$testOutputMod <- renderUI({
		req(testSet$settings)
		
		if (testSet$settings == "mod"){
			req(testRes()$type)
			
			inp <- testRes()
			# print(inp)
			if (inp$type == "linreg"){
					n <- gsub("_", " ", colnames(testSet$vals))
					pval <- 1-pf(inp$sm$fstatistic[1], inp$sm$fstatistic[2], inp$sm$fstatistic[3])
					tagList(
						if(length(inp$lin)>0){
							fluidRow(
								column(3),
								column(6,
									wellPanel(class="border-danger", align="center",
										h4("Warning: Non-linearity"),
										p(paste("The relationship between ", colnames(inp$results$model)[1], " and ", paste(inp$lin, collapse=", "), " is probably not a straight line, but rather a curve. More advanced models would probably be better capable of modelling your data.", sep=""))
										)
									),
								column(3)
								)
							},
						if (length(inp$mColl)>0){
							fluidRow(
								column(3),
								column(6,
									wellPanel(class="border-danger", align="center",
										h4("Warning: Collinearity"),
										p(paste("The variables ", paste(inp$mColl, collapse=", "), " are strongly related to each other. As a consequence, the evaluation of their significance and their predictions of the value of ", colnames(inp$results$model)[1], " are unreliable. The significance of the model overall, however, is unaffected.", sep=""))
										)
									),
								column(3)
								)
							},
						fluidRow(
							column(2),			
							column(4,
								#Summarize the test
								wellPanel(align="center",
									h4("Test"),
									p(inp$results$method),
									br(),
									p(sprintf("p-value: %g",  max(round(pval, 4), 0.0001))),
									if (pval<0.05) {p(style="color: green; font-weight:400", "significant")
										} else {p(style="color: green; font-weight:400", "not significant")},
									br(),
									p(sprintf("R2: %g",  max(round(inp$sm$r.squared, 4), 0.0001)))
									)
								),
							column(4,
								#Summarize the data
								wellPanel(align="center",
									h4("Quick overview"),
									uiOutput("coefOverview")
									)
								),						
							column(2)
							),
							
						fluidRow(
							column(2),
							column(8,
								wellPanel(align="center",
									plotOutput("testPlotMod")								
									)
								),
							column(2)
							)
							
						,fluidRow(
							column(2),
							column(8,
								wellPanel(
									h4("Summary"),
									actionLink("linregSumHelp", "How to read this?"),
									br(),
									uiOutput("fullSummaryMod")
									)
								),
							column(2)			
							)
						)				
				}
			else {tagList(
				fluidRow(
					p("Not implemented yet")
					)
				)}
			}
			
		else {invisible()}
		})
	
	observeEvent(input$linregSumHelp, {
		showModal(
			modalDialog(
				size="l",
				title="Reading the summary",
				p("This summary first informs you about the model overall. A model that is significant can explain the distribution of your predicted variable better than its mean. The extent to which this model is better is expressed by the R-squared value, which can be between 0-1, the higher the better. In social sciences, R-squared values above 0.5 are usually considered good, as humans are harder to predict than physical processes."),
				p("The intercept tells you about the value of the predicted variable a theoretical case would have, if all of its numeric characteristics equaled to zero and categories to default. As this interface takes the alphabetically first category as default, you may need to rename them in order to get the right one recognized as default, e.g. 'college, elementary, high' to '1_elementary, 2_high, 3_college'"),
				p("If the intercept is significant it means that it is significantly different from the mean, i.e. it makes sense to have it in the model."),
				p("The coefficients express the influence each of your predictors has on the outcome. The value in the column 'coefficient' is the number which you should multiply by the value of the predictor (for numbers) or 1 (for categories) and add to the intercept to obtain the prediction for the given case. E.g. if intercept is 0, coefficient of IQ for test scores is 0.15 and for educationNone -1.35, the predicted value for an uneducated participant with IQ 100 would be: 0 (intercept) + 100*0.15 (IQ) + -1.35 (education) --> 13.65"),
				p("The standardized coefficients translate the values from specific units (IQ points, centimeters) to relative units (standard deviations) which allows the comparison of the individual predictors: the further from 0 the standardized coefficient, the stronger the influence of that predictor on the outcome."),
				p("The columns Std. error, and t-value are additional statistics from which the significance of that predictor is calculated and you should report them if you report the p-value."),
				p("The significance of individual predictors tells whether they are significantly different from 0. If they were not, it would not be clear whether adjusting for this predictor is better than just taking the mean value.")				
				)
		
			)
	
		})
		
	output$freqComparisonColComp <- renderUI({
		req(testSet$settings)		
		if (testSet$settings=="colComp"){			
			frqComp()}
		else {invisible()}
		})
		
	output$freqComparisonGrComp <- renderUI({
		req(testSet$settings)
		if (testSet$settings=="grComp"){frqComp()}
		else {invisible()}
		})
		
	output$coefOverview <- renderUI({
		req(testSet$settings)
		if (testSet$settings=="mod"){frqComp()}
		else {invisible()}	
		})	
		
	output$fullSummaryMod <- renderUI({
		req(testSet$settings)
		if (testSet$settings=="mod"){
			req(testRes(), testSet$vals)
			inp <- testRes()
			n <- colnames(testSet$vals)
			if (inp$type == "linreg"){
				pval <- 1-pf(inp$sm$fstatistic[1], inp$sm$fstatistic[2], inp$sm$fstatistic[3])
				smtb <- as.data.frame(inp$sm$coefficients)
				smtb$pred <- rownames(smtb)
				intercept <- smtb[1,]
				smtb <- smtb[-1,]
				sigs <- smtb[smtb[,5]<0.05,]
				sigs <- sigs[order(abs(sigs[,2]), decreasing=T),]
				nsigs <- smtb[smtb[,5]>=0.05,]
				nsigs <- nsigs[order(abs(nsigs[,2]), decreasing=T),]
				predcats <- colnames(inp$results$model)[-1]
				prednums <- predcats[predcats %in% cookedData$nums]
				predcats <- predcats[predcats %in% cookedData$cats]
				defs <- unlist(lapply(predcats, function(i){levels(testSet$vals[,i])[1]}))
				defs <- paste(predcats, defs)
				defs <- gsub(" ^", "", defs)
				defs <- gsub(" ", " is ", defs)
				defs <- paste(defs, collapse=", ")
				
				o <- paste("<p>",paste(toupper(substring(colnames(inp$results$model)[1],1,1)), substring(colnames(inp$results$model)[1],2), collapse="", sep="") , " was modelled with ", paste(colnames(inp$results$model)[-1], collapse=", "), " as ", ifelse((ncol(inp$results$model)-1)>1, "predictors.", "predictor."), " The model is", 
					ifelse(pval<0.05, "", " not" ), " significant overall ",sprintf("(p<%g, F=%g, df=%g and %g)", max(round(pval,4),0.0001), max(round(inp$sm$fstatistic[1],4),0.0001), max(round(inp$sm$fstatistic[2],4),0.0001), max(round(inp$sm$fstatistic[3]),4),0.0001),
					", suggesting that it is", ifelse(pval<0.05, "", " not" ), " a better predictor of ",  colnames(inp$results$model)[1], " than its mean value. The r-squared is ",
					max(round(inp$sm$r.squared,4),0.0001), " meaning that the predictors can explain ", 100*max(round(inp$sm$r.squared,4),0.0001),"% of the variation around the mean. </p>",
					"<p> The intercept (the value of ", colnames(inp$results$model)[1], " if", 
					ifelse(length(prednums>0), paste(" ", paste(prednums, collapse=", "), ifelse(length(prednums)>1, " are ", " is " ), "equal to zero", sep=""), ""),
					ifelse((length(prednums)>0 & length(predcats)>0), " and ", " "),
					ifelse(length(predcats>0), defs, ""),
					") was ", round(intercept[1],4),"</p>", sep="", collapse="")
				if (length(inp$lin>0)){o <- paste(o, "<p>Since some of the predictors are non-linearly related to the outcome variable, the model fit measured by R-squared does not entirely represent the amount of variation around the mean that the they could explain.<p><br />")}
				if (length(inp$mColl>0)){o <- paste(o, "<p>Since the predictors: ", paste(inp$mColl, collapse=", "), "are affected by collinearity, their coefficients and significance values are unreliable. </p><br />")}
				
				o <- paste(o, "<p><b>Following significant predictors were identified (ordered by relative importance expressed by standardized coefficients)<br /></b></p>")
				o <- paste(o, '<table style="width:100%">')
				o <- paste(o, '<tr><th>Predictor</th><th>Coefficient</th><th>Standardized coefficient</th><th>Std.error</th><th>t-value</th><th>p-value</th></tr>')
				if (nrow(sigs) > 0){
					for (li in 1:nrow(sigs)){
						l <- sigs[li,]
						o <- paste(o, sprintf('<tr><td>%s</td><td>%g</td><td>%g</td><td>%g</td><td>%g</td><td>%g</td></tr>', l[6], l[1], l[2], l[3], l[4], l[5]))}									
					}
				
				o <- paste(o, "<tr><th colspan=6><br />Following non-significant predictors were identified (ordered by relative importance expressed by standardized coefficients)<br /><br /></td></tr>")
				o <- paste(o, '<tr><th>Predictor</th><th>Coefficient</th><th>Standardized coefficient</th><th>Std.error</th><th>t-value</th><th>p-value</th></tr>')				
				if (nrow(nsigs) > 0){
					for (li in 1:nrow(nsigs)){
						l <- nsigs[li,]
						o <- paste(o, sprintf('<tr><td>%s</td><td>%g</td><td>%g</td><td>%g</td><td>%g</td><td>%g</td></tr>', l[6], l[1], l[2], l[3], l[4], l[5]))}		
					}
				o <- paste(o, "</table>")	
				HTML(o)
				}		
			}
		else {invisible()}	
		})	
		
	frqComp <- reactive({
		req(testRes(), testSet$vals)
		inp <- testRes()
		n <- colnames(testSet$vals)
		
		# a <- colnames(testSet$vals)
				
		if (inp$type %in% c("fisher2", "chisq2")) {			
			if (testSet$settings == "colComp"){
				o <- paste("<p>Compared to", n[1], "the following changes occur in", n[2], "</p><br />")
				o <- paste(o, '<ul style="list-style-type: none;">')

				for (li in inp$larger){
					o <- paste(o, '<li><i class="fas fa-plus"></i>', li, "</li>")}
					
				for (li in inp$smaller){	
					o <- paste(o, '<li><i class="fas fa-minus"></i>', li, "</li>")}
					o <- paste(o, "</ul>")
		
				HTML(o)
				}
				
			else if (testSet$settings == "grComp"){
				n <- levels(testSet$Vals[,2])
				o <- paste("<p>Compared to", n[1], "the following changes occur in", n[2], "</p><br />")
				o <- paste(o, '<ul style="list-style-type: none;">')

				for (li in inp$larger){
					o <- paste(o, '<li><i class="fas fa-plus"></i>', li, "</li>")}
					
				for (li in inp$smaller){	
					o <- paste(o, '<li><i class="fas fa-minus"></i>', li, "</li>")}
					o <- paste(o, "</ul>")
		
				HTML(o)			
			}
		}

		# WIP
		else if (inp$type %in% c("fisher", "chisq")) {
			if (testSet$settings == "colComp"){
				o <- paste("<p>Compared to", n[1], "the following changes occur in", n[2], "</p><br />")
				# o <- paste(o, '<ul style="list-style-type: none;">')

				# for (li in inp$larger){
					# o <- paste(o, '<li><i class="fas fa-plus"></i>', li, "</li>")}
					
				# for (li in inp$smaller){	
					# o <- paste(o, '<li><i class="fas fa-minus"></i>', li, "</li>")}
					# o <- paste(o, "</ul>")
		
				HTML(o)
				}
				
			else if (testSet$settings == "grComp"){
				n <- levels(testSet$Vals[,2])
				o <- paste("<p>Compared to", n[1], "the following changes occur in", n[2], "</p><br />")
				# o <- paste(o, '<ul style="list-style-type: none;">')

				# for (li in inp$larger){
					# o <- paste(o, '<li><i class="fas fa-plus"></i>', li, "</li>")}
					
				# for (li in inp$smaller){	
					# o <- paste(o, '<li><i class="fas fa-minus"></i>', li, "</li>")}
					# o <- paste(o, "</ul>")
		
				HTML(o)			
			}
		}
		
		# else if (inp$type %in% c("ttest2", "wilcox2")) {

			# o <- '<ul style="list-style-type: none;">'

			# o <- paste(o, '<li><i class="fas fa-plus"></i>', sprintf("%s: %g", names(inp$larger), round(inp$larger, 4)), "</li>")			
			# o <- paste(o, '<li><i class="fas fa-minus"></i>', sprintf("%s: %g", names(inp$smaller), round(inp$smaller,4)), "</li>")
			# o <- paste(o, "</ul>")
			# o <- paste(o,  '<div id="effect" class="shiny-plot-output" style="width: 100% ; height: 200px"></div>')

			# }
			
		else if (inp$type %in% c("ttest2", "wilcox2")) {
			if (testSet$settings == "colComp"){
				o <- '<p>'

				o <- paste(o, '<i class="fas fa-plus"></i>', sprintf("%s: %g", names(inp$larger), round(inp$larger, 4)), "<br />")			
				o <- paste(o, '<i class="fas fa-minus"></i>', sprintf("%s: %g", names(inp$smaller), round(inp$smaller,4)), "<br />")
				HTML(o)					
				}
			else if (testSet$settings == "grComp"){
				o <- '<p>'

				o <- paste(o, '<i class="fas fa-plus"></i>', sprintf("%s: %g", names(inp$larger), round(inp$larger, 4)), "<br />")			
				o <- paste(o, '<i class="fas fa-minus"></i>', sprintf("%s: %g", names(inp$smaller), round(inp$smaller,4)), "<br />")
				HTML(o)						
				}
			
			}
			
		else if (inp$type == "anova"){
				o <- '<p><ol>'
				for (li in 1:length(inp$ordering)) {o <- paste(o, sprintf("<li>%s: %g</li>", names(inp$ordering[li]), round(inp$ordering[[li]], 4)), "<br />")	}
				o <- paste(o, "</ol></p>")

				HTML(o)							
			}

		else if (inp$type == "anovaOnRanks"){
				o <- '<p><ol>'
				for (li in 1:length(inp$ordering)) {o <- paste(o, sprintf("<li>%s: %g</li>", names(inp$ordering[li]), round(inp$ordering[[li]], 4)), "<br />")	}
				o <- paste(o, "</ol></p>")

				HTML(o)							
			}
			
		else if (inp$type == "linreg"){
			smtb <- as.data.frame(inp$sm$coefficients)
			smtb$pred <- rownames(smtb)
			intercept <- smtb[1,]
			smtb <- smtb[-1,]
			larger <- smtb[smtb$Estimate>0,]
			larger <- larger[order(abs(larger[,2]), decreasing=T),]
			smaller <- smtb[smtb$Estimate<=0,]
			smaller <- smaller[order(abs(smaller[,2]), decreasing=T),]
			# print(larger)
			# print(smaller)
			o <- paste("<p>Following influences have been identified. (Ordered by relative importance, bold predictors are significant)</p><br />")
			o <- paste(o, '<ul style="list-style-type: none;">')
			
			if (nrow(larger) > 0){
				for (li in 1:nrow(larger)){
					l <- larger[li,]
					o <- paste(o, '<li><i class="fas fa-plus"></i>', ifelse(l[5]<0.05, sprintf("<b>%s: %g</b>", l[6], round(l[1], 4)), sprintf("%s: %g", l[6], round(l[1],4))), "</li>")}
								
				}
			
			if (nrow(smaller) > 0){
				for (li in 1:nrow(smaller)){
					l <- smaller[li,]
					o <- paste(o, '<li><i class="fas fa-minus"></i>', ifelse(l[5]<0.05, sprintf("<b>%s: %g</b>", l[6], round(l[1], 4)), sprintf("%s: %g", l[6], round(l[1],4))), "</li>")}
					o <- paste(o, "</ul>")
				}		
			HTML(o)
			
			
			}
		})
				
	output$testTable <- renderTable(rownames=T, {
		req(testRes())
		inp <- testRes()
		
		if (inp$type %in% c("fisher2", "chisq2")) {
			inp$results$observed
			}
			
		else if (inp$type %in% c("ttest2", "wilcox2")) {
			inp$sumTab
			}
		})
		
	output$testPlotColComp <- renderPlot({
		req(testRes(), testSet$settings)

		if (testSet$settings== "colComp"){
			inp <- testRes()
			
			if (inp$type %in% c("fisher2", "chisq2", "fisher", "chisq", "ttest2", "wilcox2", "anova", "anovaOnRanks", "spearman", "pearson")) {
				inp$plot + theme(text=element_text(family=ifelse(settings$serif=="serif", "serif", "sans")))
				}		
			}
		else {invisible()}		
		})
			
	output$testPlotGrComp <- renderPlot({
		req(testRes(), testSet$settings)

		if (testSet$settings== "grComp"){
			inp <- testRes()
			
			if (inp$type %in% c("fisher2", "chisq2", "fisher", "chisq", "ttest2", "wilcox2", "anova", "anovaOnRanks")) {
				inp$plot + theme(text=element_text(family=ifelse(settings$serif=="serif", "serif", "sans")))
				}		
			
			}
		else {invisible()}		
		})

	output$testPlotMod <- renderPlot({
		req(testRes(), testSet$settings)
		if (testSet$settings== "mod"){
			inp <- testRes()
			print("MODELLING")
			# print(inp$type)
			if (inp$type %in% c("linreg")) {
				inp$plot + theme(text=element_text(family=ifelse(settings$serif=="serif", "serif", "sans")))
				}
			else {invisible()}	
				
			}
		else {invisible()}		
		})
				
	output$effectCol <- renderPlot({
		req(testRes())
		inp <- testRes()
		print("EFFECT")
		# print(inp$results$estimate)	
		# print(inp$method)
		
		if (testSet$settings == "colComp"){
			if (inp$type %in% c("ttest2", "wilcox2")) {
				effPlot(inp$eff, names(inp$eff))
				}
			
			else if (inp$type %in% c("spearman", "pearson")) {
				# print(inp$results$estimate)
				# print(inp$method)
				corPlot(inp$results$estimate, type=inp$type)
				}
			}

		})
	
	output$effectGr <- renderPlot({
		req(testRes())
		inp <- testRes()
		print("EFFPLOT")
		# print(inp)
		if(testSet$settings == "grComp"){
			if (inp$type %in% c("ttest2", "wilcox2")) {
				effPlot(inp$eff, names(inp$eff))
				}
			}					
		})	
	
})
