library(shiny)
library(shinyLP)
library(shinyjqui)


shinyUI(
fluidPage(
	tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"),
		tags$title("FREDDIE Shiny"),
		tags$script("
				window.onload = function() {
					$('#mainnavbar a:contains(\"Data check\")').parent().addClass('disabled');
					$('#mainnavbar a:contains(\"Data summary\")').parent().addClass('disabled');
					$('#mainnavbar a:contains(\"Visualisation\")').parent().addClass('disabled');
					$('#mainnavbar a:contains(\"Statistics\")').parent().addClass('disabled');
				};

				Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
					$('#mainnavbar a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
				});"  
		),
		tags$link(rel="stylesheet", type="text/css",href="style.css"),
		tags$script(type="text/javascript", src = "busy.js")
		
		# WIP: Animate the display of Col/Rownumber (NOT A PRIORITY)
		#tags$script(type="text/javascript", src = "animate_value.js")
		),
	
	tags$body(background="background.jpg", style="font-family: 'Halant', serif; padding-left: 0px; padding-right: 0px; padding-top: 70px;"),
	
	div(class = "busy",  
		  p("Calculation in progress.."), 
		  img(src="busy_icon1.gif")
		),
		
	navbarPage("FREDDIE", id="mainnavbar", position="fixed-top",
		
		tabPanel("Data upload",
			fluidRow(
				column(2),
				column(8,
				wellPanel(style="background-color: #ffffff", 
					h1("FREDDIE Shiny", style='text-align:center'),
					h4("Statistics without fear", style='text-align:center')
					)
				),
			column(2)
			),
			fluidRow(
				column(2),
				
				column(3,
				  wellPanel(
					h3("Upload your data", style='text-align:center'),
					fluidRow(column(2),
						column(8,
							fileInput(inputId='file1', label='',
									accept=c('text/csv', 
											 'text/comma-separated-values,text/plain', 
											 '.csv'))
							),
						column(2)
						)
					)
				  ),
				  
				column(2),
				
				column(3,
				  wellPanel(
					h3("Use sample data", style='text-align:center'),
					fluidRow(column(4),
						column(4,
							actionButton("action", label = "Start")
							),
						column(4)
						),
					hr(),
					p(a("Download sample data", href="https://onedrive.live.com/download?cid=8BF09AD1C8343122&resid=8BF09AD1C8343122%21258985&authkey=ACWhDNRScGBzKwk"),  style='text-align:center')
					)
				  ),
				  
				column(2)
				)
			),
			
		tabPanel("Data check", 
		
			# fluidRow(		
				# column(2),
				# column(8,		
					# wellPanel(style="background-color: #ffffff", 
						# h2("Summary of your data", style='text-align:center')
						# )
					# ),
				# column(2)
				# ),
			fluidRow(
				column(3),
			
				column(2,
					wellPanel(
						h3("Columns", style='text-align:center'),
						h3(textOutput("columnNumber"), style='text-align:center; color: #2222ff;')
						)
					),
					
				column(2),	
					
				column(2,
					wellPanel(
						h3("Rows", style='text-align:center'),
						h3(textOutput("rowNumber"), style='text-align:center; color: #2222ff;')					
						)
					),
				column(3)
				),
			fluidRow(
				column(2),
				column(8,
					wellPanel(
						h3("Structure of your data", style="text-align:center;"),
						p("Drag the labels to correct rows", style="text-align:center;"),
						div(style="text-align:center;", uiOutput("vartype"))
						)
					),
				column(2)	
				),
			
			fluidRow(
				column(4),
				column(2, align="center",
					actionButton("bigFriendlyButton", "Correct", icon("stats", lib = "glyphicon"), width="85%", style="background-color: green; color: white; font-size: 30px")
					),
				column(2, align="center",	
						actionButton("bigUnfriendlyButton", "Wrong", icon("stats", lib = "glyphicon"), width="80%", style="background-color: red; color: white; font-size: 28px")					
					),
				column(4)
				),
			
			tags$hr(),
			div(style="text-align:center;",
				h3("Any errors?"),
				p("Tell us more about the file you uploaded"),
				checkboxInput(inputId='header', label='Header', TRUE),
				fluidRow(
					column(6, 
						radioButtons(inputId='sep', label='Separator',
							   c(Comma=',',
								 Semicolon=';',
								 Tab='\t'),
							   '\t')
						),
					column(6,
						radioButtons(inputId='quote', label='Quote sign',
							   c(None='',
								 'Double Quote'='"',
								 'Single Quote'="'"),
							   '"')
						)
					)
				)
			),
	
		tabPanel("Data summary",
			htmlOutput("summaryOutput")
			),
	
		tabPanel("Visualisation",
			fluidRow(
				column(2),
				column(8,
					wellPanel(
						h3("Select the variables to plot", style="text-align:center;"),
						# p("Drag the labels to correct rows", style="text-align:center;"),
						div(style="text-align:center;", uiOutput("plotselect"))
						)
					),
				column(2)	
				),
			fluidRow(
				div(style="text-align:center; padding-bottom:19px",
					actionButton("doPlot", "Plot", icon("charts", lib = "glyphicon"), width="10%", style="background-color: green; color: white; font-size: 30px")
					)
				),
			
			fluidRow(column(1),
				column(10, align="center",
					plotOutput("outplot")
					),
				column(1)),
				
			fluidRow(column(1),
				column(10, align="center",
					div(class="text-center;", uiOutput("plotChoices"))
					),
				column(1))				
			
			),
			
		tabPanel("Statistics",
			jumbotron(header="Welcome to FREDDIE Shiny", content="An easy way to statistics")
			),
			
		tabPanel("Settings",
			jumbotron(header="Welcome to FREDDIE Shiny", content="An easy way to statistics")
			),
		
		navbarMenu("More",
			tabPanel("Help",
				jumbotron(header="Welcome to FREDDIE Shiny", content="An easy way to statistics")
				),
			tabPanel("About",
				jumbotron(header="Welcome to FREDDIE Shiny", content="An easy way to statistics")
				)
			)
		),
		
	hr(),
	fluidRow(column(12, align="center", HTML('This interface is a part of the <a href="https://www.anglistik.uni-freiburg.de/seminar/abteilungen/sprachwissenschaft/ls_kortmann/FREDDIE">FREDDIE</a> project at University Freiburg.'))),
	fluidRow(column(12, align="center", HTML('<a href="mailto:jiri.zamecnik@anglistik.uni-freiburg.de">Contact us</a>')))
	)
)
		