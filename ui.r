library(shiny)
library(shinyLP)
library(shinyjqui)
library(shinyBS)

shinyUI(
fluidPage(
	tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"),
		tags$title("FREDDIE Shiny"),
		tags$script("
				window.onload = function() {
					$('#mainnavbar a:contains(\"Check\")').parent().addClass('disabled');
					$('#mainnavbar a:contains(\"Summarize\")').parent().addClass('disabled');
					$('#mainnavbar a:contains(\"Explore\")').parent().addClass('disabled');
					$('#mainnavbar a:contains(\"Analyze\")').parent().addClass('disabled');
				};

				Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
					$('#mainnavbar a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
				});"  
		),
		tags$link(rel="stylesheet", type="text/css",href="style.css"),
		tags$script(type="text/javascript", src = "busy.js"),
		tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.2.0/css/solid.css", integrity="sha384-wnAC7ln+XN0UKdcPvJvtqIH3jOjs9pnKnq9qX68ImXvOGz2JuFoEiCjT8jyZQX2z", crossorigin="anonymous"),
		tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.2.0/css/regular.css", integrity="sha384-zkhEzh7td0PG30vxQk1D9liRKeizzot4eqkJ8gB3/I+mZ1rjgQk+BSt2F6rT2c+I", crossorigin="anonymous"),
		tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.2.0/css/fontawesome.css", integrity="sha384-HbmWTHay9psM8qyzEKPc8odH4DsOuzdejtnr+OFtDmOcIVnhgReQ4GZBH7uwcjf6", crossorigin="anonymous")
		
		
		# WIP: Animate the display of Col/Rownumber (NOT A PRIORITY)
		#tags$script(type="text/javascript", src = "animate_value.js")
		),
	tags$body(background="background0.jpg"),
	tags$body(style="font-family: 'Halant', serif; padding-left: 0px; padding-right: 0px; padding-top: 70px;"),
	
	div(class = "busy",  
		  p("Calculation in progress.."), 
		  img(src="busy_icon1.gif")
		),
	
	navbarPage("FREDDIE", id="mainnavbar", position="fixed-top",		
		tabPanel("Upload", class="landing-page",

			fluidRow(wellPanel(align="center", p("This is the alpha version of the new Freddie interface. It may be unstable and/or lack functions which you know from the old interface.")
				)),

			fluidRow(
				column(2),
				column(8, align="center",
					h1(id="main_title","FREDDIE Shiny", style='color: #000000;'),
					h4(id="main_subtitle","Statistics interface", style='color: #000000; margin-bottom: 50px')
				),
			column(2)
			),
			fluidRow(
				column(4),
				column(2, align="left",
					actionButton("startDemo", "Start demo version", style='text-align:center', class="btn-info btn-lg")
				  ),				
				column(2, align="right",
					actionButton("upFile", "Upload your data", style='', class="btn-primary btn-lg")
				  ),
				column(4)
				),
				
			fluidRow(
				HTML("<br />")
				),
			

			# wellPanel(class="lp-main-well",
			wellPanel(
				fluidRow(
					column(3),
					column(2,
						wellPanel(class="lp-well", align="center",
							img(src='plot.jpg', width="100%"),
							h4("Create plots"),
							p(class="lp-desc", "Only a few clicks will bring you to high quality graphics")
						
							)
						),
					column(2,
						wellPanel(class="lp-well", align="center",
							img(src='analyze.jpg', width="100%"),
							h4("Analyze data"),
							p(class="lp-desc", "You can both test your hypotheses against the data and create models")
							)
						),
					column(2,
						wellPanel(class="lp-well", align="center",
							img(src='code.jpg', width="100%"),
							h4("Code-free statistics"),
							p(class="lp-desc", "You do not need to learn how to code, everything is just a click away")
							)
						),
					column(3)
					)
				)
			),
			
		tabPanel("Check", class="padded-page",
		
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
						actionButton("bigUnfriendlyButton", "Wrong", width="80%", style="background-color: red; color: white; font-size: 28px")					
					),
				column(4)
				)			
			),
	
		tabPanel("Summarize", class="padded-page",
			uiOutput("naCount"),
			htmlOutput("summaryOutput")
			),
	
		tabPanel("Explore", class="padded-page",
			fluidRow(
				column(2),
				column(8,
					wellPanel(
						h3("Select the variables to plot", style="text-align:center;"),
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
			
			fluidRow(column(2),
				column(8, align="center",
					plotOutput("outplot"),
					br()
					),
				column(2)),
				
			fluidRow(column(2),
				column(8, align="center",
					div(class="text-center;", uiOutput("plotChoices"))
					),
				column(2))				
			
			),
			
		navbarMenu("Analyze",
			tabPanel("Start", class="padded-page",
				fluidRow(
					column(4),
					column(4,
						wellPanel(align="center", 
							h3("Test"),
							p("Check your hypothesis against your data."),
							actionLink("compSel", "Compare")
							)
						),
					column(4)
					),
					
				fluidRow(	
					column(4),
					column(4,				
						wellPanel(align="center", 
							h3("Model"),
							p("Use your data to predict the values of unseen cases or check the strength of relationship between variables."),
							actionLink("modSet", "Predict")
							)
						),
					column(4)
					)
				),
			"_______________",
			"Test",
			tabPanel("Columns ", class="padded-page",
				fluidRow(	
					column(5),
					column(2,
						actionButton("colCChange", "Select different columns", class="btn btn-info")
						),
					column(5)
					),
				uiOutput("testOutputColComp"),
				uiOutput("testSettingsColComp")
				),

			tabPanel("Groups ", class="padded-page",
				fluidRow(	
					column(5),
					column(2,			
						actionButton("grCChange", "Select groups", class="btn btn-info")
						),
					column(5)
					),				
				uiOutput("testOutputGrComp"),
				uiOutput("testSettingsGrComp")
				),
			"_______________",
			"Modelling",

			tabPanel("Model data", class="padded-page",
				fluidRow(	
					column(5),
					column(2,			
						actionButton("modChange", "Select", class="btn btn-info")
						),
					column(5)
					),						
				uiOutput("testOutputMod"),
				uiOutput("testSettingsMod")
				)
			),
			
		tabPanel("Settings", class="padded-page",
			h3("Settings", style="text-align: center"),
			uiOutput("settings"),
			fluidRow(
				column(12, align="center",
				actionButton("saveSettings", "Save", class="btn-success")
					)
				)
			),
		
		navbarMenu("More", 
			tabPanel("Help", class="padded-page",
				jumbotron(header="Welcome to FREDDIE Shiny", content="An easy way to statistics")
				),
			tabPanel("About", class="padded-page",
				jumbotron(header="Welcome to FREDDIE Shiny", content="An easy way to statistics")
				)
			)
		),
		
	
		p("\n"),
		wellPanel(style="height: 70px; margin-bottom: 0px",
			fluidRow(column(12, align="center", HTML('This interface is a part of the <a href="https://www.anglistik.uni-freiburg.de/seminar/abteilungen/sprachwissenschaft/ls_kortmann/FREDDIE">FREDDIE</a> project at University Freiburg.'))),
			fluidRow(column(12, align="center", HTML('<a href="mailto:jiri.zamecnik@anglistik.uni-freiburg.de">Contact us</a>')))
			)
		)
)
		