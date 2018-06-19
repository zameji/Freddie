library(shiny)
shinyUI(fluidPage(
  #theme=shinythemes("spacelab"),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"),
  tags$link(rel="stylesheet", type="text/css",href="style.css"),
  tags$script(type="text/javascript", src = "busy.js")
  ),
  div(class="header", style="color: #004a99, font-weight: bold; align-content: center;", 
	titlePanel("",windowTitle="FREDDIE Shiny")
  ),
div(class = "busy",  
      p("Calculation in progress.."), 
      img(src="busy_icon1.gif")
	),
  sidebarLayout(
  
    sidebarPanel(width = 3,
      h3("Data:"),
      fileInput(inputId='file1', label='Choose CSV File with your data',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
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
	  ),			   
      conditionalPanel(condition = "Output.sum",
                       uiOutput("varselector")
					   ),
	  h3("General plot settings:"),
	  fluidRow(column(6, radioButtons(inputId='color', label='Color', c("Greyscale"="grey", "Colorblind-friendly"="cb", "Color"="color"), "grey")
		),
	  column(6,radioButtons(inputId='serif', label='Font type',
		   c("Serif"="serif",
			 "Sans serif"="sans"),
		   "sans")
		)
		   ),
	  sliderInput("fontSize", "Font size", min=6, max=48, value=12)
					   
    ),
    mainPanel(
			fluidRow(uiOutput("help")),
			fluidRow(
			tabsetPanel(
				tabPanel("Data summary", 
					conditionalPanel(condition="Output.PlotType", verbatimTextOutput("sum")),
					uiOutput("overrider")
				),
				tabPanel("Variable summary", 
					fluidRow(
						column(6,
							plotOutput("SumPlotX")
							),
						column(6,	
							plotOutput("SumPlotY")
							)
					),
					fluidRow(
						column(6,align="center",
							conditionalPanel(condition="Output.PlotType",
								uiOutput("Xsettings")
								)
							),
						column(6,align="center",
							conditionalPanel(condition="Output.PlotType",
								uiOutput("Ysettings")
								)
							)
						)
				), 
				tabPanel("Plot", 
					plotOutput("Plot", 
						brush = brushOpts(
							id = "plot1_brush"
						),
						click = "plot1_click"
						),
					hr(),
					uiOutput("OutlierFilter"),
					h3("Figure settings:"),
					fluidRow(
						column(6,
							conditionalPanel(condition = "output.PlotType",
								textInput(inputId="titleInput", label='Title', value = " ")
								),
							conditionalPanel(condition = "output.PlotType",
								downloadButton("downloadData", 'Download figure')
								)
							),
						column(6,
							uiOutput("PlotSettings")
							)
						)
					),
				tabPanel("Statistical testing", 
					p(htmlOutput("TestDescription")),
					uiOutput("TestSettings"),
					h3("Test results:"),
					p(textOutput("Pvalue")),
					p(textOutput("Signalert")),
					verbatimTextOutput("Testresults"))
				)
			)
		)	  
	),
	hr(),
	fluidRow(column(12, align="center", HTML('This interface is a part of the <a href="https://www.anglistik.uni-freiburg.de/seminar/abteilungen/sprachwissenschaft/ls_kortmann/FREDDIE">FREDDIE</a> project at University Freiburg.'))),
	fluidRow(column(12, align="center", HTML('<a href="mailto:jiri.zamecnik@anglistik.uni-freiburg.de">Contact us</a>')))
)
)
