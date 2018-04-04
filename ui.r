library(shiny)
shinyUI(fluidPage(theme="bootstrap.css",
  titlePanel("FREDDIE Shiny"),
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
      conditionalPanel(condition = "output.sum",
                       h3("Variable selection:"),
                       uiOutput("varselector")
					   )
    ),
    mainPanel(
			fluidRow(
			tabsetPanel(
				tabPanel("Data summary", 
					verbatimTextOutput("sum"),
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
					h3(textOutput("PlotType")),
					plotOutput("Plot", 
						click = "plot1_click",
						brush = brushOpts(
							id = "plot1_brush"
						)),
					hr(),
					uiOutput("OutlierFilter"),
					h3("Figure settings:"),
					fluidRow(
						column(6,
							conditionalPanel(condition = "output.PlotType",
								textInput(inputId="titleInput", label='Title', value = "")
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
				tabPanel("Statistic testing", 
					p(htmlOutput("TestDescription")),
					uiOutput("TestSettings"),
                    h3("Test results:"),
					p(textOutput("Pvalue")),
                    p(textOutput("Signalert")),
					verbatimTextOutput("Testresults"))
				)
			)
		)	  
	)
)
)
