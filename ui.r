library(shiny)
shinyUI(fluidPage(
  titlePanel("FREDDIE Shiny"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h2("Data:"),
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
                       h2("variable selection:"),
                       uiOutput("varselector")
					   )
    ),
    mainPanel(
			fluidRow(
			tabsetPanel(
				tabPanel("Summary", 
					verbatimTextOutput("sum"),
					h2("Variable distribution summary:"),
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
					h2(textOutput("PlotType")),
					plotOutput("Plot"),
					hr(),
					h2("figure settings:"),
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
							conditionalPanel(condition = "output.PlotType=='bar plot:'",
								checkboxInput(inputId='leg', label='Legend', TRUE),
								checkboxInput(inputId='besideCheck', label='Do not stack', FALSE),
								radioButtons(inputId='propCheck',
								label='',
								choices = c("absolute", "relative"), selected = "absolute")
								),
							conditionalPanel(condition = "output.PlotType=='box plot:'",
								checkboxInput(inputId='notchCheck', label='Notches', FALSE)
								),
							conditionalPanel(condition = "output.PlotType=='scatter plot:'",
								checkboxInput(inputId='regrCheck', label='Regression line', FALSE)
								)

							)
						)
					),
				tabPanel("Statistic testing", 
					p(textOutput("TestDescription")),
					h2("test settings:"),
					fluidRow(column(1),
						column(11, uiOutput("TestSettings"))),
                    h2("test results:"),
					p(textOutput("Pvalue")),
                    p(textOutput("Signalert")),
					verbatimTextOutput("Testresults"))
				)
			)
		)	  
	)
)
)
