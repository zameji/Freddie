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
          radioButtons(inputId='quote', label='Quote',
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
		# conditionalPanel(condition = "output.PlotType",
			# h2("figure settings:"),
			# textInput(inputId="titleInput", label='Title', value = "",)
		  # ),
		# conditionalPanel(condition = "output.PlotType=='bar plot:'",
		   # checkboxInput(inputId='leg', label='Legend', TRUE),
		   # checkboxInput(inputId='besideCheck', label='Do not stack', FALSE),
		   # radioButtons(inputId='propCheck',
				# label='',
				# choices = c("absolute", "relative"), selected = "absolute")
		  # ),
		# conditionalPanel(condition = "output.PlotType=='box plot:'",
		   # checkboxInput(inputId='notchCheck', label='Notches', FALSE)
		  # ),
		# conditionalPanel(condition = "output.PlotType=='scatter plot:'",
		   # checkboxInput(inputId='regrCheck', label='Regression line', FALSE)
		  # )
		# conditionalPanel(condition = "output.PlotType",
		   # downloadButton("downloadData", 'Download figure')
		   # )
    ),
    mainPanel(
	
	  # conditionalPanel(condition = "output.sum",
      # h2("overview of dataset:")
      # ),
      # verbatimTextOutput("sum"),
      # conditionalPanel(condition = "output.Plot",
                       # h2(textOutput("PlotType"))
      # ),
      # plotOutput("Plot"),
      # conditionalPanel(condition = "output.Testresults",
                       # h2("statistics:"),
                       # p(textOutput("Pvalue")),
                       # p(textOutput("Signalert"))
      # ),
      # verbatimTextOutput("Testresults")
	  
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
				tabPanel("Results", 
					h2("statistics:"),
                    p(textOutput("Pvalue")),
                    p(textOutput("Signalert")),
					verbatimTextOutput("Testresults"))
				)
			)
		# hr(),

			)	  
	)
))	
