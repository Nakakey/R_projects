shinyUI(fluidPage(
	titlePanel("Dirichlet distribution"),
	fluidRow(column(10,offset=0.1,
      column(2,
             wellPanel(
        h4("parameter setting"),
        #select time-step(dynamically change)
        numericInput("alpha1",label="alpha_1",value=1),
        numericInput("alpha2",label="alpha_2",value=1),
        numericInput("alpha3",label="alpha_3",value=1),
        submitButton("Submit"),
        HTML("<br>"),
        HTML("<br>")
      )),
      column(10,
        plotlyOutput("output01", height = 900)
      )
    ))

))
