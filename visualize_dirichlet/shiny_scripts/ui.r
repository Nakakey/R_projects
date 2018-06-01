shinyUI(fluidPage(
	titlePanel(name_file),
	fluidRow(column(10,offset=0.5,
      column(3,
             selectInput("type_p",label="select result plot type",choices=c("normal","MAP","Min","full")),
             wellPanel(
        h4("data selection"),
        #select time-step(dynamically change)
        numericInput("timestep",label="time step",value=1,max=tseq),
        #select type
        selectInput("type",label="prior or posterior",choices=c("prior","posterior")),
        h5("selected timestep's result"),
        tableOutput("info"),
        numericInput("num_model",label="index of model",value=1),
        submitButton("Submit"),
        HTML("<br>"),
        HTML("<br>")
      )),
      column(9,
        plotOutput("output02",height="275px"),
        rglwidgetOutput("output01")
      )
    ))

))
