load_packages('grid','gridExtra','htmltools','plotly','rms','shinyBS','shinydashboard',
              'shinyjs','shinyWidgets','shinythemes','showtext',"shiny","stringr")
startup()
source("components.R")
source("model.R")
uu = function(x) unlist(unname(x))

ui = fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$script(src="script.js"),
                     tags$link(rel="stylesheet", type="text/css", href="animate.css")),
  theme = shinytheme("flatly"),

  #page 1
  radio(inputId = "input__oes_or_gas", label=dutch$predictors$location_simple, selected="none",
        choiceNames=uu(dutch$location), choiceValues = names(dutch$location), tooltips = ttip_locationNL),
  number_field("input__nLeeft",label=dutch$predictors$nLeeft, "20 - 100", 20, 100),
  
  #page 2
  radio(inputId="input__sCTs", label=dutch$predictors$sCTs, choiceNames=surv_oes$Design$values$sCTs, tooltips=dutch$ttip_sCTs),
  radio(inputId="input__sCN", label=dutch$predictors$sCN, choiceNames=surv_oes$Design$values$sCN, tooltips=dutch$ttip_sCN),
  
  prettyRadioButtons(inputId = "input__tTopog_name.oes", label=dutch$predictors$tTopog_name, choiceNames=uu(dutch$topo_oes),
                     choiceValues=names(dutch$topo_oes), status="success", width="100%", animation = "smooth", bigger=TRUE,
                     icon=icon("check"), plain=TRUE, selected="none"),
  prettyRadioButtons(inputId = "input__tTopog_name.gas", label=dutch$predictors$tTopog_name, choiceNames=uu(dutch$topo_gas),
                     choiceValues=names(dutch$topo_gas), status="success", width="100%", animation = "smooth", bigger=TRUE,
                     icon=icon("check"), plain=TRUE, selected="none"),
  
  radio(inputId="input__tDiffgr", label=dutch$predictors$tDiffgr, choiceNames=surv_oes$Design$values$tDiffgr, tooltips=dutch$ttip_tDiffgr),
  
  #page 3

textOutput("txt")

)
server <- function(input, output) {
  output$txt <- renderText({
    paste("You chose", input$input__tDiffgr)
  })
}
shinyApp(ui, server)
