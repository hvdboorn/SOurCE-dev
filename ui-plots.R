ids_survival = list(tmax="set_surv_tmax", confidence="set_surv_conf", percentile="set_percentile")
ids_hrqol = list(tmax="set_hrqol_tmax", norm="set_hrqol_norm", conf="set_hrqol_conf")
ids_tox = list(conf="set_tox_conf")

ui_survival = fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$script(src="script.js")),
  theme = shinytheme("flatly"),
  
  div(class="page-content",
      plotOutput("out_surv__pictograph", width="100%", height="750"),
      plotlyOutput("out_surv__km", width="100%", height="750")
  ),
  settings_surv,
  
  shinyjs::inlineCSS("#out_surv__km {display: none;}")
)

ui_hrqol = fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$script(src="script.js")),
  theme = shinytheme("flatly"),
  
  div(class="page-content",
      plotlyOutput("out_hrqol__line", width="100%", height="750"),
      plotOutput("out_hrqol__bar", width="100%", height="750")
  ),
  settings_hrqol,
  
  shinyjs::inlineCSS("#out_hrqol__bar {display: none;}")
)

ui_tox = fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$script(src="script.js")),
  theme = shinytheme("flatly"),
  
  div(class="page-content",
      plotOutput("out_tox__bar", width="100%", height="750")
  ),
  settings_tox
)
