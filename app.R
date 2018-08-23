# Load special dependecies ------------------------------------------------

#devtools::install_github('ropensci/plotly')
#devtools::install_github("hvdboorn/hgutils")
#devtools::install_github("tidyverse/ggplot2")
#install.packages("colorspace", repos="http://R-Forge.R-project.org")

# Set-up ------------------------------------------------
library(hgutils)
startup()
load_packages('grid','gridExtra','htmltools','plotly','rms','shinyBS','shinydashboard',"extrafont", "purrr", "XML", "tidyr",
              'shinyjs','shinyWidgets','shinythemes','showtext',"shiny","stringr", "yaml")
#font_add_google("Lato","Lato")
#font_import(pattern="[Ll]ato")
#loadfonts()
#showtext_auto()
windowsFonts(Lato=windowsFont("TT Lato"))

source("util/settings.R")
source("util/util.R")
source("util/components.R")
source("util/pictograph.R")
source("util/logos.R")

# Create UI ------------------------------------------------
source("ui-patient.R")
source("ui-sidebar.R")
source("ui-plots.R")

ui = tagList(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$link(rel="stylesheet", type="text/css", href="icons.css"),
            tags$script(src="script.js"), tags$script(src="elements-size.js")),
  
  navbarPage("SOurCE",
   tabPanel(dutch$UI$surv, icon=icon("heartbeat","fa-fw"),
    ui_survival, id="tab.survival", value="tab.survival"),
   tabPanel(dutch$UI$hrqol, icon=icon("child","fa-fw"), 
    ui_hrqol, id="tab.hrqol", value="tab.hrqol"),
   tabPanel(dutch$UI$toxicity, icon=icon("exclamation-triangle","fa-fw"), 
    ui_tox, id="tab.toxicity", value="tab.toxicity"),
   tabPanel(dutch$UI$entry, icon=icon("vcard","fa-fw"),
            ui_patient_entry, id="tab.patient_entry", value="tab.patient_entry"),
   
   id="tabs", inverse=TRUE, theme = shinytheme("flatly"),
   selected = "tab.patient_entry"),
  
  footer = tags$div(class="footer", style="display: table",
                    tags$div(style="display: table-cell; vertical-align: middle;", 
                             actionBttn(inputId = "information", label = NULL, style = "material-circle", color = "success", size="sm",icon = icon("info"))),
                    
                    tags$div(style="display:table-cell; text-align:right; vertical-align: middle;",
                             logoBttn("aumc",logo_amsterdam_umc, tooltip = "Amsterdam UMC", url="https://www.amsterdamumc.nl/"),
                             logoBttn("iknl",logo_iknl, tooltip = "Integraal Kankercentrum Nederland", url="https://www.iknl.nl/"),
                             logoBttn("ducg",logo_ducg, tooltip = "Dutch Upper GI Cancer Group", url="https://www.ducg.nl/"),
                             logoBttn("kwf",logo_kwf, tooltip = "KWF Kanker Bestrijding", url="https://www.kwf.nl/"),
                             tags$span(style="vertical-align: text-bottom;", "©2018 Amsterdam UMC"))),
  
  tags$script(src="disable_tabs.js")
)

#rewrite html page, add UL close tage before patient and open again one containing patient with class navbar-right. Remove active class on all tabs when clicking
#and add active class on current tab.
server = function(input, output, session)
{
  #showModal(HTML(readLines("login.HTML")))
  
  source("server-patient.R", local = TRUE)
  source("server-sidebar.R", local = TRUE)
  source("server-survival.R", local = TRUE)
  source("server-hrqol.R", local = TRUE)
  source("server-tox.R", local = TRUE)
  source("server-predictions.R", local = TRUE)
  
  #Enable tabs again once the user has pressed the submit button
  observe({
    if(!is.null(lp())) {
      shinyjs::runjs("$(\"a[data-value='tab.survival']\").attr(\"data-toggle\",\"tab\");
      $(\"a[data-value='tab.hrqol']\").attr(\"data-toggle\",\"tab\");
      $(\"a[data-value='tab.toxicity']\").attr(\"data-toggle\",\"tab\");
      $(\"ul.nav>li\").removeClass('disabled')")
    }
  })
  
  observeEvent(input$login_button, {
    if(input$password == "SOURCE") {
      removeModal()
    } else {
      shinyjs::show("password.invalid", anim=TRUE, animType="fade")
      addClass("password","incorrect-input")
    }
  })
  observeEvent(input$password, {
    removeClass("password","incorrect-input")
    shinyjs::hide("password.invalid", anim=TRUE, animType="fade")
  })
  
  observeEvent(input$information, {
    showModal(modalDialog("Hallo",title="SOURCE informatie", easyClose = TRUE, size="l", footer = NULL))
  })
}

shinyApp(ui, server)
