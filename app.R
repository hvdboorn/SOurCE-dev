# Load special dependecies ------------------------------------------------

#devtools::install_github('ropensci/plotly')
#devtools::install_github("hvdboorn/hgutils")
#devtools::install_github("tidyverse/ggplot2")
#install.packages("colorspace", repos="http://R-Forge.R-project.org")

# Set-up ------------------------------------------------
library(hgutils)
startup()
load_packages('grid','gridExtra','htmltools','plotly','rms','shinyBS','shinydashboard',
              'shinyjs','shinyWidgets','shinythemes','showtext',"shiny","stringr", "yaml")
#font_add_google("Lato","Lato")
showtext_auto()
windowsFonts(Lato=windowsFont("TT Lato"))

source("util/settings.R")
source("util/util.R")
#source('constanten.R')##Source-file met constanten en functies
#source('pictograph.R')##source-file, bevat de benodigde functie en andere bijbehorende zaken om pictograph te plotten.
source("util/components.R")
source("pictograph.R")

# Create UI ------------------------------------------------
source("ui-patient.R")
source("ui-sidebar.R")
source("ui-prediction.R")

ui = tagList(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), 
            tags$script(src="script.js"), tags$script(src="elements-size.js")),
  
  navbarPage("SOurCE",
   tabPanel(dutch$UI$entry, icon=icon("vcard","fa-fw"),
    ui_patient_entry, id="tab.patient_entry", value="tab.patient_entry"),
   tabPanel(dutch$UI$surv, icon=icon("heartbeat","fa-fw"),
    ui_survival, id="tab.survival", value="tab.survival"),
   tabPanel(dutch$UI$hrqol, icon=icon("child","fa-fw"), 
    ui_hrqol, id="tab.hrqol", value="tab.hrqol"),
   tabPanel(dutch$UI$toxicity, icon=icon("exclamation-triangle","fa-fw"), 
    id="tab.toxicity", value="tab.toxicity"),
   
   id="tabs", inverse=TRUE, theme = shinytheme("flatly"),
   selected = "tab.patient_entry"),
  
  tags$script(src="disable_tabs.js")
)

# #### Scherm voor het weergeven van de verschillende grafieken en diagram opties, het uitkomsten scherm.
# slokdarm_maag_content = fluidPage(#theme = shinytheme("yeti"),
#   tags$div(id="plot_area",mainPanel(side="left",width=10,
#                                     
#                                     tags$head(tags$script(src="elements-size.js")),
#                                     
#     tabsetPanel(id="slokdarm_maag_uitkomsten",type = "pills",selected="EXTRA",###Panel met verschillende tabs
#         tabPanel(value="over",title = "Overleving",###Tab voor de overleving
#         hidden(plotlyOutput("out_over_lijn",width="100%",height="750")),###(plotly)Outputobject voor overleving in lijnweergave
#         plotOutput("out_over_picto", width="100%",height="750")),###Ouputobject voor overleving met pictograph
#       tabPanel(value="kwal",title="Kwaliteit van leven",###Tab voor de QoL
#         plotOutput("out_kwal_bar",width="100%",height="750"),###Outputobject voor QoL in staafdiagram
#         plotlyOutput("out_kwal_lijn",width="100%",height="750")),###Outputobject voor QoL in lijnweergave
#       tabPanel(value="tox",title="Bijwerkingen",###Tab voor de toxiciteit
#         plotOutput("out_tox",width="100%",height="750")),##Ouputobject voor toxiciteit
#       tabPanel(title="EXTRA", value="EXTRA")))),
#   
#   
#   
# ##knop voor weergeven van alle bijwerkingen van tox
# 
# tags$meta(name="viewport",content="width=device-width, initial-scale=1.0")

#TODO: disable tabs if submit is not pressed yet, rewrite shiny:::buildTabset?
server = function(input, output, session)
{
  source("server-patient.R", local = TRUE)
  source("server-sidebar.R", local = TRUE)
  source("server-survival.R", local = TRUE)
  source("server-hrqol.R", local = TRUE)
  
  #Enable tabs again once the user has pressed the submit button
  observe({
    if(!is.null(lp())) {
      shinyjs::runjs("$(\"a[data-value='tab.survival']\").attr(\"data-toggle\",\"tab\");
      $(\"a[data-value='tab.hrqol']\").attr(\"data-toggle\",\"tab\");
      $(\"a[data-value='tab.toxicity']\").attr(\"data-toggle\",\"tab\");
      $(\"ul.nav>li\").removeClass('disabled')")
    }
  })
}

# 
#   ###Functie voor het creeren van de kwaliteit van leven data
#   ###is_eso = parameter voor keuze tussen slokdarm of maag (eso of gas)

# 
# 
#   ###Functie voor het creeren van de toxiciteit data
#   ###is_eso = parameter voor keuze tussen slokdarm of maag (eso of gas)
#   get_tox_data = function(is_eso){pre=ifelse(is_eso,"eso","gas")
#   eventReactive(c(input[[paste0(pre,"_update")]],input[[paste0(pre,"_show_conf")]],input[["chemo_treats"]],input[["first_line_treats"]]),{
#     if (is_eso) {fit=eso_fit;chemo_opt = input$chemo_treats; copy=eso_get_patient();}
#     else {fit=gas_fit;chemo_opt = input$chemo_treats; copy=gas_get_patient();}
#     
#     ###Bijwerkingen van geselecteerde chemo-behandelingen worden gekozen.
#     datalist = lapply(chemo_opt, function(x) toxicity[toxicity$Behandeling == x,])
#     toxic_all = toxicity[toxicity$Behandeling %in% chemo_opt,]#do.call(rbind,datalist)
#     ####Selectie van wat de top3 bijwerkingen zijn, per geselecteerde chemokuur
#     datalist = lapply(chemo_opt, function(x) tox_top3[tox_top3$Behandeling == x,])
#     toxic_top3 = tox_top3[tox_top3$Behandeling %in% chemo_opt,]
#     
#     top3_bijwerkingen = c((unique(toxic_top3[,c("Bijwerking")])))[[1]]
# 
#     ###De bijwerkingen uit de top3_bijwerkingen worden opgehaald uit toxic_all.
#     datalist = list()
#     datalist = lapply(top3_bijwerkingen, function(x) toxic_all[toxic_all$Bijwerking == x,])
# 
#     new_toxic_top3 = do.call(rbind,datalist)
# 
#     return(list(labels=labels,dat=copy,toxic_top3=toxic_top3,toxic_all=toxic_all,chemo_treats = chemo_opt))
#   },ignoreNULL = F,ignoreInit = F
#   )}
# 
# ####Functie voor het plotten van de QoL in een lijnweergave
#   plot_hrqol_line = function() {renderPlotly({
#     if (input$eso_or_gas == "Slokdarm") is_eso = T else is_eso = F
#     qol_data = get_kwal_data(is_eso)()
#     data_kwal_lijn <<- qol_data[['kwallijn']]
#     data_kwal_lijn$behandel = trans(data_kwal_lijn$behandel)
#     norm_score = input$kwal_norm
#     draw_nothing = attr(qol_data$patient_data, "no_therapies")
#     
#     y_labels = c(ifelse(norm_score,"Gemiddelde persoon",NA), kwal_scores)
#     y_breaks = c(ifelse(norm_score,HRQOL_GENPOP,NA), seq(0,100,by=10))
#     if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}
# 
#     regular_plot = ggplot(data=data_kwal_lijn, aes(x=maand, y=qol,group=behandel, color=behandel,
#                                          text = paste(behandel,"<br>Algemeen:",qol))) +
#            geom_line(size=1)+geom_point(size=3)
#       empty_plot = ggplot(data=data_kwal_lijn, aes(x=maand, y=qol,group=behandel, color=behandel,
#                                            text = character(0))) + geom_blank()
#       plot = if(!draw_nothing) regular_plot  else empty_plot
#       plot = plot + theme(panel.border = element_rect(fill=NA,colour="black",size=1),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
#                          plot.title = element_text(size=size_title),
#                          axis.title = element_text(size=size_title),
#                          text = element_text("Lato", size=size_text),
#                          legend.justification = c(0.1),
#                          legend.position = "top",
#                          legend.text=element_text(size=size_legend),
#                          plot.margin=unit(c(2,1,1,1),"cm")) +
#                    ggtitle("Kwaliteit van leven over tijd")+
#                    guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#                    scale_color_manual(name="",values=lijn_kleur) +
#                    scale_x_continuous(name="\nTijd na diagnose",limits=c(0,MAX_KWAL_DUUR),breaks=seq(0,MAX_KWAL_DUUR,by=3),labels=kwal_xas)+
#                    scale_y_continuous(name="", limits = c(0,100), labels = y_labels, breaks = y_breaks)+
#                    if(norm_score)geom_hline(aes(yintercept=HRQOL_GENPOP), color="black", linetype="dashed",size=1)
#      p = ggplotly(plot, tooltip=c("text"))%>%
#       config(p = ., collaborate = F, displayModeBar = F,autosizable=F,scrollZoom=F,showAxisRangeEntryBoxes=F,showAxisDragHandles=F,displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(weg1))%>%
#       layout(legend = list(orientation='h', y=55),hovermode="x",
#              hoverlabel=list(font=list(size=20)))
# 
#     p %>% layout(margin=list(r=120))
#     })
#   }
# 
# #### Functie voor het plotten van de QoL in een staafdiagram
#   plot_hrqol_bar = function() {renderPlot({
#     is_eso = input$eso_or_gas == "Slokdarm"
#     qol_data = get_kwal_data(is_eso)()
#     de_kwaliteit_data = qol_data[['data_kwaliteit']]
#     de_kwaliteit_data$behandel = factor(de_kwaliteit_data$behandel, levels=qol_data[['treats']])
#     de_kwaliteit_data$behandel = trans(de_kwaliteit_data$behandel)
#     kwaliteit_moment = input$kwaliteit_moment
#     norm_score = input$kwal_norm
#     draw_nothing = attr(qol_data$patient_data, "no_therapies")
#     
#     y_labels = c(ifelse(norm_score,"Gemiddelde persoon",NA), kwal_scores)
#     y_breaks = c(ifelse(norm_score,HRQOL_GENPOP,NA), seq(0,100,by=10))
#     if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}
# 
#     dedata <<- de_kwaliteit_data[de_kwaliteit_data$maand == kwaliteit_moment,]###De data wordt afgestemd op de tijd die is gekozen.
#     moment = if (kwaliteit_moment == 6) paste("een half jaar") else paste(kwaliteit_moment, "maanden")###Tijd
# 
#     a = ggplot(data=dedata, aes(x=behandel, y=qol,fill=behandel,
#                                 text = paste(behandel,
#                                              "<br>Algemeen:", qol))) + geom_col(position = position_dodge(width=0.1))
#     b = ggplot(data=dedata, aes(x=behandel, y=qol,fill=behandel,
#                                 text = character(0))) + geom_blank()
#                 plot = if(draw_nothing) b else a
#                 plot = plot + theme(panel.border = element_rect(fill=NA,colour="black",size=1),panel.grid.minor = element_blank(),
#                                     panel.grid.major = element_blank(),
#                       text = element_text('Lato', size=18),
#                       plot.title = element_text(size=size_title),
#                       axis.text.x = element_text(size=size_title),
#                       legend.justification = c(0.1),
#                       legend.position = "top",
#                       legend.text=element_text(size=size_legend),
#                       plot.margin=unit(c(2,1,1,1),"cm")) +
#                 ggtitle(sprintf("Kwaliteit van leven na %s",moment))+
#                 scale_x_discrete(name=NULL,labels = function(x) str_wrap(x, width =20))+
#                 scale_fill_manual(name=NULL, values = lijn_kleur)+
#                 scale_y_continuous(name = NULL, limits = c(0,100), labels = y_labels, breaks = y_breaks)
# 
#     if(input$kwal_show_conf )###Optie voor het weergeven van confidence interval
#     {
#       plot = plot + geom_errorbar(aes(ymin =qol*0.95, ymax = qol*1.05),width=0.15,position = "dodge")###Confidence interval balk
#     }
#     if(input$kwal_norm )###Wanneer optie voor score van algemene bevolking wordt aangeklikt
#     {
#       plot = plot + geom_hline(aes(yintercept=HRQOL_GENPOP), color="black", linetype="dashed",size=1)###Gestippelde lijn voor de algemene bevolking score
#     }
#     plot
#   })}
# 
#   plot_toxicity = function() {renderPlot({
#       is_eso = input$eso_or_gas == "Slokdarm"
#       tox_data = get_tox_data(is_eso)()
#       tox_top3 = tox_data[['toxic_top3']]
#       tox_all <<- tox_data[['toxic_all']]
#       all = input$tox_all
#       chemo_kuren = tox_data[['chemo_treats']]
# 
#       behandel_var = paste0(toxicity$Behandeling," (",tolower(toxicity$Gradering),")")
# 
#       tox_kleur = tox_kleur[1:length(chemo_behandelingen)]
#       light_tox_kleur = tox_light_colors[1:length(chemo_behandelingen)]###lichtere kleuren voor de milde balken
#       tox_guide = as.vector(rbind(tox_kleur,light_tox_kleur))##De guide van de plot krijgt de tox_kleuren en de lichte tox_kleuren
#       names(tox_guide) = sapply(1:length(unique(behandel_var)),function(x)unique(behandel_var)[x])
# 
#       #if(all){###Wanneer optie voor het weergeven van alle bijwerkingen wordt aangeklikt
#           toxplot = ggplot(data=if(all) tox_all else tox_top3, aes(x=Bijwerking,y=Proportie,group=Behandeling,fill=behandel_grd))
#           toxplot = toxplot + if(length(chemo_kuren) == 0) geom_blank() else geom_bar(stat="identity",position="dodge")
#           toxplot = toxplot + scale_fill_manual(name="",values=tox_guide) +
#           #scale_alpha_manual(name="",labels=c("Mild","Ernstig, medische hulp nodig"),values = c(1,0.25))+
#           theme(plot.margin=unit(c(2,1,1,1),"cm"),
#                 line = element_blank(),
#                 text = element_text("Lato", size=20),
#                 axis.title.x = element_text(size=size_title),
#                 plot.title = element_text(size=size_title),
#                 legend.text=element_text(size=size_legend),
#                 legend.justification = c(0, 1), legend.position = "top")+
#           ggtitle("Alle bijwerkingen van chemotherapieën")+
#           scale_x_discrete(name="Bijwerkingen",labels = function(x) str_wrap(x, width =10)) +
#           scale_y_continuous(name=NULL, limits = c(0,100), labels = percentage_scores, breaks = seq(0,100,by=10))
#       toxplot
#       })}

# Run app ----
shinyApp(ui, server)
