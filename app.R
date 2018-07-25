#devtools::install_github('ropensci/plotly')
#devtools::install_github("hvdboorn/hgutils")
#devtools::install_github("tidyverse/ggplot2")
#install.packages("colorspace", repos="http://R-Forge.R-project.org")
#eventueel later terugzetten: %>% shinycssloaders::withSpinner(color="#a4c408",type=1)

library(hgutils)
startup()
load_packages('grid','gridExtra','htmltools','plotly','rms','shinyBS','shinydashboard',
              'shinyjs','shinyWidgets','shinythemes','showtext',"shiny","stringr")
#font_add_google("Lato","Lato")
showtext_auto()
# if(!"Lato" %in% fonts()) {
#   ttf_import("www/",recursive = FALSE)
# }
# loadfonts()
windowsFonts(Lato=windowsFont("TT Lato"))

source('constanten.R')##Source-file met constanten en functies
source('pagerui.R')##Source-file nodig voor de navigatiebalk onderaan bij de gegevensinvoer
source('pictograph.R')##source-file, bevat de benodigde functie en andere bijbehorende zaken om pictograph te plotten.
source("components.R")

##### Invoer veld voor het kiezen tussen slokdarm of maag tumor

####### Invoerscherm voor de slokdarmtumor ###############
invoer_eso <- hidden(tags$div(id="eso_invoer",
box(id="eso_invoer_box", status = "success", solidHeader = T,collapsible=F,collapsed = F,width=NULL,height =NULL,
  if(!SKIP){fluidPage(fluidRow(id="eso_page1",
    column(width = 12,
           
           
      tags$div(class="form-group", id="fg-age", tags$label("for"="eso_nLeeft", "Leeftijd"),
      tags$input(type="number", class="form-control", id="eso_nLeeft", placeholder="18 - 100", min=18, max=100, style="width: 20%")))),

    hidden(fluidRow(id="eso_page2",
      column(width = 12,
        radioGroupButtons(inputId = "eso_sCTs",label="cT stadium primaire tumor",
                          choices=eso_fit$Design$values$sCTs,selected = "none",checkIcon=list(yes=icon("check")),
                          status="success",size="normal"),
        radioGroupButtons(inputId = "eso_sCN",label="cN stadium primaire tumor",
                          choices=eso_fit$Design$values$sCN,selected = "none",checkIcon=list(yes=icon("check")),
                          status="success",size="normal"),
             radioGroupButtons(inputId = "eso_tDiffgr",label="Tumor differentiatiegraad",
                               choices=eso_fit$Design$values$tDiffgr,selected = "none",checkIcon=list(yes=icon("check")),
                               status="success",size="normal")))),
    hidden(fluidRow(id="eso_page3",
      column(width = 12,
        prettyRadioButtons(inputId = "eso_tTopog_name", label="Locatie primaire tumor",
                            choiceValues=eso_used_locations, status="success", width="100%",
           animation = "smooth", bigger=TRUE, icon=icon("check"), plain=TRUE, selected="none",
           choiceNames = eso_locatie_namen)))),
    hidden(fluidRow(id="eso_page4",column(width = 12,
        radioGroupButtons(inputId = "eso_only_lymf_meta",label="Alleen afstandsmetastasen in de lymfeklieren",
                          choices=c("Ja","Nee"),selected = "none", size="normal", checkIcon=list(yes=icon("check")), status = "success"),
        tags$div(class="collapse lp", style="padding: 0px; box-shadow: 0 0 transparent;",
        radioGroupButtons(inputId = "eso_liver_meta",label="Lever metastasen",
                          choices=c("Ja","Nee"),selected = "none",size="normal", checkIcon=list(yes=icon("check")), status = "success"),
        radioGroupButtons(inputId = "eso_peri_meta",label="Peritoniale metastasen",
                          choices=c("Ja","Nee"),selected = "none",size="normal", checkIcon=list(yes=icon("check")), status = "success")),
        
        tags$div(class="form-group", id="fg-eso_nmeta", tags$label("for"="eso_nmeta", "Aantal locaties met metastasen"),
                 tags$input(type="number", class="form-control", id="eso_nmeta", placeholder="1 - 6", min=1, max=6, style="width: 20%"))))),
    
        hidden(actionBttn(inputId = "eso_update",label="Voer gegevens in", style = "fill", color="success")),
        tags$br(),tags$br(),tags$br(),fluidRow(pageruiInput('eso_pager',page_current = 1,pages_total = 4)))}
        else{actionBttn(inputId = "eso_update",label="Voer gegevens in", style = "fill", color="success")})))

#### Gegevensinvoer scherm
invoer_pat =
  fluidPage(id="invoer",
            radio(inputId = "eso_or_gas", label="Locatie tumor:", selected=ifelse(SKIP,"Slokdarm","none"), 
                              choiceNames=list("Slokdarm", "Maag")),### Invoer voor keuze tussen slokdarmtumor en maagtumor
            tags$br(),
            fluidRow(invoer_eso))###Gegevensinvoer scherm voor slokdarmtumor en maagtumor

#### Scherm voor het weergeven van de verschillende grafieken en diagram opties, het uitkomsten scherm.
slokdarm_maag_content = fluidPage(#theme = shinytheme("yeti"),
  tags$div(id="plot_area",mainPanel(side="left",width=10,
                                    
                                    tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerHeight", $("#opties_rechts").height());
Shiny.onInputChange("innerWidthLijn",$("#out_over_lijn").width());
                             Shiny.onInputChange("innerWidthPicto",$("#out_over_picto").width());
});
$(window).resize(function(e) {Shiny.onInputChange("innerHeight", $("#opties_rechts").height());
                             Shiny.onInputChange("innerWidthLijn",$("#out_over_lijn").width());
                             Shiny.onInputChange("innerWidthPicto",$("#out_over_picto").width());
                                                          });')),
                                    
    tabsetPanel(id="slokdarm_maag_uitkomsten",type = "pills",selected="EXTRA",###Panel met verschillende tabs
        tabPanel(value="over",title = "Overleving",###Tab voor de overleving
        hidden(plotlyOutput("out_over_lijn",width="100%",height="750")),###(plotly)Outputobject voor overleving in lijnweergave
        plotOutput("out_over_picto", width="100%",height="750")),###Ouputobject voor overleving met pictograph
      tabPanel(value="kwal",title="Kwaliteit van leven",###Tab voor de QoL
        plotOutput("out_kwal_bar",width="100%",height="750"),###Outputobject voor QoL in staafdiagram
        plotlyOutput("out_kwal_lijn",width="100%",height="750")),###Outputobject voor QoL in lijnweergave
      tabPanel(value="tox",title="Bijwerkingen",###Tab voor de toxiciteit
        plotOutput("out_tox",width="100%",height="750")),##Ouputobject voor toxiciteit
      tabPanel(title="EXTRA", value="EXTRA")))),
  
  
  div(class="sidenav",id="opties_rechts",###Kolom voor de verschillende opties aan de rechterzijde van het scherm
    tags$div(class="border-div",
         tags$p(class="align_right",
    h4("Wijzig weergave"),
    dual_switch(inputId = "dual-switch", "male","line-chart",c("picto_button"="Pictogram","line_button"="Kaplan Meier")),

    hidden(actionBttn("icon_menu_button", icon=icon("line-chart", class="fa-fw"), color="primary",style = "material-flat",size="lg")),
###Div met daarin de knop voor het switchen tussen weergaves van overleving
#info icon with information.
# tags$a(href="#", "data-toggle"="popover", title="Behandelingen", "data-placement"="left", 
#        "data-content"="In deze lijst kunt verschillende behandelingen vinden.", "data-trigger"="focus",
#        tags$span(class="glyphicon glyphicon-info-sign")),
        tags$h4("Behandelingen"),
        tags$button(class="action-button bttn bttn-material-flat bttn-lg bttn-success bttn-no-outline",
                    id="menu_button_treatment", type="button", "data-toggle"="collapse", 
                    "data-target"="#treatment_container", icon("medkit","fa-fw")),
        tags$div(id="treatment_container", class="collapse in",
                 prettyCheckboxGroup(inputId = "first_line_treats", label=NULL, choiceValues=eso_used_treats, status="success",
                                     animation = "smooth", bigger=TRUE, icon=icon("check"), plain=TRUE,
                                     choiceNames = trans(eso_used_treats)),
                 hidden(prettyCheckboxGroup(inputId="chemo_treats", label=NULL, choices=chemo_behandelingen, selected="none", status = "success",
                                            animation="smooth",bigger=TRUE, icon=icon("check"), plain=TRUE)),
                 actionBttn("clear_treatment_selection", "wissen", icon=icon("times"), style="simple", color="warning",
                            size="xs")),
    tags$h4("Diagram opties"),

      dropdown(
        hidden(checkboxGroupButtons(inputId = "percentile_buttons",label = "Uitkomst",choices = c("slecht","typisch","goed"),
                                    checkIcon=list(yes=icon("check")), status="warning", size = "sm")),
      hidden(materialSwitch(inputId = "over_show_conf",label = "95% CI",status="warning", right=TRUE)),##knop voor confidence bij overleving
      sliderInput(inputId="xmax",label="Tijd na diagnose",value=12,min=0,max=24),##knop
      hidden(materialSwitch(inputId = "kwal_norm",label ="Algemene bevolking",value = TRUE,status="warning", right=TRUE)),##knop voor algemene bevolking score
      hidden(materialSwitch(inputId = "kwal_show_conf",label ="95% CI",value = F,status="warning", right=TRUE)),##knop voor confidence interval bij kwaliteit van leven
      hidden(sliderInput(inputId="kwaliteit_moment",label="Follow-Up",value=0,min=0,max=MAX_KWAL_DUUR,step=3,width = "200px")),##Follow-up schuifbalk bij kwaliteit van leven
      hidden(materialSwitch(inputId="tox_all",label="Alle bijwerkingen",value=F,status="warning", right=TRUE)), 
      
      style="material-flat", status="success", size="lg","icon"=icon("gears", class = "fa-fw"), right=TRUE, animate = FALSE,
      inputId="menu_button_diagramopties")))))
##knop voor weergeven van alle bijwerkingen van tox

tags$meta(name="viewport",content="width=device-width, initial-scale=1.0")

ui = navbarPage("SOurCE",id="tabs", inverse=TRUE, tabPanel("Invoer patiënt", invoer_pat, id="gegevensinvoer"),
                tabPanel("Content", slokdarm_maag_content, id="uitkomsten"), selected = ifelse(SKIP,"Content","Invoer patiënt"),useShinyjs(),
                header = tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$script(src="script.js"),
                                   tags$link(rel="stylesheet", type="text/css", href="animate.css")),
                theme = shinytheme("flatly"))

server <- function(input, output, session)
{
  selected = "picto_button"
  removeClass(class = "bttn-primary", selector = "button[id*='menu_button']")
  removeClass(class = "bttn-success", selector = "button[id*='menu_button']")
  addClass(class = "btn btn-warning", selector = "button[id*='menu_button']")
  
  hideTab(inputId = "slokdarm_maag_uitkomsten", target="EXTRA") #hide the last tab so no tabs are selected at first
  
  onclick("iddual-switch", {
    selected = setdiff(c("picto_button","line_button"),selected)
    updateRadioGroupButtons(session, "dual-switch", selected = selected)
    runjs("document.activeElement.blur();")
    
    toggleElement("out_over_picto", condition=selected=="picto_button")
    toggleElement("out_over_lijn", condition=selected=="line_button")
    
    
    toggleElement("percentile_buttons", condition=selected=="line_button")
    toggleElement("over_show_conf", condition=selected=="line_button")
    toggleElement("xmax", condition=selected=="line_button" | selected=="picto_button")
  })

  observeEvent(input$clear_treatment_selection, {updatePrettyCheckboxGroup(session, "first_line_treats", selected = "")})

  #get data from the form and put it in a Data.Frame so we can make predictions
  get_patient = function(is_eso) {reactive(
  {
      getInput = function(x) input[[paste0(ifelse(is_eso,"eso_","gas_"), x)]]
      obs = if(is_eso) eso_sample else gas_sample
      leeft = getInput("nLeeft")
      geslacht = getInput("pGesl")
      ct = getInput("sCTs")
      cn = getInput("sCN")
      dgr = getInput("tDiffgr")
      topog = getInput("tTopog_name")
      morf = getInput("tMorfology_simple")
      metacat = getInput("nmeta_cat")
      nmeta = getInput("nmeta")
      only_lymf = getInput("only_lymf_meta")
      liver_met = getInput("liver_meta")
      treatments = input$first_line_treats

      if(!SKIP)##Wanneer skip niet aan staat, worden de inputvalues ingeladen op de sample
      {
        obs[1,"nLeeft"]             = as.numeric(leeft)##leeftijd
        obs[1,"sCTs"]               = toString(ct)##T stadium
        obs[1,"sCN"]                = as.numeric(cn)##N stadium
        obs[1,"tDiffgr"]            = toString(dgr)##differentiatiegraad
        obs[1,"tTopog_name"]        = toString(topog)###locatie tumor
        if(is_eso)##Bij slokdarmtumor (esophagus)
        {
          if(only_lymf=="Ja")##wanneer alleen lymfeklier metastasen, andere vormen niet aanwezig
          {
            obs[1,'only_lymf_meta'] = "Yes"
            obs[1,'peri_meta'] = "No"
            obs[1,'liver_meta'] = "No"
          } else { ##Wanneer anders dan lymfeklier (dus niet alleen lymfeklier)
            obs[1,'only_lymf_meta'] = "No"
            obs[1,'liver_meta'] = ifelse(liver_met=="Ja","Yes","No")
            obs[1,'peri_meta'] = ifelse(getInput("peri_meta")=="Ja","Yes","No")
          }
          obs[1,"nmeta"]              = as.numeric(nmeta)###Aantal metastaten
        } else { ###Bij maagtumor (gastric)
          if(only_lymf=="Ja")
          {
            obs[1,'only_lymf_meta'] = "Yes"
            obs[1,'liver_meta'] = "No"
          } else {
            obs[1,'only_lymf_meta'] = "No"
            obs[1,'liver_meta'] = ifelse(liver_met=="Ja","Yes","No")
          }
          obs[1,"nmeta_cat"]          = toString(metacat)##Aanta metastasen (categoriaal)
        }
      } else { ###Wanneer het invoer-scherm geskipt wordt, worden de verschillende values al een waarde toegekend
        obs[1,"nLeeft"]             = 67
        obs[1,"sCTs"]               = "1"
        obs[1,"sCN"]                = 2
        obs[1,"tDiffgr"]            = "G1"
        obs[1,"tTopog_name"]        = if (is_eso) "Cervical" else "Fundus"
        if(is_eso) {
          obs[1,'only_lymf_meta'] = "Yes"
          obs[1,'peri_meta'] = "No"
          obs[1,'liver_meta'] = "No"
          obs[1,"nmeta"] = 2
        } else {
          obs[1,'only_lymf_meta'] = "Yes"
          obs[1,'liver_meta'] = "No"
          obs[1,"nmeta_cat"] = "1"
        }
      }
      
      N = length(treatments)
      obs = obs[rep(1,N),]
      if(N > 0)
        obs[1:N,"first_line"] = treatments ###Behandeling
      attr(obs,"no_therapies") = N==0
      obs
  }
  )}
  eso_get_patient = get_patient(TRUE)
  gas_get_patient = get_patient(FALSE)

  ###Functie voor het creeren van de kwaliteit van leven data
  ###is_eso = parameter voor keuze tussen slokdarm of maag (eso of gas)
  get_kwal_data = function(is_eso){pre=ifelse(is_eso,"eso","gas")
  eventReactive(c(input[[paste0(pre,"_update")]],input[["chemo_treats"]],input[["first_line_treats"]]),{
    if(input$eso_or_gas == "Slokdarm"){ patient=eso_get_patient(); treats=input$first_line_treats}
    
    else if(input$eso_or_gas == "Maag"){patient=gas_get_patient(); treats=input$first_line_treats}
    
    
    ####QoL DATA IS RANDOM!!!######
    behandel = rep(treats,times=1,each=3)
    qol = mround(sapply(1:length(behandel), function(x) mean(runif(5, 0, 100))), 5)
    if(length(behandel)==0) {
      behandel=character(0)
      qol = numeric(0)
    }
    maand = rep(seq(0,MAX_KWAL_DUUR,by = 3),times=length(treats),each=1)
    
    kwal_data = data.frame(behandel,maand,qol)#fysiek,rol,emot,sociaal,qogn)
    ###de algemene qol score wordt hier toegevoegd, deze is het gemiddelde van de scores van de onderdelen van kwaliteit van leven (fysiek, rol, etc)
    kwal_lijn = kwal_data[,c("behandel","maand","qol")]###Data in de vorm van lijn, alleen behandeling, tijd en QoL score nodig, aparte scores niet nodig
    return(list(treats=treats,data_kwaliteit = kwal_data,kwallijn=kwal_lijn,patient_data=patient))
  },ignoreNULL = F,ignoreInit = F)
  }


  ###Functie voor het creeren van de toxiciteit data
  ###is_eso = parameter voor keuze tussen slokdarm of maag (eso of gas)
  get_tox_data = function(is_eso){pre=ifelse(is_eso,"eso","gas")
  eventReactive(c(input[[paste0(pre,"_update")]],input[[paste0(pre,"_show_conf")]],input[["chemo_treats"]],input[["first_line_treats"]]),{
    if (is_eso) {fit=eso_fit;chemo_opt = input$chemo_treats; copy=eso_get_patient();}
    else {fit=gas_fit;chemo_opt = input$chemo_treats; copy=gas_get_patient();}
    
    ###Bijwerkingen van geselecteerde chemo-behandelingen worden gekozen.
    datalist = lapply(chemo_opt, function(x) toxicity[toxicity$Behandeling == x,])
    toxic_all = toxicity[toxicity$Behandeling %in% chemo_opt,]#do.call(rbind,datalist)
    ####Selectie van wat de top3 bijwerkingen zijn, per geselecteerde chemokuur
    datalist = lapply(chemo_opt, function(x) tox_top3[tox_top3$Behandeling == x,])
    toxic_top3 = tox_top3[tox_top3$Behandeling %in% chemo_opt,]
    
    top3_bijwerkingen = c((unique(toxic_top3[,c("Bijwerking")])))[[1]]

    ###De bijwerkingen uit de top3_bijwerkingen worden opgehaald uit toxic_all.
    datalist = list()
    datalist = lapply(top3_bijwerkingen, function(x) toxic_all[toxic_all$Bijwerking == x,])

    new_toxic_top3 = do.call(rbind,datalist)

    return(list(labels=labels,dat=copy,toxic_top3=toxic_top3,toxic_all=toxic_all,chemo_treats = chemo_opt))
  },ignoreNULL = F,ignoreInit = F
  )}

####Functie voor het plotten van de QoL in een lijnweergave
  plot_hrqol_line = function() {renderPlotly({
    if (input$eso_or_gas == "Slokdarm") is_eso = T else is_eso = F
    qol_data = get_kwal_data(is_eso)()
    data_kwal_lijn <<- qol_data[['kwallijn']]
    data_kwal_lijn$behandel = trans(data_kwal_lijn$behandel)
    norm_score = input$kwal_norm
    draw_nothing = attr(qol_data$patient_data, "no_therapies")
    
    y_labels = c(ifelse(norm_score,"Gemiddelde persoon",NA), kwal_scores)
    y_breaks = c(ifelse(norm_score,HRQOL_GENPOP,NA), seq(0,100,by=10))
    if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}

    regular_plot = ggplot(data=data_kwal_lijn, aes(x=maand, y=qol,group=behandel, color=behandel,
                                         text = paste(behandel,"<br>Algemeen:",qol))) +
           geom_line(size=1)+geom_point(size=3)
      empty_plot = ggplot(data=data_kwal_lijn, aes(x=maand, y=qol,group=behandel, color=behandel,
                                           text = character(0))) + geom_blank()
      plot = if(!draw_nothing) regular_plot  else empty_plot
      plot = plot + theme(panel.border = element_rect(fill=NA,colour="black",size=1),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                         plot.title = element_text(size=size_title),
                         axis.title = element_text(size=size_title),
                         text = element_text("Lato", size=size_text),
                         legend.justification = c(0.1),
                         legend.position = "top",
                         legend.text=element_text(size=size_legend),
                         plot.margin=unit(c(2,1,1,1),"cm")) +
                   ggtitle("Kwaliteit van leven over tijd")+
                   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
                   scale_color_manual(name="",values=lijn_kleur) +
                   scale_x_continuous(name="\nTijd na diagnose",limits=c(0,MAX_KWAL_DUUR),breaks=seq(0,MAX_KWAL_DUUR,by=3),labels=kwal_xas)+
                   scale_y_continuous(name="", limits = c(0,100), labels = y_labels, breaks = y_breaks)+
                   if(norm_score)geom_hline(aes(yintercept=HRQOL_GENPOP), color="black", linetype="dashed",size=1)
     p <- ggplotly(plot, tooltip=c("text"))%>%
      config(p = ., collaborate = F, displayModeBar = F,autosizable=F,scrollZoom=F,showAxisRangeEntryBoxes=F,showAxisDragHandles=F,displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(weg1))%>%
      layout(legend = list(orientation='h', y=55),hovermode="x",
             hoverlabel=list(font=list(size=20)))

    p %>% layout(margin=list(r=120))
    })
  }

#### Functie voor het plotten van de QoL in een staafdiagram
  plot_hrqol_bar = function() {renderPlot({
    is_eso = input$eso_or_gas == "Slokdarm"
    qol_data = get_kwal_data(is_eso)()
    de_kwaliteit_data = qol_data[['data_kwaliteit']]
    de_kwaliteit_data$behandel <- factor(de_kwaliteit_data$behandel, levels=qol_data[['treats']])
    de_kwaliteit_data$behandel = trans(de_kwaliteit_data$behandel)
    kwaliteit_moment = input$kwaliteit_moment
    norm_score = input$kwal_norm
    draw_nothing = attr(qol_data$patient_data, "no_therapies")
    
    y_labels = c(ifelse(norm_score,"Gemiddelde persoon",NA), kwal_scores)
    y_breaks = c(ifelse(norm_score,HRQOL_GENPOP,NA), seq(0,100,by=10))
    if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}

    dedata <<- de_kwaliteit_data[de_kwaliteit_data$maand == kwaliteit_moment,]###De data wordt afgestemd op de tijd die is gekozen.
    moment = if (kwaliteit_moment == 6) paste("een half jaar") else paste(kwaliteit_moment, "maanden")###Tijd

    a = ggplot(data=dedata, aes(x=behandel, y=qol,fill=behandel,
                                text = paste(behandel,
                                             "<br>Algemeen:", qol))) + geom_col(position = position_dodge(width=0.1))
    b = ggplot(data=dedata, aes(x=behandel, y=qol,fill=behandel,
                                text = character(0))) + geom_blank()
                plot = if(draw_nothing) b else a
                plot = plot + theme(panel.border = element_rect(fill=NA,colour="black",size=1),panel.grid.minor = element_blank(),
                                    panel.grid.major = element_blank(),
                      text = element_text('Lato', size=18),
                      plot.title = element_text(size=size_title),
                      axis.text.x = element_text(size=size_title),
                      legend.justification = c(0.1),
                      legend.position = "top",
                      legend.text=element_text(size=size_legend),
                      plot.margin=unit(c(2,1,1,1),"cm")) +
                ggtitle(sprintf("Kwaliteit van leven na %s",moment))+
                scale_x_discrete(name=NULL,labels = function(x) str_wrap(x, width =20))+
                scale_fill_manual(name=NULL, values = lijn_kleur)+
                scale_y_continuous(name = NULL, limits = c(0,100), labels = y_labels, breaks = y_breaks)

    if(input$kwal_show_conf )###Optie voor het weergeven van confidence interval
    {
      plot = plot + geom_errorbar(aes(ymin =qol*0.95, ymax = qol*1.05),width=0.15,position = "dodge")###Confidence interval balk
    }
    if(input$kwal_norm )###Wanneer optie voor score van algemene bevolking wordt aangeklikt
    {
      plot = plot + geom_hline(aes(yintercept=HRQOL_GENPOP), color="black", linetype="dashed",size=1)###Gestippelde lijn voor de algemene bevolking score
    }
    plot
  })}

  plot_toxicity = function() {renderPlot({
      is_eso = input$eso_or_gas == "Slokdarm"
      tox_data = get_tox_data(is_eso)()
      tox_top3 = tox_data[['toxic_top3']]
      tox_all <<- tox_data[['toxic_all']]
      all = input$tox_all
      chemo_kuren = tox_data[['chemo_treats']]

      behandel_var = paste0(toxicity$Behandeling," (",tolower(toxicity$Gradering),")")

      tox_kleur = tox_kleur[1:length(chemo_behandelingen)]
      light_tox_kleur = tox_light_colors[1:length(chemo_behandelingen)]###lichtere kleuren voor de milde balken
      tox_guide = as.vector(rbind(tox_kleur,light_tox_kleur))##De guide van de plot krijgt de tox_kleuren en de lichte tox_kleuren
      names(tox_guide) = sapply(1:length(unique(behandel_var)),function(x)unique(behandel_var)[x])

      #if(all){###Wanneer optie voor het weergeven van alle bijwerkingen wordt aangeklikt
          toxplot = ggplot(data=if(all) tox_all else tox_top3, aes(x=Bijwerking,y=Proportie,group=Behandeling,fill=behandel_grd))
          toxplot = toxplot + if(length(chemo_kuren) == 0) geom_blank() else geom_bar(stat="identity",position="dodge")
          toxplot = toxplot + scale_fill_manual(name="",values=tox_guide) +
          #scale_alpha_manual(name="",labels=c("Mild","Ernstig, medische hulp nodig"),values = c(1,0.25))+
          theme(plot.margin=unit(c(2,1,1,1),"cm"),
                line = element_blank(),
                text = element_text("Lato", size=20),
                axis.title.x = element_text(size=size_title),
                plot.title = element_text(size=size_title),
                legend.text=element_text(size=size_legend),
                legend.justification = c(0, 1), legend.position = "top")+
          ggtitle("Alle bijwerkingen van chemotherapieën")+
          scale_x_discrete(name="Bijwerkingen",labels = function(x) str_wrap(x, width =10)) +
          scale_y_continuous(name=NULL, limits = c(0,100), labels = percentage_scores, breaks = seq(0,100,by=10))
      toxplot
      })}

  
  
  ###Functie voor het creeren van de overleving data
  ###is_eso = parameter voor keuze tussen slokdarm of maag (eso of gas)
  predict_survival = function(is_eso) {pre=ifelse(is_eso,"eso","gas")
  eventReactive(c(input[[paste0(pre,"_update")]],input[["first_line_treats"]]),{
    
    if (is_eso) {
      fit=eso_fit;patient=eso_get_patient()
    } else {
      fit=gas_fit;patient=gas_get_patient()
    }
    labels=patient$first_line#input$first_line_treats
    
    nr = if(is.data.frame(patient)) nrow(patient) else 1
    
    #if(length(labels) == 0) labels="None"
    
    df = suppressWarnings(data.frame(survest(fit,newdata=patient[1,]))); df$label=patient$first_line[1]
    if (nr > 1)
    {
      for (i in 2:nr)
      {
        ndf = suppressWarnings(data.frame(survest(fit,newdata=patient[i,]))); ndf$label=patient$first_line[i]
        df = rbind(df,ndf)
      }
    }
    df = df[,names(df) %nin% c("linear.predictors","std.err")]
    df =df[complete.cases(df),]
    
    return(list(survival=df, labels=labels, patient_data = patient))
  },ignoreNULL = FALSE,ignoreInit = FALSE)
  }
####Functie voor het plotten van de overleving in een lijngrafiek
  plot_survival_km = function() renderPlotly(
    {
      is_eso = input$eso_or_gas == "Slokdarm"
      surv_data <<- predict_survival(is_eso)()
      survival <<- surv_data$survival
      survival$time = survival$time/MAAND_DUUR ###tijd in analysis wordt omgezet in maanden.
      conf =  input$over_show_conf
      xmax =  input$xmax
      labels = surv_data[['labels']]
      draw_nothing = attr(surv_data$patient_data, "no_therapies")

      med_surv = survival
      survival=survival[survival$time<=xmax,]###analysis is analysis tot de ingevoerde xmax
      survival$label = trans(survival$label)###de behandelingen in analysis worden vertaald naar het nederlands.

      behandeling_labels <<- data.frame()###nieuwe dataframe voor het creeren van de annotations in de lijngrafiek
      if(!draw_nothing){
        for (l in unique(survival$label))##voor alle behandelingen die aangeklikt zijn, en dus in analysis staan
        {
          a=survival[survival$label==l,]###alle rijen met de geselecteerde behandeling
          behandeling_labels=rbind(behandeling_labels,a[nrow(a),])###Elke laatste rij van analysis van elke behandeling wordt hier in behandeling_labels gezet
        }
        ###de rijen in behandeling_labels worden gesorteerd op basis van de surv, van klein naar groot.
        behandeling_labels = behandeling_labels[order(behandeling_labels$surv),]
        ###De y positie voor de annotations worden hier toegevoegd aan behandeling_labels.
        behandeling_labels$new_y = separate_values(behandeling_labels$surv, 0.08)
        
        ###annotations worden hier gedefinieerd.
        annotations = apply(behandeling_labels, 1, function(b) list(
          xref = "paper",
          x = 1,
          y = as.numeric(b[["surv"]]),
          xanchor = 'left',
          yanchor = "middle",
          showarrow = T,
          arrowhead = 0,
          arrowcolor = lijn_kleur[b["label"]],
          font=list(family="Lato", size=18, color=lijn_kleur[b["label"]]),
          axref="x",
          ayref="y",
          ax = 40,
          ay= as.numeric(b[["new_y"]]),####y-positie voor de annotations
          text=str_wrap(as.character(b["label"]), width =20)))
      }

      ###De scenario/waarschijnlijkheid van de overleving ligt tussen de 0.25 en 0.75. Dit wordt gehaald uit de analysis
      worst_range = survival[survival$surv >=0.75,]
      typical_range = survival[survival$surv >=0.25 & survival$surv<=0.75,]
      best_range = survival[survival$surv <= 0.25,]
      breaks = get_breaks(limits=xmax, N=12, max_breaks = 15, include_bounds = TRUE)
      lbls = breaks_to_labels(breaks)
      
      normal = ggplot(data=survival, aes(x=time, y=surv, group=label, fill=label, color=label,
                                         text = paste((label), mround(surv*100, 5),"%")))
      empty = ggplot(data=survival, aes(x=time, y=surv, group=label, fill=label, color=label,
                                        text = character(0)))
      
      p = if(!draw_nothing) {normal+geom_line(group=1,size=1)} else {empty+geom_blank()}
             p = p+ggtitle("Overleving")+
             scale_x_continuous(name ="Tijd na diagnose (in maanden)" , limits = c(0, xmax),
                               breaks = breaks, labels=lbls, expand=c(0,0)) +
             scale_y_continuous("",limits=c(0,1),labels=function(x) paste0(round(x*100),"%"), 
                               breaks = seq(0,1, by = 0.1),expand=c(0,0)) +
             scale_fill_manual(name="",values=lijn_kleur) + scale_color_manual(name="",values=lijn_kleur) +
               theme(legend.position = "top",panel.border = element_rect(fill=NA,colour="black",size=1),
                     plot.margin = unit(c(2,5,1,1),units = "cm"),
                     legend.text = element_text(size=size_legend),
                     panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                     plot.title=element_text(size=size_title,vjust=-5),
                     text = element_text("Lato",size = size_text),
                     axis.title.x = element_text(vjust=-2, size=size_title))+
             ###Confidence interval gebied om de lijn
             if(conf) geom_ribbon(data=survival, aes(x=time, ymax=upper, ymin=lower, group =label, fill=label, color=label),
                                 alpha=0.2,position = "identity",inherit.aes = F)

      if("typisch" %in% input$percentile_buttons && !draw_nothing  && nrow(typical_range) > 0)###Als de scenario/waarschijnlijkheid functie wordt aangeklikt
      {
        p = p + geom_area(data = typical_range,aes(x=time,y=surv,group =label, fill=label, color=label),
                                alpha =0.3,position = "identity",show.legend = F,inherit.aes = F)
      }
      if("slecht" %in% input$percentile_buttons && !draw_nothing && nrow(worst_range) > 0) {
        p = p + geom_area(data = worst_range,aes(x=time,y=surv,group =label, fill=label, color=label),
                          alpha =0.5,position = "identity",show.legend = F,inherit.aes = F)
      }
       if("goed" %in% input$percentile_buttons && !draw_nothing && nrow(best_range) > 0) {
         p = p + geom_area(data = best_range,aes(x=time,y=surv,group =label, fill=label, color=label),
                           alpha =0.1,position = "identity",show.legend = F,inherit.aes = F)
       }
    plotly_lijn <- ggplotly(p, height=input$innerHeight*0.85, tooltip=c("text"), type="scatter")%>%
    config(p = ., collaborate = F, displayModeBar = F,autosizable=F, scrollZoom=F,showAxisRangeEntryBoxes=F,
    showAxisDragHandles=F,displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(weg1))%>%
    layout(legend = list(orientation='h', y=55),hovermode="x",hoverlabel=list(font=list(size=20))) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

    if(!draw_nothing)
    {
      for(i in 1:length(annotations))##De annotations worden toegevoegd aan de plotly plot
      {
        plotly_lijn =  plotly_lijn%>%layout(annotations=annotations[[i]])
      }
    }
    plotly_lijn
  })

  #### Functie voor het plotten van de overleving in een pictograph
  plot_survival_pictograph = function() renderPlot(
    {
      wide = session$clientData[["output_out_over_picto_width"]] <= 690 #add factor
      if (input$eso_or_gas == "Slokdarm") is_eso = T else is_eso = F
      surv_data <<- predict_survival(is_eso)()
      survival <<- surv_data[['survival']]
      behandeling = surv_data[['labels']]
      maand_na_diag = input$xmax##Aantal maanden na diagnose
      xmax =  maand_na_diag*MAAND_DUUR###xmax is xmax input (in maanden) * maand_duur om het om te zetten in aantal dagen
      survival=survival[survival$time<=xmax,]
      draw_nothing = attr(surv_data$patient_data, "no_therapies")

      behandeling_nl = as.character(trans(behandeling))##behandelingen vertaald naar NL
      
      picto = if(draw_nothing) {
          survival = 100; names(survival)="EMPTY"
          plot_pictograph(survival,pictograph_icons,from_top = T,picto_title="", show_legend = FALSE, wide = wide)
        } else {
          overlevingen <<- survival$surv[survival$time == max(survival$time)]
          if(length(behandeling)<=2) {
              behand_titel = paste0(tolower(behandeling_nl[1]),ifelse(length(behandeling)>1,paste0(" and ",tolower(behandeling_nl[2])),""))
              if(maand_na_diag %% 6 ==0) {
                if(maand_na_diag/12==0.5) periode = "een half" else periode=maand_na_diag/12
                titel_picto_een_treat = sprintf("Overleving na %s jaar bij %s",periode,behand_titel)
              } else {
                titel_picto_een_treat = sprintf("Overleving na %s maanden bij %s",maand_na_diag,behand_titel)
              }
              survival = lapply(overlevingen, function(x) x*100); names(survival) = behandeling_nl
              plot_pictograph(survival,pictograph_icons, from_top = T,picto_title=titel_picto_een_treat,wide=wide)
            } else if(length(behandeling) > 2) {
              list_survivals = list()
              for(i in 1:length(behandeling_nl))###Voor alle behandelingen
              { ##De overlevingen worden in list_survivals gezet
                list_survivals[[i]] = list(overlevingen[i] * 100);names(list_survivals[[i]])=c(STATES[[behandeling_nl[i]]])
              }
               sub_pictos = lapply(1:length(behandeling),function(i) plot_pictograph(list_survivals[[i]],pictograph_icons,from_top = T,
                                                                              picto_title=paste(behandeling_nl[i]),wide=length(behandeling)==3))
               picto_titel=textGrob(sprintf("Overleving na %s maanden",maand_na_diag), gp=gpar(fontface="bold",fontsize=size_title))
               margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
               N = length(sub_pictos)
               grid = if(N==3) list(rows=3, columns=1) else get_square_grid(N)###
               picto_ncol = grid$rows###Aantal benodigde kolommen (in de functie staat het nog als rows)
               picto_nrow = grid$columns### Aantal benodigde rows (in de functie staat het nog als columns)
               arrangeGrob(grobs=sub_pictos,ncol=picto_ncol, nrow = picto_nrow,top=picto_titel)
               #grid.arrange(grobs = lapply(sub_pictos, "+", margin),ncol=3, nrow = 1,top=picto_titel,padding=unit(10,"mm"))
            }
        }
      
        grid.newpage()
        grid.draw(picto)
    }, height=reactive(input$innerHeight*0.85))

  # ##Functie voor het ophalen van de plots
  # get_de_plots = function()
  #   {##De verschillende plot functies worden aangeroepen op de verschillende outputs
        output$out_over_lijn = plot_survival_km()
        output$out_kwal_lijn = plot_hrqol_line()
        output$out_kwal_bar = plot_hrqol_bar()
        output$out_over_picto = plot_survival_pictograph()
        output$out_tox = plot_toxicity()
  #   }
  # get_de_plots()##Functie voor het ophalen en weergeven van de plots wordt gestart
   
   observeEvent(input$icon_menu_button,{
     if (input$slokdarm_maag_uitkomsten == "kwal") {
         #button_values$kwal_but_val = button_values$kwal_but_val + 1##wanneer op de knop gedrukt wordt, wordt de value verhoogd met 1
         updateActionButton(session, "icon_menu_button",
           icon=if(input$icon_menu_button%%2==1) icon("line-chart","fa-fw") else icon("bar-chart","fa-fw"))
         
         toggleElement("out_kwal_bar", condition=input$icon_menu_button%%2==1)
         toggleElement("out_kwal_lijn", condition=input$icon_menu_button%%2==0)
     }
   })

   ##switcht tussen het invoerformulier voor slokdarmkanker en maagkanker
  observeEvent(input[['eso_or_gas']],{
    if(input$eso_or_gas == "Slokdarm"){
      showElement("eso_invoer")
      hideElement("gas_invoer")
    }
    else if(input$eso_or_gas == "Maag") {
      hideElement("eso_invoer")
      showElement("gas_invoer")
    }
  })

##Hier wordt wanneer op update gedrukt wordt, geswitcht van de invoer naar de uitkomsten
  observeEvent(input$eso_update,{
    updateTabItems(session, "tabs", "Content")
    })
  ##Hier wordt wanneer op update gedrukt wordt, geswitcht van de invoer naar de uitkomsten
  observeEvent(input$gas_update,{
    if(input$tabs == "gegevensinvoer")
      {
      newtab <- switch(input$tabs, "gegevensinvoer" = "uitkomsten","uitkomsten" = "gegevensinvoer")
      updateTabItems(session, "tabs", newtab)
      }
    })

  
########### VOOR DE INVOER VAN TYPE METASTASEN, een voor een #############
  observeEvent(input$eso_only_lymf_meta, {
    state = ifelse(input$eso_only_lymf_meta=="Nee","show","hide")
    runjs(paste0("$('.collapse.lp').collapse('",state,"')"))
  })
###Bij het geven van input bij peri metastasen, wordt de invoer van aantal metastasen en 'voer gegevens in' knop beschikbaar
  observeEvent(input[['eso_nmeta']], {
    if(!is.null(input$eso_nmeta))
      showElement(id="eso_update")
  })

  observeEvent(input[['gas_only_lymf_meta']],{
    if(input$gas_only_lymf_meta == "Ja"){
      hideElement(id="gas_liver_meta")##Geen lever metastasen mogelijk wanneer alleen lymfeklier metastasen
      showElement(id="gas_nmeta_cat")##Alleen lymfeklier metastasen, dus aantal metastasen en invoer knop worden beschikbaar
      showElement(id="gas_update")
    }
    else if (input$gas_only_lymf_meta == "Nee"){###Wanneer niet alleen lymfeklier metastasen
      showElement(id="gas_liver_meta")##input voor levermetastasen wordt beschikbaar
      hideElement(id="gas_nmeta_cat")
      hideElement(id="gas_update")
    }})

  observeEvent(input[['gas_liver_meta']],{
    if(input$gas_liver_meta =="Ja" |input$gas_liver_meta =="Nee" ){
      showElement(id="gas_nmeta_cat")
      showElement(id="gas_update")
    }
    })
  

  observeEvent(c(input[["eso_sCN"]],input[["eso_sCTs"]],input[["eso_tDiffgr"]]),{
    if(is.null(input$eso_sCTs) || is.null(input$eso_sCN) || is.null(input$eso_tDiffgr))
    {
      disable("eso_pager__page-next-button")
    }
    else
    {
      enable("eso_pager__page-next-button") 
    }
  })
  
  observeEvent(input[["eso_tTopog_name"]],{
    if(is.null(input$eso_tTopog_name))
    {
      disable("eso_pager__page-next-button") 
    }
    else
    {
      enable("eso_pager__page-next-button") 
    }
  })
  
  observeEvent(input[['eso_pager']],{##Observes de paginateller onderaan de invoer van de gegevens bij slokdarmkanker.
    observeEvent(input[['gas_pager']],{##Observes de paginateller onderaan de invoer van de gegevens bij maagkanker.
      observeEvent(input$eso_or_gas,{##Kijkt of het gaat om slokdarm of maag.
        if(input$eso_or_gas == "Slokdarm")
          { keuze_eso_maag = "eso"
            pager_pos = input$eso_pager}##De teller positie wordt opgeslagen
        else {keuze_eso_maag = "gas"
            pager_pos = input$gas_pager}
    if(pager_pos$page_current==1){####Pagina 1 van invoer wordt weergeven
      showElement(id=paste0(keuze_eso_maag,"_page1"))
      hideElement(id=paste0(keuze_eso_maag,"_page2"))
      hideElement(id=paste0(keuze_eso_maag,"_page3"))
      hideElement(id=paste0(keuze_eso_maag,"_page4"))
      hideElement(id=paste0(keuze_eso_maag,"_update"))
    }
    else if(pager_pos$page_current==2){####Pagina 2 van invoer wordt weergeven
      hideElement(id=paste0(keuze_eso_maag,"_page1"))
      showElement(id=paste0(keuze_eso_maag,"_page2"))
      hideElement(id=paste0(keuze_eso_maag,"_page3"))
      hideElement(id=paste0(keuze_eso_maag,"_page4"))
      hideElement(id=paste0(keuze_eso_maag,"_update"))
      if(is.null(input$eso_sCTs) || is.null(input$eso_sCN) || is.null(input$eso_tDiffgr))
      {
        #updatePageruiInput(session, "eso_pager",page_current = 2)
        disable("eso_pager__page-next-button") 
      }
    }
    else if(pager_pos$page_current==3){####Pagina 3 van invoer wordt weergeven
      hideElement(id=paste0(keuze_eso_maag,"_page1"))
      hideElement(id=paste0(keuze_eso_maag,"_page2"))
      showElement(id=paste0(keuze_eso_maag,"_page3"))
      hideElement(id=paste0(keuze_eso_maag,"_page4"))
      hideElement(id=paste0(keuze_eso_maag,"_update"))
      if(is.null(input$eso_tTopog_name))
      {
        #updatePageruiInput(session, "eso_pager",page_current = 3)
        disable("eso_pager__page-next-button") 
      }
    }
    else if(pager_pos$page_current==4){####Pagina 4 van invoer wordt weergeven
      hideElement(id=paste0(keuze_eso_maag,"_page1"))
      hideElement(id=paste0(keuze_eso_maag,"_page2"))
      hideElement(id=paste0(keuze_eso_maag,"_page3"))
      showElement(id=paste0(keuze_eso_maag,"_page4"))
      #hideElement(id=paste0(keuze_eso_maag,"_update"))
      }
    })
  })
})
 }

# Run app ----
shinyApp(ui, server)
