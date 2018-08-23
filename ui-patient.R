ui_patient_entry = fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"), tags$script(src="script.js")),
  theme = shinytheme("flatly"),
  
  
  tags$div(class="col-sm-12 col-md-10 col-md-offset-1 col-lg-6 col-lg-offset-3",
           tags$h3(dutch$UI$patient_info),
           tags$div(class="form-container jumbotron",
                    tags$br(),
                    
                    progressBar(id="progress-bar", value=0, status="warning"),
                    
                    #PAGE 1
                    radio(inputId = "input.1__oes_or_gas", label=dutch$LOC, selected=ifelse(SKIP,"Oesophagus","none"),
                          choiceNames=uu(dutch$LOCS), choiceValues = names(dutch$LOCS), tooltips = dutch$TTIPS$location_simple),
                    radio(inputId = "input.1__pGesl", label=dutch$SURV$PREDS$pGesl, selected=ifelse(SKIP,"Male","none"),
                          choiceNames=uu(dutch$SURV$sex), choiceValues = names(dutch$SURV$sex)),
                    number_field("input.1__nLeeft",label=dutch$SURV$PREDS$nLeeft, placeholder=paste(RANGE$nLeeft,collapse = " - "), value=ifelse(SKIP,"55",""),
                                 min=RANGE$nLeeft[1], max=RANGE$nLeeft[2], invalid_text=dutch$ERR$invalid_range),
                    
                    #PAGE 2
                    hidden(radio(inputId="input.2__sCTs", label=dutch$SURV$PREDS$sCTs, choiceNames=surv_oes$Design$values$sCTs, tooltips=dutch$TTIPS$scTS, selected = ifelse(SKIP,"1","none"))),
                    hidden(radio(inputId="input.2__sCN", label=dutch$SURV$PREDS$sCN, choiceNames=surv_oes$Design$values$sCN, tooltips=dutch$TTIPS$sCN, selected = ifelse(SKIP,"1","none"))),
                    hidden(radio(inputId="input.2__tDiffgr", label=dutch$SURV$PREDS$tDiffgr, choiceNames=surv_oes$Design$values$tDiffgr, tooltips=dutch$TTIPS$grade, selected = ifelse(SKIP,"G1","none"))),
                    
                    hidden(prettyRadioButtons(inputId = "input.2__tTopog_name.oes", label=dutch$SURV$PREDS$tTopog_name, choiceNames=uu(dutch$SURV$topo$OES),
                                              choiceValues=names(dutch$SURV$topo$OES), status="success", width="100%", animation = "smooth", bigger=TRUE,
                                              icon=icon("check"), plain=TRUE, selected=ifelse(SKIP,"Junction","none"))),
                    hidden(prettyRadioButtons(inputId = "input.2__tMorfology_simple.oes", label=dutch$SURV$PREDS$tMorfology_simple, choiceNames=uu(dutch$SURV$hist),
                                              choiceValues=names(dutch$SURV$hist), status="success", width="100%", animation = "smooth", bigger=TRUE,
                                              icon=icon("check"), plain=TRUE, selected=ifelse(SKIP,"Adenocarcinoma","none"))),
                    
                    #PAGE 3
                    number_field("input.3__nmeta",label=dutch$SURV$PREDS$nmeta, placeholder=paste(RANGE$nmeta,collapse = " - "),
                                 min=RANGE$nmeta[1], max=RANGE$nmeta[2], invalid_text=dutch$ERR$invalid_range, value = ifelse(SKIP,"1","")),
                    
                    hidden(radio(inputId="input.3__only_lymf_meta", label=dutch$SURV$PREDS$only_lymf_meta, choiceNames=uu(dutch$SURV$BIN),
                                 choiceValues=names(dutch$SURV$BIN), tooltips=dutch$TTIPS$only_lymf, selected = ifelse(SKIP,"Yes","none"))),
                    hidden(radio(inputId="input.3__has_c770.oes", label=dutch$SURV$PREDS$has_c770, choiceNames=uu(dutch$SURV$BIN),  selected = ifelse(SKIP,"No","none"),
                                 choiceValues=names(dutch$SURV$BIN))),
                    hidden(radio(inputId="input.3__has_c771", label=dutch$SURV$PREDS$has_c771, choiceNames=uu(dutch$SURV$BIN), selected = ifelse(SKIP,"No","none"),
                                 choiceValues=names(dutch$SURV$BIN))),
                    hidden(radio(inputId="input.3__has_c772", label=dutch$SURV$PREDS$has_c772, choiceNames=uu(dutch$SURV$BIN), selected = ifelse(SKIP,"No","none"),
                                 choiceValues=names(dutch$SURV$BIN))),
                    collapse(classname = "lp", inputId="lp_collapse",collapsed = TRUE,
                             radio(inputId="input.3__liver_meta", label=dutch$SURV$PREDS$liver_meta, choiceNames=uu(dutch$SURV$BIN),
                                   choiceValues=names(dutch$SURV$BIN), selected = names(dutch$SURV$BIN)[1]),
                             radio(inputId="input.3__peri_meta.oes", label=dutch$SURV$PREDS$peri_meta, choiceNames=uu(dutch$SURV$BIN),
                                   choiceValues=names(dutch$SURV$BIN), selected = names(dutch$SURV$BIN)[1])
                    ),
                    
                    collapse(classname = "number_meta", inputId="nm_collapse",collapsed = TRUE, error_msg(dutch$ERR$error, dutch$ERR$nmeta))
           ),
           
           tags$div(class="form-container",
              div(actionBttn(inputId = "submit_data",label=dutch$UI$submit, style = "fill", color="warning", size="lg"), style="float:right"),
              navigation_buttons()
           )
  ))
