settings_surv = tags$div(class="sidebar",id="surv_plot_options",###Kolom voor de verschillende opties aan de rechterzijde van het scherm
  tags$div(class="border-div",
    tags$p(class="align_right",
      h4(dutch$UI$display),
      dual_switch(inputId = "set_surv_dual-switch", "male","line-chart",c("surv_picto_button"=dutch$UI$settings$surv$pic,
                                                                     "surv_line_button"=dutch$UI$settings$surv$km)),
      
      tags$h4(dutch$UI$settings$treats),
      tags$button(class="action-button bttn bttn-material-flat bttn-lg bttn-success bttn-no-outline",
        id="set_surv_treat_collapse", type="button", "data-toggle"="collapse",
        "data-target"="#set_surv_treatment_container", icon("medkit","fa-fw")),
      tags$div(id="set_surv_treatment_container", class="collapse in",
        prettyCheckboxGroup(inputId = "set_surv_first_line.oes", label=NULL, choiceNames = unname(dutch$SURV$fl$OES), choiceValues=names(dutch$SURV$fl$OES), 
                            status="success", animation = "smooth", bigger=TRUE, icon=icon("check"), plain=TRUE),
        hidden(prettyCheckboxGroup(inputId = "set_surv_first_line.gas", label=NULL, choiceNames = unname(dutch$SURV$fl$GAS), choiceValues=names(dutch$SURV$fl$GAS), 
                                   status="success", animation = "smooth", bigger=TRUE, icon=icon("check"), plain=TRUE)),
        #hidden(prettyCheckboxGroup(inputId="set_chemo", label=NULL, choices=chemo_behandelingen, selected="none", status = "success",
        #                           animation="smooth",bigger=TRUE, icon=icon("check"), plain=TRUE)),
        actionBttn("set_surv_clear_treatment", dutch$UI$settings$clear_sel, icon=icon("times"), 
                   style="simple", color="warning",size="xs")
      ),
      
      tags$h4(dutch$UI$settings$d_settings),
      tags$button(class="action-button bttn bttn-material-flat bttn-lg bttn-success bttn-no-outline",
                  id="set_surv_settings_collapse", type="button", "data-toggle"="collapse",
                  "data-target"="#set_surv_settings_container", icon("gears","fa-fw")),
      tags$div(id="set_surv_settings_container", class="collapse",
               hidden(checkboxGroupButtons(inputId = "set_percentile",label = dutch$UI$settings$percentile,choiceNames = unname(dutch$UI$settings$perc_labs),
                                           choiceValues = names(dutch$UI$settings$perc_labs),
                                           checkIcon=list(yes=icon("check")), status="warning", size = "sm")),
               hidden(materialSwitch(inputId = "set_surv_conf",label = "95% CI",status="warning", right=TRUE)),
               sliderInput(inputId="set_surv_tmax",label=dutch$UI$settings$tmax,value=12,min=0,max=CONST$surv_tmax)
      )
    )
  )
)

settings_hrqol = tags$div(class="sidebar",id="plot_options",###Kolom voor de verschillende opties aan de rechterzijde van het scherm
  tags$div(class="border-div",
    tags$p(class="align_right",
     tags$h4(dutch$UI$display),
     dual_switch(inputId = "set_hrqol_dual-switch", "line-chart", "bar-chart",c("hrqol_line_button"=dutch$UI$settings$hrqol$line,
                                                                               "hrqol_bar_button"=dutch$UI$settings$hrqol$bar)),

     tags$h4(dutch$UI$settings$treats),
     tags$button(class="action-button bttn bttn-material-flat bttn-lg bttn-success bttn-no-outline",
                 id="set_surv_treat_collapse", type="button", "data-toggle"="collapse",
                 "data-target"="#set_treatment_container", icon("medkit","fa-fw")),
     tags$div(id="set_treatment_container", class="collapse in",
              prettyCheckboxGroup(inputId = "set_hrqol_first_line.oes", label=NULL, choiceNames = unname(dutch$SURV$fl$OES), choiceValues=names(dutch$SURV$fl$OES),
                                  status="success", animation = "smooth", bigger=TRUE, icon=icon("check"), plain=TRUE),
              hidden(prettyCheckboxGroup(inputId = "set_hrqol_first_line.gas", label=NULL, choiceNames = unname(dutch$SURV$fl$GAS), choiceValues=names(dutch$SURV$fl$GAS),
                                         status="success", animation = "smooth", bigger=TRUE, icon=icon("check"), plain=TRUE)),
              actionBttn("set_hrqol_clear_treatment", dutch$UI$settings$clear_sel, icon=icon("times"), style="simple", color="warning",size="xs")
     ),

     tags$h4(dutch$UI$settings$d_settings),
     tags$button(class="action-button bttn bttn-material-flat bttn-lg bttn-success bttn-no-outline",
                 id="set_hrqol_settings_collapse", type="button", "data-toggle"="collapse",
                 "data-target"="#set_hrqol_settings_container", icon("gears","fa-fw")),
     tags$div(id="set_hrqol_settings_container", class="collapse",
              materialSwitch(inputId = "set_hrqol_conf",label = "95% CI",status="warning", right=TRUE),
              materialSwitch(inputId = "set_hrqol_norm",label = dutch$UI$settings$qol_norm,status="warning", right=TRUE),
              sliderInput(inputId="set_hrqol_tmax",label=dutch$UI$settings$tmax,value=CONST$hrqol_tmax,min=0,max=CONST$hrqol_tmax,step = 3)
     )
    )
  )
)
# 
# # dropdown(   
# #   style="material-flat", status="success", size="lg","icon"=icon("gears", class = "fa-fw"), right=TRUE, animate = TRUE,
# #         inputId="set_menu_diagram_options"
# # )