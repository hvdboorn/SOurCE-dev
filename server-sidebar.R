selected_surv = "surv_picto_button"
selected_hrqol = "hrqol_line_button"
current_tab = NULL

removeClass(class = "bttn-primary", selector = "button[id*='collapse']")
removeClass(class = "bttn-success", selector = "button[id*='collapse']")
addClass(class = "btn btn-warning", selector = "button[id*='collapse']")

onclick("id-set_surv_dual-switch", {
  selected_surv = setdiff(c("surv_picto_button","surv_line_button"), selected_surv)
  updateRadioGroupButtons(session, "set_surv_dual-switch", selected = selected_surv)
  runjs("document.activeElement.blur();")
  
  toggle("out_surv__pictograph", condition=selected_surv=="surv_picto_button")
  toggle("out_surv__km", condition=selected_surv=="surv_line_button")
  
  
  toggle("set_percentile", condition=selected_surv=="surv_line_button")
  toggle("set_surv_conf", condition=selected_surv=="surv_line_button")
  toggle("set_surv_tmax", condition=selected_surv %in% c("surv_line_button", "surv_picto_button"))
})

onclick("id-set_hrqol_dual-switch", {
  selected_hrqol = setdiff(c("hrqol_line_button","hrqol_bar_button"), selected_hrqol)
  updateRadioGroupButtons(session, "set_hrqol_dual-switch", selected = selected_hrqol)
  runjs("document.activeElement.blur();")
  
  toggle("out_hrqol__line", condition=selected_hrqol=="hrqol_line_button")
  toggle("out_hrqol__bar", condition=selected_hrqol=="hrqol_bar_button")
  
  toggle("set_hrqol_conf", condition=selected_hrqol %in% c("hrqol_line_button", "hrqol_bar_button"))
  toggle("set_hrqol_tmax", condition=selected_hrqol %in% c("hrqol_line_button", "hrqol_bar_button"))
})

observeEvent(location(), {
  toggle("set_surv_first_line.oes", condition=location()=="Oesophagus")
  toggle("set_surv_first_line.gas", condition=location()=="Stomach")
  toggle("set_hrqol_first_line.oes", condition=location()=="Oesophagus")
  toggle("set_hrqol_first_line.gas", condition=location()=="Stomach")
})

observeEvent(input$tabs, {
  prev_tab = current_tab
  current_tab <<- input$tabs
  
  if(!is.null(prev_tab)) {
    if(prev_tab == "tab.survival") {
      updatePrettyCheckboxGroup(session, "set_hrqol_first_line.oes", selected = input$set_surv_first_line.oes)
      updatePrettyCheckboxGroup(session, "set_hrqol_first_line.gas", selected = input$set_surv_first_line.gas)
    } else if (prev_tab == "tab.hrqol") {
      updatePrettyCheckboxGroup(session, "set_surv_first_line.oes", selected = input$set_hrqol_first_line.oes)
      updatePrettyCheckboxGroup(session, "set_surv_first_line.gas", selected = input$set_hrqol_first_line.gas)
    }
  }
})

observeEvent(c(input$set_surv_clear_treatment, input$set_hrqol_clear_treatment), {
  updatePrettyCheckboxGroup(session, "set_surv_first_line.oes", selected = "")
  updatePrettyCheckboxGroup(session, "set_surv_first_line.gas", selected = "")
  updatePrettyCheckboxGroup(session, "set_hrqol_first_line.oes", selected = "")
  updatePrettyCheckboxGroup(session, "set_hrqol_first_line.gas", selected = "")
})

observeEvent(c(input$set_tox_clear_treatment), {
  updatePrettyCheckboxGroup(session, "set_tox_first_line", selected = "")
})