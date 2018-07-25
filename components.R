button_tooltip = function(base, inputId, title) {
  base %<>% str_replace(paste0("(?s)(<button )((?:(?!</button>).)*?value=\"",inputId,"\".*?</button>)"), 
                        paste0("\\1data-toggle=\"tooltip\" data-trigger=\"hover\" data-placement=\"auto top\" title=\"\" ",
                               "data-original-title=\"",title,"\" \\2"))
}

collapse = function(classname, ...) {
  tags$div(class=paste("collapse", classname), style="padding: 0px; box-shadow: 0 0 transparent;", ...)
}

number_field = function(id, label, placeholder, min, max) {
  div(
  tags$label(class="control-label", "for"=id, label),
  tags$input(type="number", class="form-control", id=id, placeholder=placeholder, 
             min=min, max=max, style="width: 20%"))
}

radio = function(inputId, label, choiceNames, choiceValues=choiceNames,
                 tooltips=list(), size=c("normal","xs","sm","lg"), selected="none",
                 status=c("success","info","primary","warning","danger")) {
  # inputId="keuze_hulp"
  # label="Differentiatiegraad"
  # choiceNames=c("G1","G2","G3","G4")
  # choiceValues=choiceNames
  # tooltips = list(G1="Goed gedifferentieerd",G4="Ongedifferentieerd")
  # size="normal"
  # status="success"
  # cl = "input-element"
  # selected="none"
  
  base = radioGroupButtons(inputId, label, choiceNames = choiceNames, choiceValues = choiceValues,
           checkIcon=list(yes=icon("check")), selected = selected,
           #status=status,size=size) %>% as.character
           status=match.arg(status), size=match.arg(size)) %>% as.character
  for (id in names(tooltips)) {
    base %<>% button_tooltip(id, tooltips[id])
  }

  HTML(base)
}

dual_switch = function(inputId, icon1, icon2, tooltip) {
  paste0('<div id=\"id',inputId,'\" class="form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline">
  <div id=\"',inputId,'\" class="radioGroupButtons" style="margin-top: 3px; margin-bottom: 3px; ">
  <div aria-label="..." class="btn-group btn-group-container-sw" data-toggle="buttons" role="group">
  <div class="btn-group" role="group">
  <button class="btn radiobtn action-button bttn bttn-material-flat bttn-lg bttn-no-outline btn-warning active dual">
  <input type="radio" autocomplete="off" name=\"',inputId,'\" id="',names(tooltip)[1],'" value="',names(tooltip)[1],'" 
    checked="checked"><i class="fa fa-',icon1,' fa-fw"></i></input>
  </button></div>
  <div class="btn-group" role="group">
  <button class="btn radiobtn action-button bttn bttn-material-flat bttn-lg bttn-no-outline btn-warning dual">
  <input type="radio" autocomplete="off" name=\"',inputId,'\" id="',names(tooltip)[2],'" value="',names(tooltip)[2],'">
  <i class="fa fa-',icon2,' fa-fw"></i></input>
  </button></div></div></div></div>') %>% button_tooltip(names(tooltip)[1],tooltip[1]) %>% 
    button_tooltip(names(tooltip[2]),tooltip[2]) %>% HTML
}
