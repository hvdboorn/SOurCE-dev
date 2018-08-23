source("util/html2R.R")

button_tooltip = function(base, inputId, title) {
  base %<>% str_replace(paste0("(?s)(<button )((?:(?!</button>).)*?value=\"",inputId,"\".*?</button>)"), 
                        paste0("\\1data-toggle=\"tooltip\" data-trigger=\"hover\" data-container=\"body\" data-placement=\"auto top\" title=\"\" ",
                               "data-original-title=\"",title,"\" \\2"))
}

collapse = function(classname, inputId, collapsed=TRUE, ...) {
  tags$div(class=paste0("collapse", ifelse(collapsed," "," in "), classname), id=inputId, style="padding: 0px; box-shadow: 0 0 transparent;", ...)
}

error_msg = function(title, message) {
  tags$div(class="alert alert-danger", tags$h4(class="alert-heading", title), message)
}

password_field = function(id, label, placeholder, invalid_text) {
  div(class="form-group",
      tags$label(class="form-control-label", "for"=id, label),
      tags$input(type="password", class="form-control", id=id, placeholder=placeholder),
      tags$div(style="height: 20px", hidden(tags$small(id=paste0(id,".invalid"), class="form-text invalid-text",invalid_text))))
}

number_field = function(id, label, value="", placeholder, min, max, invalid_text) {
  div(class="form-group", id=paste0(id,"_total"),
  tags$label(class="form-control-label", "for"=id, label),
  tags$input(type="number", class="form-control", id=id, placeholder=placeholder, value=value,
             min=min, max=max, style="width: 40%"),
  tags$div(style="height: 20px", hidden(tags$small(id=paste0(id,".invalid"), class="form-text invalid-text",invalid_text(min,max)))))
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

  eval(parse(text=html2R(base,prefix = TRUE)))
}

logoBttn = function(inputId, label, tooltip, url) {
  oc = HTML("window.open('",url,"','_blank')")
  actionBttn(inputId = inputId, label = label, style = "material-circle", size="sm") %>% as.character %>%
    str_replace_all("(button class=\".*?)(\")","\\1 logo-button\\2") %>%
    
    str_replace(paste0("(?s)(<button )((?:(?!</button>).)*?id=\"",inputId,"\".*?</button>)"), 
                paste0("\\1data-toggle=\"tooltip\" data-trigger=\"hover\" data-container=\"body\" data-placement=\"auto top\" title=\"\" ",
                       "data-original-title=\"",tooltip,"\" onclick=\"",oc,"\" \\2")) %>%
    
    html2R(prefix = TRUE) %>% parse(text=.) %>% eval
}

navigation_buttons = function() {
  prev = '<button class="action-button bttn bttn-fill bttn-lg bttn-warning bttn-no-outline nav_button" id="prev" type="button" disabled>
          <i class="fa fa-arrow-left fa-fw"></i></button>'
  nxt = '<button class="action-button bttn bttn-fill bttn-lg bttn-warning bttn-no-outline nav_button" id="next" type="button" disabled>
          <i class="fa fa-arrow-right fa-fw"></i></button>'
  
  eval(parse(text=html2R(paste0("<div class='nav_buttons'>", 
                                prev, nxt, "</div>"),prefix = TRUE)))
}

dual_switch = function(inputId, icon1, icon2, tooltip=c(NA,NA)) {
  #browser()
  base = paste0('<div id=\"id-',inputId,'\" class="form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline">
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
  </button></div></div></div></div>')
  
  if(!is.na(tooltip[1]) & !is.na(tooltip[2])) {
    base %<>% button_tooltip(names(tooltip)[1],tooltip[1]) %>% 
    button_tooltip(names(tooltip[2]),tooltip[2])
  }
  
  eval(parse(text=html2R(base,prefix = TRUE)))
}
