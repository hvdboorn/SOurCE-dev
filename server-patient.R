# Reactive values ---------------------------------------------------------

location = reactive(input$input.1__oes_or_gas)
fit = reactive(if(location() == "Oesophagus") {surv_oes} else {surv_gas})
fl_treats = reactive(fit()$Design$values$first_line)
selected_treatments_surv = eventReactive(c(input$set_surv_first_line.oes, input$set_surv_first_line.gas), {
  if(location() == "Oesophagus") input$set_surv_first_line.oes else input$set_surv_first_line.gas
}, ignoreNULL = FALSE)

selected_treatments_hrqol = eventReactive(c(input$set_hrqol_first_line.oes,input$set_hrqol_first_line.gas), {
  if(location() == "Oesophagus") input$set_hrqol_first_line.oes else input$set_hrqol_first_line.gas
}, ignoreNULL = FALSE)

shinyjs::addClass("submit_data","warning pulse")
PAGE = reactiveVal(1)
input_ids = c("input.1__oes_or_gas", "input.1__pGesl", "input.1__nLeeft_total", "input.2__sCTs", "input.2__sCN", "input.2__tDiffgr", 
              "input.2__tTopog_name.oes", "input.2__tMorfology_simple.oes", "input.3__nmeta_total", 
              "input.3__only_lymf_meta", "input.3__has_c770.oes", "input.3__has_c771", "input.3__has_c772", 
              "input.3__liver_meta", "input.3__peri_meta.oes")
check_ids = str_replace_all(input_ids, "_total","")
page_number = function(x) str_match(x, "\\d") %>% as.numeric
is_valid_range = function(x, range) !is.null(x) && !is.na(x) && str_detect(x,"^\\d+$") && as.numeric(x) >= range[1] && as.numeric(x) <= range[2]
not_empty = function(x) !is.null(x) && !is.na(x) && x!=""

#filter on location; take out elements for gastric cancer if oesophageal is selected and vice versa
filter_location = function(x) ifelse(not_empty(input$input.1__oes_or_gas), 
                              !str_detect(x, ifelse(input$input.1__oes_or_gas=="Oesophagus","\\.gas","\\.oes")), TRUE)
observe({
  #apply location filter and remove metastatic location if there is only lymph node metastasis
  elements_needed = input_ids %>%
    setdiff(if(!not_empty(input$input.3__only_lymf_meta) || input$input.3__only_lymf_meta=="Yes")
      c("input.3__liver_meta", "input.3__peri_meta.oes") else NULL) %>% Filter(filter_location, .)
  
  is_filled_correctly = function(ids) {
    for (id in ids) {
      if(!not_empty(input[[str_replace_all(id,"_total","")]]))
        return(FALSE)
      
      #numeric field
      if(str_detect(id, "_total$")) {
        id_new = str_match(id, "input\\.\\d__([^\\.]*?)(?:\\..*)?_total")[-1]
        valid = is_valid_range(input[[str_replace_all(id,"_total","")]], RANGE[[id_new]])
        if(!valid)
          return(FALSE)
      }
    }
    TRUE
  }
  
  valid_elements = sapply(elements_needed, is_filled_correctly)
  
  runjs(paste0('document.getElementById("prev").disabled = ',tolower(as.character(PAGE()==1)),';'))
  runjs(paste0('document.getElementById("next").disabled = ',tolower(as.character(PAGE()==3 | 
                                                             !all(valid_elements[page_number(names(valid_elements))==PAGE()]))),';'))
  
  nmeta_error = if("input.3__nmeta_total" %in% names(valid_elements) && valid_elements[["input.3__nmeta_total"]]) {
    n_meta = input[["input.3__nmeta"]]
    locs = valid_elements[valid_elements] %>% names %>% str_subset("liver_meta|peri_meta|c77")
    n_locs = sapply(locs, function(x) input[[x]] %in% c("TRUE","Yes")) %>% unlist %>% sum

    state = ifelse(n_locs > n_meta,"show","hide")
    runjs(paste0("$('.collapse.number_meta').collapse('",state,"')"))
    n_locs > n_meta
  } else {
    FALSE
  }
  
  N = length(elements_needed)
  N_valid = sum(valid_elements)
  updateProgressBar(session, "progress-bar", (N_valid/N)*100)
  
  #don't toggle liver and peri metastasis fields, as they hide anyway
  lp_vars = c("input.3__liver_meta", "input.3__peri_meta.oes")
  for(id in setdiff(input_ids, lp_vars)) {
    if(id %in% elements_needed & page_number(id) == PAGE()) {
      showElement(id=id, anim=TRUE, animType="fade")
    } else {
      hideElement(id=id, anim=FALSE)
    }
  }
  toggleState(id="submit_data", condition = N_valid==N & !nmeta_error)

  toggleClass(id="lp_collapse", class="in", condition = any(lp_vars %in% elements_needed) & PAGE() == 3)
  
  for(id in lp_vars[str_detect(lp_vars,"\\.((gas)|(oes))$")]) {
    toggle(id=id, condition = if(!not_empty(input$input.1__oes_or_gas)) {
        TRUE
      } else if(input$input.1__oes_or_gas == "Oesophagus") {
        str_detect(id,"\\.oes")
      } else {
        str_detect(id,"\\.gas")
      })
  }
})

observeEvent(input[["next"]], {
  PAGE(PAGE()+1)
  
})
observeEvent(input[["prev"]], {
  PAGE(PAGE()-1)
})

#validate age/n_meta input
validate_input = function(id, range) {
  observeEvent(input[[id]], {
    if(is_valid_range(input[[id]], range)) {
      shinyjs::removeClass(id,"incorrect-input"); addClass(id,"correct-input");
      shinyjs::hide(paste0(id,".invalid"), anim=TRUE, animType="fade")
    } else {
      shinyjs::removeClass(id,"correct-input"); addClass(id,"incorrect-input");
      shinyjs::show(paste0(id,".invalid"), anim=TRUE, animType="fade")
    }
  }, ignoreInit = TRUE)
}
validate_input("input.1__nLeeft", RANGE$nLeeft)
validate_input("input.3__nmeta", RANGE$nmeta)

#hide liver and peritoneal metastasis fields if there is only lymf node metastasis
observeEvent(input$input.3__only_lymf_meta, {
  state = ifelse(input$input.3__only_lymf_meta=="No","show","hide")
  runjs(paste0("$('.collapse.lp').collapse('",state,"')"))
})

lp = eventReactive(input$submit_data, {
  if(input$submit_data==0) #for first execution
    return(NULL)

  needed_vars = fit()$Design$name[fit()$Design$assume!="interaction"]
  corresponding = sapply(needed_vars, function(x) check_ids %>% Filter(filter_location, .) %>% str_subset(x))
  values = lapply(corresponding, function(x) if(length(x) > 0) {input[[x]]} else {NA})
  values$first_line = setdiff(fit()$Design$values$first_line, "other")
  predict(fit(), gendata(fit(), factors=values)) %>% set_names(values$first_line)
}, ignoreNULL = FALSE)

predict_survival = reactive({
  if (is.null(lp()))
    return(NULL)
  n_months = input$set_surv_tmax
  tmax = n_months * CONST$month
  
  surv = stfu(lapply(lp()[names(lp()) %in% selected_treatments_surv()], function(x) survest(fit(), linear.predictors = x) %>% 
  {.[c("time","surv","lower","upper")]} %>% data.frame %>% {.[complete.cases(.) & .[,"time"]<=tmax,]}))
  
  for(i in names(surv)) {
    surv[[i]]$label = tr(i)
  }
  names(surv) = tr(names(surv))
  
  return(surv)
})

hrqol = eventReactive(input$submit_data, {
  treatments = setdiff(fit()$Design$values$first_line, "other")
  times = seq(0, CONST$hrqol_tmax, by=3)
  
  get_sample = function(mean=50, sd=20, n=5, min=0, max=100, times) {
    do.call(rbind, 
      lapply(times, function(t) {
      hrqol = mean(rnorm(n, mean, sd))
      df=data.frame(time=t*CONST$month, hrqol=hrqol,
                 lower = hrqol-qnorm(0.975)*sd/sqrt(n),
                 upper = hrqol+qnorm(0.975)*sd/sqrt(n))
      df[,c("hrqol","lower","upper")] = round(df[,c("hrqol","lower","upper")])
      df
      })
    )
  }
  
  lapply(treatments, function(t) get_sample(times = times)) %>% set_names(treatments)
})

predict_hrqol = reactive({
  if (is.null(hrqol()))
    return(NULL)
  n_months = input$set_hrqol_tmax
  tmax = n_months * CONST$month
  
  hrqol_scores = lapply(hrqol()[names(hrqol()) %in% selected_treatments_hrqol()], function(x) x[complete.cases(x) & x[,"time"]<=tmax,])
  
  for(i in names(hrqol_scores)) {
    hrqol_scores[[i]]$label = tr(i)
  }
  names(hrqol_scores) = tr(names(hrqol_scores))

  return(hrqol_scores)
})
