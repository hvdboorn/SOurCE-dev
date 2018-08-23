lp = eventReactive(input$submit_data, {
  if(input$submit_data==0) #for first execution
    return(NULL)
  
  needed_vars = fit()$Design$name[fit()$Design$assume!="interaction"]
  corresponding = sapply(needed_vars, function(x) check_ids %>% Filter(filter_location, .) %>% str_subset(x))
  values = lapply(corresponding, function(x) if(length(x) > 0) {input[[x]]} else {NA})
  values$first_line = setdiff(fit()$Design$values$first_line, "other")
  
  PT <<- gendata(fit(), factors=values)
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

predict_tox = reactive({
  tox = toxicity_data[input$set_tox_first_line]
  if(input$set_tox_display) { #TOP 3
    tox_names = tox %>% lapply(. %>% group_by(tox) %>% summarise(total=sum(prop)) %>% arrange(desc(total)) %>% slice(1:3) %>% .$tox) %>%
      unlist %>% unique
    tox = lapply(tox, function(x) x[x$tox %in% tox_names,])
  }
  tox = lapply(tox, function(t) {levels(t$tox) = unlist(dutch$TOX$toxnames); droplevels(t, except=which(names(tox) == "grade"))})
  return(tox)
})