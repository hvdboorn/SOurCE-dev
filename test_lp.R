lp = readRDS("data/lp.rds")
predict_survival = reactive({
  selected_trt = c("none","stent")#input$first_line_treats
  fit = model()
  lin_pred = lp()
  
  surv = stfu(lapply(lin_pred[names(lin_pred) %in% selected_trt], function(x) survest(fit, linear.predictors = x) %>% 
                  {.[c("time","surv","lower","upper")]} %>% data.frame %>% {.[complete.cases(.),]}))
  
  return(surv)
})