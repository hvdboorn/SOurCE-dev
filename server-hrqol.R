draw_hrqol_line = function(settings) {
  hrqol = predict_hrqol()
  draw_nothing = length(hrqol) == 0
  treatments = names(hrqol)
  colors = as.list(CONST$colors[seq(length.out=length(treatments))]) %>% set_names(treatments)
  n_months = input[[settings$tmax]]
  norm_score = input[[settings$norm]]
  t_max = n_months*CONST$month
  
  y_labels = c(ifelse(norm_score,dutch$PLOT$hrqol_norm,NA), seq(0,100,by=10))
  y_breaks = c(ifelse(norm_score,CONST$hrqol_mean,NA), seq(0,100,by=10))
  if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}
  
  df_hrqol <<- if(!draw_nothing) {do.call(rbind,hrqol)} else {data.frame(time=numeric(0),upper=numeric(0),lower=numeric(0),label=character(0),hrqol=numeric(0))}
  
  
  normal = ggplot(data=df_hrqol, aes(x=time, y=hrqol,group=label, color=label,
                                     text = paste0(label,": ",hrqol))) + geom_line(size=1)+geom_point(size=3)
  empty = ggplot(data=df_hrqol, aes(x=time, y=hrqol,group=label, color=label,
                                    text = character(0))) + geom_blank()
  
  p = if(!draw_nothing) normal else empty
  p = p + source_theme + theme(plot.margin=unit(c(2,1,1,1),"cm")) +
    ggtitle(to_title(dutch$UI$hrqol_title))+
    scale_color_manual(name="",values=unlist(colors)) +
    scale_x_continuous(name="", limits=c(0,t_max),breaks=sort(unique(df_hrqol$time)),labels=function(X) str_wrap(dutch$PLOT$x_hrqol(X),20))+
    scale_y_continuous(name="", limits = c(0,100), labels = y_labels, breaks = y_breaks)
  if(norm_score) {
    
    p=p+geom_hline(aes(yintercept=CONST$hrqol_mean), color="black", linetype="dashed",size=1)
  }
  if(input[[settings$conf]] & !draw_nothing){
    p=p+geom_ribbon(mapping=aes(ymax=upper, ymin=lower, fill=label), show.legend=FALSE, alpha=0.2, position = "identity")+guides(fill=FALSE)
  }
  
  ggp = ggplotly(p, tooltip=c("text"))%>%
    config(p = ., collaborate = F, displayModeBar = F,autosizable=F,scrollZoom=F,showAxisRangeEntryBoxes=F,showAxisDragHandles=F,
           displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(plotly_rem))%>%
    layout(legend = list(orientation='h', y=55),hovermode="x", hoverlabel=list(font=list(size=CONST$size_legend)))
  
  name_regex = "\\((.*?),\\d+\\)"
  for(i in 1:length(ggp$x$data)) {
    if("fill" %in% names(ggp$x$data[[i]])) {
      ggp$x$data[[i]]$showlegend = FALSE
      ggp$x$data[[i]]$hoverinfo= NULL
    } else {
      ggp$x$data[[i]]$name %<>% {if(str_detect(., name_regex)) {str_match(.,name_regex)[-1]} else {.}}
    }
  }
  
  ggp %<>% apply_plotly_settings
}

output$out_hrqol__line = renderPlotly({
  draw_hrqol_line(ids_hrqol)
})


# Bar Plot ----------------------------------------------------------------
draw_hrqol_bar = function(settings) {
  hrqol = predict_hrqol()
  draw_nothing = length(hrqol) == 0
  treatments = names(hrqol)
  colors = as.list(CONST$colors[seq(length.out=length(treatments))]) %>% set_names(treatments)
  n_months = input[[settings$tmax]]
  norm_score = input[[settings$norm]]
  t_max = n_months*CONST$month
  
  y_labels = c(ifelse(norm_score,dutch$PLOT$hrqol_norm,NA), seq(0,100,by=10))
  y_breaks = c(ifelse(norm_score,CONST$hrqol_mean,NA), seq(0,100,by=10))
  if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}
  
  df_hrqol = if(!draw_nothing) {do.call(rbind,hrqol)} else {data.frame(time=numeric(0),upper=numeric(0),lower=numeric(0),label=character(0),hrqol=numeric(0))}
  df_hrqol = df_hrqol[df_hrqol$time == t_max,]
  moment = if (n_months == 6) paste("een half jaar") else paste(n_months, "maanden")###Tijd
  
  normal = ggplot(data=df_hrqol, aes(x=label, y=hrqol,fill=label)) + geom_col(position = position_dodge(width=0.1))
  empty = ggplot(data=df_hrqol, aes(x=label, y=hrqol,fill=label)) + geom_blank()
  p = if(draw_nothing) empty else normal
  p = p + source_theme + theme(
                plot.margin=unit(c(2,1,1,1),"cm")) +
    ggtitle(to_title(sprintf("%s na %s",dutch$UI$hrqol, moment)))+
    scale_x_discrete(name=NULL,labels = function(x) str_wrap(x, width =12))+
    scale_fill_manual(name=NULL, values = unlist(colors))+
    scale_y_continuous(name = NULL, limits = c(0,100), labels = y_labels, breaks = y_breaks)
  
  if(input[[settings$conf]] & !draw_nothing) {
    p = p + geom_errorbar(aes(ymin =hrqol*0.95, ymax = hrqol*1.05),width=0.15,position = "dodge")
  }
  
  if(norm_score) {
    p = p + geom_hline(aes(yintercept=CONST$hrqol_mean), color="black", linetype="dashed",size=1)
  }
  
  gggp<<-p
  p
}

output$out_hrqol__bar = renderPlot({
  draw_hrqol_bar(ids_hrqol)
})