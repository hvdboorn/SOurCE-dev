output$out_hrqol__line = renderPlotly({
  hrqol = predict_hrqol()
  draw_nothing = length(hrqol) == 0
  treatments = names(hrqol)
  colors = as.list(CONST$colors[seq(length.out=length(treatments))]) %>% set_names(treatments)
  n_months = input$set_hrqol_tmax
  norm_score = input$set_hrqol_norm
  t_max = n_months*CONST$month
  
  y_labels = c(ifelse(norm_score,dutch$PLOT$hrqol_norm,NA), seq(0,100,by=10))
  y_breaks = c(ifelse(norm_score,CONST$hrqol_mean,NA), seq(0,100,by=10))
  if(norm_score) {y_labels = setdiff(y_labels,"70"); y_breaks = setdiff(y_breaks, 70)}
  
  df_hrqol = if(!draw_nothing) {do.call(rbind,hrqol)} else {data.frame(time=numeric(0),upper=numeric(0),lower=numeric(0),label=character(0),hrqol=numeric(0))}
  
  
  normal = ggplot(data=df_hrqol, aes(x=time, y=hrqol,group=label, color=label,
                        text = paste0(label,": ",hrqol))) + geom_line(size=1)+geom_point(size=3)
  empty = ggplot(data=df_hrqol, aes(x=time, y=hrqol,group=label, color=label,
                        text = character(0))) + geom_blank()
  
  plot = if(!draw_nothing) normal else empty
  plot = plot + theme(panel.border = element_rect(fill=NA,colour="black",size=1),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                      plot.title = element_text(size=CONST$size_title),
                      axis.title = element_text(size=CONST$size_title),
                      text = element_text(size=CONST$size_text),
                      legend.justification = c(0.1),
                      legend.position = "top",
                      legend.text=element_text(size=CONST$size_legend),
                      plot.margin=unit(c(2,1,1,1),"cm")) +
    ggtitle(dutch$UI$hrqol_title)+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    scale_color_manual(name="",values=unlist(colors)) +
    scale_x_continuous(name=dutch$PLOT$time_hrqol, limits=c(0,t_max),breaks=sort(unique(df_hrqol$time)),labels=dutch$PLOT$x_hrqol)+
    scale_y_continuous(name="", limits = c(0,100), labels = y_labels, breaks = y_breaks)+
    {if(norm_score) geom_hline(aes(yintercept=CONST$hrqol_mean), color="black", linetype="dashed",size=1)}+
    {if(input$set_hrqol_conf & !draw_nothing) geom_ribbon(data=df_hrqol, aes(x=time, ymax=upper, ymin=lower, group =label, fill=label, color=label),
                                      alpha=0.2,position = "identity",inherit.aes = F)}
    
  ggplotly(plot, tooltip=c("text"))%>%
    config(p = ., collaborate = F, displayModeBar = F,autosizable=F,scrollZoom=F,showAxisRangeEntryBoxes=F,showAxisDragHandles=F,
           displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(plotly_rem))%>%
    layout(legend = list(orientation='h', y=55),hovermode="x", hoverlabel=list(font=list(size=CONST$size_legend)))
})