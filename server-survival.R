draw_pictograph = function(settings) {
  is_wide = session$clientData[["output_out_surv__pictograph_width"]] <= 690 #add factor
  survival = predict_survival()
  treatments = names(survival)
  n_months = input[[settings$tmax]]
  
  colors = as.list(c(CONST$colors[seq(length.out=length(treatments))], CONST$empty)) %>% set_names(c(treatments,"EMPTY"))
  pictograph_icons = get_pictograph_icons(category_colors = colors, deceased_color = list(CONST$empty) %>% 
                                            set_names(dutch$PLOT$deceased))
  picto = if(length(survival)==0) {
    surv = list(EMPTY=100)
    plot_pictograph(surv,pictograph_icons,from_top = T,picto_title="", show_legend = FALSE, width = ifelse(is_wide,0.9,0.4), 
                    title_size = CONST$size_title,alive = dutch$PLOT$alive)
  } else {
    surv = lapply(survival, function(x) round(tail(x$surv,n = 1)*100))
    if(length(surv) <= 2) {
      plot_title = ifelse(length(surv)==1, dutch$PLOT$time1(n_months, treatments), dutch$PLOT$time2(n_months, treatments))
      plot_pictograph(surv,pictograph_icons, from_top = T,picto_title=to_title(plot_title),width = ifelse(is_wide,0.9,0.4),title_size = CONST$size_title,
                      alive = dutch$PLOT$alive)
    } else if(length(survival) > 2) {
      sub_pictos = lapply(1:length(treatments),function(i) plot_pictograph(surv[[i]] %>% set_names(treatments[[i]]),pictograph_icons,from_top = T,
                                                                           picto_title=to_title(treatments[i]),
                                                                           width=ifelse(length(survival)==4,0.65,0.9), show_legend = FALSE,
                                                                           title_size = CONST$size_legend,
                                                                           alive = dutch$PLOT$alive))
      plot_title = textGrob(to_title(dutch$PLOT$time0(n_months)), gp=gpar(fontfamily="Lato",fontsize=CONST$size_title))
      grid = if(length(survival)==3) list(rows=3, columns=1) else get_square_grid(length(survival))
      arrangeGrob(grobs=sub_pictos, ncol=grid$rows, nrow = grid$cols, top=plot_title, padding=unit(2.5,"line"))
    }
  }
  
  grid.newpage()
  grid.draw(picto)
}

output$out_surv__pictograph = renderPlot({
    picto = draw_pictograph(ids_survival)
  }, height=reactive(input$innerHeight*0.85))

# km ----------------------------------------------------------------------
draw_kaplan_meier = function(settings) {
  survival = predict_survival()
  draw_nothing = length(survival) == 0
  treatments = names(survival)
  colors = as.list(CONST$colors[seq(length.out=length(treatments))]) %>% set_names(treatments)
  n_months = input[[settings$tmax]]
  
  if(!draw_nothing){
    treat_annotations = do.call(function(...) rbind.data.frame(...,stringsAsFactors=FALSE),
                                lapply(names(survival), function(s) list(label=s, surv=tail(survival[[s]]$surv, n=1))))
    
    treat_annotations = treat_annotations[order(treat_annotations$surv),]
    treat_annotations$new_y = separate_values(treat_annotations$surv, 0.1)
    
    ###annotations worden hier gedefinieerd.
    annotations = apply(treat_annotations, 1, function(a) list(
      xref = "paper",
      x = 1,
      y = as.numeric(a[["surv"]]),
      xanchor = 'left',
      yanchor = "middle",
      showarrow = T,
      arrowhead = 0,
      arrowcolor = colors[a["label"]],
      font=list(size=18, color=colors[a["label"]]),
      axref="x",
      ayref="y",
      ax = 40,
      ay= as.numeric(a[["new_y"]]),####y-positie voor de annotations
      text=str_wrap(as.character(a["label"]), width =10)))
  }
  
  ###De scenario/waarschijnlijkheid van de overleving ligt tussen de 0.25 en 0.75. Dit wordt gehaald uit de analysis
  #browser()
  surv = if(!draw_nothing) {do.call(rbind,survival)} else {data.frame(time=numeric(0),upper=numeric(0),lower=numeric(0),label=character(0),surv=numeric(0))}
  breaks = get_breaks(limits=n_months, N=12, max_breaks = 15, include_bounds = TRUE)
  lbls = breaks_to_labels(breaks)
  
  normal = ggplot(data=surv, aes(x=time, y=surv, group=label, fill=label, color=label,
                                 text = paste((label), mround(surv*100, 5),"%")))
  empty = ggplot(data=surv, aes(x=time, y=surv, group=label, fill=label, color=label,
                                text = character(0)))
  
  p = if(!draw_nothing) {normal+geom_line(size=1)} else {empty+geom_blank()}
  p = p+ggtitle(to_title(dutch$UI$surv))+
    scale_x_continuous(name =dutch$PLOT$time_xaxis , limits = c(0, n_months*CONST$month),
                       breaks = breaks*CONST$month, labels=lbls, expand=c(0,0)) +
    scale_y_continuous("",limits=c(0,1),labels=function(x) paste0(round(x*100),"%"),
                       breaks = seq(0,1, by = 0.1),expand=c(0,0)) +
    scale_fill_manual(name="",values=unlist(colors)) + scale_colour_manual(name="",values=unlist(colors)) +
    source_theme + theme(plot.margin = unit(c(2,5,1,1),units = "cm"))+

    if(input[[settings$confidence]] & !draw_nothing) geom_ribbon(data=surv, aes(x=time, ymax=upper, ymin=lower, group =label, fill=label, color=label),
                                                                 alpha=0.2,position = "identity",inherit.aes = F)
  
  if("mid" %in% input[[settings$percentile]] && !draw_nothing  && nrow(surv[surv$surv >=0.25 & surv$surv<=0.75,]) > 0)
  {
    typical_range = surv[surv$surv >=0.25 & surv$surv<=0.75,]
    p = p + geom_area(data = typical_range,aes(x=time,y=surv,group =label, fill=label, color=label),
                      alpha =0.3,position = "identity",show.legend = F,inherit.aes = F)
  }
  if("bad" %in% input[[settings$percentile]] && !draw_nothing && nrow(surv[surv$surv >=0.75,]) > 0) {
    worst_range = surv[surv$surv >=0.75,]
    p = p + geom_area(data = worst_range,aes(x=time,y=surv,group =label, fill=label, color=label),
                      alpha =0.5,position = "identity",show.legend = F,inherit.aes = F)
  }
  if("good" %in% input[[settings$percentile]] && !draw_nothing && nrow(surv[surv$surv <= 0.25,]) > 0) {
    best_range = surv[surv$surv <= 0.25,]
    p = p + geom_area(data = best_range,aes(x=time,y=surv,group =label, fill=label, color=label),
                      alpha =0.1,position = "identity",show.legend = F,inherit.aes = F)
  }
  
  km = ggplotly(p, height=input$innerHeight*0.85, tooltip=c("text"), type="scatter")%>%
    config(p = ., collaborate = F, displayModeBar = F,autosizable=F, scrollZoom=F,showAxisRangeEntryBoxes=F,
           showAxisDragHandles=F,displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(plotly_rem))%>%
    layout(legend = list(orientation='h', y=55),hovermode="x",hoverlabel=list(font=list(size=CONST$size_legend))) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  gggggp<<-km
  if(!draw_nothing)
  {
    for(i in 1:length(annotations))##De annotations worden toegevoegd aan de plotly plot
    {
      km =  km %>% layout(annotations=annotations[[i]])
    }
  }
  
  km %<>% apply_plotly_settings
}

output$out_surv__km = renderPlotly({
    draw_kaplan_meier(ids_survival)
 })