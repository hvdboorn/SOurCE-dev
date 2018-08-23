draw_tox_bar = function(settings) {
  tox = predict_tox()

  tox=toxicity_data[1]
  # tox_names = tox %>% lapply(. %>% group_by(tox) %>% summarise(total=sum(prop)) %>% arrange(desc(total)) %>% slice(1:3) %>% .$tox) %>%
  #   unlist %>% unique
  # tox = lapply(tox, function(x) x[x$tox %in% tox_names,])
  
  draw_nothing = length(tox) == 0
  treatments = names(tox)
  grading = c("#B6B6B6","#414141") %>% set_names(c(dutch$TOX$grade12, dutch$TOX$grade34))
  
  df_tox = if(!draw_nothing) {do.call(rbind,tox)} else {data.frame(tox=character(0),grade=character(0),chemo=character(0),
                                                                   lower=numeric(0),upper=numeric(0),prop=numeric(0))}
  df_tox %<>% complete(tox, grade, chemo) #adds NA's to make bars of equal width
  df_tox$color_mapping = if(!draw_nothing) {factor(paste0(df_tox$chemo," (",df_tox$grade,")"))} else {factor(character(0))}
  levels(df_tox$color_mapping) = c(levels(df_tox$color_mapping), names(grading))
  
  colors = as.list(CONST$colors[seq(length.out=length(treatments))]) %>% set_names(treatments)
  light_colors = as.list(CONST$light_colors[seq(length.out=length(treatments))]) %>% set_names(treatments)
  color_guide =  sapply(levels(df_tox$color_mapping), function(cn) {
    if(cn %in% names(grading)) return(grading[[cn]])
    chemo_name = str_match(cn, "^(.*?) \\(.*\\)$")[2]
    ifelse(str_detect(cn,"1-2"),light_colors[chemo_name], colors[chemo_name])
  }) %>% unlist %>% {if(!draw_nothing) set_names(., levels(df_tox$color_mapping)) else {NULL}}
  
  bar_padding = 0.25
  
  normal=ggplot(data=df_tox[df_tox$grade=="grade 1-2",], aes(x=as.numeric(tox), y=prop,fill=color_mapping, group=chemo, text = paste0(comb,": ",prop))) + 
         geom_col(position=position_dodge2(padding=bar_padding, preserve="total"), na.rm = TRUE)+
    geom_col(data=df_tox[df_tox$grade=="grade 3-4",], mapping=aes(x=as.numeric(tox)+min(0.05,(1-bar_padding)/(length(levels(df_tox$tox))*length(tox))*10)),
            position=position_dodge2(padding=bar_padding, preserve="total"), na.rm = TRUE)
  empty = ggplot(data=df_tox, aes(x=tox, y=prop,fill=color_mapping, group=color_mapping, text = character(0))) + geom_blank()
  p = if(draw_nothing) empty else normal
  p = p + source_theme + theme(plot.margin=unit(c(2,1,1,1),"cm")) +
    ggtitle(to_title(dutch$UI$toxicity))+
    scale_x_continuous(name=NULL, breaks=seq(length.out = length(levels(df_tox$tox))), labels = str_wrap(levels(df_tox$tox), width=12))+
    scale_fill_manual(name=NULL, drop=FALSE, values=color_guide,
                      breaks=str_subset(names(color_guide),"(?:grade 3-4)|(?:^[^0-9]+$)"),
                      labels=c(sapply(str_subset(names(color_guide),"grade 3-4"), function(x) str_match(x,"^(.*?) \\(")[2]),names(grading)))+
    scale_y_continuous(name = NULL, limits = c(0,1), labels = paste0(seq(0,100,by=10),"%"), breaks = seq(0,1,by=0.1))
  p
  # ggp=ggplotly(p, tooltip=c("text"))%>%
  #   config(p = ., collaborate = F, displayModeBar = F,autosizable=F,scrollZoom=F,showAxisRangeEntryBoxes=F,showAxisDragHandles=F,
  #          displaylogo=F,autoScale2d = F,modeBarButtonsToRemove=array(plotly_rem))%>%
  #   layout(legend = list(orientation='h', y=55),hovermode="x", hoverlabel=list(font=list(size=CONST$size_legend))) %>% apply_plotly_settings
  p
}

output$out_tox__bar = renderPlot({
  draw_tox_bar(ids_tox)
})