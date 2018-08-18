#setwd("S:/Dropbox/Dropbox/source_webinterface/test")
#setwd("C:/Users/hgvandenboorn/Dropbox/source_webinterface/test")
# setwd("C:/Users/Florian/Dropbox/V5.0_SOurCE-WebSite")
GENDER = list("MALE","FEMALE"); names(GENDER) = GENDER
PICTOGRAPH_ICONS = "pictograph_icons"

N_DIST = 0.4
HOR_EXP = 1.5

#' Check if color string
#'
#' @param x A possible color string.
#'
#' @return Whether x is defined as a "#"-sign followed by 6 hexadecimal digits
#'
#' @examples
is_col = function(x) {all(sapply(x,function(y) grepl("^#[0-9a-f]{6}$",tolower(y))))}

#recolors the given greyscale icon into any given color
#' Recolor icon
#'
#' @param icon A greyscale RGB image array with values in [0-1]
#' @param new_color The color in which the icon should be painted.
#'
#' @return A colored image.
#'
#' @examples
recolor_icon = function(icon, new_color)
{
  new_rgb = c(col2rgb(new_color)/255)
  for (dim in 1:3)
  {
    new_color = new_rgb[dim]
    old_col = icon[,,dim]
    icon[,,dim] = old_col + (1-old_col) * new_color #color the dark parts
  }
  return(icon)
}

#creates a list of named colored icons and appends an icon to depict deceased individuals
#' Title
#'
#' @param category_colors A named list of color strings
#' @param deceased_color  A named list of size 1 with a color string
#' @param gender Either "MALE" or "FEMALE", for the icon image
#'
#' @return A "pictograph_icons" object containing icon raster images, color codes, annotations for 
#' deceased and a name vector
#'
#' @examples
get_pictograph_icons = function(category_colors, deceased_color=c(deceased="#cccccc"), gender=names(GENDER))
{
  if (is.null(names(category_colors))) stop("Argument category_colors must be a named vector of color strings.")
  if (is.null(names(deceased_color)) | length(deceased_color)!=1) stop("Argument deceased_color must be a named vector of color strings.")
  if (!is_col(category_colors) | !is_col(deceased_color)) stop("Argument category_colors and deceased_color must be color strings (e.g. #000000)")
  
  category_colors = unlist(category_colors)
  deceased_color = unlist(deceased_color)
  gender = unlist(match.arg(gender))
  #icon = readPNG(paste0(gender,".png"))[,,1:3]
  icon = readRDS("data/pictograph-icon.rds")
  colors = c(category_colors,deceased_color) #list of used colors
  is_deceased = c(rep(F,length(colors)-1),T); names(is_deceased) = names(colors) #which icon depicts a deceased individual?
  colored_icons = lapply(colors,function(x) {rast = rasterGrob(recolor_icon(icon,x))})#$raster
  attr(colored_icons,"colors") = colors
  attr(colored_icons,"is_deceased") = is_deceased
  
  #Collect results
  pictograph_icons = list(icons=colored_icons, ratio=dim(icon)[1]/dim(icon)[2])
  class(pictograph_icons) = c(class(pictograph_icons),PICTOGRAPH_ICONS)
  return(pictograph_icons)
}

#' adds the survival of a treatment to the dataset at the correct locations
#'
#' @param data A melted data.frame containing X, Y and VAL to store the icon information in
#' @param surv A vector of named survival probabilities (0-100)
#' @param from_top Whether to plot the colored images from the top (default) or bottom.
#'
#' @return An updated version of data
#'
#' @examples
add_survival = function(data, surv, from_top = TRUE)
{
  #start with the highest survival and override the lower parts with lower survivals
  surv = surv[order(unlist(surv),decreasing = T)]
  
  for (i in 1:length(surv))
  {
    survival = surv[[i]]
    treatment = names(surv)[i]
    
    Y_sel = if(from_top) data$Y > 10-floor(survival/10) else data$Y <= survival/10
    X_sel = if(from_top) data$Y == ceiling(10-survival/10) else data$Y == ceiling(survival/10)
    
    data$VAL[Y_sel] = treatment
    data$VAL[X_sel & data$X <= (survival %% 10)] = treatment
  }
  
  data
}

#' Pictograph
#'
#' @param surv A vector of named survival probabilities (0-100)
#' @param icons A "pictograph_icons" object containing icon raster images, color codes, annotations for 
#' deceased and a name vector
#' @param from_top Whether to plot the colored images from the top (default) or bottom.
#'
#' @return A ggplot pictograph
#'
#' @examples
# plot_pictograph = function(surv, pictograph_icons, from_top = TRUE,picto_title)
# {
#   if (!PICTOGRAPH_ICONS %in% class(pictograph_icons)) stop(paste0("Argument icons must be of class '",PICTOGRAPH_ICONS,"'"))
#   icons = pictograph_icons$icons
#   if (!all(names(surv) %in% names(icons))) stop(paste0("The names in 'surv' must be present in 'icons'"))
#   
#   deceased = names(icons)[attr(icons,"is_deceased")]
#   legend_order = c(names(surv)[order(unlist(surv))], deceased)
#   if (!from_top) legend_order = rev(legend_order)
# 
#   #dataset representation of icons
#   data = expand.grid(Y=1:10,X=1:10); names(data)=c("X","Y")
#   data$VAL = deceased
#   data = add_survival(data, surv, from_top = from_top)
#   data$VAL = factor(data$VAL,levels = legend_order)
#   amounts = c(surv[legend_order][1:length(surv)],sum(data$VAL==deceased)); names(amounts) = legend_order
#   
#   #set up plotting area. Plot tiles so legend becomes visible
#   pictograph = ggplot(data,mapping = aes(x=X*HOR_EXP,y=Y,fill=VAL)) + geom_point(size=5,shape=21) +
#                scale_fill_manual(NULL, values=attr(icons,"colors"), labels = paste0(names(amounts)," (",amounts,"/100)"), drop=F) +
#                guides(fill=guide_legend(label.vjust = 0.6)) +
#                ggtitle(picto_title)+
#           
#                coord_fixed(ratio = pictograph_icons$ratio) #icons are at ratio 1:2 so axes are changed to reflect this
#   
#   # add the icons one by one
#   for(i in 1:nrow(data))
#   {
#     GR = icons[[as.character(data$VAL[i])]]
#     #GR=rasterGrob(recolor_icon(icon,colors[1]))
#     x = data$X[i]*HOR_EXP
#     y = data$Y[i]
#     pictograph = pictograph + annotation_custom(GR, xmin = x-N_DIST, xmax = x+N_DIST, ymin = y-N_DIST, ymax = y+N_DIST)
#   }
#   
#   #remove all theme information but put the legend on top and return the plot 
#  pictograph + theme_nothing() + theme(legend.position = "top",
#                                       #c(0,ifelse(from_top,1,0)), 
#                                       plot.title = element_text(size=24,hjust=0.5,vjust = 1, margin = unit(c(5,1,5,1),"mm")),
#                                       plot.margin = unit(c(1,1,5,1),"mm"),
#                                       legend.justification = c(0,ifelse(from_top,1,0)), legend.key.size = unit(5,"mm"),
#                                       legend.text = element_text(size = 16), legend.key.height = unit(10,"mm"))
# }

plot_pictograph = function(surv, pictograph_icons, from_top = TRUE, picto_title, wide=F, show_legend = TRUE)
{
  if (!PICTOGRAPH_ICONS %in% class(pictograph_icons)) stop(paste0("Argument icons must be of class '",PICTOGRAPH_ICONS,"'"))
  icons = pictograph_icons$icons
  if (!all(names(surv) %in% names(icons))) stop(paste0("The names in 'surv' must be present in 'icons'"))
  
  deceased = names(icons)[attr(icons,"is_deceased")]
  legend_order = c(names(surv)[order(unlist(surv))], deceased)
  if (!from_top) legend_order = rev(legend_order)
  
  #dataset representation of icons
  data = expand.grid(Y=1:10,X=1:10); names(data)=c("X","Y")
  data$VAL = deceased
  data = add_survival(data, surv, from_top = from_top)
  data$VAL = factor(data$VAL,levels = legend_order)
  amounts = c(surv[legend_order][1:length(surv)],sum(data$VAL==deceased)); names(amounts) = legend_order
  VERT_EXP=HOR_EXP*2
  
  grid.newpage()
  vp1 = viewport(x=0.5,y=0.01,width=ifelse(wide,0.9,0.4),height=ifelse(show_legend, 0.85, 0.925),just="bottom",layout=grid.layout(10,10))
  #pushViewport(vp1)
  ics = lapply(1:nrow(data), function(i)
  {
    GR = icons[[as.character(data$VAL[i])]]$raster
    #GR=rasterGrob(recolor_icon(icon,colors[1]))
    x = data$X[i]*HOR_EXP
    y = data$Y[i]*VERT_EXP
    
    #grid.raster(GR, x = x/max(data$X*HOR_EXP)-0.05, y = y/max(data$Y*VERT_EXP)-0.05, width = 0.05, height=0.1)
    
    rasterGrob(GR, x=0.5, y=0.5,height=0.95, vp=viewport(layout.pos.col=data$X[i],layout.pos.row=11-data$Y[i]), interpolate = T)
  })
  #grid.ls(viewports=TRUE, grobs=FALSE)
  #popViewport()
  #pushViewport()
  vp2 = viewport(x=0.5,y=0.9,height=0.1)
  lg = legendGrob(legend_order,nrow=1,pch=21,gp=gpar(fontsize=CONST$size_legend,#fontfamily="Lato",
                                                     fill=attr(icons,"colors")[legend_order],col=attr(icons,"colors")[legend_order]))
  
  vp3 = viewport(x=0.5,y=0.975)
  tg = textGrob(picto_title,gp=gpar(fontsize=CONST$size_title))#,fontfamily="Lato"))
  #popViewport()
  #grid.grab()
  
  imPlot = gTree(children=Reduce(gList,ics),vp=vp1,name="images_plot")
  legendPlot = gTree(children=gList(lg),vp=vp2,name="legend_plot")
  titlePlot = gTree(children=gList(tg),vp=vp3,name="title_plot")
  wholePlot = gTree(children = if(show_legend) gList(imPlot,legendPlot,titlePlot) else gList(imPlot, titlePlot))
  # grid.newpage()
  # grid.draw(wholePlot)
  #set up plotting area. Plot tiles so legend becomes visible
  # pictograph = ggplot(data,mapping = aes(x=X*HOR_EXP,y=Y,fill=VAL)) + geom_point(size=5,shape=21) +
  #   scale_fill_manual(NULL, values=attr(icons,"colors"), labels = paste0(names(amounts)," (",amounts,"/100)"), drop=F) +
  #   guides(fill=guide_legend(label.vjust = 0.6)) +
  #   
  #   coord_fixed(ratio = pictograph_icons$ratio) #icons are at ratio 1:2 so axes are changed to reflect this
  # 
  # # add the icons one by one
  # for(i in 1:nrow(data))
  # {
  #   GR = icons[[as.character(data$VAL[i])]]
  #   #GR=rasterGrob(recolor_icon(icon,colors[1]))
  #   x = data$X[i]*HOR_EXP
  #   y = data$Y[i]
  #   pictograph = pictograph + annotation_custom(GR, xmin = x-N_DIST, xmax = x+N_DIST, ymin = y-N_DIST, ymax = y+N_DIST)
  # }
  # 
  # #remove all theme information but put the legend on top and return the plot 
  # pictograph + theme_nothing() + theme(legend.position = c(1,ifelse(from_top,1,0)), 
  #                                      plot.margin = unit(c(1,1,5,1),"mm"),
  #                                      legend.justification = c(0,ifelse(from_top,1,0)), legend.key.size = unit(5,"mm"),
  #                                      legend.text = element_text(size = 10), legend.key.height = unit(10,"mm"))
}

# #example
# #STATES = list("Chemo","Resectie","Chemoradiatie"); names(STATES) = STATES
#treatments = as.list(used_treats_nl)
#STATES = c(treatments,"EMPTY"); names(STATES) = STATES#lijst met behandelingen is dezelfde lijst als used_treats_nl
#COLORS = as.list(c(picto_kleur,"#dddddd")); names(COLORS) = names(STATES)#kleuren van de pictograph komen uit lijn_kleur
# #list("#911eb4","#05acaf","#25a60a"); 
#pictograph_icons = get_pictograph_icons(category_colors = COLORS, deceased_color = list("Overleden"="#dddddd"))
#surv = list(37,58);names(surv)=c(STATES$Chemo,STATES$Resectie)
#behandelingen = c("Geen behandeling","Bestraling (tumor)")
#surv = list(37,58);names(surv)=c(STATES[[behandelingen[1]]],STATES[[behandelingen[2]]])
##grid.draw(plot_pictograph(surv, pictograph_icons,from_top = T,picto_title = "EEN TITEL"))
