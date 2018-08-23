SKIP = TRUE #skip patient entry
dutch = yaml.load_file("data/dict-nl.yml",eval.expr=TRUE)
surv_oes = readRDS("data/surv-oesophagus.rds")
surv_gas = readRDS("data/surv-gastric.rds")
toxicity_data = readRDS("data/toxicity.rds")

CONST = list(hrqol_mean = 71.2, #gemiddelde score met sd van 22.4, mediaan is 75 IQD (58.3-83.3)
             hrqol_med = 75,
             month = ((((4*365+1)*25-1)*4+1)/(400*12)), #~30.44 days
             hrqol_tmax = 6,
             surv_tmax = 24,
             empty = "#dddddd",
             colors = c("#911eb4","#05acaf","#25a60a","#1641ba","#866635","#ff9730","#cf9499","#173d28","#8da9bf")[-7],
             light_colors = c('#CA75EF','#5BD3D6','#61D257','#7588F3','#C09E73','#FFC2A0','#F4B8BD','#698774','#B1CCE2')[-7],
             size_text = 14, size_title = 25, size_legend = 20)
RANGE = list(nLeeft=c(18, 100), nmeta=c(1,7))

tr = function(X) sapply(X, function(x) rm_na(c(dutch$SURV$fl$OES[[x]], dutch$SURV$fl$GAS[[x]]))[1])
mround <- function(x, base) base*round(x/base)

l1=array("autoScale2d")
l2=array("hoverCompareCartesian")
l3=array("hoverClosestCartesian")
l4=array("toImage")
l5=array("zoom2d")
l6=array("pan2d")
l7=array("select2d")
l8=array("lasso2d")
plotly_rem=cbind(l1,l2,l3,l4,l5,l6,l7,l8)

apply_plotly_settings = function(ggp) {
  ggp$x$layout$legend$font$size = CONST$size_legend
  ggp$x$layout$titlefont$size = CONST$size_title
  ggp$x$layout$xaxis$titlefont$size = CONST$size_title
  ggp$x$layout$xaxis$tickfont$size = CONST$size_legend
  ggp$x$layout$yaxis$tickfont$size = CONST$size_legend
  ggp
}

source_theme = theme(
  panel.border = element_rect(fill=NA,colour="black",size=1),
  
  plot.title = element_text(size=CONST$size_title, hjust=0.5, family="Lato"),
  legend.text = element_text(size=CONST$size_legend, family="Lato"),
  text = element_text(size = CONST$size_text, family="Lato"),
  axis.text = element_text(size=CONST$size_legend, family="Lato"),
  axis.title = element_text(size=CONST$size_title, family="Lato"),
  
  legend.justification = c(0),
  legend.position = "top",
  
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank()
)

# color_blind = c("#000000", "#004949", "#009292", "#FF6DB6", "#FFB6DB", "#490092", "#006DDB", "#B66DFF", 
#                 "#6DB6FF", "#B6DBFF", "#920000", "#924900", "#DB6D00", "#24FF24", "#adad45")
# old_colors = c("#911eb4","#05acaf","#25a60a","#1641ba","#866635","#ff9730","#cf9499","#173d28","#8da9bf")[-7]
# # image(1:length(color_blind), 1, as.matrix(1:length(color_blind)), col=color_blind, xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
#  image(1:length(old_colors), 1, as.matrix(1:length(old_colors)), col=old_colors, axes=FALSE , xlab="", ylab="")
#  df = data.frame(col=old_colors, x=1:length(old_colors), y=1,stringsAsFactors = FALSE)
#  ggplot(df)+geom_raster(mapping=aes(x=x,y=y,fill=col))+scale_fill_manual(values=old_colors)+
#    coord_equal(ratio=3)+guides(fill=FALSE)+scale_x_continuous(breaks=1:nrow(df))
#  selection=color_blind[c(2,3,6,7,9,10,13,15)]
#  CONST$colors=selection