SKIP = TRUE #skip patient entry
dutch = yaml.load_file("data/dict-nl.yml",eval.expr=TRUE)
surv_oes = readRDS("data/surv-oesophagus.rds")
surv_gas = readRDS("data/surv-gastric.rds")

CONST = list(hrqol_mean = 71.2, #gemiddelde score met sd van 22.4, mediaan is 75 IQD (58.3-83.3)
             hrqol_med = 75,
             month = ((((4*365+1)*25-1)*4+1) / 400/12), #~30.44 days
             hrqol_tmax = 6,
             surv_tmax = 24,
             empty = "#dddddd",
             colors = c("#911eb4","#05acaf","#25a60a","#1641ba","#866635","#ff9730","#cf9499","#173d28","#8da9bf"),
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