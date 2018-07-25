MAAND_DUUR = 30.4375##duur van een maand in dagen
SKIP = FALSE##variabele voor het overslaan van de gegevensinvoer, if T wordt de invoer overgeslagen
CHEMO_TOP3 = 3###Top 3 meest voorkomende chemo's
MAX_KWAL_DUUR = 6##maximum tijd voor voorspelling van kwaliteit van leven in maanden
HRQOL_GENPOP = 71.2 #gemiddelde score met sd van 22.4, mediaan is 75 IQD (58.3-83.3)

size_text = 14
size_title = 25
size_legend = 20

str_wrap = function(x, width) sapply(x, function(y) paste(strwrap(y,width), collapse="\n"))
mround <- function(x,base){
  base*round(x/base)
}

breaks_to_labels = function(breaks)
{
  six_months = breaks%%6 == 0 & breaks > 0
  breaks[six_months] = paste0(breaks[six_months],"\n",breaks[six_months]/12," jaar")
  gsub("0\\.5","half",breaks)
}
### Functie voor het vertalen van de Engelse termen voor behandelingen, naar het Nederlands
trans = function(X) factor(unname(sapply(X,function(z) used_treats_nl[names(used_treats_nl)==as.character(z)])),levels=used_treats_nl)

###Functie voor het vertalen van de Engelse bijwerkingen naar het Nederlands
trans_tox = function(X) factor(unname(sapply(X,function(z) tox_en_nl[names(tox_en_nl)==as.character(z)])),levels=tox_en_nl)

#####Voor het testen########
# de_behandeling = c("Geen behandeling","Chemoradiatie","Stent")
# overlevingen = c(0.5,0.6,0.2)
############################

kwal_bar_knop = bsButton(inputId="kwal_bar_but", label=img (src="bar.png", width="50", height="50"),
                         icon = NULL, style = "warning",size = "large", type = "action", block = FALSE, disabled = FALSE)

kwal_lijn_knop = bsButton(inputId="kwal_lijn_but", label=img (src="line.png", width="50", height="50"),
                          icon = NULL, style = "warning",size = "large", type = "action", block = FALSE, disabled = FALSE)

###Lijst met kleuren die gebruikt worden in de verschillende diagrammen
colors = c("#911eb4","#05acaf","#25a60a","#1641ba","#866635","#ff9730","#cf9499","#173d28","#8da9bf")
#light_colors = lighten(colors,amount=0.4,method="relative",space="HCL")
light_colors = c('#CA75EF','#5BD3D6','#61D257','#7588F3','#C09E73','#FFC2A0','#F4B8BD','#698774','#B1CCE2')
lijn_kleur = colors;tox_kleur = colors; tox_light_colors = light_colors; picto_kleur = colors###lijn_kleur = kleuren voor lijndiagram en staafdiagram, tox_kleur voor de toxicity diagram en picto_kleur voor de pictograph

used_treats_nl = c("None"="Geen behandeling", "Chemotherapy"="Chemotherapie",'Chemoradiation' = "Chemoradiatie",
                   "Chemotherapy + short-term radiation"="Chemotherapie + korte bestraling",
                   "Resection (primary tumor)"="Operatie (tumor)",
                   "Resection (metastasis)"="Operatie (uitzaaiing)",
                   "Radiotherapy (primary tumor)" = "Bestraling (tumor)",
                   "Radiotherapy (metastasis)" = "Bestraling (uitzaaiing)",
                   "Stent" = "Stent")
behandelingen_nl_picto = list("Geen behandeling", "Chemotherapie","Bestraling (tumor)","Bestraling (uitzaaiing)",
                              "Chemoradiatie","Chemotherapie + korte bestraling","Operatie (uitzaaiing)","Stent","Operatie (tumor)")

###De namen van de locatie van de tumor bij slokdarmtumor, in het Nederlands.
eso_locatie_namen = c("Cervicaal", "Thoracaal bovenste gedeelte", "Thoracaal middelste gedeelte",
                      "Thoracaal onderste gedeelte (intra-abdominaal)","Gastro-Oesofagale junctie","Meerdere aaneengesloten gebieden","Slokdarm NOS")


eso_used_treats = eso_fit$Design$values$first_line[eso_fit$Design$values$first_line %in% names(used_treats_nl)]
gas_used_treats = gas_fit$Design$values$first_line[gas_fit$Design$values$first_line %in% names(used_treats_nl)]

eso_used_locations = eso_fit$Design$values$tTopog_name; eso_used_locations[grepl("Over*",eso_used_locations)]="Overlapping lesion"
gas_used_locations = gas_fit$Design$values$tTopog_name; gas_used_locations[grepl("Over*",gas_used_locations)]="Overlapping lesion"

eso_used_locations = c(eso_used_locations[1:4],eso_used_locations[7],eso_used_locations[5:6])##locaties worden op een andere volgorde gezet.
eso_age = eso_fit$Design$limits$nLeeft
gas_age = gas_fit$Design$limits$nLeeft

the_treatments = paste(unique(c(eso_used_treats, gas_used_treats)))
behandelingen_nl = trans(the_treatments)
names(lijn_kleur) = sapply(1:length(behandelingen_nl),function(x)behandelingen_nl[x])

###Voor plotly, dit zijn opties die uiteindelijk niet weergeven hoeven te worden, als de toolbar wordt weergeven.
l1=array("autoScale2d")
l2=array("hoverCompareCartesian")
l3=array("hoverClosestCartesian")
l4=array("toImage")
l5=array("zoom2d")
l6=array("pan2d")
l7=array("select2d")
l8=array("lasso2d")
weg1=cbind(l1,l2,l3,l4,l5,l6,l7,l8)

###kwal_scores is score voor QoL 0 t/m 100 met 0 is slecht en 100 is uitstekend
kwal_scores = as.character(seq(0,100,by=10)); kwal_scores[which(kwal_scores=="0")]="0 Slecht"; kwal_scores[which(kwal_scores=="100")]="100 Uitstekend"
percentage_scores = paste0(seq(0,100,by=10),"%")###percentage 0 tot 100 %, o.a. gebruikt bij de y-as van overlevingsgrafiek
kwal_xas = paste(as.character(seq(0,MAX_KWAL_DUUR,by=3)),"maanden");kwal_xas[kwal_xas=="6 maanden"] = "half jaar"###x-as van kwaliteit van leven

####De bijwerkingen die weergeven worden, in het Engels
tox1_en = c("Febrile neutropenia","Diarrhea","Nausea/vomiting","Anorexia",
            "Neuropathy","Fatigue","Hand foot syndrome","Stomatitis","Mucositis","Alopecia")
###De bijwerkingen die weergeven worden, in het Nederlands
tox1_nl = c("Febriele neutropenie","Diarree","Misselijkheid/ Overgeven","Anorexia",
            "Neuropathie","Vermoeidheid","Hand voet syndroom","Mond slijmvlies ontsteking","Slijmvlies ontstekig","Haaruitval")

###De bijwerkingen die weergeven worden in het Engels met hun Nederlandse vertaling
tox_en_nl = c("Febrile neutropenia" = "Febriele neutropenie","Diarrhea" = "Diarree","Nausea/vomiting"="Misselijkheid/ Overgeven",
              "Anorexia"="Anorexia","Neuropathy"="Neuropathie","Fatigue"="Vermoeidheid","Hand foot syndrome"="Hand voet syndroom",
              "Stomatitis"="Mond slijmvlies ontsteking","Mucositis"="Slijmvlies ontstekig","Alopecia"="Haaruitval")

####De excel data per chemokuur wordt geladen in toxicity
#library(readxl)
# toxicity <- rbind(read_excel("./tox/table_toxplot_CAPOXDTX.xlsx"),read_excel("./tox/table_toxplot_CAPOX.xlsx"),
#                   read_excel("./tox/table_toxplot_5FU.xlsx"),read_excel("./tox/table_toxplot_OxT.xlsx"))#,
#                   #read_excel("./tox/table_toxplot_5FU-DTX.xlsx"))
toxicity = readRDS("tox/toxicity.rds")
chemo_behandelingen = unique(toxicity$chemo)###De chemobehandelingen bevinden zich in de kolom chemo
toxicity = toxicity[c("tox","grade","chemo","estimate")];toxicity$estimate = toxicity$estimate*100###Alleen kolommen tox, grade, chemo en estimate nodig; de estimate wordt *100 gedaan om een percentage te krijgen
names(toxicity) = c("Bijwerking","Gradering","Behandeling","Proportie")###Nederlandse termen worden aan de kolommen gekoppeld
toxicity$Gradering[which(toxicity$Gradering=="grade 1-2")] = "Mild";toxicity$Gradering[which(toxicity$Gradering=="grade 3-4")] = "Ernstig"##Grade 1-2 wordt mild genoemd, grade 3-4 ernstig
toxicity = toxicity[- which(toxicity$Gradering == "grade 5"),]###Bijwerkingen met grade 5 worden uit de data gehaald
toxicity = toxicity[which(toxicity$Bijwerking %in% tox1_en),]###Alleen de bijwerkingen die ook in tox1_en staan (de bijwerkingen die gekozen zijn om te weergeven) worden gebruikt

###voor het kiezen van top 3 moeten alopecia en febrile neutropenia er eerst uit. Dit zijn namelijk met enkele graderingen (alleen 1-2 of 3-4). 
tox_data = toxicity[- which(toxicity$Bijwerking=="Alopecia" | toxicity$Bijwerking=="Febrile neutropenia"),]
##tox_een_grad dataframe met de bijwerkingen met een gradering
tox_een_grad = toxicity[which(toxicity$Bijwerking=="Alopecia" | toxicity$Bijwerking=="Febrile neutropenia"),]
tox_een_grad$Totaal = tox_een_grad$Proportie##de totale proportie is gelijk aan de proportie van de gradering
oneven_nr = seq(1,nrow(tox_data),2)##oneven getallen generatie. Nodig voor het kiezen van de milde en ernstige bijwerkingen
tot_prop = c()
for(i in 1:length(oneven_nr)){
  ###De totale proportie van een bijwerking is de milde prop + de ernstige prop. Dus oneven rij + even rij. 
tot_prop[i] = sum(tox_data$Proportie[oneven_nr[i]],tox_data$Proportie[oneven_nr[i]+1])
}
tox_data$Totaal = rep(tot_prop,each=2,times=1)##De totale proportie wordt bij elke bijwerking toegevoegd. Elke totale proportie 2x want milde bijwerking is 1 rij, en ernstige bijwerking is 1 rij
tox_data = rbind(tox_data,tox_een_grad)##de tox data met bijwerkingen met 2 graderingen en toxdata met een gradering worden weer bij elkaar gevoegd


milde_tox3 = list()
ernstige_tox3 = list()
for(i in 1:length(chemo_behandelingen)){
  milde_tox = tox_data[which(tox_data$Behandeling==chemo_behandelingen[i]&tox_data$Gradering=="Mild"),]
  milde_tox3[[i]] = milde_tox[order(milde_tox$Totaal,
                          decreasing = T)[1:CHEMO_TOP3],]

  ernstige_tox = tox_data[which(tox_data$Behandeling==chemo_behandelingen[i]&tox_data$Gradering=="Ernstig"),]
  ernstige_tox3[[i]] = ernstige_tox[order(ernstige_tox$Totaal,
                                    decreasing = T)[1:CHEMO_TOP3],]
  }
ernstige_tox3 = do.call(rbind,ernstige_tox3)
milde_tox3 = do.call(rbind,milde_tox3)
tox_top3 = rbind(milde_tox3,ernstige_tox3)

levels = do.call(paste, expand.grid(unique(toxicity$Behandeling),c("(mild)","(ernstig)")))
toxicity$behandel_grd = factor(paste0(toxicity$Behandeling," (",tolower(toxicity$Gradering),")"),levels)
tox_top3$behandel_grd = factor(paste0(tox_top3$Behandeling," (",tolower(tox_top3$Gradering),")"),levels)
toxicity$Bijwerking = as.character(trans_tox(toxicity$Bijwerking))##de bijwerkingen worden vertaald naar NL
tox_top3$Bijwerking = as.character(trans_tox(tox_top3$Bijwerking))##de bijwerkingen worden vertaald naar NL

# ordered_colors = colors[col2rgb(colors) %>% colMeans %>% order]
# oude_gemiddelde = col2rgb(ordered_colors) %>% colMeans
# nieuwe_gemiddelde = seq(0,255,length.out = 9)
# new_colors = darken(ordered_colors,nieuwe_gemiddelde/oude_gemiddelde-1,method="absolute",space = "HCL")
# image(1:9,1,as.matrix(1:9),col=new_colors,xlab="HLS",ylab="",xaxt="n",yaxt="n",bty="n")
