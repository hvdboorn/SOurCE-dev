surv_oes = readRDS("oesophagus_survival.model")
surv_gas = readRDS("gastric_survival.model")

surv_category = setdiff(union(surv_oes$Design$name[surv_oes$Design$assume == "category"],
                              surv_gas$Design$name[surv_gas$Design$assume == "category"]), "nmeta_cat")
surv_numeric = union(surv_oes$Design$name[surv_oes$Design$assume == "asis"],
                     surv_gas$Design$name[surv_gas$Design$assume == "asis"])
# numeric_limits = surv_oes$Design$limits[which(row.names(surv_oes$Design$limits) %in% c("Low","High")), surv_numeric]
# numeric_limits = surv_gas$Design$limits[which(row.names(surv_gas$Design$limits) %in% c("Low","High")), surv_numeric]
# Age: 20-100, nmeta: 1-6
surv_pred_namesNL = list(nLeeft="Leeftijd", "sCN"="Klinische N-stadium", "sCTs"="Klinische T-stadium", "nmeta"="Aantal locaties met afstandsmetastasen",
                         tTopog_name="Locatie primaire tumor", liver_meta="Lever metastasen", peri_meta="Peritoneale metastasen", location_simple="Locatie primaire tumor",
                         only_lymf_meta="Alleen afstandsmetastasen in lymfeklieren", first_line="Eerste behandeling", tDiffgr="Gradering")
surv_topo_oesNL = list(Cervical="Cervicaal", "Upper thoracic"="Bovenste deel thorax", "Mid-thoracic"="Middelste deel thorax",
                      "Lower thoracic"="Intra-abdominale thorax","Overlapping lesion"="Meerdere aaneengesloten gebieden",
                    "Junction"="Gastro-oesophageale junctie","Esophagus NOS"="Overig")
surv_topo_gasNL = list(Fundus="Fundus", Corpus="Corpus", "Antrum Pylori"="Antrum Pylori", "Pylorus"="Pylorus", "Lesser curvature NOS"="Kleine maagboog",
                    "Greater curvature NOS"="Grote maagboog", "Overlapping lesion"="Meerdere aaneengesloten gebieden", "Stomach NOS"="Overig")
surv_pred_binairyNL = list(No="Nee", Yes="Ja")
surv_gradeNL = list(G1="1", G2="2", G3="3", G4="4")
locationNL = list("Oesophagus"="Slokdarm","Stomach"="Maag")

treatment_oesNL = list("None"="Alleen ondersteunend", "Chemotherapy"="Chemotherapie", "Chemoradiation"="Chemoradiatie",
                    "Chemotherapy + short-term radiation"="Chemotherapie + korte bestraling",
                    "Resection (metastasis)"="Operatie (uitzaaiing)", "Radiotherapy (primary tumor)"="Bestraling (tumor)",
                    "Radiotherapy (metastasis)"="Bestraling (uitzaaiing)", "Stent"="Stentplaatsing")
treatment_gasNL = list("None"="Alleen ondersteunend", "Chemotherapy"="Chemotherapie",
                       "Chemotherapy + short-term radiation"="Chemotherapie + korte bestraling", "Resection (primary tumor)"="Operatie (tumor)",
                       "Resection (metastasis)"="Operatie (uitzaaiing)", "Radiotherapy (primary tumor)"="Bestraling (tumor)",
                       "Radiotherapy (metastasis)"="Bestraling (uitzaaiing)", "Stent"="Stentplaatsing")
histology_oesNL = list("Adenocarcinoma"="Adenocarcinoom", "Squamous cell"="Plaveiselcelcarcinoom", "Other"="Andere carcinoom")
ttip_locationNL = list("Slokdarm inclusief junctie", "Maag exclusief junctie") %>% set_names(names(locationNL))
ttip_sCNNL = list("Geen regionale lymfeklier metastasen", "1-2 regionale lymfeklier metastasen",
                       "3-6 regionale lymfeklier metastasen", "7 of meer regionale lymfeklier metastasen") %>% set_names(surv_oes$Design$values$sCN)
ttip_sCTsNL = list("Tumor is de lamina propia, muscularis mucosae of submucosa doordrongen.", "Tumor is de muscularis propria doordrongen.",
                   "Tumor is de adventitia doordrongen.", "Tumor is doorgedrongen tot naastgelegen structuren.", "Onbekend") %>% set_names(surv_oes$Design$values$sCTs)
ttip_tDiffgrNL = list("Goed gedifferentieerd", "Matig gedifferentieerd", "Slecht gedifferentieerd", "Ongedifferentieerd") %>% set_names(surv_oes$Design$values$tDiffgr)

dutch=list(predictors=surv_pred_namesNL, topo_oes=surv_topo_oesNL, topo_gas=surv_topo_gasNL, binary=surv_pred_binairyNL, location=locationNL,
           grade=surv_gradeNL, treatment_oes=treatment_oesNL, treatment_gas=treatment_gasNL, histology=histology_oesNL, 
           ttip_location=ttip_locationNL, ttip_sCN=ttip_sCNNL, ttip_sCTs=ttip_sCTsNL, ttip_tDiffgr=ttip_tDiffgrNL)


# for(c in surv_category) {
#   cat(c,":",surv_oes$Design$values[[c]],"\n")
# }
# for(c in surv_category) {
#   cat(c,":",surv_gas$Design$values[[c]],"\n")
# }
