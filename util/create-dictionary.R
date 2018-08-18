library(hgutils)
load_packages('grid','gridExtra','htmltools','plotly','rms','shinyBS','shinydashboard', "magrittr",
              'shinyjs','shinyWidgets','shinythemes','showtext',"shiny","stringr","XML", "purrr","yaml")
startup()
fit = readRDS("data/surv-oesophagus.rds")


# Predictors --------------------------------------------------------------

PREDICTOR_NAMES = list(first_line="Eerste behandeling",
                       has_c770="Lymfeklier metastasen in hoofd/hals gebied", 
                       has_c771="Thoracale lymfeklier metastasen",
                       has_c772="Abdominale lymfeklier metastasen",
                       liver_meta="Lever metastasen", nLeeft="Leeftijd", nmeta="Aantal locaties met afstandsmetastasen",
                       only_lymf_meta="Alleen afstandsmetastasen in lymfeklieren",
                       peri_meta="Peritoneale metastasen",
                       pGesl="Geslacht", sCN="Klinische N-stadium", sCTs="Klinische T-stadium",
                       tDiffgr="Gradering", tMorfology_simple="Histologie", 
                       tTopog_name="Locatie primaire tumor")

first_line = list(OES = list("None"="Alleen ondersteunend", "Chemotherapy"="Chemotherapie", "Chemoradiation"="Chemoradiatie",
                             "Chemotherapy + short-term radiation"="Chemotherapie + korte bestraling",
                             "Resection (metastasis)"="Operatie (uitzaaiing)", "Radiotherapy (primary tumor)"="Bestraling (tumor)",
                             "Radiotherapy (metastasis)"="Bestraling (uitzaaiing)", "Stent"="Stentplaatsing"),
                  GAS = list("None"="Alleen ondersteunend", "Chemotherapy"="Chemotherapie",
                             "Chemotherapy + short-term radiation"="Chemotherapie + korte bestraling", "Resection (primary tumor)"="Operatie (tumor)",
                             "Resection (metastasis)"="Operatie (uitzaaiing)", "Radiotherapy (primary tumor)"="Bestraling (tumor)",
                             "Radiotherapy (metastasis)"="Bestraling (uitzaaiing)", "Stent"="Stentplaatsing"))
BINARY = list(No="Nee", Yes="Ja")
sex = list(Male="Man", "Female"="Vrouw")
grade = list(G1="1", G2="2", G3="3", G4="4")
hist = list("Adenocarcinoma"="Adenocarcinoom", "Squamous cell"="Plaveiselcelcarcinoom", "Other"="Andere carcinoom")
topo = list(OES = list(Cervical="Cervicaal", "Upper thoracic"="Bovenste deel thorax", "Mid-thoracic"="Middelste deel thorax",
                      "Lower thoracic"="Intra-abdominale thorax","Overlapping lesion"="Meerdere aaneengesloten gebieden",
                    "Junction"="Gastro-oesophageale junctie","Esophagus NOS"="Overig"),
            GAS = list(Fundus="Fundus", Corpus="Corpus", "Antrum Pylori"="Antrum Pylori", "Pylorus"="Pylorus", "Lesser curvature NOS"="Kleine curvatuur",
                    "Greater curvature NOS"="Grote curvatuur", "Overlapping lesion"="Meerdere aaneengesloten gebieden", "Stomach NOS"="Overig"))
SURV = list(PREDS=PREDICTOR_NAMES, fl=first_line, BIN=BINARY, sex=sex, grd=grade, hist=hist, topo=topo)

# UI Titles -------------------------------------------------------------------
UI = list(prev_next = list(previous="vorige","next"="volgende"),
          submit = "Voer gegevens in",
          entry = "Patiënt",
          surv = "Overleving",
          ch_display = "Wijzig weergave",
          display_surv = list(pictograph="Pictogram", km="Kaplan-Meier"))

# Misc. -------------------------------------------------------------------

location_simple = "Locatie primaire tumor"
locations = list("Oesophagus"="Slokdarm","Stomach"="Maag")

ERR = list(invalid_range = function(min, max) sprintf("Ongeldige invoer: vul een getal in tussen de %s en %s", min, max),
           nmeta = "Er zijn meer metastasen locaties aangegeven dan het totaal aantal metastasen.",
           error = "Fout")

# Tooltips ----------------------------------------------------------------

TTIPS = list(location_simple = list("Slokdarm inclusief junctie", "Maag exclusief junctie") %>% set_names(names(locations)),
             sCN = list(N0="Geen regionale lymfeklier metastasen", N1="1-2 regionale lymfeklier metastasen",
                       N2="3-6 regionale lymfeklier metastasen", N3="7 of meer regionale lymfeklier metastasen") %>% set_names(fit$Design$values$sCN),
             scTS = list(T1="Tumor is de lamina propia, muscularis mucosae of submucosa doordrongen.", T2="Tumor is de muscularis propria doordrongen.",
                   T3="Tumor is de adventitia doordrongen.", T4="Tumor is doorgedrongen tot naastgelegen structuren.",
                   TX="Onbekend") %>% set_names(fit$Design$values$sCTs),
             grade = list(G1="Goed gedifferentieerd", G2="Matig gedifferentieerd", G3="Slecht gedifferentieerd", 
                   G4="Ongedifferentieerd") %>% set_names(fit$Design$values$tDiffgr),
             only_lymf = list(Y="Afstandsmetastasen uitsluitend in lymfeklieren", 
                   N="Afstandsmetastasen in tenminste 1 orgaan"))

# Create dictionary -------------------------------------------------------

dutch = list(SURV=SURV, LOC=location_simple, LOCS=locations, TTIPS=TTIPS, ERR=ERR, UI=UI)
write_yaml(dutch, "data/dict-nl.yml")
