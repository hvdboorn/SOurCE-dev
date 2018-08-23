load_packages("readxl", "dplyr", "magrittr")

colnames = c(comb="names", tox="tox", grade="grade", chemo="chemo", prop="estimate", lower="lower", upper="upper")
files = list.files("S://Dropbox//Toxplot Palliatief//Analysis//output",pattern = "^table.*\\.xlsx", full.names = TRUE) %>% sort %>%
        lapply(. %>% read_xlsx %>% extract(, colnames) %>% set_colnames(names(colnames)) %>% {.[.$tox %in% c(names(dutch$TOX$toxnames),"Vomiting","Nausea"),]})
names(files) = sapply(files, function(x) x$chemo[1])
files = files[order(names(files))]

grd12 = c("Vomiting : grade 1-2","Nausea : grade 1-2","Nausea/vomiting : grade 1-2")
grd34 = c("Vomiting : grade 3-4","Nausea : grade 3-4","Nausea/vomiting : grade 3-4")
for(i in names(files)) {
  f=files[[i]]
  if(any(grd12 %in% f$comb)) {
    max_row = f[f$comb %in% grd12,] %>% arrange(desc(prop)) %>% slice(1)
    max_row$comb = "Nausea/vomiting : grade 1-2"; max_row$tox="Nausea/vomiting"
    f %<>% filter(!comb %in% grd12) %>% rbind(max_row)
  }
  if(any(grd34 %in% f$comb)) {
    max_row = f[f$comb %in% grd34,] %>% arrange(desc(prop)) %>% slice(1)
    max_row$comb = "Nausea/vomiting : grade 3-4"; max_row$tox="Nausea/vomiting"
    f %<>% filter(!comb %in% grd34) %>% rbind(max_row)
  }
  
  f$tox = factor(f$tox, names(dutch$TOX$toxnames))
  f$grade = factor(f$grade)
  
  files[[i]] = f[order(f$tox, f$grade),]
}
saveRDS(files, "data/toxicity.rds")