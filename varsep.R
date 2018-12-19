setwd("~/uni/statml/project/")
source("setup.R")
gnpredsetup()

groupnames=levels(dbpredgname$gname)

g1="ISIL"
g2="Taliban"

dbsep =dbpredgname %>%filter( gname %in% c(g1,g2)) %>% mutate(gname=factor(gname)) 
fit=rpart(formula = formula(gname~.),dbgntrain, method = "class", control = rpart.control(maxdept=1, cp = 0.001))