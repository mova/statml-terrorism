
setwd("~/uni/statml/project/")
source("setup.R")
library("randomForest")

dbpredgname = db  %>%
	select(c(imonth, iday, success, suicide, attacktype1_txt,nperps, targtype1_txt,gname, weaptype1_txt,weapsubtype1_txt,nkill, nwound, nkillter))
dbpredgname=dbpredgname[!apply(is.na(dbpredgname), 1, any),]
top.groups.attacks=names(summary(dbpredgname$gname)[2:7])
dbpredgname = dbpredgname %>%filter( gname %in% top.groups.attacks) %>%
	mutate(gname=factor(gname)) 

### !! 

for (col in colnames(dbpredgname)) {
	v=dbpredgname[,col]
	s=summary(dbpredgname[,col])
	#if there are to many NAs then convert the variable to a know/unknown factor
	if (!is.factor(v)) {
		if (mean(is.na(v))>.2) {
			dbpredgname[,col]= factor(ifelse(!is.na(v),"known", "unknown"))
		}
	}
	else{
		#select all levels the are less than 200 attacks
		owfactors=names(summary(dbpredgname[,col])[summary(dbpredgname[,col])<400])
		levels(dbpredgname[,col])<- sapply(levels(v),function(x) ifelse(x %in% owfactors,"Other",x))
		rm(owfactors)
	}
}


min.attacks=min(summary(dbpredgname$gname))

### We need a training and a testing set
# same size for each group and set to avoid fitting for the dominant group (test sample) and wrong mi
# independent
# Problem:attacks very unevenly distributed over the groups
# smallest group determines training sample size
gnprsample <- function() {
	dbgntrain<<-dbpredgname %>% filter(F)
	dbgntest<<-dbpredgname %>% filter(F)
	for (gn in levels(dbpredgname$gname)) {
		dbfil=dbpredgname[dbpredgname$gname==gn,]
		n=floor(min.attacks/2)
		trainidx=sample(1:nrow(dbfil),size=n)
		dbgntrain<<-rbind(dbgntrain, dbfil[trainidx,])
		
		dbfil=dbfil[-trainidx,]
		testidx=sample(1:nrow(dbfil),size=n)
		dbgntest<<-rbind(dbgntest, dbfil[testidx,])
	}
	rm(gn,dbfil,n,trainidx,testidx)	
}

rungnforest <- function() {
	gnprsample()
	#model <- randomForest(y=dbgntrain$gname,x=dbgntrain,xtest=dbgntest,ytest=dbgntest$gname ,na.action = na.omit)
	forest <- randomForest(gname~., data=dbgntrain, ntree=100, nodesize=10, importance=T, na.action = na.omit)
	
	summary(forest)
	#rowSums(r$confusion)
	plot(forest)
	dbgntest==predict(r, dbgntest)
	
	
	forest.pred=predict(forest, dbgntest)
	
	s2=summary(gntree)
	s1=summary(gntree.pruned)
	mcm=missclassmat(dbgntest$gname,gntree.pred)
	
	#savedres<-data.frame(method="glm", mcm=I(list(mcm)), mcerror=mcmerror(mcm), parameters=I(list(gntree$call)) , summary1=I(list(s1)), summary2=I(list(s2)) ) %>% filter(F)
	savegnres(data.frame(method="tree", mcm=I(list(mcm)), mcerror=mcmerror(mcm), parameters=I(list(gntree$call)) , summary1=I(list(s1)), summary2=I(list(s2)) ))
}

rungnforest()
