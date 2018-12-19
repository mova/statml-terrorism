
library(dplyr)
library(GGally)
# library(OpenStreetMap)
# library(gdata)
library(knitr)
library(xtable)
library(glmnet)
library(lattice)
library(RColorBrewer)
library(gplots)
library(reshape2)
library("randomForest")
library(rpart)
library(nnet)


library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer



mycolorpalette = colorRampPalette(brewer.pal(8, "BuGn"))(25)



setwd("~/uni/statml/project/")
#dbfull= read.csv("input/gtd/globalterrorismdb_0718dist.csv") 
load("input/gtdfull.Rda")
db <- dbfull%>%
	#filter(region_txt %in% c("Eastern Europe","Western Europe", "North America" )  ) %>%
	#filter(iyear>=2000 ) %>%
	#filter(!is.na(longitude)) %>%
	#filter(!is.na(latitude)) %>% 
	select(c(iyear, imonth, iday,longitude,latitude, country_txt, provstate, city, latitude, longitude,summary, gname, success, suicide, attacktype1_txt, attacktype2_txt, targtype1_txt, natlty1_txt, nperps,claimed,weaptype1_txt,weapsubtype1_txt,nkill, nwound, nkillter))%>%
	mutate(gname = recode_factor(gname, "Islamic State of Iraq and the Levant (ISIL)" = "ISIL", "Shining Path (SL)" = "Shining Path","Irish Republican Army (IRA)" ="IRA", "Revolutionary Armed Forces of Colombia (FARC)"="FARC", "Farabundo Marti National Liberation Front (FMLN)"="FMLN","New People's Army (NPA)"="NPA", "Communist Party of India - Maoist (CPI-Maoist)"="CPI" ))
db$nperps[db$nperps==-99]<-NA 
db$claimed[db$claimed==-9]<-NA 


gnpredsetup <- function() {
	dbpredgname = db  %>%
		select(c(imonth, iday, success, suicide, attacktype1_txt,nperps, targtype1_txt,gname, weaptype1_txt,weapsubtype1_txt,nkill, nwound)) #, nkillter
	#dbpredgname=dbpredgname[!apply(is.na(dbpredgname), 1, any),]
	dbpredgname=dbpredgname[apply(is.na(dbpredgname), 1, sum)<4,]
	top.groups.attacks<<-names(summary(dbpredgname$gname)[2:7])
	dbpredgname =dbpredgname %>%filter( gname %in% top.groups.attacks) %>%
		mutate(gname=factor(gname)) 

	for (col in colnames(dbpredgname)) {
		v=dbpredgname[,col]
		s=summary(dbpredgname[,col])
		#if there are to many NAs then convert the variable to a know/unknown factor
		if (!is.factor(v)) {
			if (mean(is.na(v))>.5) {
				#dbpredgname[,col]= factor(ifelse(!is.na(v),"known", "unknown"))
			}
		}
		else{
			#select all levels the are less than 200 attacks
			owfactors=names(summary(dbpredgname[,col])[summary(dbpredgname[,col])<600])
			levels(dbpredgname[,col])<- sapply(levels(v),function(x) ifelse(x %in% owfactors,"Other",x))
			rm(owfactors)
		}
	}
	dbpredgname<<-dbpredgname
}

gnprsample <- function(method, nas.in.test) {
	### We need a training and a testing set
	# same size for each group and set to avoid fitting for the dominant group (test sample) and wrong mi
	# independent
	# Problem:attacks very unevenly distributed over the groups
	# smallest group determines training sample size
	min.attacks<<-min(summary(dbpredgname$gname))
	dbgntrain=dbpredgname %>% filter(F)
	dbgntest=dbpredgname %>% filter(F)
	for (gn in levels(dbpredgname$gname)) {
		dbfil=dbpredgname[dbpredgname$gname==gn,]
		n=floor(min.attacks/2)
		trainidx=sample(1:nrow(dbfil),size=n)
		dbgntrain=rbind(dbgntrain, dbfil[trainidx,])
		
		dbfil=dbfil[-trainidx,]
		testidx=sample(1:nrow(dbfil),size=n)
		dbgntest=rbind(dbgntest, dbfil[testidx,])
	}
	rm(gn,dbfil,n,trainidx)	
#	return(guesnas(method, nas.in.test,dbgntrain,dbgntest))
# }
# 
# 
# 
# guesnas <- function(method, nas.in.test,dbgntrain,dbgntest) {
	vv <- colSums( is.na(dbgntrain))
	vv <- vv[vv>0]
	changed.cols<- names(vv)
	print("########################## Start dbgntrain NA prediction ####")
	while (length(vv)>0) {
		curcol=which.min(vv)
		curcolname=names(curcol)
		
		## This assumes there is enough other rows with out any NA's
		trainingrows= apply(!is.na( dbgntrain ), 1, all)
		rows.to.predict= is.na( dbgntrain[,curcolname] )
		
		repdf=dbgntrain %>% select(-c(names(vv[-curcol])))
		
		trainnaset <- repdf[trainingrows,]
		prednaset <- repdf[rows.to.predict,]
		
		dbgntrain[rows.to.predict,curcolname]=predna(method,curcolname,trainnaset,prednaset)

		vv<-vv[-curcol]
	}
	if (any(is.na(dbgntrain))) {
		stop("NAs in Training after prediction fit.")
	}
	print("########################## End dbgntrain NA prediction ####")
	if (!nas.in.test) {
		print("########################## Start dbgntest NA prediction ####")
		if (any(is.na(dbgntrain))) {
			stop("NAs in Training before Test prediction fit.")
		}
		for (curcolname in colnames(dbgntest[apply(is.na(dbgntest),2,any)])) {
			rows.to.predict= is.na( dbgntest[,curcolname] )
			print(summary(rows.to.predict))
			prednaset <- dbgntest[rows.to.predict,]
			print(summary(prednaset))
			print(summary(dbgntrain))
			pd=predna(method,curcolname,dbgntrain,prednaset)
			dbgntest[rows.to.predict,curcolname]=pd
		}
		print("########################## End dbgntest NA prediction ####")
	}
	return(list(dbgntrain,dbgntest))
}

predna <- function(method,curcolname,trainnaset,prednaset) {
	
	if(method=="rf") {
		fit=randomForest(as.formula(paste(curcolname, "~.-gname")),data=trainnaset, ntree=100, nodesize=10)
		return(predict(fit,prednaset ))
	}
	else if (method=="mn") { 
		fit=multinom(as.formula(paste(curcolname, "~.-gname")),data=trainnaset, family="binomial",MaxNWts =1000000,linout=TRUE)
		return(predict(fit,prednaset))
	}
	else if(method=="mean"){
		return(rep(mean(trainnaset[,curcolname],na.rm = T), nrow(prednaset)))
	}
	else if(method=="natoinf"){
		return(rep(-99, nrow(prednaset)))
	}
}


missclassmat <- function(truev,pred) {
	if (length(pred)!=length(truev)) {
		print("not the same length")
		return()
	}
	n=length(levels(truev))
	missclasstable=matrix(0,nrow = n,ncol = n)
	rownames(missclasstable) = levels(truev)
	colnames(missclasstable) = levels(truev)
	
	for (i in 1:length(truev)) {
		missclasstable[truev[i],pred[i]]=missclasstable[truev[i],pred[i]]+1
	}
	return(missclasstable)
	rm(missclasstable,i)
}


mcmerror <- function(mcm) {
	return(1-sum(diag(mcm))/sum(mcm))
}


##Initialisation
#savedres=gnpr.results=data.frame(method=character(), mcm=I(list()), mcerror=numeric(), parameters=I(list()),summary=I(list()),summary2=I(list()))
#savedres<-savedres %>% filter(F)
#save(savedres, file = "gnpred.Rda")
#savedres=savedres[savedres$method!="multinom-mn",]
load("gnpred.Rda" )
savegnres <- function(a) {
	load("gnpred.Rda",new.env() )
	savedres<<- rbind(savedres, a)
	save(savedres, file = "gnpred.Rda")
}

rms <- function(x) {
	return(sqrt(mean(x^2, na.rm=T)))
}

##### A lot to slow
# distmean <- function(x) {
# 	lenv=c()
# 	restx=x
# 	for (i in 1:(length(x)-1)) {
# 		elem = x [i]
# 		if(length(restx)>0){
# 			restx=restx[2:length(restx)]
# 			for(elem2 in restx) {
# 				lenv=c(lenv,rms(elem- elem2))
# 			}
# 		}
# 	}
# 	return(mean(lenv))
# }


extractres <- function(m) {
	mnres=m
	mcmmean=apply(simplify2array( mnres$mcm), c(1,2), mean)
	mnres.mcmsd=apply(simplify2array( mnres$mcm), c(1,2), sd)
	return(list(
	mnres=mnres,
	mcmmean=mcmmean,
	mnres.mcmsd=mnres.mcmsd,
	meanerror=mean(mnres$mcerror),
	meanerrorsd=sd(mnres$mcerror),
	asymmetry=rms(mcmmean-t(mcmmean)),
	deviance=rms(mnres.mcmsd)
	))
}

plotmat <- function(mcm) {
	longData<-melt(mcm)
	longData<-longData[longData$value!=0,]
	ggplot(longData, aes(x = Var2, y = Var1)) +
		geom_raster(aes(fill=value))+
		scale_fill_gradient(low="#F7FCFD",high="#005824")+
		xlab("Group")+ ylab("Predicted Group")
}
