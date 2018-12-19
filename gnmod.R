setwd("~/uni/statml/project/")
source("setup.R")


gnpredsetup()
source("forests.R")
source("tree.R")
source("multinom.R")

methodv =as.data.frame( expand.grid(c("rungnforest","rungnmultinom","rungntree"), c("rf","mn", "mean", "natoinf"),c(T), stringsAsFactors = F))
methodv=methodv[order(methodv[,1],methodv[,2],methodv[,3]),]
mn<<-methodv[1,]


registerDoParallel(cl)


# for (i in 1:length(rownames( methodv))) {
# 	mn=methodv[i,]
# 	a <- foreach(1:10, .packages=c("dplyr", "glmnet", "nnet","reshape2","randomForest", "rpart"), .combine=rbind) %dopar% {
# 		if ("rungnforest"==mn[1]) {
# 			rungnforest(as.character(mn[2]),as.logical(mn[3]))
# 		}
# 		if ("rungnmultinom"==mn[1]) {
# 			rungnmultinom(as.character(mn[2]),as.logical(mn[3]))
# 		}
# 		if ("rungntree"==mn[1]) {
# 			rungntree(as.character(mn[2]),as.logical(mn[3]))
# 		}
# 	}
# 	savegnres(a)
# }
stopCluster(cl)


for (j  in 1:10) {

for (i in 1:length(rownames( methodv))) {
	mn=methodv[i,]
	print("#################################################################")
	print(mn)
	print("#################################################################")
	
	if ("rungnforest"==mn[1]) {
		savegnres(rungnforest(as.character(mn[2]),as.logical(mn[3])))
	}
	if ("rungnmultinom"==mn[1]) {
		savegnres(rungnmultinom(as.character(mn[2]),as.logical(mn[3])))
	}
	if ("rungntree"==mn[1]) {
		savegnres(rungntree(as.character(mn[2]),as.logical(mn[3])))
	}
	# mn=methodv[i,]
	# a <- foreach(1:1, .packages=c("dplyr", "glmnet", "nnet","reshape2","randomForest", "rpart"), .combine=rbind) %dopar% {
	# 	print(mn)
	# }
	# print(a)
}
	
}


mn=c("rungnforest","mean",TRUE)

# restab=
# for (i in 1:length(rownames( methodv))) {
# 	mn=methodv[i,]
# 	restab=rbind(restab, savedres %>% filter(method==mn[1] AND nas.in.test==mn[1] ))
# }

for (i in 1:nrow(savedres)){print(all(rowSums( savedres$mcm[[i]])==1293))}

methodv2 =as.data.frame( expand.grid(c("forest","multinom","tree"), c("rf","mn", "mean", "natoinf"),c(T), stringsAsFactors = F))
methodv2=methodv2[order(methodv2[,-1],methodv2[,2],methodv2[,3]),]

restab=data.frame()
for (i in 1:length(rownames( methodv2))) {
	mn=methodv2[i,]
	r=savedres %>% filter(as.character(method)==as.character(mn[1]) & as.character(nanmethod)==as.character(mn[2]) & as.logical(nas.in.test)== as.logical(mn[3]) )
	if (nrow(r)>0) {
		exres = extractres(r)
		restab=rbind(restab, cbind(mn[1:2], exres[4:7]))	
	}
}

restab=restab[order(restab$Var1, decreasing = T),]
rownames(restab)=c()
xtable(restab, digits = 3,include.rownames=FALSE)


