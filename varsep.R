setwd("~/uni/statml/project/")
source("setup.R")

gnpredsetup()
colnames(dbpredgname)=c("month", "day","success","suicide" , "attack-type","number-terrorists","target-type", "gname","weapon-type","weapon-subtype","number-killed","number-wounded")

groupnames=levels(dbpredgname$gname)
groupmat=expand.grid(groupnames, groupnames, stringsAsFactors = F)

bestsplit <- function(g1,g2) {
	dbsep =dbpredgname %>%filter( gname %in% c(g1,g2)) %>% mutate(gname=factor(gname))
	fit=rpart(formula = formula(gname~.),dbsep, method = "class", control = rpart.control(maxdept=2, cp = 0.001))
	return(names(which.max(fit$variable.importance)))
}

bestsepv=c(character())
for (i in 1:nrow(groupmat)) {
	g1=groupmat[i,1]
	g2=groupmat[i,2]
	if (g1!=g2) {
		r=as.character(bestsplit(g1,g2 ))
	}
	else{
		r=" "
	} 
	bestsepv=c(bestsepv,r)#,paste(g1,g2,r))
	##why do i need this paste to get the right vector
}

missclasssplit=cbind(groupmat, bestsepv)


methodv2 =as.data.frame( expand.grid(c("forest","multinom","tree"), c("rf","mn", "mean", "natoinf"),c(T), stringsAsFactors = F))
methodv2=methodv2[order(methodv2[,-1],methodv2[,2],methodv2[,3]),]


mn=c("forest","mean",T)
r=savedres %>% filter(as.character(method)==as.character(mn[1]) & as.character(nanmethod)==as.character(mn[2]) & as.logical(nas.in.test)== as.logical(mn[3]) )
if (nrow(r)>0) {
	exres = extractres(r)	
}
mcm=exres$mcmmean


longData<-melt(mcm-diag(diag(mcm)))
longData<-longData[longData$value!=0,]
colnames(missclasssplit)=c("Var1","Var2","split")

longData=longData[order(longData[,1],longData[,2]),]
missclasssplit=missclasssplit[order(missclasssplit[,1],missclasssplit[,2]),]
missclasssplit=missclasssplit %>% filter(Var1!=Var2)

pdf("doc/plots/forest-sim.pdf", width = 11, height = 5)
ggplot(merge(longData,missclasssplit ), aes(x = Var2, y = Var1, label=split)) +
	geom_raster(aes(fill=value))+
	scale_fill_gradient(low="#F7FCFD",high="#005824")+
	xlab("Group")+ ylab("Predicted Group")+
	geom_text()+ theme(text = element_text(size=16),legend.position="none")

dev.off()


############ Importance Plot
nanmethod="mean"
nas.in.test=T
levels(dbpredgname$weapon.subtype) <-c("","Other","Rifle","Handgun","Landmine","Projectile","Suicide","Unknown Explosion","Unknown Gun","Vehicle")
#dbgnsample=gnprsample(nanmethod,nas.in.test)

if (any(is.na(dbgntrain))) {
	stop("NAs in Training before main fit.")
}
if ((!nas.in.test) && any(is.na(dbgntrain))) {
	stop("NAs in Test despite prediction")
}
forest <- randomForest(formula(gname~.), data=dbpredgname, ntree=100, nodesize=10, importance=T)#, na.action = na.omit)

longData<-melt(forest$importance[,1:6])
longData<-longData[longData$value!=0,]


pdf("doc/plots/forest-importance.pdf", width = 7, height = 2.5)
ggplot(longData, aes(y = Var2, x = Var1)) +
	geom_raster(aes(fill=value))+
	scale_fill_gradient(low="#F7FCFD",high="#005824")+
	xlab("Group")+ ylab("Predicted Group")+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10), text = element_text(size=12),axis.title.x=element_blank(),axis.title.y=element_blank(), legend.position="none")
dev.off()
#############################
pdf("doc/plots/gname-weapon.pdf", width = 6, height = 4)
ggpairs(dbpredgname%>% select(gname, "weapon-type"))

colnames(dbpredgname)=c("month", "day","success","suicide" , "attack.type","number.terrorists","target.type", "gname","weapon.type","weapon.subtype","number.killed","number.wounded")

pdf("doc/plots/weapontype.pdf", width = 4, height = 4)
ggplot(dbpredgname%>% select("gname", "weapon.type"), aes(x=weapon.type, fill=gname )) +
	geom_bar()+scale_fill_brewer(palette="Dark2")
dev.off()	

pdf("doc/plots/weaponsubtype.pdf", width = 7, height = 4.5)
ggplot(dbpredgname%>% select("gname", "weapon.subtype"), aes(x=weapon.subtype, fill=gname )) +
	geom_bar()+scale_fill_brewer(palette="Dark2")+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10), text = element_text(size=12),axis.title.x=element_blank(), legend.title = element_blank())
dev.off()
