rungnforest <- function(nanmethod,nas.in.test) {
	dbgnsample=gnprsample(nanmethod,nas.in.test)
	dbgntrain<-dbgnsample[[1]]
	dbgntest<-dbgnsample[[2]]
	
	if (any(is.na(dbgntrain))) {
		stop("NAs in Training before main fit.")
	}
	if ((!nas.in.test) && any(is.na(dbgntrain))) {
		stop("NAs in Test despite prediction")
	}
	forest <- randomForest(formula(gname~.), data=dbgntrain, ntree=100, nodesize=10, importance=T)#, na.action = na.omit)
	forest.pred=predict(forest, dbgntest)
	
	s2=NA
	s1=forest$forest
	mcm=missclassmat(dbgntest$gname,forest.pred)

	return(data.frame(method="forest",
 nanmethod=nanmethod,nas.in.test=nas.in.test, mcm=I(list(mcm)), mcerror=mcmerror(mcm), parameters=I(list(forest$call)) , summary1=I(list(s1)), summary2=I(list(s2)) ))
}


# longData<-melt(forest$importance[,1:6])
# longData<-longData[longData$value!=0,]
# 
#
# pdf("doc/plots/forest-importance.pdf", width = 6, height = 4)
# ggplot(longData, aes(x = Var2, y = Var1)) +
# 	geom_raster(aes(fill=value))+
# 	scale_fill_gradient(low="#F7FCFD",high="#005824")+
# 	xlab("Group")+ ylab("Predicted Group")
# dev.off()


#pdf("doc/plots/forest-sim.pdf", width = 6, height = 4)
#plotmat(res$mcmmean-diag(diag(res$mcmmean)))
#dev.off()
