rungntree <- function(nanmethod,nas.in.test) {
	dbgnsample=gnprsample(nanmethod,nas.in.test)
	dbgntrain<-dbgnsample[[1]]
	dbgntest<-dbgnsample[[2]]
	
	if (any(is.na(dbgntrain))) {
		stop("NAs in Training before main fit.")
	}
	if ((!nas.in.test) && any(is.na(dbgntrain))) {
		stop("NAs in Test despite prediction")
	}
	gntree=rpart(formula = formula(gname~.),dbgntrain, method = "class", control = rpart.control(cp = 0.001))
	gntree.pred=predict(gntree, dbgntest,type = "class")
	
	bestcp <- gntree$cptable[which.min(gntree$cptable[,"xerror"]),"CP"]
	gntree.pruned=rpart::prune(gntree, cp=bestcp)
	
	s2=summary(gntree)
	s1=summary(gntree.pruned)
	mcm=missclassmat(dbgntest$gname,gntree.pred)
	return(data.frame(method="tree",
 nanmethod=nanmethod,nas.in.test=nas.in.test, mcm=I(list(mcm)), mcerror=mcmerror(mcm), parameters=I(list(gntree$call)) , summary1=I(list(s1)), summary2=I(list(s2)) ))
}


