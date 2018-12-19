rungnmultinom <- function(nanmethod,nas.in.test) {
	dbgnsample=gnprsample(nanmethod,nas.in.test)
	dbgntrain<-dbgnsample[[1]]
	dbgntest<-dbgnsample[[2]]
	
	if (any(is.na(dbgntrain))) {
		stop("NAs in Training before main fit.")
	}
	if ((!nas.in.test) && any(is.na(dbgntrain))) {
		stop("NAs in Test despite prediction")
	}
	gnmultinomfit <- multinom(formula = formula(gname~.),dbgntrain, family="binomial")#, softmax=TRUE)
	gnmultinomfit.pred=predict(gnmultinomfit, dbgntest)
	mcm=missclassmat(dbgntest$gname,gnmultinomfit.pred)
	s1=summary(gnmultinomfit)
	return(data.frame(method="multinom", nanmethod=nanmethod,nas.in.test=nas.in.test, mcm=I(list(mcm)), mcerror=mcmerror(mcm), parameters=I(list(gnmultinomfit$call)) , summary1=I(list(s1)), summary2=NA))

}


