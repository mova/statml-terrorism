

setwd("~/uni/statml/project/")
source("setup.R")
db=db %>% select(c(iyear,success,longitude,latitude, suicide, nperps, claimed, nkill, nwound))
timedy=data.frame() # db %>% filter(F)

for (y in c(1970:1992,1994:2017)) {
	sel= db %>% filter(iyear==y)
	#sel=sel%>% mutate_each(funs(function(x) x/nrow(sel)), c("success","suicide", "claimed"))
	#sel[,c("success","suicide", "claimed")]=sel[,c("success","suicide", "claimed")]/length(sel)
	rmsloc=rms(sample(na.omit(sel$longitude), 400)-sample(na.omit(sel$longitude), 400))+rms(sample(na.omit(sel$latitude), 400)-sample(na.omit(sel$latitude), 400))
	sel=sel %>% select(-c(longitude,latitude))
	
	v=as.data.frame(t( apply(sel, 2, function(x) sum(x, na.rm = T))))
	v[,c("success","suicide", "claimed")]=v[,c("success","suicide", "claimed")]/nrow(sel)
	v=cbind(v,rmsloc)
	v$iyear=y
	if (nrow(timedy)==0) {
		timedy<<-v
	}
	else {
		timedy<<-rbind(timedy, v  )		
	}

}

### stolen
normalit<-function(m){
	(m - min(m))/(max(m)-min(m))
}
timedy=timedy %>%  mutate_each(funs(normalit), nkill, nwound, nperps, rmsloc)

pdf("doc/plots/time.pdf", width = 6, height = 3)
ggplot(melt(timedy%>% select(-nperps,-nwound) ,id="iyear")  , aes(x=iyear,  y=value, colour=variable))+
	geom_line()

dev.off()
