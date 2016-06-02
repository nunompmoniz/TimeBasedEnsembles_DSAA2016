###############################
# EXAMPLE OF MEAN PROPORTION OF AVAILABLE DATA

library(scales)
library(dplyr)
library(grid)

analysis <- read.csv("NewData/Analysis.csv")
analysis$X <- NULL
analysis.obama <- analysis[analysis$IDLink %in% links[links$Topic=="obama",]$IDLink,]

proportion <- analysis.obama

prop.tbl <- data.frame(Timeslice=0,Proportion=0)

for(i in 1:144) {
  aux <- proportion[proportion[,(i+1),]>=0,]
  aux <- mean(aux[,(i+1)]/aux$TS144,na.rm=TRUE)
  row <- data.frame(Timeslice=i,Proportion=aux)
  prop.tbl <- rbind(prop.tbl,row)
}

prop.tbl.rare <- data.frame(Timeslice=0,Proportion=0)

proportion <- proportion[proportion$TS144>=min(boxplot.stats(proportion$TS144,coef=3)$out),]

for(i in 1:144) {
  aux <- proportion[proportion[,(i+1),]>=0,]
  aux <- mean(aux[,(i+1)]/aux$TS144,na.rm=TRUE)
  row <- data.frame(Timeslice=i,Proportion=aux)
  prop.tbl.rare <- rbind(prop.tbl.rare,row)
}

proportion.plot <- ggplot(prop.tbl,aes(x=Timeslice,y=Proportion)) + geom_line(size=1) + expand_limits(y=0) + geom_line(aes(x=prop.tbl.rare$Timeslice,y=prop.tbl.rare$Proportion),linetype="dashed")

pdf("proportion.pdf",width=6,height=4)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
print(proportion.plot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
dev.off()

#####################
#EXAMPLE OF AUTOMATICALLY GENERATED RELEVANCE FUNCTIONS

library(grid)

df <- read.csv("NewData/Df_ExampleRelFunc.csv")

frequency.plot <- ggplot(df, aes(x = Var1, y = Freq)) + 
  geom_point() +
  labs(x = "\nNr. of Tweets", y = "Frequency\n") + 
  geom_vline(xintercept=48,linetype="dashed") + 
  scale_x_continuous(limits=c(0,550),breaks=c(0,100,200,300,400,500)) + 
  scale_y_continuous(limits=c(0,2400),breaks=seq(0,2400,by=400))


relevance.plot <- ggplot(tbl, aes(x = tpt, y = phi)) + 
  geom_line(size=1) +
  labs(x = "\nNr. of Tweets", y = "Relevance\n") + 
  geom_vline(xintercept=48,linetype="dashed") + 
  scale_x_continuous(limits=c(0,550),breaks=c(0,100,200,300,400,500)) + 
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2))

pdf("samplerel.pdf",width=10,height=4)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(frequency.plot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(relevance.plot,vp=viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()


