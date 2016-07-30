setwd("/Users/andrewbertoli/Dropbox/United Government/BuildData")

data=read.csv("Election_Data_Updated.csv")

data$IdeologyDifference=data$LeaderIdeologyScore-data$RunnerUpIdeologyScore

dems=data[data$Democracy==1&is.na(data$Z)==FALSE&is.na(data$IdeologyDifference)==FALSE,]
dems=dems[-which(abs(dems$IdeologyDifference)<2),]

dems$Z=dems$Z*(2*as.numeric(dems$IdeologyDifference>0)-1)

dems$T=as.numeric(dems$Z>0)

close=dems[abs(dems$Z)<=0.04,]

outcomes=c("MedHighDisputes","HighDisputes","AllMedHighDisputes","AllHighDisputes","RevAggression")

t_test_results=matrix(0,nrow=length(outcomes),ncol=3)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T)
t_test_results[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[1],-output$conf.int[2])}



theme_nolegend <- function (base_size = 9, base_family = "", height, width) 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          legend.position="none", 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA,colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80",  colour = "grey50"), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"))
}

cd <- as.data.frame(matrix(NA,length(outcomes),5))
conditions <- c("All Disputes Initiated","All High-Level Disputes Initiated","All Total Disputes","All Total High-Level Disputes","All Revisionist Disputes")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- t_test_results[,1]
cd$lower <- t_test_results[,2]
cd$upper <- t_test_results[,3]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])
# make the graph
library(ggplot2)

f <- ggplot(cd, 
            aes(x=mean,y=measure,color=measure))
plot3 <- f+geom_vline(xintercept=0, linetype="longdash")+

  geom_errorbarh(aes(xmax =  upper, 
                     xmin = lower),
                 size=1.5, height=0)+
  geom_point(stat="identity",size=4,fill="white")+
  xlab("Estimated Treatment Effect")+ylab("")+ labs(title="") +  theme_nolegend() +theme(axis.text=element_text(size=10),axis.title=element_text(size=13),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+xlim(-1,1) # Aggression During Term

ggsave("AggressionPlotIdeo.pdf",width=4,height=2,scale = 2.6)
















# Test for change in presidents

data2=data[data$WinnerIncumbent==1|data$RunnerUpIncumbent==1,]
data2=data2[data2$Democracy==1&abs(data2$Z)<0.04,]

t.test(abs(Aggression-PreviousAggression)~WinnerIncumbent,data2)
t.test(abs(AllMIDs-PreviousAllMIDs)~WinnerIncumbent,data2)