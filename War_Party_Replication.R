library(rdrobust)
require(ggplot2)
require(RCurl)
require(devtools)
source_url("https://raw.githubusercontent.com/AndrewBertoli/Natural-Experiments/master/RDPlot.R")


setwd("/Users/andrewbertoli/Dropbox/Electoral-RDs/2Empirics/Data")

data=read.csv("Election_Data_Updated.csv")

data$Z=(data$PresVotes - data$PresSecondVotes)/(data$PresVotes + data$PresSecondVotes)

setwd("/Users/andrewbertoli/Dropbox/Electoral-RDs/1Drafts/WarParty")

data$IdeologyDifference=data$LeaderIdeologyScore-data$RunnerUpIdeologyScore

dems=data[data$Democracy==1&is.na(data$Z)==FALSE&is.na(data$IdeologyDifference)==FALSE&is.na(data$DisputesInitiated)==FALSE,]
dems=dems[-which(abs(dems$IdeologyDifference)<2),]


dems$Z=dems$Z*(2*as.numeric(dems$IdeologyDifference>0)-1)

dems$T=as.numeric(dems$Z>0)


pdf("ForcingDensityIdeo.pdf", height=4, width=5)
m <- ggplot(dems, aes(x=Z*50))
m + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -50.00001)+
  theme_bw()+theme(axis.title = element_text(size=16),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percent of Votes That Right-Wing\nCandidate Was from Winning")+
  ylab("Density") + labs(title="Ideology")+
  scale_x_continuous(breaks=seq(-20, 20, 5),labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"))+
  ylim(0,20)
dev.off()







close=dems[abs(dems$Z)<=0.04,]

close[close$Z>0,c("Country","Year","DisputesInitiated","AllDisputes")][order(close[close$Z>0,]$Year),]

close[close$Z<0,c("Country","Year","DisputesInitiated","AllDisputes")][order(close[close$Z<0,]$Year),]


scale_number=mean(close$Days)/365
dems$DisputesInitiated=dems$DisputesInitiated*scale_number
dems$HighDisputesInitiated=dems$HighDisputesInitiated*scale_number
dems$AllDisputes=dems$AllDisputes*scale_number
dems$AllHighDisputes=dems$AllHighDisputes*scale_number
dems$RevisionistDisputes=dems$RevisionistDisputes*scale_number

close=dems[abs(dems$Z)<=0.04,]

outcomes=c("DisputesInitiated","HighDisputesInitiated","AllDisputes","AllHighDisputes","RevisionistDisputes")

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
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes","All Revisionist Disputes")
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
  xlab("Estimated Treatment Effect")+ylab("")+ labs(title="") +  theme_nolegend() +theme(axis.text=element_text(size=10),axis.title=element_text(size=13),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+xlim(-5,5) # Aggression During Term

ggsave("AggressionPlotIdeo.pdf",width=4,height=2,scale = 1.6)

rdrobust(dems$DisputesInitiated,dems$Z)

bandwidth=rdbwselect(dems$DisputesInitiated,dems$Z)[[3]][[1]]

pdf("IdeologyRDGraph.pdf",width=5,height=5)
RDPlot(dems$Z,dems$DisputesInitiated,Bandwidth=bandwidth,xlab="Win-Loss Margin for Right-Wing Candidate",ylab="Militarized Disputes Initiated",Main="",Tick.Marks = seq(-.2,.2,by=0.05),Labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"),ylim=c(-0.5,4),Breaks=seq(-0.3,0.3,by=0.03),xlim=c(-0.15,0.15))
dev.off()






# Incumbency

dems=data[data$Democracy==1,]
dems=dems[is.na(dems$Z)==FALSE&is.na(dems$DisputesInitiated)==FALSE,]
dems=dems[dems$WinnerPartyInc==1|dems$RunnerUpPartyInc==1,]
dems=dems[-which(dems$WinnerPartyInc==1&dems$RunnerUpPartyInc==1),]

dems$Z=-dems$Z*(2*as.numeric(dems$WinnerPartyInc-dems$RunnerUpPartyInc>0)-1)

dems$T=as.numeric(dems$Z>0)

dems$AbsoluteChangeDisputesInitiated=with(dems,abs(DisputesInitiated-PreviousTermDisputesInitiated))
dems$AbsoluteChangeHighDisputesInitiated=with(dems,abs(HighDisputesInitiated-PreviousTermHighDisputesInitiated))
dems$AbsoluteChangeAllDisputes=with(dems,abs(AllDisputes-AllPreviousTermDisputes))
dems$AbsoluteChangeAllHighDisputes=with(dems,abs(AllHighDisputes-AllPreviousTermHighDisputes))
dems$AbsoluteChangeRevisionistDisputes=with(dems,abs(RevisionistDisputes-PreviousTermRevisionistDisputes))


pdf("ForcingDensityInc.pdf", height=4, width=5)
m <- ggplot(dems, aes(x=-Z*50))
m + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -50.00001)+
  theme_bw()+theme(axis.title = element_text(size=16),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percent of Votes That Candidate from\nIncumbent Party Was from Winning")+
  ylab("Density") + labs(title="Incumbency")+
  scale_x_continuous(breaks=seq(-20, 20, 5),labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"))+  ylim(0,20)
dev.off()




close=dems[abs(dems$Z)<=0.04,]

close[close$Z>0,c("Country","Year","AbsoluteChangeDisputesInitiated","AbsoluteChangeAllDisputes")][order(close[close$Z>0,]$Year),]

close[close$Z<0,c("Country","Year","AbsoluteChangeDisputesInitiated","AbsoluteChangeAllDisputes")][order(close[close$Z<0,]$Year),]

scale_number=mean(close$Days)/365
dems$AbsoluteChangeDisputesInitiated=dems$AbsoluteChangeDisputesInitiated*scale_number
dems$AbsoluteChangeHighDisputesInitiated=dems$AbsoluteChangeHighDisputesInitiated*scale_number
dems$AbsoluteChangeAllDisputes=dems$AbsoluteChangeAllDisputes*scale_number
dems$AbsoluteChangeAllHighDisputes=dems$AbsoluteChangeAllHighDisputes*scale_number
dems$AbsoluteChangeRevisionistDisputes=dems$AbsoluteChangeRevisionistDisputes*scale_number

close=dems[abs(dems$Z)<=0.04,]



outcomes=c("AbsoluteChangeDisputesInitiated","AbsoluteChangeHighDisputesInitiated","AbsoluteChangeAllDisputes","AbsoluteChangeAllHighDisputes","AbsoluteChangeRevisionistDisputes")

t_test_results=matrix(0,nrow=length(outcomes),ncol=3)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,alternative="greater")
t_test_results[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[2],-output$conf.int[1])}



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
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes","All Revisionist Disputes")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- t_test_results[,1]
cd$lower <- t_test_results[,3]
cd$upper <- rep(0,5)
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
  xlab("Estimated Treatment Effect")+ylab("")+ labs(title="") +  theme_nolegend() +theme(axis.text=element_text(size=10),axis.title=element_text(size=13),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+xlim(-5,5) # Aggression During Term

ggsave("AggressionPlotInc.pdf",width=4,height=2,scale = 1.6)




bandwidth=rdbwselect(dems$AbsoluteChangeDisputesInitiated,dems$Z)[[3]][[1]]

dems=dems[is.na(dems$AbsoluteChangeDisputesInitiated)==FALSE&abs(dems$Z)<0.2,]

pdf("IncumbentRDGraph.pdf")
RDPlot(dems$Z,dems$AbsoluteChangeDisputesInitiated,Bandwidth=bandwidth,xlab="Win/Loss Margin for Challenger Candidate",ylab="Absolute Change in Aggression",Main="",Tick.Marks = seq(-.2,.2,by=0.05),Labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"),ylim=c(-0.5,4),Breaks=seq(-0.3,0.3,by=0.03),xlim=c(-0.15,0.15))
dev.off()