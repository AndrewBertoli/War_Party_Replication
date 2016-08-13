library(reshape)
library(rdrobust)
library(cowplot)
require(ggplot2)
require(RCurl)
require(devtools)
source_url("https://raw.githubusercontent.com/AndrewBertoli/Natural-Experiments/master/RDPlot.R")


setwd("/Users/andrewbertoli/Dropbox/Electoral-RDs/2Empirics/Data")

data=read.csv("Election_Data_Updated.csv")

#data[data$Country=="Cyprus"&data$Year==1993,]$HighDisputesInitiated=0.40
#data[data$Country=="Cyprus"&data$Year==1993,]$DisputesInitiated=0.40
#data[data$Country=="Cyprus"&data$Year==1998,]$PreviousHighDisputesInitiated=0.40
#data[data$Country=="Cyprus"&data$Year==1998,]$PreviousTermHighDisputesInitiated=0.40
#data[data$Country=="Cyprus"&data$Year==1998,]$PreviousDisputesInitiated=0.40
#data[data$Country=="Cyprus"&data$Year==1998,]$PreviousTermDisputesInitiated=0.40

#data[data$Country=="Taiwan"&data$Year==2004,]$DisputesInitiated=0.25
#data[data$Country=="Taiwan"&data$Year==2004,]$PreviousHighDisputesInitiated=0.25
#data[data$Country=="Taiwan"&data$Year==2004,]$PreviousTermHighDisputesInitiated=0.25

#data$Z=(data$PresVotes - data$PresSecondVotes)/(data$PresVotes + data$PresSecondVotes)

setwd("/Users/andrewbertoli/Dropbox/Electoral-RDs/1Drafts/WarParty")

data$IdeologyDifference=data$LeaderIdeologyScore-data$RunnerUpIdeologyScore

dems_base=data[data$Democracy==1&is.na(data$Z)==FALSE&is.na(data$IdeologyDifference)==FALSE&is.na(data$DisputesInitiated)==FALSE,]
dems=dems_base[-which(abs(dems_base$IdeologyDifference)<2),]

dems$Z=dems$Z*(2*as.numeric(dems$IdeologyDifference>0)-1)

dems$T=as.numeric(dems$Z>0)


pdf("ForcingDensityIdeo.pdf", height=4, width=5)
m <- ggplot(dems, aes(x=Z*50))
m = m + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -50.00001)+
  theme_bw()+theme(axis.title = element_text(size=13),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percent of Votes That Right-Wing\nCandidate Was from Winning")+
  ylab("Density") + labs(title="Ideology")+
  scale_x_continuous(breaks=seq(-20, 20, 5),labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"))+
  ylim(0,20)
m  
dev.off()







close=dems[abs(dems$Z)<=0.04,]


# BalancePlot

covs=c("PreviousDisputesInitiated","PreviousHighDisputesInitiated","AllPreviousDisputes","AllPreviousHighDisputes","PreviousRevisionistDisputes","PreviousHighRevisionistDisputes", "irst", "milex", "milper", "pec", "tpop", "upop")

est=matrix(NA,ncol=3,nrow=length(covs))

for(i in 1:length(covs)){
output=t.test(dems[abs(dems$Z)<=0.04,covs[i]]~as.numeric(dems[abs(dems$Z)<=0.04,]$Z>0))
est[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[1],-output$conf.int[2])/sd(dems[,covs[i]],na.rm=TRUE)}


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

cd <- as.data.frame(matrix(NA,length(covs),5))
conditions <- c("Previous Disputes Initiated","Previous High-Level Disputes Initiated","All Previous Disputes","All Previous High-Level Disputes","Previous Revisionist Disputes", "Previous High-Level Revisionist Disputes", "Iron and Steel Production",  "Military Expenditures", "Military Personel",     "Energy Consumption",  "Total Population", "Urban Population")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- as.numeric(est[,1])
cd$lower <- as.numeric(est[,3])
cd$upper <- as.numeric(est[,2])
cd$ord <- c(length(covs):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])
# make the graph
library(ggplot2)

f <- ggplot(cd, 
            aes(x=mean,y=measure,color=measure))
balanceplot1 <- f+geom_vline(xintercept=0, linetype="longdash")+

  geom_errorbarh(aes(xmax =  upper, 
                     xmin = lower),
                 size=1.5, height=0)+
  geom_point(stat="identity",size=4,fill="white")+
  xlab("Difference (Standardized)")+ylab("")+ labs(title="Ideology") +  theme_nolegend()+theme(axis.text=element_text(size=10),axis.title=element_text(size=12),axis.title.x = element_text(hjust=1),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+xlim(-2,2)

balanceplot1




# External Validity

colnames(nmc2)=c("Urban Population", "Total Population", "Energy Consumption", "Military Personel",  "Military Expenditures","Iron and Steel Production")

# for(i in 1:ncol(nmc2)){nmc2[,i]=nmc2[,i]/mean(nmc2[,i],na.rm=TRUE)}

for(i in 1:ncol(nmc2)){nmc2[,i]=log(nmc2[,i]+1)}

sample2=close[,c("upop", "tpop", "pec", "milper",  "milex","irst")]

for(i in 1:ncol(sample2)){sample2[,i]=log(sample2[,i]+1)}


fill=matrix(NA,nrow=nrow(nmc2)-nrow(sample2),ncol=ncol(sample2))

colnames(fill)=c("Urban Population", "Total Population", "Energy Consumption", "Military Personel",  "Military Expenditures","Iron and Steel Production")

sample2=rbind(sample2,fill)

caps=cbind(sample2[,1],nmc2[,1],sample2[,2],nmc2[,2],sample2[,3],nmc2[,3],sample2[,4],nmc2[,4],sample2[,5],nmc2[,5],sample2[,6],nmc2[,6])

caps=cbind(sample2[,1],nmc2[,1],sample2[,2],nmc2[,2],sample2[,3],nmc2[,3],sample2[,4],nmc2[,4],sample2[,5],nmc2[,5],sample2[,6],nmc2[,6])

colnames(caps)=c("Sample Urban Population","Population Urban Population", "Sample Total Population", "Population Total Population","Sample Energy Consumption", "Population Energy Consumption","Sample Military Personel","Population Military Personel","Sample Military Expenditures",  "Population Military Expenditures","Sample Iron and Steel Production","Population Iron and Steel Production")

caps=melt(caps)

colnames(caps)[2:3]=c("Variable","Value")

caps$Variable=factor(caps$Variable,levels=c("Sample Urban Population","Population Urban Population", "Sample Total Population", "Population Total Population","Sample Energy Consumption", "Population Energy Consumption","Sample Military Personel","Population Military Personel","Sample Military Expenditures",  "Population Military Expenditures","Sample Iron and Steel Production","Population Iron and Steel Production"),ordered=TRUE)

ExternalValidity1 = ggplot(caps, aes(Variable,Value)) + geom_boxplot(fill=rep(c("cornflowerblue","lightgrey"),6)) + coord_flip() + ylab("ln(value)") + xlab("") + theme_bw() +theme(axis.title=element_text(size=16)) + ggtitle("Ideology") +theme(plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))

ExternalValidity1





close[close$Z>0,c("Country","Year","DisputesInitiated","HighDisputesInitiated")][order(close[close$Z>0,]$Year),]

close[close$Z<0,c("Country","Year","DisputesInitiated","HighDisputesInitiated")][order(close[close$Z<0,]$Year),]

t.test(DisputesInitiated~T,close)

t.test(HighDisputesInitiated~T,close)

#scale_number=mean(close$Days)/365
#dems$DisputesInitiated=dems$DisputesInitiated*scale_number
#dems$HighDisputesInitiated=dems$HighDisputesInitiated*scale_number
#dems$AllDisputes=dems$AllDisputes*scale_number
#dems$AllHighDisputes=dems$AllHighDisputes*scale_number
#dems$RevisionistDisputes=dems$RevisionistDisputes*scale_number

#close=dems[abs(dems$Z)<=0.04,]

outcomes=c("DisputesInitiated","HighDisputesInitiated","AllDisputes","AllHighDisputes","RevisionistDisputes","HighRevisionistDisputes")

t_test_results=matrix(0,nrow=length(outcomes),ncol=3)

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T)
t_test_results[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[1],-output$conf.int[2])}

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),(output$estimate[2]-output$estimate[1])/sd(close[,outcomes[i]]),-output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", "Standardized Lower Bound")
rownames(standardized_results)=outcomes

t_test_results=standardized_results

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes","Revisionist Disputes","High-Level Revisionist Disputes")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- t_test_results[,3]
cd$lower <- t_test_results[,4]
cd$upper <- t_test_results[,5]
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
  xlab("Estimated Treatment Effect (Standardized)")+ylab("")+ labs(title="") +  theme_nolegend() +theme(axis.text=element_text(size=8.2),axis.title=element_text(size=12.5),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+scale_x_continuous(breaks=c(-0.5,0,0.2,0.5,0.8,1),labels=c("-0.5","0","small (0.2)","medium (0.5)","large (0.8)","1")) # Aggression During Term

ggsave("AggressionPlotIdeo.pdf",width=4,height=2,scale = 1.6)




bandwidth=rdbwselect(dems$HighDisputesInitiated,dems$Z)[[3]][[1]]

pdf("IdeologyRDGraph.pdf",width=5,height=5)
RDPlot(dems$Z,dems$HighDisputesInitiated,Bandwidth=bandwidth,xlab="Win/Loss Margin for Right-Wing Candidate",ylab="High-Level Militarized Disputes Initiated",Main="",Tick.Marks = seq(-.2,.2,by=0.05),Labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"),ylim=c(-0.5,2.5),xlim=c(-0.2,0.2),NBoots=10000,Plot.Raw.Data=TRUE,Plot.Means=FALSE,Raw.Data.Colors=c("Blue","Red"))
dev.off()




# Adjusting the minimum ideology distance between parties


diff1=dems_base[-which(abs(dems_base$IdeologyDifference)<1),]
diff1$Z=diff1$Z*(2*as.numeric(diff1$IdeologyDifference>0)-1)
diff1$T=as.numeric(diff1$Z>0)
t.test(DisputesInitiated~T,diff1[abs(diff1$Z)<=0.04,])
t.test(HighDisputesInitiated~T,diff1[abs(diff1$Z)<=0.04,])


diff2=dems_base[-which(abs(dems_base$IdeologyDifference)<2),]
diff2$Z=diff2$Z*(2*as.numeric(diff2$IdeologyDifference>0)-1)
diff2$T=as.numeric(diff2$Z>0)
t.test(DisputesInitiated~T,diff2[abs(diff2$Z)<=0.04,])
t.test(HighDisputesInitiated~T,diff2[abs(diff2$Z)<=0.04,])

diff3=dems_base[-which(abs(dems_base$IdeologyDifference)<3),]
diff3$Z=diff3$Z*(2*as.numeric(diff3$IdeologyDifference>0)-1)
diff3$T=as.numeric(diff3$Z>0)
t.test(DisputesInitiated~T,diff3[abs(diff3$Z)<=0.04,])
t.test(HighDisputesInitiated~T,diff3[abs(diff3$Z)<=0.04,])










# Incumbency

dems=data[data$Democracy==1,]
dems=dems[is.na(dems$Z)==FALSE&is.na(dems$DisputesInitiated)==FALSE,]
dems=dems[dems$WinnerPartyInc==1|dems$RunnerUpPartyInc==1,]
dems=dems[-which(dems$WinnerPartyInc==1&dems$RunnerUpPartyInc==1),]

dems$Z=-dems$Z*(2*as.numeric(dems$WinnerPartyInc-dems$RunnerUpPartyInc>0)-1)

dems$T=as.numeric(dems$Z>0)

# Balance Plot

covs=c("PreviousDisputesInitiated","PreviousHighDisputesInitiated","AllPreviousDisputes","AllPreviousHighDisputes","PreviousRevisionistDisputes","PreviousHighRevisionistDisputes", "irst", "milex", "milper", "pec", "tpop", "upop")

est=matrix(NA,ncol=3,nrow=length(covs))

for(i in 1:length(covs)){
output=t.test(dems[abs(dems$Z)<=0.04,covs[i]]~as.numeric(dems[abs(dems$Z)<=0.04,]$Z>0))
est[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[1],-output$conf.int[2])/sd(dems[,covs[i]],na.rm=TRUE)}

cd <- as.data.frame(matrix(NA,length(covs),5))
conditions <- c("Previous Disputes Initiated","Previous High-Level Disputes Initiated","All Previous Disputes","All Previous High-Level Disputes","Previous Revisionist Disputes", "Previous High-Level Revisionist Disputes", "Iron and Steel Production",  "Military Expenditures", "Military Personel",     "Energy Consumption",  "Total Population", "Urban Population")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- as.numeric(est[,1])
cd$lower <- as.numeric(est[,3])
cd$upper <- as.numeric(est[,2])
cd$ord <- c(length(covs):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])
# make the graph
library(ggplot2)

f <- ggplot(cd, 
            aes(x=mean,y=measure,color=measure))
plot1 <- f+geom_vline(xintercept=0, linetype="longdash")+

  geom_errorbarh(aes(xmax =  upper, 
                     xmin = lower),
                 size=1.5, height=0)+
  geom_point(stat="identity",size=4,fill="white")+
  xlab("Difference (Standardized)")+ylab("")+ labs(title="Incumbency") +  theme_nolegend()+theme(axis.text=element_text(size=10),axis.title=element_text(size=12),axis.title.x = element_text(hjust=1),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+xlim(-2,2)

balanceplot2

plot_grid(balanceplot1,balanceplot2,ncol=2)

ggsave("PlaceboPlotsWarParty.pdf",width=3.2,height=1.2,scale = 3)


# ExternalValidity






dems$AbsoluteChangeDisputesInitiated=with(dems,abs(DisputesInitiated-PreviousTermDisputesInitiated))
dems$AbsoluteChangeHighDisputesInitiated=with(dems,abs(HighDisputesInitiated-PreviousTermHighDisputesInitiated))
dems$AbsoluteChangeAllDisputes=with(dems,abs(AllDisputes-AllPreviousTermDisputes))
dems$AbsoluteChangeAllHighDisputes=with(dems,abs(AllHighDisputes-AllPreviousTermHighDisputes))
dems$AbsoluteChangeRevisionistDisputes=with(dems,abs(RevisionistDisputes-PreviousTermRevisionistDisputes))
dems$AbsoluteChangeHighRevisionistDisputes=with(dems,abs(HighRevisionistDisputes-PreviousTermHighRevisionistDisputes))

pdf("ForcingDensityInc.pdf", height=4, width=5)
m2 <- ggplot(dems, aes(x=-Z*50))
m2=m2 + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -50.00001)+
  theme_bw()+theme(axis.title = element_text(size=13),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percent of Votes That Candidate from\nIncumbent Party Was from Winning")+
  ylab("Density") + labs(title="Incumbency")+
  scale_x_continuous(breaks=seq(-20, 20, 5),labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"))+  ylim(0,20)
m2  
dev.off()

plot_grid(m,m2,ncol=2)

ggsave("ForcingDensity.pdf",width=2.4,height=1.1,scale = 3)



close=dems[abs(dems$Z)<=0.04,]

close[close$Z>0,c("Country","Year","AbsoluteChangeDisputesInitiated","AbsoluteChangeHighDisputesInitiated")][order(close[close$Z>0,]$Year),]

close[close$Z<0,c("Country","Year","AbsoluteChangeDisputesInitiated","AbsoluteChangeHighDisputesInitiated")][order(close[close$Z<0,]$Year),]

t.test(AbsoluteChangeDisputesInitiated~T,close,alternative="less")
t.test(AbsoluteChangeHighDisputesInitiated~T,close,alternative="less")

#scale_number=mean(close$Days)/365
#dems$AbsoluteChangeDisputesInitiated=dems$AbsoluteChangeDisputesInitiated*scale_number
#dems$AbsoluteChangeHighDisputesInitiated=dems$AbsoluteChangeHighDisputesInitiated*scale_number
#dems$AbsoluteChangeAllDisputes=dems$AbsoluteChangeAllDisputes*scale_number
#dems$AbsoluteChangeAllHighDisputes=dems$AbsoluteChangeAllHighDisputes*scale_number
#dems$AbsoluteChangeRevisionistDisputes=dems$AbsoluteChangeRevisionistDisputes*scale_number

#close=dems[abs(dems$Z)<=0.04,]



outcomes=c("AbsoluteChangeDisputesInitiated","AbsoluteChangeHighDisputesInitiated","AbsoluteChangeAllDisputes","AbsoluteChangeAllHighDisputes","AbsoluteChangeRevisionistDisputes","AbsoluteChangeHighRevisionistDisputes")

t_test_results=matrix(0,nrow=length(outcomes),ncol=3)



for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.90)
t_test_results[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[2],-output$conf.int[1])}

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.90)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),(output$estimate[2]-output$estimate[1])/sd(close[,outcomes[i]]),-output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", "Standardized Lower Bound")
rownames(standardized_results)=outcomes

t_test_results=standardized_results

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

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes","Revisionist Disputes","High-Level Revisionist Disputes")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- t_test_results[,3]
cd$lower <- t_test_results[,4]
cd$upper <- t_test_results[,5]
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
  xlab("Estimated Treatment Effect (Standardized)")+ylab("")+ labs(title="") +  theme_nolegend() + theme(axis.text=element_text(size=8.7),axis.title=element_text(size=12.5),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+ scale_x_continuous(breaks=c(-0.5,0,0.2,0.5,0.8,1),labels=c("-0.5","0","small (0.2)","medium (0.5)","large (0.8)","1")) # Aggression During Term

ggsave("AggressionPlotInc.pdf",width=4,height=2,scale = 1.6)




bandwidth=rdbwselect(dems$AbsoluteChangeHighDisputesInitiated,dems$Z)[[3]][[1]]

dems=dems[is.na(dems$AbsoluteChangeHighDisputesInitiated)==FALSE&abs(dems$Z)<0.23,]

pdf("IncumbentRDGraph.pdf",height=5,width=5)
RDPlot(dems$Z,dems$AbsoluteChangeHighDisputesInitiated,Bandwidth=bandwidth,xlab="Win/Loss Margin for Candidate from Challenger Party",ylab="Absolute Change in High-Level Disputes Initiated",Main="",Tick.Marks = seq(-.2,.2,by=0.05),Labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"),ylim=c(-0.05,1.1),xlim=c(-0.2,0.2),Confidence.Level=0.9,Type="One-Sided",NBoots=10000,Plot.Raw.Data=TRUE,Plot.Means=FALSE,Raw.Data.Colors=c("Blue","Red"))
dev.off()











# Adjusting the size of the RD window

window_sizes=c(0.01,0.02,0.03,0.04,0.05)
for(i in window_sizes){
dat=dems[abs(dems$Z)<=(i*2),]
output=t.test(AbsoluteChangeHighDisputesInitiated~T,dat,alternative="less")
print(c(i,output$estimate[2]-output$estimate[1],output$p.value,dim(dat)[1]))}

