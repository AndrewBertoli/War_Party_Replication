# Start by loading the following libraries

require(reshape)
require(rdrobust)
require(cowplot)
require(ggplot2)
require(RCurl)
require(devtools)
source_url("https://raw.githubusercontent.com/AndrewBertoli/Natural-Experiments/master/RDPlot.R")


# Read the data from Github

data=read.csv("https://raw.githubusercontent.com/AndrewBertoli/War_Party_Replication/master/Data.csv",stringsAsFactors=FALSE)

# Create the forcing variable (distance each case was from the cut-point)

data$Z=(data$PresVotes - data$PresSecondVotes)/(data$PresVotes + data$PresSecondVotes)

# Create a variable showing the ideologial differnce between the two candidates in each case. We only want to use
# the cases where the candidates were at least two points apart (on a five-point ideology scale)

data$IdeologyDifference=data$LeaderIdeologyScore-data$RunnerUpIdeologyScore

# Only keep cases where the country was a democracy and where we have data on ideological difference and disputes initiated.

dems_base=data[data$Democracy==1&is.na(data$Z)==FALSE&is.na(data$IdeologyDifference)==FALSE&
is.na(data$DisputesInitiated)==FALSE,]

# Drop all cases where the two candidates were less than two points away from each other on the idological scale.
# Name the new dataset "ideology"

ideology=dems_base[-which(abs(dems_base$IdeologyDifference)<2),]

# Create a forcing varaible that denotes how far the right-wing candidate was from winning

ideology$Z=ideology$Z*(2*as.numeric(ideology$IdeologyDifference>0)-1)

# Create a treatment variable {0,1} that denotes whether the right-wing candidate won. 

ideology$T=as.numeric(ideology$Z>0)

# Create the forcing variable plot


ForcingDensity1 = ggplot()+ geom_histogram(aes(x=ideology$Z[ideology$Z<0]*50),fill="powderblue",
binwidth=2, color="black",origin = -50.00001)+ geom_histogram(aes(x=ideology$Z[ideology$Z>0]*50),
fill="cornflowerblue",binwidth=2, color="black",origin = 0.00001)+theme_bw()+
theme(axis.title = element_text(size=13),plot.title=element_text(size=20,face="bold",hjust=0.46))+
geom_vline(xintercept=0, colour="black")+xlab("Win/Loss Margin for\nRight-Wing Candidate")+
ylab("Density") + labs(title="Ideology")+scale_x_continuous(breaks=seq(-20, 20, 5),
labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"))+ylim(0,20)

ForcingDensity1

# Subset to just the cases that were in th 48%-52% range.
# Recall here that Z is the percent difference between the winner and runner up, so 52%-48%=4%

close=ideology[abs(ideology$Z)<=0.04,]



# Make the balance plot for ideology

covs=c("PreviousDisputesInitiated","PreviousHighDisputesInitiated","AllPreviousDisputes",
"AllPreviousHighDisputes","PreviousRevisionistDisputes","PreviousHighRevisionistDisputes", 
"irst", "milex", "milper", "pec", "tpop", "upop")

est=matrix(NA,ncol=3,nrow=length(covs))

for(i in 1:length(covs)){
output=t.test(ideology[abs(ideology$Z)<=0.04,covs[i]]~as.numeric(ideology[abs(ideology$Z)<=0.04,]$Z>0))
est[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[1],-output$conf.int[2])/
  sd(ideology[,covs[i]],na.rm=TRUE)}


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
conditions <- c("Previous Disputes Initiated","Previous High-Level Disputes Initiated",
"All Previous Disputes","All Previous High-Level Disputes","Previous Revisionist Disputes", 
"Previous High-Level Revisionist Disputes", "Iron and Steel Production", "Military Expenditures", 
"Military Personel",     "Energy Consumption",  "Total Population", "Urban Population")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- as.numeric(est[,1])
cd$lower <- as.numeric(est[,3])
cd$upper <- as.numeric(est[,2])
cd$ord <- c(length(covs):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])

# make the graph
BalancePlot1=ggplot(cd, aes(x=mean,y=measure,color="dodgerblue2"))+geom_vline(xintercept=0, linetype="longdash")+
geom_errorbarh(aes(xmax =  upper, xmin = lower), size=1.5, height=0,color="dodgerblue2")+
geom_point(stat="identity",size=4,fill="white",color="dodgerblue2")+ xlab("Difference (Standardized)")+
ylab("")+ labs(title="Ideology") +theme(legend.position="none",axis.text=element_text(size=10),
axis.title=element_text(size=12),axis.title.x = element_text(hjust=1),
plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+xlim(-2,2)

BalancePlot1




# Create the external validity graph for ideology

# Download the National Material Capabilties dataset from the Correlates of War database
# http://www.correlatesofwar.org/data-sets/national-material-capabilities

nmc=read.csv("NMC_v4_0(1).csv",stringsAsFactors=FALSE)

# Download the Polity IV dataset
# http://www.systemicpeace.org/inscrdata.html

polity=read.csv("Polity.csv",stringsAsFactors=FALSE)

# Subset to just the democracies in the Polity IV dataset

democracies=polity[polity$polity>=6,]

# Create an index to match the country-years in democracies to country-years in nmc.

democracies$index=paste(democracies$scode,democracies$year,sep=" ")
nmc$index=paste(nmc$stateabb,nmc$year,sep=" ")

# Merge the two datasets

alldems=(merge(democracies,nmc,by=c("index","index")))

# Remove all columns except the capabilities

alldems=alldems[,c("upop","tpop","pec","milper","milex","irst")]

# Convert all covariates to log(x+1) to mitigate the outlier problem. We use the "+1" so that the 0's don't go to -infinity.

for(i in 1:ncol(alldems)){alldems[,i]=log(alldems[,i]+1)}

# Get the covariates for the cases from the ideology dataset that were close to the cut-point

sample2=close[,c("upop", "tpop", "pec", "milper",  "milex","irst")]

# Again, convert all covariates to log(x+1) to mitigate the outlier problem.

for(i in 1:ncol(sample2)){sample2[,i]=log(sample2[,i]+1)}

# We now want to add NA's to the end of the covaraites in sample2 so that we can merge it with alldems in one dataframe

fill=matrix(NA,nrow=nrow(alldems)-nrow(sample2),ncol=ncol(sample2))

colnames(fill)=c("Urban Population", "Total Population", "Energy Consumption", "Military Personel",  
                 "Military Expenditures","Iron and Steel Production")

colnames(sample2)=c("Urban Population", "Total Population", "Energy Consumption", "Military Personel",  
                    "Military Expenditures","Iron and Steel Production")

sample2=rbind(sample2,fill)

# Now sample2 has the same dimensions as alldems

# Now we can merge sample2 and alldems into a new dataframe called "caps"

caps=cbind(sample2[,1],alldems[,1],sample2[,2],alldems[,2],sample2[,3],alldems[,3],sample2[,4],
           alldems[,4],sample2[,5],alldems[,5],sample2[,6],alldems[,6])

# We can next give the appropriate column names to caps 

colnames(caps)=c("Sample Urban Population","Population Urban Population", "Sample Total Population", 
                 "Population Total Population","Sample Energy Consumption", "Population Energy Consumption",
                 "Sample Military Personel","Population Military Personel","Sample Military Expenditures",  
                 "Population Military Expenditures","Sample Iron and Steel Production",
                 "Population Iron and Steel Production")

# We next want to melt caps to make it easier to use for ggplot2

caps=melt(caps)

# Now we can give the appropiate column names to the melted version of caps

colnames(caps)[2:3]=c("Variable","Value")

# We now want to change the levels of caps$Variable so that the covariates will appear in the correct order in our external validity graph

caps$Variable=factor(caps$Variable,levels=c("Sample Urban Population","Population Urban Population", 
                                            "Sample Total Population", "Population Total Population",
                                            "Sample Energy Consumption", "Population Energy Consumption",
                                            "Sample Military Personel","Population Military Personel",
                                            "Sample Military Expenditures", "Population Military Expenditures",
                                            "Sample Iron and Steel Production","Population Iron and Steel Production"),
                     ordered=TRUE)

# Finally, we can make the external validity graph for ideology

ExternalValidity1 = ggplot(caps, aes(Variable,Value)) + geom_boxplot(fill=rep(c("cornflowerblue","lightgrey"),6)) +
coord_flip()+ylab("ln(value)")+xlab("")+theme_bw()+theme(axis.title=element_text(size=16))+ggtitle("Ideology") +
theme(plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))

ExternalValidity1



# We can now make the talbe for the ideology cases

close[close$Z>0,c("Country","Year","DisputesInitiated","HighDisputesInitiated")][order(close[close$Z>0,]$Year),]

close[close$Z<0,c("Country","Year","DisputesInitiated","HighDisputesInitiated")][order(close[close$Z<0,]$Year),]


# Next, we can look at the results from t-tests

t.test(DisputesInitiated~T,close)

t.test(HighDisputesInitiated~T,close)


# We discuss in the paper that the case of Costa Rica is very questionable. It did not involve any military action. 
# It was just a minor incident that started because two Costa Rican police officers chased some subjects across the 
# border and were arrested by the Nicaraguan police. We can briefly check the results after this case is dropped.

costa_rica_dropped=close
costa_rica_dropped$HighDisputesInitiated[costa_rica_dropped$Country=="Costa Rica"]=0
costa_rica_dropped$DisputesInitiated[costa_rica_dropped$Country=="Costa Rica"]=0

t.test(DisputesInitiated~T,costa_rica_dropped)
t.test(HighDisputesInitiated~T,costa_rica_dropped)
t.test(HighDisputesInitiated>0~T,costa_rica_dropped)

# The final test here addresses the issue that the U.S. (2001) is an outlier.

# We can now make the coefficent plot for ideology.

outcomes=c("DisputesInitiated","HighDisputesInitiated","AllDisputes","AllHighDisputes")

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),
                           (output$estimate[2]-output$estimate[1])/sd(close[,outcomes[i]]),
                           -output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/
                           sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", 
                                 "Standardized Lower Bound")
rownames(standardized_results)=outcomes

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes") 
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- standardized_results[,3]
cd$lower <- standardized_results[,4]
cd$upper <- standardized_results[,5]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])

# make the graph
AggressionPlot1 = ggplot(cd, aes(x=mean,y=measure,color=measure))+geom_vline(xintercept=0, linetype="longdash")+
geom_errorbarh(aes(xmax =  upper,  xmin = lower), size=1.5, height=0)+ geom_point(stat="identity",size=4,fill="white")+
xlab("Estimated Treatment Effect (Standardized)")+ylab("")  + theme(legend.position="none",
axis.text.x=element_text(size=7.7),axis.text.y=element_text(size=10.7),
axis.title=element_text(size=12.5),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+ 
scale_x_continuous(limits=c(-1.25,1.25),breaks=c(-1.2,-1,-0.8,-0.5,-0.2,0,0.2,0.5,0.8,1,1.2),
labels=c("-1.2\nvery large","-1","-0.8\nlarge","-0.5\nmedium","-0.2\nsmall","0","0.2\nsmall","0.5\nmedium",
"0.8\nlarge","1","1.2\nvery large")) +  geom_vline(xintercept=c(-1.2,-0.8,-0.5,-0.2,0.2,0.5,0.8,1.2),
linetype=rep("dashed",8),colour=c("turquoise4","turquoise3","turquoise2","turquoise1","turquoise1",
"turquoise2","turquoise3","turquoise4"))+scale_color_manual(name="",values=c("dodgerblue4","dodgerblue",
"darkblue","dodgerblue3")) 

ggsave("AggressionPlotIdeo.pdf",width=4,height=2,scale = 1.6)






# We can now make the version of the graph with revisionist disputes included that is available in the
# Online Appendix

outcomes=c("DisputesInitiated","HighDisputesInitiated","AllDisputes","AllHighDisputes",
           "RevisionistDisputes","HighRevisionistDisputes")

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.90)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),
                           (output$estimate[2]-output$estimate[1])/sd(close[,outcomes[i]]),
                           -output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/
                           sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", 
                                 "Standardized Lower Bound")
rownames(standardized_results)=outcomes

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes",
                "Revisionist Disputes","High-Level Revisionist Disputes") 
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- standardized_results[,3]
cd$lower <- standardized_results[,4]
cd$upper <- standardized_results[,5]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])

AggressionPlot1_Rev <- ggplot(cd, aes(x=mean,y=measure,color=measure))+geom_vline(xintercept=0, linetype="longdash")+
geom_errorbarh(aes(xmax =  upper, xmin = lower), size=1.5, height=0)+geom_point(stat="identity",size=4,fill="white")+
xlab("Estimated Treatment Effect (Standardized)")+ylab("")  + theme(legend.position="none",
axis.text.x=element_text(size=7.7),axis.text.y=element_text(size=10.7),axis.title=element_text(size=12.5),
plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+ scale_x_continuous(limits=c(-1.25,1.25),
breaks=c(-1.2,-1,-0.8,-0.5,-0.2,0,0.2,0.5,0.8,1,1.2),labels=c("-1.2\nvery large","-1","-0.8\nlarge",
"-0.5\nmedium","-0.2\nsmall","0","0.2\nsmall","0.5\nmedium","0.8\nlarge","1","1.2\nvery large")) +  
geom_vline(xintercept=c(-1.2,-0.8,-0.5,-0.2,0.2,0.5,0.8,1.2),linetype=rep("dashed",8),colour=c("turquoise4",
"turquoise3","turquoise2","turquoise1","turquoise1","turquoise2","turquoise3","turquoise4"))+
scale_color_manual(name="",values=c("darkblue","dodgerblue3","dodgerblue4","dodgerblue","darkblue","dodgerblue3"))

ggsave("AggressionPlotIdeoRev.pdf",width=4,height=2,scale = 1.6)



# Testing whether electing left-wing candidates leads to medium, large, or very large increases in
# state aggression.

t.test(HighDisputesInitiated+as.numeric(close$T==1)*1.2*sd(close$HighDisputesInitiated)~T,close)
t.test(HighDisputesInitiated+as.numeric(close$T==1)*0.8*sd(close$HighDisputesInitiated)~T,close)
t.test(HighDisputesInitiated+as.numeric(close$T==1)*0.5*sd(close$HighDisputesInitiated)~T,close)



# Adjusting the minimum ideology distance between parties

# Any difference
diff1=dems_base[-which(abs(dems_base$IdeologyDifference)<1),]
diff1$Z=diff1$Z*(2*as.numeric(diff1$IdeologyDifference>0)-1)
diff1$T=as.numeric(diff1$Z>0)
t.test(DisputesInitiated~T,diff1[abs(diff1$Z)<=0.04,])
t.test(HighDisputesInitiated~T,diff1[abs(diff1$Z)<=0.04,])

# At least a two-point difference
diff2=dems_base[-which(abs(dems_base$IdeologyDifference)<2),]
diff2$Z=diff2$Z*(2*as.numeric(diff2$IdeologyDifference>0)-1)
diff2$T=as.numeric(diff2$Z>0)
t.test(DisputesInitiated~T,diff2[abs(diff2$Z)<=0.04,])
t.test(HighDisputesInitiated~T,diff2[abs(diff2$Z)<=0.04,])

# At least a three-point difference
diff3=dems_base[-which(abs(dems_base$IdeologyDifference)<3),]
diff3$Z=diff3$Z*(2*as.numeric(diff3$IdeologyDifference>0)-1)
diff3$T=as.numeric(diff3$Z>0)
t.test(DisputesInitiated~T,diff3[abs(diff3$Z)<=0.04,])
t.test(HighDisputesInitiated~T,diff3[abs(diff3$Z)<=0.04,])






# We can now turn to the incumbency analysis. First subset the data to cases where the incumbent party
# was either winner or runner-up (but not both).

incumbency=dems_base[(dems_base$WinnerPartyInc==1+dems_base$RunnerUpPartyInc)==1,]

# Now create a forcing variable that denotes how close the candidate from the challenger party 
# was from winning the presidency.

incumbency$Z=-incumbency$Z*(2*as.numeric(incumbency$WinnerPartyInc-incumbency$RunnerUpPartyInc>0)-1)

# Create a treatment variable {0,1} that denotes whether the candidate from the challenger party won. 

incumbency$T=as.numeric(incumbency$Z>0)






# Distribution of the forcing varialbe

ForcingDensity2 = ggplot() + geom_histogram(aes(x=incumbency$Z[incumbency$Z<0]*50),fill="powderblue", 
binwidth=2, color="black",origin = -50.00001)+ geom_histogram(aes(x=incumbency$Z[incumbency$Z>0]*50),
fill="cornflowerblue", binwidth=2, color="black",origin = 0.00001)+theme_bw()+
theme(axis.title = element_text(size=13),plot.title=element_text(size=20,face="bold",hjust=0.5))+
geom_vline(xintercept=0, colour="black")+ xlab("Win/Loss Margin for\nChallenger Party Candidate")+
ylab("Density") + labs(title="Incumbency")+scale_x_continuous(breaks=seq(-20, 20, 5),
labels=c("-20%","-15%","-10%","-5%","0%","5%","10%","15%","20%"))+  ylim(0,20)

# We can make a pdf with the two forcing density graphs side-by-side

plot_grid(ForcingDensity1,ForcingDensity2,ncol=2)

ggsave("ForcingDensity.pdf",width=2.4,height=1.1,scale = 3)



# Balance Plot

covs=c("PreviousDisputesInitiated","PreviousHighDisputesInitiated","AllPreviousDisputes","AllPreviousHighDisputes",
       "PreviousRevisionistDisputes","PreviousHighRevisionistDisputes", "irst", "milex", "milper", "pec", "tpop", "upop")

est=matrix(NA,ncol=3,nrow=length(covs))

for(i in 1:length(covs)){
output=t.test(incumbency[abs(incumbency$Z)<=0.04,covs[i]]~as.numeric(incumbency[abs(incumbency$Z)<=0.04,]$Z>0))
est[i,]=c(output$estimate[2]-output$estimate[1],-output$conf.int[1],-output$conf.int[2])/sd(incumbency[,covs[i]],na.rm=TRUE)}

cd <- as.data.frame(matrix(NA,length(covs),5))
conditions <- c("Previous Disputes Initiated","Previous High-Level Disputes Initiated","All Previous Disputes",
"All Previous High-Level Disputes","Previous Revisionist Disputes", "Previous High-Level Revisionist Disputes", 
"Iron and Steel Production", "Military Expenditures", "Military Personel", "Energy Consumption", "Total Population", 
"Urban Population")
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- as.numeric(est[,1])
cd$lower <- as.numeric(est[,3])
cd$upper <- as.numeric(est[,2])
cd$ord <- c(length(covs):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])

BalancePlot2 <- ggplot(cd, aes(x=mean,y=measure,color=measure)) +geom_vline(xintercept=0, linetype="longdash")+
geom_errorbarh(aes(xmax =  upper, xmin = lower),size=1.5, height=0,color="dodgerblue2")+
geom_point(stat="identity",size=4,fill="white",color="dodgerblue2")+
xlab("Difference (Standardized)")+ylab("")+ labs(title="Incumbency") +theme(legend.position="none",
axis.text=element_text(size=10),axis.title=element_text(size=12),axis.title.x = element_text(hjust=1),
plot.title = element_text(lineheight=3,size=rel(1.5),face="bold",hjust=-0.5))+xlim(-2,2)

BalancePlot2


# Make a pdf with both the balance plots side-by-side

plot_grid(BalancePlot1,BalancePlot2,ncol=2)

ggsave("PlaceboPlotsWarParty.pdf",width=3.2,height=1.2,scale = 3)


# External Validity (following the same approach in the ideology analysis)

close=incumbency[abs(incumbency$Z)<=0.04,]

sample2=close[,c("upop", "tpop", "pec", "milper",  "milex","irst")]

for(i in 1:ncol(sample2)){sample2[,i]=log(sample2[,i]+1)}

fill=matrix(NA,nrow=nrow(alldems)-nrow(sample2),ncol=ncol(sample2))

colnames(fill)=c("Urban Population", "Total Population", "Energy Consumption", "Military Personel",  
                 "Military Expenditures","Iron and Steel Production")

colnames(sample2)=c("Urban Population", "Total Population", "Energy Consumption", "Military Personel",  
                    "Military Expenditures","Iron and Steel Production")

sample2=rbind(sample2,fill)

caps=cbind(sample2[,1],alldems[,1],sample2[,2],alldems[,2],sample2[,3],alldems[,3],
           sample2[,4],alldems[,4],sample2[,5],alldems[,5],sample2[,6],alldems[,6])

caps=cbind(sample2[,1],alldems[,1],sample2[,2],alldems[,2],sample2[,3],alldems[,3],
           sample2[,4],alldems[,4],sample2[,5],alldems[,5],sample2[,6],alldems[,6])

colnames(caps)=c("Sample Urban Population","Population Urban Population", "Sample Total Population", 
                 "Population Total Population","Sample Energy Consumption", "Population Energy Consumption",
                 "Sample Military Personel","Population Military Personel","Sample Military Expenditures",  
                 "Population Military Expenditures","Sample Iron and Steel Production",
                 "Population Iron and Steel Production")

caps=melt(caps)

colnames(caps)[2:3]=c("Variable","Value")

caps$Variable=factor(caps$Variable,levels=c("Sample Urban Population","Population Urban Population", 
                                            "Sample Total Population", "Population Total Population",
                                            "Sample Energy Consumption", "Population Energy Consumption",
                                            "Sample Military Personel","Population Military Personel",
                                            "Sample Military Expenditures",  "Population Military Expenditures",
                                            "Sample Iron and Steel Production","Population Iron and Steel Production"),
                     ordered=TRUE)

ExternalValidity2 = ggplot(caps, aes(Variable,Value)) + geom_boxplot(fill=rep(c("cornflowerblue","lightgrey"),6)) + 
coord_flip() + ylab("ln(value)") + xlab("") + theme_bw() +theme(axis.title=element_text(size=16)) 
+ ggtitle("Incumbency") +theme(plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))

ExternalValidity2

# We can now create a pdf with the two external validity graphs side-by-side 

plot_grid(ExternalValidity1,ExternalValidity2,ncol=2)

ggsave("External_Validity.pdf",width=3.1,height=1.1,scale = 3)




# Now we need to calculate the absolute change for the outcomes in our dataset

incumbency$AbsoluteChangeDisputesInitiated=with(incumbency,abs(DisputesInitiated-PreviousTermDisputesInitiated))
incumbency$AbsoluteChangeHighDisputesInitiated=with(incumbency,abs(HighDisputesInitiated-PreviousTermHighDisputesInitiated))
incumbency$AbsoluteChangeAllDisputes=with(incumbency,abs(AllDisputes-AllPreviousTermDisputes))
incumbency$AbsoluteChangeAllHighDisputes=with(incumbency,abs(AllHighDisputes-AllPreviousTermHighDisputes))
incumbency$AbsoluteChangeRevisionistDisputes=with(incumbency,abs(RevisionistDisputes-PreviousTermRevisionistDisputes))
incumbency$AbsoluteChangeHighRevisionistDisputes=with(incumbency,abs(HighRevisionistDisputes-PreviousTermHighRevisionistDisputes))



# We can now subset to the cases where incumbent parties were within 2% of the cut-point. 
# Again, remember here that Z is the percent difference between the winner and runner up, so 52%-48%=4%

close=incumbency[abs(incumbency$Z)<=0.04,]

# We can now make the talbe for the incumbency cases

close[close$Z>0,c("Country","Year","AbsoluteChangeDisputesInitiated",
                  "AbsoluteChangeHighDisputesInitiated")][order(close[close$Z>0,]$Year),]

close[close$Z<0,c("Country","Year","AbsoluteChangeDisputesInitiated",
                  "AbsoluteChangeHighDisputesInitiated")][order(close[close$Z<0,]$Year),]

# Next, we can look at the results from t-tests

t.test(AbsoluteChangeDisputesInitiated~T,close,alternative="less")
t.test(AbsoluteChangeHighDisputesInitiated~T,close,alternative="less")


# We can now make the coefficent plot for ideology.

outcomes=c("AbsoluteChangeDisputesInitiated","AbsoluteChangeHighDisputesInitiated","AbsoluteChangeAllDisputes",
           "AbsoluteChangeAllHighDisputes") 

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.90)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),(output$estimate[2]-
output$estimate[1])/sd(close[,outcomes[i]]),-output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/
sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", 
                                 "Standardized Lower Bound")

rownames(standardized_results)=outcomes

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes") 
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- standardized_results[,3]
cd$lower <- standardized_results[,4]
cd$upper <- standardized_results[,5]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])

AggressionPlot2 <- ggplot(cd, aes(x=mean,y=measure,color=measure))+geom_vline(xintercept=0, linetype="longdash")+
geom_errorbarh(aes(xmax =  upper, xmin = lower), size=1.5, height=0)+
geom_point(stat="identity",size=4,fill="white")+ xlab("Estimated Treatment Effect (Standardized)")+
ylab("")  + theme(legend.position="none",axis.text.x=element_text(size=7.7),
axis.text.y=element_text(size=10.7),axis.title=element_text(size=12.5), 
plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+ 
scale_x_continuous(limits=c(-1.25,1.25), breaks=c(-1.2,-1,-0.8,-0.5,-0.2,0,0.2,0.5,0.8,1,1.2),
labels=c("-1.2\nvery large","-1","-0.8\nlarge","-0.5\nmedium","-0.2\nsmall","0","0.2\nsmall",
         "0.5\nmedium","0.8\nlarge","1","1.2\nvery large")) +  
  geom_vline(xintercept=c(-1.2,-0.8,-0.5,-0.2,0.2,0.5,0.8,1.2),linetype=rep("dashed",8),
  colour=c("turquoise4","turquoise3","turquoise2","turquoise1","turquoise1","turquoise2",
           "turquoise3","turquoise4"))+
  scale_color_manual(name="", values=c("dodgerblue4","dodgerblue","darkblue","dodgerblue3"))


ggsave("AggressionPlotInc.pdf",width=4,height=2,scale = 1.6)












# We can now make the version of the graph with revisionist disputes included that is available in the
# Online Appendix

outcomes=c("AbsoluteChangeDisputesInitiated","AbsoluteChangeHighDisputesInitiated","AbsoluteChangeAllDisputes",
"AbsoluteChangeAllHighDisputes","AbsoluteChangeRevisionistDisputes","AbsoluteChangeHighRevisionistDisputes")

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.90)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),(output$estimate[2]-
output$estimate[1])/sd(close[,outcomes[i]]),-output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/
sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", 
                                 "Standardized Lower Bound")
rownames(standardized_results)=outcomes


cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated","High-Level Disputes Initiated","All Disputes","All High-Level Disputes",
                "Revisionist Disputes","High-Level Revisionist Disputes") 
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- standardized_results[,3]
cd$lower <- standardized_results[,4]
cd$upper <- standardized_results[,5]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])


# make the graph

AggressionPlot2_Rev <- ggplot(cd, aes(x=mean,y=measure,color=measure))+geom_vline(xintercept=0, 
linetype="longdash")+geom_errorbarh(aes(xmax =  upper, xmin = lower),size=1.5, height=0)+
geom_point(stat="identity",size=4,fill="white")+xlab("Estimated Treatment Effect (Standardized)")+
ylab("")  + theme(legend.position="none",axis.text.x=element_text(size=7.7),axis.text.y=element_text(size=10.7),
axis.title=element_text(size=12.5),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold"))+ 
scale_x_continuous(limits=c(-1.25,1.25),breaks=c(-1.2,-1,-0.8,-0.5,-0.2,0,0.2,0.5,0.8,1,1.2),
labels=c("-1.2\nvery large","-1","-0.8\nlarge","-0.5\nmedium","-0.2\nsmall","0","0.2\nsmall","0.5\nmedium",
"0.8\nlarge","1","1.2\nvery large")) + geom_vline(xintercept=c(-1.2,-0.8,-0.5,-0.2,0.2,0.5,0.8,1.2),
linetype=rep("dashed",8),colour=c("turquoise4","turquoise3","turquoise2","turquoise1","turquoise1","turquoise2",
"turquoise3","turquoise4"))+scale_color_manual(name="",values=c("darkblue","dodgerblue3","dodgerblue4",
                                                                "dodgerblue","darkblue","dodgerblue3"))
                     
ggsave("AggressionPlotIncRev.pdf",width=4,height=2,scale = 1.6)





# Now we can check what percentage of variation in high-level disputes initiated is
# explained by incumbent parties winning vs. losing.

summary(lm(AbsoluteChangeHighDisputesInitiated~T,close))

# Approx 4% of variance explained


# We can now make the RD graph for incumbency

bandwidth=rdbwselect(incumbency$AbsoluteChangeHighDisputesInitiated,incumbency$Z)[[3]][[1]]

# The following piece of code prevents the RDPlot function from breaking by removing
# cases that are far from the cut-point.

incumbency=incumbency[is.na(incumbency$AbsoluteChangeHighDisputesInitiated)==FALSE&abs(incumbency$Z)<0.23,]


# The following code produces the graph.

pdf("IncumbentRDGraph.pdf",height=4,width=5)
par(oma=c(0,0,0,1),mar=c(4,6,1,1))
RDPlot(incumbency$Z,incumbency$AbsoluteChangeHighDisputesInitiated,Bandwidth=bandwidth,
       xlab="Win/Loss Margin for Candidate\nfrom Challenger Party",
       ylab="Absolute Change in High-Level\nDisputes Initiated per Year",
       Main="",Tick.Marks = seq(-.2,.2,by=0.1),Labels=c("-20%","-10%","0%","10%","20%"),
       ylim=c(-0.05,1.1),xlim=c(-0.15,0.15),Confidence.Level=0.9,Type="One-Sided",
       NBoots=10000,Plot.Raw.Data=TRUE,Plot.Means=FALSE,Raw.Data.Colors=c("cadetblue3","cornflowerblue"))
dev.off()


# Now let's look at disputes in the first year of the new leaders term.

t.test(FYDisputesInitiated~T,close)
t.test(FYHighDisputesInitiated~T,close)

t.test(FYDisputesInitiated>0~T,close)
t.test(FYHighDisputesInitiated>0~T,close)

t.test(FYAllDisputes~T,close)
t.test(FYAllHighDisputes~T,close)

t.test(FYAllDisputes>0~T,close)
t.test(FYAllHighDisputes>0~T,close)

# Now we can make the coefficient plot for disputes in the first year of the new leaders term.

outcomes=c("FYDisputesInitiated","FYHighDisputesInitiated","FYAllDisputes","FYAllHighDisputes") 

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.95)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),
(output$estimate[2]-output$estimate[1])/sd(close[,outcomes[i]]),-output$conf.int[1]/sd(close[,outcomes[i]]),
-output$conf.int[2]/sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", 
                                 "Standardized Lower Bound")
rownames(standardized_results)=outcomes

t_test_results=standardized_results

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated\nin First Year","High-Level Disputes Initiated\nin First Year",
                "All Disputes\nin First Year","All High-Level Disputes\nin First Year") 
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- standardized_results[,3]
cd$lower <- standardized_results[,4]
cd$upper <- standardized_results[,5]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])


AggressionPlot3 <- ggplot(cd, aes(x=mean,y=measure,color=measure))+geom_vline(xintercept=0, 
linetype="longdash") + geom_errorbarh(aes(xmax =  upper, xmin = lower),size=1.5, height=0)+
geom_point(stat="identity",size=4,fill="white")+xlab("Estimated Treatment Effect (Standardized)")+
ylab("") + theme(legend.position="none",axis.text.x=element_text(size=7.7),axis.text.y=element_text(size=10.7),
axis.title=element_text(size=12.5),plot.title = element_text(lineheight=2.2,size=rel(1.5),face="bold"))+ 
scale_x_continuous(limits=c(-1.4,1.4),breaks=c(-1.2,-1,-0.8,-0.5,-0.2,0,0.2,0.5,0.8,1,1.2),
labels=c("-1.2\nvery large","-1","-0.8\nlarge","-0.5\nmedium","-0.2\nsmall","0","0.2\nsmall","0.5\nmedium",
"0.8\nlarge","1","1.2\nvery large")) + geom_vline(xintercept=c(-1.2,-0.8,-0.5,-0.2,0.2,0.5,0.8,1.2),
linetype=rep("dashed",8),colour=c("turquoise4","turquoise3","turquoise2","turquoise1",
                                  "turquoise1","turquoise2","turquoise3","turquoise4"))+
scale_color_manual(name="",values=c("darkblue","dodgerblue3","dodgerblue4",
                                      "dodgerblue","darkblue","dodgerblue3")) 

ggsave("AggressionPlotInc2.pdf",width=4,height=2,scale = 1.6)























# We can now make the version of the graph with revisionist disputes included that is available in the
# Online Appendix

outcomes=c("FYDisputesInitiated","FYHighDisputesInitiated","FYAllDisputes","FYAllHighDisputes",
           "FYRevisionist","FYHighRevisionist")

standardized_results=matrix(0,nrow=length(outcomes),ncol=5)

for(i in 1:length(outcomes)){
output=t.test(close[,outcomes[i]]~close$T,conf.level=0.95)
standardized_results[i,]=c((output$estimate[2]-output$estimate[1]),sd(close[,outcomes[i]]),(output$estimate[2]-
output$estimate[1])/sd(close[,outcomes[i]]),-output$conf.int[1]/sd(close[,outcomes[i]]),-output$conf.int[2]/
sd(close[,outcomes[i]]))}

colnames(standardized_results)=c("Estimate","SD","Standardized Estimate","Standardized Upper Bound", 
                                 "Standardized Lower Bound")
rownames(standardized_results)=outcomes

cd <- as.data.frame(matrix(NA,length(outcomes),6))
conditions <- c("Disputes Initiated\nin First Year","High-Level Disputes Initiated\nin First Year",
                "All Disputes\nin First Year","All High-Level Disputes\nin First Year","Revisionist\nDisputes",
                "High-Level Revisionist\nDisputes") 
names(cd) <- c("mean","upper","lower","ord","measure")
cd$mean <- standardized_results[,3]
cd$lower <- standardized_results[,4]
cd$upper <- standardized_results[,5]
cd$ord <- c(length(outcomes):1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])

AggressionPlot3_Rev <- ggplot(cd, aes(x=mean,y=measure,color=measure))+geom_vline(xintercept=0, 
linetype="longdash")+geom_errorbarh(aes(xmax =  upper, xmin = lower), size=1.5, height=0)+
geom_point(stat="identity",size=4,fill="white")+xlab("Estimated Treatment Effect (Standardized)")+
ylab("")  + theme(legend.position="none",axis.text.x=element_text(size=7.7),axis.text.y=element_text(size=10.7),
axis.title=element_text(size=12.5),plot.title = element_text(lineheight=2.2,size=rel(1.5),face="bold"))+ 
scale_x_continuous(limits=c(-1.4,1.4),breaks=c(-1.2,-1,-0.8,-0.5,-0.2,0,0.2,0.5,0.8,1,1.2),
labels=c("-1.2\nvery large","-1","-0.8\nlarge","-0.5\nmedium","-0.2\nsmall","0","0.2\nsmall",
"0.5\nmedium","0.8\nlarge","1","1.2\nvery large")) + 
geom_vline(xintercept=c(-1.2,-0.8,-0.5,-0.2,0.2,0.5,0.8,1.2),linetype=rep("dashed",8),
colour=c("turquoise4","turquoise3","turquoise2","turquoise1",
         "turquoise1","turquoise2","turquoise3","turquoise4"))+
scale_color_manual(name="",values=c("darkblue","dodgerblue3","dodgerblue4",
                                    "dodgerblue","darkblue","dodgerblue3")) 

ggsave("AggressionPlotInc2Rev.pdf",width=4,height=2,scale = 1.6)

