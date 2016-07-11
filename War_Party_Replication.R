














# Test for change in presidents

data2=data[data$WinnerIncumbent==1|data$RunnerUpIncumbent==1,]
data2=data2[data2$Democracy==1&abs(data2$Z)<0.04,]

t.test(abs(Aggression-PreviousAggression)~WinnerIncumbent,data2)
t.test(abs(AllMIDs-PreviousAllMIDs)~WinnerIncumbent,data2)