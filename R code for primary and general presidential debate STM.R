

## Structural Topic Model (STM) of Primary & General Presidential Debates (2000-2016)

# Debate Transcripts stored in: text <- c(R1,...,P15)
# Metadata stored in: metDat<-data.frame(debateID,year)

library(stm) # upload stm package from the library

# Process text data:
debates<-textProcessor(documents=text,
                       metadata=metaDat,
                       verbos=FALSE) # create object of processed data.


# Estimate STM:
stm1<-stm(documents=debates$documents,
          vocab=debates$vocab,
          K=20,
          prevalence=~debateID+s(year),
          data=metDat)
          #convergence reached at 382 EM iterations

# Visualize Results Using:
par(mfcol=c(1,4))
plot.STM(stm1,type="labels",topics=1:5,
         width=40,text.cex=1.25,n=15,
         main="")
plot.STM(stm1,type="labels",topics=6:10,
         width=40,text.cex=1.25,n=15,
         main="")
plot.STM(stm1,type="labels",topics=11:15,
         width=40,text.cex=1.25,n=15,
         main="")
plot.STM(stm1,type="labels",topics=16:20,
         width=40,text.cex=1.25,n=15,
         main="")


# Visualize with New Topic Labels
topicNames1<-c("Topic 1:\nAgreement on Money & Taxes",
              "Topic 2:\nHealth Insurance, Immigration,\n& Border Security",
              "Topic 3:\nThe Public & Government Leaders",
              "Topic 4:\nModerator Questions on Taxes, the Senate,\n& Public Opinion",
              "Topic 5:\nGovernment & the Constitution")
topicNames2<-c("Topic 6:\nDebate over Public Opinion on\nRe: Taxes & Healthcare",
              "Topic 7:\nImportance of Prior Government Experience",
              "Topic 8:\nThe Economy, Budget Cuts, &\nGovernment Spending",
              "Topic 9:\nDiscussion About Hillary Clinton",
              "Topic 10:\nChanging Gun Laws")
topicNames3<-c("Topic 11:\nThe Iraq War & America's International\nRole in the World",
              "Topic 12:\nAbility to Create Legislation",
              "Topic 13:\nChanges to Healthcare",
              "Topic 14:\nThe Direction of the Country",
              "Topic 15:\nPublic Opinion on the Iraq War")
topicNames4<-c("Topic 16:\nHow to Address Major Issues that Face\nthe Country",
               "Topic 17:\nModerator Questions about Candidates'\nSupport for Various Legislation",
               "Topic 18:\nCandidate Promises to 'Get to Work'\nImmediately if Elected",
               "Topic 19:\nGlobal Security:\nIran, China,Nuclear Weapons,\n& the Military",
               "Topic 20:\nFederal Spending on Social Security,\nEducation, Medicare, & Medicaid")

par(mfcol=c(1,4))
plot.STM(stm1,type="labels",topics=1:5,
         width=50,text.cex=1,
         topic.names=topicNames1,
         custom.labels="",
         main="")
plot.STM(stm1,type="labels",topics=6:10,
         width=50,text.cex=1,
         topic.names=topicNames2,
         custom.labels="",
         main="")
plot.STM(stm1,type="labels",topics=11:15,
         width=50,text.cex=1,
         topic.names=topicNames3,
         custom.labels="",
         main="")
plot.STM(stm1,type="labels",topics=11:15,
         width=50,text.cex=1,
         topic.names=topicNames4,
         custom.labels="",
         main="")

# Display Expected Topic Proportions
topicNames<-c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5","Topic 6",
              "Topic 7","Topic 8","Topic 9","Topic 10","Topic 11","Topic 12",
              "Topic 13","Topic 14","Topic 15","Topic 16","Topic 17",
              "Topic 18","Topic 19","Topic 20")
par(mfcol=c(1,1),bty="n",col="grey40",lwd=5)
plot.STM(stm1,type="summary",
         custom.labels="",
         topic.names=topicNames)

# Estimate Model Effects:
stm1effect<-estimateEffect(formula=1:20~debateID+s(year),
                           stmobj=stm1,
                           metadata=metDat)        #smooth the effect of year

## Make Overlapping Continuous Plots: 

#1 "Agreement on Money & Taxes"
par(bty="n",lwd=2,xaxt="n",col="black")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[1],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Agreement on Money & Taxes",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[1],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[1],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#2 "Health Insurance, Immigration, & Border Security"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[2],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Health Insurance, Immigration, & Border Security",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[2],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[2],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#3 "The Public & Government Leaders"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[3],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="The Public & Government Leaders",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[3],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[3],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#4 "Moderator Questions on Taxes, the Senate, & Public Opinion"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[4],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Moderator Questions on Taxes, the Senate, & Public Opinion",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[4],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[4],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#5 "Government & the Constitution"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[5],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Government & the Constitution",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[5],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[5],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#6 "Debate over Public Opinion on Taxes & Healthcare"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[6],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Debate over Public Opinion on Taxes & Healthcare",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[6],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[6],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#7 "Importance of Prior Government Experience"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[7],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Importance of Prior Government Experience",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[7],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[7],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#8 "The Economy, Budget Cuts, & Government Spending"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[8],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Propo",
                    main="The Economy, Budget Cuts, & Government Spending",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[8],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[8],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#9 "Discussion about Hillary Clinton"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[9],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Discussion About Hillary Clinton",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[9],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[9],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#10 "Changing Gun Laws"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[10],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Changing Gun Laws",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[10],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[10],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#11 "The Iraq War & America's International Role in the World"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[11],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="The Iraq War & America's International\nRole in the World",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.7),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[11],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[11],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#12 "Ability to Create Legislation"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[12],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Ability to Create Legislation",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[12],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[12],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#13 "Changes to Healthcare"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[13],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Changes to Healthcare",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[13],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[13],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#14 "The Direction of the Country"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[14],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="The Direction of the Country",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[14],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[14],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#15 "Public Opinion on the Iraq War"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[15],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Public Opinion on the Iraq War",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[15],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[15],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#16 "How to Address Major Issues that Face the Country"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[16],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="How to Address Major Issues that Face\nthe Country",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[16],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[16],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#17 "Moderator Questions about Candidates' Support for Various Legislation"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[17],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Moderator Questions about Candidates' Support\nfor Various Legislation",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[17],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[17],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#18 "Candidate Promises to 'Get to Work' Immediately if Elected"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[18],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Candidate Promises to 'Get to Work' Immediately\nif Elected",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[18],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[18],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

#19 "Global Security: Iran, China, Nuclear Weapons, & the Military"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[19],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Global Security:\nIran, China, Nuclear Weapons, & the Military",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[19],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[19],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)


#20 "Federal Spending of Social Security, Education, Medicare, & Medicaid"
par(bty="n",lwd=2,xaxt="n")
plot.estimateEffect(stm1effect,                    #Topic proportions in Rep. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[20],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    main="Federal Spending on Social Security,\nEducation, Medicare, & Medicaid",
                    moderator="debateID",
                    moderator.value="Republican",
                    ylim=c(-.1,.45),xlim=c(2000,2016),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stm1effect,                    #Topic proportions in Dem. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[20],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Democrat",
                    ylim=c(-.1,.5),
                    linecol="blue",
                    printlegend=F,add=T)
plot.estimateEffect(stm1effect,                    #Topic proportions in Pres. debates
                    covariate="year",
                    model=stm1,
                    topics=stm1effect$topics[20],
                    method="continuous",
                    xlab="Election Year",
                    ylab="Expected Topic Proportions",
                    moderator="debateID",
                    moderator.value="Presidential",
                    ylim=c(-.1,.45),
                    linecol="green",
                    printlegend=F,add=T)
abline(h=0,lty=4,lwd=1,col="grey45")
abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")
par(xaxt="s")
axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)