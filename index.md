[Back to Main Page](https://milesdwilliams15.github.io/)

Many with a background in text analysis are likely familiar with the structural topic model (STM), an unsupervised method of machine learning for text analysis that relies on a set of document "metadata" (i.e., a matrix of document covariates) in the identification of "topics" and the estimation of topic distributions over documents and word distributions over topics.

The "stm" package in R offers users lots of options for visualizing results from STM model objects and estimated effects. However, the commands available with the stm package for making these visualizations (plot.STM() and plot.estimateEffect()) leave much to be desired in terms of making crisp, visually appealing graphics. 

While it would be amazing if someone would develop ggplot2 functionality for the stm package, because stm plots make use of R's base plotting function (plot()), users still have substantial flexibility to make better graphics than those offered by the stm package's base plotting commands. This is because stm plots are responsive to the par() command, which offers lots of options for personalizing graphical parameters. Below, I'll lay out a couple of examples for making graphics that are both more visually appealing and easier to interpret.

## Taking stm Plots from "Flab" to "Fab"
### Top Topics
One of the basic plotting commands in the stm package is plot.STM(). This command offers users several options in terms of displaying results from STM estimation. One of these options allows the user to display STM topics ordered by expected topic proportions. 

Here's an example of a very basic plot of top topics by expected topic proportions using a structural topic model of primary and general election presidential debates (2000 to 2016) where K=20:

    # Estimate STM:
    stm1<-stm(documents=debates$documents,vocab=debates$vocab,
              K=20,prevalence=~debateID+s(year),
              data=metDat) #convergence reached at 382 EM iterations

    # Display Expected Topic Proportions
    topicNames<-c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5","Topic 6",
              "Topic 7","Topic 8","Topic 9","Topic 10","Topic 11","Topic 12",
              "Topic 13","Topic 14","Topic 15","Topic 16","Topic 17",
              "Topic 18","Topic 19","Topic 20")
    plot.STM(stm1,type="summary",custom.labels="",topic.names=topicNames)

![top topics old](https://cloud.githubusercontent.com/assets/23504082/21202506/264a14d0-c214-11e6-9879-71c69d1b7fe3.jpg)

While the above graphic "does the job," we can do better. By changing the parameters using the par() command, we can make a better looking visualization.

Use the par() command:

    par(bty="n",col="grey40",lwd=5)

Then make a new plot.STM() plot:

    plot.STM(stm1,type="summary",custom.labels="",topic.names=topicNames)

That should give you the below graphic:

![top topics](https://cloud.githubusercontent.com/assets/23504082/21201674/7d7441a2-c211-11e6-8675-8ba3576535ef.jpg)

While this is basically the same plot as the one before, it looks a better (at least better than the basic plot offered by the stm package).

### A "Continuous" Plot:  Expected Topic Proportions by Debate and by Election Year
The stm package also allows the user to conduct further regression analysis of estimated effects using the estimateEffect() command. This can be done easily using the following code:

    # Estimate Model Effects:
    stm1effect<-estimateEffect(formula=1:20~debateID+s(year),
                               stmobj=stm1,metadata=metDat) #smooth the effect of year

Above, I've estimated topic proportions for each of the 20 topics estimated via the STM per presidential debate as a function of whether the debate was a Democratic or Republican primary debate or a general election debate and by the election year.

Following estimation, the plot.estimateEffect() command can be used to display expected topic proportions over time and by debate type. Below, I plot the expected topic proportions for Topic 7, which has to do with presidential candidate discussion of the importance of prior government experience.

    #7 "Importance of Prior Government Experience"
    plot.estimateEffect(stm1effect,  #Topic proportions in Rep. debates
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
    plot.estimateEffect(stm1effect,  #Topic proportions in Dem. debates
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
    plot.estimateEffect(stm1effect,  #Topic proportions in Pres. debates
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
    legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
       lty=1)

![continuous plot old](https://cloud.githubusercontent.com/assets/23504082/21202547/41d5e418-c214-11e6-85c3-c6ccf6c5b873.jpg)

That code gives me the above graphic. While not horrible, this visualization leaves much to be desired. One notable problem is that the years plotted along the x axis are not actually the election years included in model estimation. Fixing this problem would go a long way in making the graph easier to interpret and, overall, more informative. To do this, I'll have to make use of the par() command, the abline() command, and the axis() command.

    #7 "Importance of Prior Government Experience"
    par(bty="n",lwd=2,xaxt="n")  # Get rid of the box around the plot, make the lines thicker,
                                 # and tell R to get rid of the x axis.
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
    abline(h=0,lty=4,lwd=1,col="grey45")  # Put a dotted line on the y axis at 0.
    abline(v=c(2000,2004,2008,2012,2016),lty=2,lwd=1,col="grey45")  # Put dotted lines 
                                                                    # on the x axis at each election year.
    par(xaxt="s") # Tell R that it's OK to plot an x axis.
    # Plot a new x axis that is specified to the parameters we want:
    axis(1,at=c(2000,2004,2008,2012,2016),labels=c(2000,2004,2008,2012,2016),las=2)
    legend("topright",legend=c("Republican","Democratic","General"),col=c("red","blue","green"),
           lty=1) # Add the legend.

As you can see below, the new graphical parameters make for a much better graph:

![continuous plot](https://cloud.githubusercontent.com/assets/23504082/21201663/717a62dc-c211-11e6-95ae-9c5f893ed0af.jpg)

Not only is it clearer when the election years are, the lines are thicker, which makes for an easier to interpret graph. 

(By the way, it's clear that 2008 was the "year of experience," while more recently prior government experience has fallen off sharply as an issue discussed by presidential candidates. Maybe this finding offers some support for the idea that 2016 was the "year of the outsider?")

## Conclusion
All in all, R users need not be limited by the plotting functions available in the stm package when displaying the results from STM estimation. Hopefully this page serves as a source of inspiration for those interested in turning their stm plots from blah to ah-ha!

[Back to Main Page](https://milesdwilliams15.github.io/)