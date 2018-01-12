#I can't do a the column transforms I want to do with the geometry variable. Not something I can avoid
#So, not to mess up the order of the columns referenced below, we can just set geometry to NULL.
#This has to do with sf, which I don't know.
CountyPop2 <- CountyPop
st_geometry(CountyPop2) <- NULL

#We're going to create columns that show percentage of the population represented by each demographic
#Start by initializing several variables for loop
count <- 0
estimateCols <- 0
demog <- c()
newPercentVars <- c()

for (name in names(popvars)){
  count <- count + 1
  estimateCols <- 2*count
  colnum <-  count + 19
  #first create columns that reflect the percent total for each demographic
  CountyPop2[colnum] <- CountyPop2[estimateCols]/CountyPop2[2]
  
  #then create human-readable names for each column
  demog[count] <- gsub('Pop', '', name)
  newPercentVars[count] <- paste0("percent", .simpleCap(demog[count]))
  colnames(CountyPop2)[colnum] <- newPercentVars[count]
}

#Summary stats for all of County, we'll add these as median lines to the plots later
CountyWideTotalPops <- apply(CountyPop2[, seq(2, 16, 2)], 2, sum)
CountyWideMedianPercentages<- apply(CountyPop2[, c(19:26)], 2, median, na.rm=T)

CountyWidePercentPops <- CountyWideTotalPops/CountyWideTotalPops[1]

#create and save pie chart for basic demographics
png(filename="./plots/BasicDemographicInformation.png")
pie(CountyWideTotalPops[-1])
dev.off()


#bins for percentages
br <- seq(0,1,by=0.1)

#Residents? Does ACS capture non-citizens?

#Create two plots for Number of Census Tracts by Percentage White/Black Residents
png(filename="./plots/NumberofNeighborhoodsbyPercentageWhite.png")
hist(CountyPop2$percentWhite, br, plot = T, col="black", 
     main="Number of Census Tracts by \n Percentage White Residents",
     xlab="Percentage of White Residents", ylab="Number of Census Tracts", 
     border="white")
abline(v = CountyWidePercentPops[2], col="red", lwd=4)
abline(v = CountyWideMedianPercentages[2], col="blue", lwd=4)
legend("topleft", c("Median", "Mean"), col=c("blue", "red"), lwd=4)
dev.off()

png(filename="./plots/NumberofNeighborhoodsbyPercentageBlack.png")
hist(CountyPop2$percentBlack, br, plot = T, col="black", 
     main="Number of Census Tracts by \n Percentage Black Residents",
     xlab="Percentage of Black Residents", ylab="Number of Census Tracts",
     border="white")
abline(v = CountyWidePercentPops[3], col="red", lwd=4)
abline(v = CountyWideMedianPercentages[3], col="blue", lwd=4)
legend("topright", c("Median", "Mean"), col=c("blue", "red"), lwd=4)
dev.off()


#white to black dissimilarity index
CountyTotalPop <- sum(CountyPop2$totalPop)
blackPropCounty <- sum(CountyPop2$blackPop)/ CountyTotalPop
whitePropCounty <- sum(CountyPop2$whitePop)/ CountyTotalPop
nonwhitePropCounty <- 1 - whitePropCounty
whitePopCounty <- sum(CountyPop2$whitePop)
nonwhitePopCounty <- CountyTotalPop - whitePopCounty

CountyPop2$dissimilarityNumerator <- (CountyPop2$totalPop * abs(CountyPop2$percentBlack-blackPropCounty))
disSum <- sum(CountyPop2$dissimilarityNumerator, na.rm=T)

whiteToBlackdissimilarityIndexCounty <- disSum/(2 * CountyTotalPop * blackPropCounty*(1-  blackPropCounty))

#white to non-white dissimilarity
#Another dissimilarity index (1/2) SUM (bi /B - wi / W)
CountyPop2$percentNonwhite <- 1- CountyPop2$percentWhite
CountyPop2$totalNonwhite <- CountyPop2$totalPop - CountyPop2$whitePop

CountyPop2$dissimilarityPreSum <- abs((CountyPop2$totalNonwhite/nonwhitePopCounty) - (CountyPop2$whitePop/whitePopCounty))
dissimilarityIndexCounty2 <- sum(CountyPop2$dissimilarityPreSum)/2



#Measures of Centralization, relative centralization

#Measures of Exposure, relative clustering

#Some resources:
#http://strimas.com/r/tidy-sf/
#http://moderndata.plot.ly/interactive-r-visualizations-with-d3-ggplot2-rstudio/
