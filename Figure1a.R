# Figure1a.R
# Figure1 in the paper
# I generate the scatter plot of math scores against GDP per capita (PPP)
# Source is PISA-OECD Table 1.2.3a and Table IV.3.2

library(dplyr) # for data manipulations

# I placed the raw data file on github for easy recovery
gdp_math_scatter.rdata <- read.table("https://raw.githubusercontent.com/Zagamog/PISA-2012/master/GDP_MATH_SCATTER.txt",
                        header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

# Just check the file is okay; data is sorted in order of increaseing GDP per capita
# I want to look at only those countries below U$20,000 per capita GDP in  PPP 2010
gdp_math_scatter.rdata


attach(gdp_math_scatter.rdata) # This is not strictly kosher, but it means I can avoid referring to 
                         # the datframe name each time that I invoke a variable
plot(pisa_m~gdp, 
     xlim=c(0,65000), ylim=c(300,700), xaxs="i", yaxs="i",  #xaxs = i instead of r to eliminate offset
     pch=19,col="indianred4",
     xlab='GDP per Capita in PPP 2010', ylab= 'PISA Math Average Score 2012', font.lab=1,
     data=gdp_math_scatter.rdata)

# I want Vietnam to stand out in the plot
points(4098,511,col="blue",bg="red",pch=24)
points(4098,511,col="blue",bg="red",pch=25)



text(4098,511,labels="Vietnam (511)",pos=4,cex=0.75,col="red") # pos=4 means to the side

# Key other countries, I don't want to clutter the graph by naming all countries
points(9555,376,col="blue",bg="blue",pch=19)
text(9555,376,labels="Colombia (376)",pos=4,cex=0.75,col="blue") # pos=4 means to the side
text(4700,375,labels="Indonesia",pos=1,cex=0.75,col="blue") # pos=1 means to the bottom
text(9400,360,labels="Peru",pos=4,cex=0.75,col="blue") # pos=4 means to the right side
text(18805,613,labels="Shanghai-China",pos=1,cex=0.75) # pos=1 means to the bottom
text(48962,531,labels="Switzerland",pos=1,cex=0.75,col="red") # pos=1 means to the bottom
text(36030,519,labels="Finland",pos=2,cex=0.75,col="red") # pos=2 means to the left side


# I need to highlight a few other countries as well
points(48962,531,col="red",bg="red",pch=19)
points(36030,519,col="red",bg="red",pch=19)
points(4638,375,col="blue",bg="blue",pch=19)
points(9350,368,col="blue",bg="blue",pch=19)


temp1 <- filter(gdp_math_scatter.rdata, gdp_math_scatter.rdata$country != "vietnam" & gdp_math_scatter.rdata$country != "Shanghai_China"
                & gdp_math_scatter.rdata$country != "luxembourg" & gdp_math_scatter.rdata$country != "Qatar")


loess_fit <- loess(pisa_m ~ gdp, data=temp1, span=2)
lines(temp1$gdp, predict(loess_fit), col = "purple",lwd=2, lty=2,add=T)




# Save data file locally for later use
save(gdp_math_scatter.rdata, file = "C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/gdp_math_scatter.rdata") 

# I then use RStudio to save the output as a .pdf file that I can include in my Latex input file
# for publication. 

myCindices


