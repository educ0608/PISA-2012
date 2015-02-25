# Figure1a.R
# Figure1 in the paper
# I generate the scatter plot of math scores against GDP per capita (PPP)
# Source is PISA-OECD Table 1.2.3a and Table IV.3.2

# I placed the raw data file on github for easy recovery
gdp_math_scatter.rdata <- read.table("https://raw.githubusercontent.com/Zagamog/PISA-2012/master/GDP_MATH_SCATTER.txt",
                        header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

# Just check the file is okay; data is sorted in order of increaseing GDP per capita
# I want to look at only those countries below U$20,000 per capita GDP in  PPP 2010
gdp_math_scatter.rdata


attach(gdp_math_scatter.rdata) # This is not strictly kosher, but it means I can avoid referring to 
                         # the datframe name each time that I invoke a variable
plot(pisa_m~gdp, 
     xlim=c(0,25000), ylim=c(300,700), xaxs="i", yaxs="i",  #xaxs = i instead of r to eliminate offset
     pch=19,col="indianred4",
     xlab='GDP per Capita in PPP 2010', ylab= 'PISA Math Average Score 2012', font.lab=2,
     data=gdp_math_scatter.rdata[1:26,])

# I want Vietnam to stand out in the plot
points(4098,511,col="red",bg="red",pch=24)
points(4098,511,col="red",bg="red",pch=25)

text(4098,511,labels="Vietnam",pos=4,cex=0.75) # pos=4 means to the side

# Key other countries, I don't want to clutter the graph by naming all countries
points(9555,376,col="blue",bg="blue",pch=19)
text(9555,376,labels="Colombia",pos=4,cex=0.75) # pos=4 means to the side
text(4638,375,labels="Indonesia",pos=1,cex=0.75) # pos=1 means to the bottom
text(9350,368,labels="Peru",pos=1,cex=0.75) # pos=1 means to the bottom
text(15868,388,labels="Argentina",pos=4,cex=0.75) # pos=4 means to the side
text(9748,427,labels="Thailand",pos=3,cex=0.75) # pos=3 means to the top
text(18805,613,labels="Shanghai-China",pos=1,cex=0.75) # pos=1 means to the bottom

# I want to plot a least squares trend line
abline((lm(pisa_m~gdp, data=gdp_math_scatter.rdata[1:26,])),lty=3, untf=T, lwd=3,col="grey")

# Save data file locally for later use
save(gdp_math_scatter.rdata, file = "C:/Country/Vietnam/Data/PISA/RDATA/PISA-2012/gdp_math_scatter.rdata") 

# I then use RStudio to save the output as a .pdf file that I can include in my Latex input file
# for publication. 



