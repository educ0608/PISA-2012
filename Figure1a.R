# Figure1a.R
# Figure1 in the paper
# I generate the scatter plot of math scores against GDP per capita (PPP)
# Source is PISA-OECD Table 1.2.3a and Table IV.3.2

# I load the data for the scatter plot - the filename reflects my age ! 

cards <-"country  gdp  pisa_m
vietnam  4098	511
indonesia	4638	375
Jordan	5752	386
albania	8631	394
Peru	9350	368
tunisia	9410	388
colombia	9555	376
thailand	9748	427
Serbia	11421	449
costa_rica	11579	407
kazakhstan	12092	432
brazil	12537	391
montenegro	13147	410
uruguay	14004	409
bulgaria	14203	439
romania	14531	445
malaysia	15077	421
mexico	15195	413
turkey	15775	448
argentina	15868	388
latvia	16902	491
chile	17312	423
lithuania	18022	479
Shanghai_china	18805	613
croatia	19026	471
russian_federation	19811	482"

# fig1_in <-read.table(textConnection(cards),header=TRUE)
# closeAllConnections()
# fig1_in

# The data is in the format of an R dataframe as we can check
# class(fig1_in)
load("C:/Country/Vietnam/Data/PISA/RDATA/gdp_math_scatter.rdata")

attach(gdp_math_scatter) # This is not strictly kosher, but it means I can avoid referring to the datframe name each time
                # that I invoke a variable
plot(pisa_m~gdp, 
     xlim=c(0,25000), ylim=c(300,700), xaxs="i", yaxs="i",  #xaxs = i instead of r to eliminate offset
     pch=19,col="indianred4",
     xlab='GDP per Capita in PPP 2010', ylab= 'PISA Math Average Score 2012', font.lab=2,
     data=gdp_math_scatter[1:26,])

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
abline((lm(pisa_m~gdp, data=gdp_math_scatter)),lty=3, untf=T, lwd=3)

# I then use RStudio to save the output as a .pdf file that I can include in my Latex input file
# for publication. 



