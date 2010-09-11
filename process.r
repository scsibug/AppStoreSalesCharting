# Process Apple AppStore sales data into charts.
#
# Usage:
#   R --no-save --args SalesData.txt < process.R
#
# Licensed under the New BSD License, see LICENSE 
# file distributed with this work.

defaultChartDim <- 800 # Default size of graphics

# Read tab-sep file from first command line argument.
filename <- commandArgs(TRUE)[1]
s_all <- read.table(filename, header=TRUE, sep="\t")

# Convert date columns
s_all$Begin.Date <- as.Date(s_all$Begin.Date,"%m/%d/%Y")
s_all$End.Date <- as.Date(s_all$End.Date,"%m/%d/%Y")

# Group by currency, summing units sold
sale_currencies <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Currency))
colnames(sale_currencies) <- c("Currency", "Count")

# Group by country, summing units sold
sale_countries <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Country.Code))
colnames(sale_countries) <- c("Country", "Count")

# Sales per period-ending-date, overall.
units_by_time <- aggregate(s_all$Units,FUN=sum,by=list(s_all$End.Date))
colnames(units_by_time) <- c("End.Date", "Units")
png("units_linechart.png", width=defaultChartDim, height=defaultChartDim)
plot(units_by_time$End.Date, units_by_time$Units, type="b", ylab="Units", xlab="Period End Date")
dev.off()

# Sales by period-ending-date, grouped by country.
# Grouped either weekly or daily based on the original data provided.
countries_by_time <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Country.Code,s_all$End.Date))
colnames(countries_by_time) <- c("Country", "End.Date", "Units")
xrange <- range(countries_by_time$End.Date)
yrange <- range(countries_by_time$Units)
png("country_linechart.png", width=defaultChartDim, height=defaultChartDim)
plot(xrange, yrange, type="n", ylab="Units", xlab="Period End Date")
countries_by_time$Cf <- as.numeric(countries_by_time$Country)
ncountries <- max(countries_by_time$Cf)
colors <- rainbow(ncountries)
linetype <- c(1:ncountries)
plotchar <- seq(18,18+ncountries,1)
countrylabels <- vector(mode="list")
for (i in 1:ncountries) {
  country <- subset(countries_by_time, Cf==i)
  countrylabels <- append(countrylabels, toString(country$Country[1]))
  lines(country$End.Date, country$Units,type="b", lwd=1.5,lty=linetype[i],col=colors[i],pch=plotchar[i])
}
title("Sales by period ending date, grouped by country")
legend(xrange[1],yrange[2], countrylabels, cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Country")
dev.off()

# Sales by period-ending-date, grouped by currency.
# Grouped either weekly or daily based on the original data provided.
currency_by_time <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Currency,s_all$End.Date))
colnames(currency_by_time) <- c("Currency", "End.Date", "Units")
xrange <- range(currency_by_time$End.Date)
yrange <- range(currency_by_time$Units)
png("currency_linechart.png", width=defaultChartDim, height=defaultChartDim)
plot(xrange, yrange, type="n", ylab="Units", xlab="Period End Date")
currency_by_time$Cf <- as.numeric(currency_by_time$Currency)
ncurrencies <- max(currency_by_time$Cf)
colors <- rainbow(ncurrencies)
linetype <- c(1:ncurrencies)
plotchar <- seq(18,18+ncurrencies,1)
currlabels <- vector(mode="list")
for (i in 1:ncurrencies) {
  currency <- subset(currency_by_time, Cf==i)
  currlabels <- append(currlabels, toString(currency$Currency[1]))
  lines(currency$End.Date, currency$Units,type="b", lwd=1.5,lty=linetype[i],col=colors[i],pch=plotchar[i])
}
title("Sales by period ending date, grouped by currency")
legend(xrange[1],yrange[2], currlabels, cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Currency")

dev.off()

# Pie chart of overall sales volume grouped by currency
png("currency_piechart.png", width=defaultChartDim, height=defaultChartDim)
pie(sale_currencies$Count, labels=sale_currencies$Currency,main="Units sold by currency")
dev.off()

# Pie chart of overall sales volume grouped by country
png("country_piechart.png", width=defaultChartDim, height=defaultChartDim)
pie(sale_countries$Count, labels=sale_countries$Country,main="Units sold by country")
dev.off()

