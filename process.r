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

# Group by currency, summing units sold
sale_currencies <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Currency))
colnames(sale_currencies) <- c("Currency", "Count")

# Group by country, summing units sold
sale_countries <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Country.Code))
colnames(sale_countries) <- c("Country", "Count")

# Pie chart of overall sales volume grouped by currency
png("currency_piechart.png", width=defaultChartDim, height=defaultChartDim)
pie(sale_currencies$Count, labels=sale_currencies$Currency,main="Units sold by currency")
dev.off()

# Pie chart of overall sales volume grouped by country
png("country_piechart.png", width=defaultChartDim, height=defaultChartDim)
pie(sale_countries$Count, labels=sale_countries$Country,main="Units sold by country")
dev.off()

