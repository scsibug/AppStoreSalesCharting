s_all <- read.table("WeeklySales.txt", header=TRUE, sep="\t")

sale_currencies <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Currency))
colnames(sale_currencies) <- c("Currency", "Count")

sale_countries <- aggregate(s_all$Units,FUN=sum,by=list(s_all$Country.Code))
colnames(sale_countries) <- c("Country", "Count")

# Pie chart of currencies & countries

png("currency_piechart.png")
pie(sale_currencies$Count, labels=sale_currencies$Currency,main="Sales by Currency")
dev.off()

png("country_piechart.png")
pie(sale_countries$Count, labels=sale_countries$Country,main="Sales by Country")
dev.off()

