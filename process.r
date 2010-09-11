s_all <- read.table("WeeklySales.txt", header=TRUE, sep="\t")
sales <- aggregate(s_all$Units,FUN=sum,by=list(s_all$SKU, s_all$Promo.Code, s_all$Developer.Proceeds, s_all$Customer.Currency, s_all$Units))
colnames(sales) <- c("Product", "Promo", "Individual_Proceeds", "Currency", "Units")
sale_currencies <- aggregate(sales$Units,FUN=sum,by=list(sales$Currency))
colnames(sale_currencies) <- c("Currency", "Units")

# Pie chart of currencies

png("currency_piechart.png")
pie(sale_currencies$Units, labels=sale_currencies$Currency,main="Currency by Sales")
dev.off()

