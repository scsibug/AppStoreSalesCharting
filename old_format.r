sales <- read.table("SampleDailySalesOldFmt.txt", header=TRUE, sep="\t")

# Pie chart of currencies

currencies <- table(sales["Customer.Currency"])
#currframe <- as.vector(as.data.frame(currencies)[1])

png("currency_piechart.png")
pie(table(sales["Customer.Currency"]), main="Currency by Sales")
dev.off()

png("currency_barchart.png")
plot(sales["Customer.Currency"], main="Currency by Sales")
dev.off()

png("sale_count_barchart.png")
plot(sales["End.Date"], main="Downloads by Day")
dev.off()

png("sale_count_linechart.png")
plot(table(sales["End.Date"]),
     main="Downloads by Day",
     xlab="Date",
     ylab="Sales",
     type="l")
dev.off()

