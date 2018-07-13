library(RCurl)
library(readxl)
library(arules)
library(arulesViz)
library(plyr)


temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online Retail.xlsx",  temp.file, mode = "wb")
retail <- read_excel(temp.file)


retail <- read.csv('Online Retail.csv')
retail <- retail[complete.cases(retail), ]

head(retail)
summary(retail)

retail <- retail[retail$Quantity >= 0, ]

retail_sorted  <- retail[order(retail$CustomerID),]

itemList <- ddply(retail,c("InvoiceNo"), 
                    function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
itemList$InvoiceNo <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

print('Description of the transactions')
transaction <- read.transactions('market_basket.csv', format = 'basket', sep=',', rm.duplicates = TRUE)

itemFrequencyPlot(transaction, topN=20, type='absolute')


#Rule1
# calculate minimum support: 4 items per day for 7 days a week for 18536 items = 4*7/18536
rules <- apriori(transaction, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='lift', decreasing = TRUE)

#remove redundent rules
rules <- rules[!is.redundant(rules)]
summary(rules)

inspect(rules[1:20])

topRules <- rules[1:20]

plot(topRules, method="graph")

#RHS = Decoration
rules <- apriori(data = transaction, parameter = list(supp= 0.001, conf=0.8),appearance = list(default = "lhs",rhs = c("DECORATION") ))
inspect(rules)

#LHS = Decoration
rules <- apriori(data = transaction, parameter = list(supp= 0.001, conf=0.8),appearance = list(lhs= c("DECORATION"), default = "rhs" ))
inspect(rules)