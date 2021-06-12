install.packages("arules")
install.packages("tidyverse")
install.packages("arulesViz")

library(arulesViz)
library(arules)
library(tidyverse)

###################################
data(Groceries)
summary(Groceries)
itemLabels(Groceries)
inspect(Groceries[1:2])

itemFrequencyPlot(Groceries,support = 0.1)
itemFrequencyPlot(Groceries,topN = 12)

apriori(Groceries)

rule_set <- apriori(Groceries,parameter = list(support = 0.002, conf = 0.66, minlen = 2, maxlen = 3))

inspect(rule_set[1:3])
inspect(head(sort(rule_set ,by = "lift"),6))

 ####################################

# Read CSV file from local computer and assign to the data variablr
data  <- read.csv( file = "/home/vikas/Documents/acadamic/system-simulation/data.csv",sep = ";")

# Create Group from basket id for data set
group_data <- group_by(data,basket_id)

# Make a list of item base on grop set
basket <- summarise(group_data,count = n(),basket_list = list(article_name))
basket
# Filter data first 25000 basket list analysis 
basket_list <- basket$basket_list[1:25000]

basket_list[1:2]

# Convert list in to transaction 
retail_transaction <- as(basket_list,"transactions")
retail_transaction
# Test plot function and set of rulse
itemFrequencyPlot(retail_transaction,topN = 8)
apriori(retail_transaction)

# Create Rule for basket set 
basket_rule <- apriori(retail_transaction, parameter = list(support = 0.0002, conf = 0.66, minlen = 2, maxlen = 3))

# filter and select first 6 rule from top
basket_rule_six <- head(sort(basket_rule ,by = "lift"),6)
inspect(head(sort(basket_rule ,by = "lift"),6))
# Create plot with method two-key plot
plot(basket_rule,method = "two-key plot",interactive = TRUE)
# Create Extra plote for vizulization
plot(basket_rule,methods = "grouped", control = list(10))
plot(basket_rule_six, method="grouped")


#Filter rule apply on tweezer with magnifing glass 
tweezer_rule <- subset(basket_rule, subset = rhs %in% "tweezers with magnifying glass")
tweezer_rule_head <- head(sort(tweezer_rule ,by = "lift"),4)
inspect(tweezer_rule_head)
# Create plot with method paracoord 
plot(tweezer_rule_head,method="paracoord")




