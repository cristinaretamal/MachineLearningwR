# install.packages("arules")
library(arules)

################################################
# read data

data(Groceries)

##############################################
# Data preparation

# summary statistics
summary(Groceries)

# contents of the spare matrix
inspect(Groceries[1:5])

# support level for the first three items
itemFrequency(Groceries[,1:3])

###############################################
# Visualizing

# proportion of transactions. 
#Histogram showing the 8 items with at least 10% support
itemFrequencyPlot(Groceries, support=0.1)

# Histogram limiting the plot to a specific number of items
itemFrequencyPlot(Groceries, topN=20)





