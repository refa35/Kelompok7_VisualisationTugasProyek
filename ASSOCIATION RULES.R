#memanggil data data
data=read.csv("C:/Users/USER/Downloads/GroceryStoreDataSet.csv")
View(data)
str(data)
# transformasi Data
list <- c()

for(i in 1:nrow(data)) {
  c <- unlist(strsplit(data[i,1],","))
  list <- append(list, c)
}
# c <- unlist(strsplit(rawdata2[1,1],","))
library(arules)
library(arulesViz)
list <- unique(list)
list

m <- matrix(NA, nrow=nrow(data), ncol=length(list))

for(j in 1:length(list)) {
  for (i in 1:nrow(data)) {
    m[i,j] <- grepl(list[j], data[i,], fixed = TRUE)
  }
}


data <- as.data.frame(m)
colnames(data) <- list
View(data)


#acosiation rule
library(arules)
library(arulesViz)
rules.all<-apriori(data)
rules.all
inspect(rules.all)

rules <- apriori(data, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
quality(rules) <- round(quality(rules), digits=3)


#menghilangkan reduntdancy
redundant <- is.redundant(rules)
which(redundant)

rules.pruned<-rules[!redundant]
inspect(rules.pruned)

#lIft 
rules.pruned <- head(rules.pruned, 5, by = "support")
inspect(rules.pruned)

#visualisasi
dev.new(width=500, height=500)
plot(rules, measure=c("support", "confidence"), shading="lift", interactive=FALSE)

