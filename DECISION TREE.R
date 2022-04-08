#memanggil data data
data=read.csv("C:/Users/USER/Downloads/data.csv")
View(data)
str(data)
options(scipen=999)
dev.new(width=500, height=500)
#plot x
data["id"] <- c(1:4600)
plot(data$id,data$sqft_lot)
# Preprocessing
# data cleaning
# menghilangkan pencilan dan missing data
library(dplyr)
data <- filter(data, sqft_lot < 800000)
plot(data$id,data$sqft_lot)
plot(data$id,data$price)
data <- filter(data, price < 5000000)
# Dimension Reduction
head(data)

data <- data[-c(1,14,15,16,17,18,19)]
head(data)


#Transformasi data
data$floors <- as.factor(data$floors)
data$view <- as.factor(data$view)
data$condition <- as.factor(data$condition)


#data discretiazation
mean = mean(data$price)

data$price[data$price < mean] <- 0
data$price[data$price >= mean] <- 1
data$price <- as.factor(data$price)
head(data)
#Membagi data
house<-data
head(house)
str(house)
set.seed(1234)
ind <- sample(2,nrow(house),replace=TRUE, prob=c(0.7,0.3))
house.train<-house[ind==1,]
house.test<-house[ind==2,]
str(house.train)

# train a decision tree
library(rpart)
library(rpart.plot)
names(house)
house_rpart <- rpart(price~., data = house.train)

attributes(house_rpart)
print(house_rpart)
plot(house_rpart)
text(house_rpart)
prp(house_rpart, digits=-3)

pt <- which.min(house_rpart$cptable[,"xerror"])
cp <- house_rpart$cptable[opt, "CP"]
print(house_rpart$cptable)
plotcp(house_rpart)
house_prune <- prune(house_rpart, cp = cp)
print(house_prune)
plot(house_prune)
text(house_prune, use.n=T)
prp(house_prune, digits=-3)

opt <- which.min(house_rpart$cptable[,"xerror"])
cp <- house_rpart$cptable[opt, "CP"]
print(house_rpart$cptable)
plotcp(house_rpart)
house_prune <- prune(house_rpart, cp = cp)
print(house_prune)
plot(house_prune)
text(house_prune, use.n=T)
prp(house_prune, digits=-3)
# prediction
house_pred <- predict(house_prune, newdata=house.test, type="class")
table <- table(house_pred, house.test$price)

accuracy <- (table[1,1]+table[2,2])/(table[1,1]+table[2,2]+table[2,1]+table[1,2])  
accuracy

precision <- table[1,1]/(table[1,1]+table[2,1])
precision

recall <- table[1,1]/(table[1,1]+table[1,2])
recall

fmeasures <- (2*(precision)*(recall))/(precision+recall)
fmeasures
prp(house_prune, extra = 6, box.palette = "auto")
prp(house_prune, extra = 6, box.palette = c("pink", "palegreen3"))
prp(house_prune, extra = 6, box.palette = "Blues") 
prp(house_prune, extra = 6, box.palette = "-Blues")







