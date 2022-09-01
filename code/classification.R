library(car)
library(rgl)
library(mvtnorm)
library(MASS)
library(class)
library(e1071)

setwd("~/Documents/wireless_project/second_capture/processed")

#browsing <- read.csv("processed_browsing.csv", stringsAsFactors = T)
#idle <- read.csv("processed_idle.csv", stringsAsFactors = T)
#instagram <- read.csv("processed_instagram.csv", stringsAsFactors = T)
#netflix <- read.csv("processed_netflix.csv", stringsAsFactors = T)
#spotify <- read.csv("processed_spotify.csv", stringsAsFactors = T)
videocall <- read.csv("processed_videocall.csv", stringsAsFactors = T)
voip <- read.csv("processed_voip.csv", stringsAsFactors = T)
youtube <- read.csv("processed_youtube.csv", stringsAsFactors = T)

# This is the final dataset
#dataset <- rbind(browsing, idle, instagram, netflix, spotify, videocall, voip, youtube)
dataset <- rbind(videocall, voip, youtube)
# We look at the boxplot per column to study the variance
notype <- dataset[,1:11]
X11()
boxplot(notype, col = 'yellow')

# The data isn't centered, we center the data around its mean
notype <- scale(notype, scale = FALSE)
boxplot(notype, col = 'yellow')

# We standardize the data because of the variance of one of the features
notype <- scale(notype)
boxplot(notype, col = 'yellow')

# We compute the principal components and also the scores
pc <- princomp(notype, scores = T)
summary(pc)

# Scree plot (plot of explained variance per component)

plot(cumsum(pc$sd^2)/sum(pc$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(dataset),labels=1:ncol(dataset),las=2)

# We try to interpret the first 5 PCs

x11()
par(mfcol = c(2,2))
for(i in 1:4) barplot(pc$loadings[,i], ylim = c(-1, 1), main=paste("PC",i), col = "yellow")

# We see that they are mostly weighted means and contrasts between features

X11()
plot(pc$scores[,1:2], col=dataset$type_of_traffic, pch=19)

# We plot the pairs and color accordingly

X11()
pairs(pc$scores[,1:3], col=dataset$type_of_traffic, pch = 19)
legend("topright", fill = unique(dataset$type_of_traffic), legend = c(levels(dataset$type_of_traffic)))


#################################################################
#         Training our classifier
#################################################################
# Next step -> shuffle data and extract 70% for testing
shuffled <- dataset[sample(1:nrow(dataset)),]
train <- shuffled[1:(floor(0.65*nrow(shuffled))),]
test_traffic <- shuffled[(nrow(train) + 1):nrow(shuffled),]

# Normalize training data
notype <- scale(train[,1:11])

X11()
boxplot(notype, col = 'yellow')

# We compute the principal components and also the scores
pc <- princomp(notype, scores = T)
summary(pc)

X11()
pairs(pc$scores[,1:3], col=train$type_of_traffic, pch = 19)
legend("topright", fill = unique(train$type_of_traffic), legend = c(levels(train$type_of_traffic)))


# Leave-one-out cross validation on training set to choose optimal parameter
errors <- vector(mode="numeric", length = 49)
for(k in 2:50) {
  for(i in 1:nrow(notype)) {
    trainset <- notype[-i,]
    testset <- notype[i,]
    train_target <- train$type_of_traffic[-i]
    traffic.knn <- knn(train = trainset, test = testset, cl = train_target, k = k)
    if(traffic.knn[1] != train$type_of_traffic[i])
      errors[k-1] <- errors[k-1] + 1 
  }
}
min(errors)

# The minimum test error (via LOO) is 3 and we can choose from k in (2,3,4,5). Intuition tells us 5 is a reasonable parameter

# We scale testing dataset
test_scaled <- scale(test_traffic[,1:11], attr(notype, "scaled:center"), attr(notype, "scaled:scale"))

# We "train" KNN
traffic.knn <- knn(train = notype, test = test_scaled, cl = train$type_of_traffic, k = 5)
traffic.knn

# Confusion table 
table(class.true = test$type_of_traffic, class.assigned=traffic.knn)


# Now we plot the classification regions, we use the first 3 PCs

x11()
plot(pc$scores[,1:2], main='Traffic', pch=19, col = train$type_of_traffic)

legend("topright", fill = unique(train$type_of_traffic), legend = c(levels(train$type_of_traffic)))

x  <- seq(min(pc$scores[,1]), max(pc$scores[,1]), length=300)
y  <- seq(min(pc$scores[,2]), max(pc$scores[,2]), length=300)
xy <- expand.grid(xcoord=x, ycoord=y)

debris.knn <- knn(train = debris[,1:2], test = xy, cl = debris[,3], k = 21)
z  <- as.numeric(debris.knn)

contour(x, y, matrix(z, 300), levels=c(1.5, 2.5), drawlabels=F, add=T)

graphics.off()