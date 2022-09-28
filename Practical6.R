# Practical 6

# Name:Sinenhlanhla Dlamini

# Student Number:u19028386

#Packages:

library(readr)
library(readxl)
library(ggplot2)



# Data:

# The first dataset of interest is:

bank <- read_csv("bank_data.csv")

# The second dataset of interest is:

spirals <- read_excel("spiral.xlsx")

# The third dataset of interest is:

cluster <- read.csv("cluster.csv")

### QUESTION 1 ###
# Question 1a: 
bank$y <- ifelse(bank$y == "yes", 1, 0)
bank$y <- factor(bank$y, levels = c(0,1))
y <- bank$y

# Question 1b:
set.seed(15)
split <- round(nrow(bank)*0.70)
index = sample(1:nrow(bank),split, replace = FALSE)
Q1b_train <- bank[index,]
Q1b_test <- bank[-index,]


# Question 1c:
Q1c <- glm(y ~., Q1b_train, family = "binomial")


# Question 1d:
Q1d <- predict(Q1c, Q1b_test, type = "response")


# Question 1.e:
Q1e <- ifelse(Q1d > 0.5,1,0)


### QUESTION 2 ### 

# Question 2.a:
set.seed(18)
Q2a <- kmeans(spirals, centers = 3, nstart = 20)

# Question 2.b:
wss <- function(k) {
  kmeans(cluster, k, nstart = 10 )$tot.withinss
}

k.max <- 15
wss_value<- sapply(1:k.max,wss)
plot(1:k.max,wss_value, type= "b", xlab = "The Number of clusters K",
     ylab = "Within cluster sum of squares")

Q2b <- 3
### QUESTION 3 ###



# Question 3:
set.seed(72)
Q3a <- kmeans(cluster, centers = 2, nstart = 20)


Q3b <- kmeans(cluster, centers = 3, nstart = 20)


Q3c <- kmeans(cluster, centers = 4, nstart = 20)

Q3d <- kmeans(cluster, centers = 5, nstart = 20)


Q3e <- Q3c
