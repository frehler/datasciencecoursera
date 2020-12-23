data <- read.csv("resources/hw1_data.csv")
names(data)
data[1:2,]
nrow(data)
tail(data)
data$Ozone[47]
sum(is.na(data$Ozone))
summary(data)       
mean(subset(data, Ozone > 31 & Temp > 90)$Solar.R)
mean(subset(data, Month == 6)$Temp)
max(subset(data, Month == 5)$Ozone, na.rm=TRUE)

cube <- function(x, n){
  x^3
}
cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}

z <- 10
f <- function(x){
  g <- function(y){
    y + z
  }
  z <- 4
  x + g(x)
}
f(3)

x <- 5
y <- if(x < 3){
  NA
} else {
  10
}
y

values <- c(1,2,3,4)
for(v in values) print(v)

m <- matrix(1:6, 2, 3)
m
