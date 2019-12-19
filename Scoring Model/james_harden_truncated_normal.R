library(readxl)
library(truncdist)
library(truncnorm)

#Get James Harden scoring stats
stats <- read_excel("C:\\Users\\chris\\Desktop\\NBA-Data-and-Models\\Scoring Model\\james_harden_2018.xlsx")

#Get points and remove any NAs
points <- na.omit(stats$PTS)

#Visually check histogram of points
hist(points)

#Get mean and SD of his scoring
mean_points <- mean(points)
sd_points <- sd(points)

#Model using truncated normal
x <- seq(0, 100, length=1000)
hx <- dtruncnorm(x, a=0, mean = mean_points, sd = sd_points)

#Graph histogram vs. estimated density
h<-hist(points, breaks=10, col="red", xlab="Points Scored",
        main="Histogram with \n Truncated Normal Curve", freq=FALSE)

lines(x, hx, col="blue", lwd=2)

hx_2 <- dtrunc(x, spec = "t", a=0, df =2)

#Graph histogram vs. estimated density
h<-hist(points, breaks=10, col="red", xlab="Points Scored",
        main="Histogram with \n Truncated Normal Curve", freq=FALSE)

lines(x, hx_2, col="blue", lwd=2)
     
